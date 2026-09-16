(* Run Popup UI *)

type state = State.t


(* Cover creation *)

let zoom (st : state) zoom =
  Popup.set_zoom st.popup zoom;
  st.geometry.popup_shown <- Some (Api.Mouse.pos (Ui.window st.geometry.ui));
  Ui.modal st.geometry.ui "popup.zoom"


(* Custom Column edit creation *)

let edit_custom (st : state) (tab : _ Table.t) (view : _ Library.view) mouse kind attrs pos name s =
  Popup.set_custom st.popup name s
    (fun s ->
      match Query.parse_custom kind s with
      | Ok _ -> Library.error st.library ""; true
      | Error s -> Library.error st.library s; false
    )
    (fun name' s' ->
      let attr, w = List.nth attrs pos in
      let attr' = `Custom (name', s', ref Data.Unset) in
      let attrs' =
        List.take pos attrs @ [attr', w] @ List.drop (pos + 1) attrs in
      view.columns <- Iarray.of_list attrs';
      view.sorting <-
        List.map (fun (a, o) -> (if a = attr then attr' else a), o) view.sorting;
      Table.dirty tab;
      Option.iter (Library.save_dir st.library) st.library.current
    );
  st.geometry.popup_shown <- Some mouse;
  Ui.modal st.geometry.ui "popup.custom"


(* Menu creation *)

let menu' (st : state) items op =
  Popup.set_menu st.popup items op;
  st.geometry.popup_shown <- Some (Api.Mouse.pos (Ui.window st.geometry.ui));
  Ui.modal st.geometry.ui "popup.menu"


let command_menu st cmds =
  menu' st (Iarray.map fst cmds) (fun k -> snd (Iarray.get cmds k) ())


(* Header Menu creation *)

let header_menu (st : state) (tab : _ Table.t) (view : _ Library.view) kind
  pos current_attrs unused_attrs hide =
  let mouse = Api.Mouse.pos (Ui.window st.geometry.ui) in
  let c = Ui.text_color st.geometry.ui in
  let current_attrs' =
    if current_attrs <> [] then current_attrs else
    if pos >= Iarray.length view.columns then [] else
    [fst (Iarray.get view.columns pos)]
  and custom =
    if pos >= Iarray.length view.columns then None else
    match Iarray.get view.columns pos with
    | `Custom (l, s, _), _ -> Some (l, s)
    | _ -> None
  in
  let removes =
    current_attrs' |>
    List.map (fun a ->
      `Entry (c, "Remove " ^ Library.attr_name a, Layout.KeyBind.na,
         current_attrs <> []), a)
    |> List.sort compare
  and edits =
    current_attrs' |>
    List.map (fun a ->
      `Entry (c, "Edit " ^ Library.attr_name a ^ "...", Layout.KeyBind.na,
        custom <> None))
    |> List.sort compare
  and adds =
    unused_attrs |>
    List.map (fun a ->
      `Entry (c, "Add " ^ Library.attr_name a, Layout.KeyBind.na, true), a)
    |> List.sort compare
  and customs =
    [ `Entry (c, "Add Custom Column...", Layout.KeyBind.na, true) ]
  in
  let sep = if removes = [] then [] else [`Separator] in
  let (items1 : _ iarray), f =
    Iarray.of_list
      List.(map fst removes @ edits @ sep @ map fst adds @ customs),
    (fun k ->
      let n = List.length removes in
      let n' = if removes = [] then 0 (* no sep *) else 2*n + 1 in
      let attrs = Iarray.to_list view.columns in
      if k < n then
      (
        (* Remove entry *)
        let _, attr = List.nth removes k in
        view.sorting <- List.filter (fun (a, _) -> a <> attr) view.sorting;
        let attrs' = List.filter (fun (a, _) -> a <> attr) attrs in
        view.columns <- Iarray.of_list attrs';
        Option.iter (Library.save_dir st.library) st.library.current;
      )
      else if k < 2*n then
      (
        (* Edit entry *)
        Option.iter (fun (l, s) ->
          edit_custom st tab view mouse kind attrs pos l s
        ) custom;
      )
      else if k >= n' && k - n' < List.length adds then
      (
        (* Add entry *)
        let _, attr = List.nth adds (k - n') in
        let i = min (pos + 1) (List.length attrs) in
        let attrs' = List.take i attrs @ [attr, 40] @ List.drop i attrs in
        view.columns <- Iarray.of_list attrs';
        Option.iter (Library.save_dir st.library) st.library.current;
      )
      else
      (
        (* Add custom entry *)
        let attr = `Custom ("", "", ref Data.Unset) in
        let i = min (pos + 1) (List.length attrs) in
        let attrs' = List.take i attrs @ [attr, 40] @ List.drop i attrs in
        edit_custom st tab view mouse kind attrs' i "" "";
      )
    )
  in
  let items2 : _ iarray =
    if hide = None then [||] else
    [|
      `Separator;
      `Entry (c, "Hide Column Headers", Layout.KeyBind.na, true)
    |];
  in
  menu' st (Iarray.append items1 items2) (fun k ->
    if k < Iarray.length items1 then
      f k
    else
      Option.iter ((|>) ()) hide
  )


(* Runners *)

let run_menu (st : state) (menu : Popup.menu) =
  let geo = st.geometry in
  let pop = st.popup in
  let x, y = Option.get geo.popup_shown in

  let (module WindowUi) = Option.get st.layout in
  let module MenuUi = WindowUi.Menu () in

  match MenuUi.menu x y menu.hscroll menu.vscroll menu.items with
  | `None -> ()

  | `Close ->
    geo.popup_shown <- None;
    Popup.clear pop

  | `Click k ->
    let op = menu.op in
    geo.popup_shown <- None;
    Popup.clear pop;
    op k

  | `Scroll (h, v) ->
    menu.hscroll <- h;
    menu.vscroll <- v


let run_zoom (st : state) (zoom : Popup.zoom) =
  let geo = st.geometry in
  let ctl = st.control in

  let resizing = ref false in
  let zoom_opt =
    match zoom with
    | Track _ | Album _ as zoom -> Some zoom
    | Current -> Option.map (fun track -> Popup.Track track) ctl.current
  in
  Option.iter (fun zoom' ->
    let path, artist, title, year, num =
      match zoom' with
      | Popup.Track track ->
        let artist = Data.track_attr_string track `Artist in
        let title = Data.track_attr_string track `Title in
        let aartist = Data.track_attr_string track `AlbumArtist in
        let atitle = Data.track_attr_string track `AlbumTitle in
        let num = String.trim (Data.track_attr_string track `DiscTrack) in
        track.path, aartist, atitle, Data.track_attr_string track `Year,
        (if aartist = artist && atitle = title then "" else num)

      | Popup.Album (album : Data.album) ->
        album.path,
        Data.album_attr_string album `AlbumArtist,
        Data.album_attr_string album `AlbumTitle,
        Data.album_attr_string album `Year,
        ""

      | Popup.Current -> assert false
    in
    let img_opt = Library.load_cover st.library (Ui.window geo.ui) path in
    let var = (zoom = Current) in
    let vis = if zoom = Current then ctl.zoom else `Cover in
    let size w =
      match vis with
      | `Cover ->
        let img = Option.value img_opt ~default: (Ui.nocover geo.ui) in
        Ui.image_size geo.ui (w, w) `Shrink img
      | `Turntable | `Oscilloscope -> w, w
      | `Spectrum | `Waveform -> w, w/2
    in
    let inv_size (w, h) =
      match vis with
      | `Cover ->
        let img = Option.value img_opt ~default: (Ui.nocover geo.ui) in
        let w', h' = Ui.image_size geo.ui (w, w) `Shrink img in
        if w' = w then w else h
      | `Turntable | `Oscilloscope -> w
      | `Spectrum | `Waveform -> w
    in

    let (module WindowUi) = Option.get st.layout in
    let module Zoom = WindowUi.Zoom(struct let size = size let var = var end) in

    let text =
      artist ^ " - " ^ title ^
      (if year = "" then "" else " (" ^ year ^ ")") ^
      (if num = "" then "" else ", track " ^ num)
    in
    Run_visualization.run st Zoom.image_area vis img_opt;
    Zoom.text text;

    (* Resize *)
    if zoom = Current then
    (
      Option.iter (fun ((x', y', w', h'), (lft, top, rgt, bot)) ->
        resizing := true;

        let ww, wh = Api.Window.size (Ui.window geo.ui) in
        let w, h = size geo.zoom_size in

        (* Correct ratio *)
        let ratio = float w /. float h in
        let ratio' = float w' /. float h' in
        let w'', h'' =
          let adapt_w () = int_of_float (float h' *. ratio), h' in
          let adapt_h () = w', int_of_float (float w' /. ratio) in
          if (lft || rgt) && not (top || bot) then
            adapt_h ()
          else if (top || bot) && not (lft || rgt) then
            adapt_w ()
          else if ratio' >= ratio then
            adapt_h ()
          else
            adapt_w ()
        in

        (* Adjust position *)
        let to_top = top || not bot && y' >= wh - h'' in
        let to_lft = lft || not rgt && x' >= ww - w'' in
        let x'' = max 0 (x' - if to_lft then w'' - w' else 0) in
        let y'' = max 0 (y' - if to_top then h'' - h' else 0) in

        geo.popup_shown <- Some (x'', y'');
        geo.zoom_size <- inv_size (w'', h'');
      ) Zoom.resize
    )
  ) zoom_opt;

  if zoom_opt = None
  || zoom = Current && Control.silent ctl &&
      List.mem (Control.status ctl) [`Stopped; `Ejected]
  || not !resizing && Api.Mouse.(is_released `Left || is_pressed `Right) then
  (
    Ui.nonmodal geo.ui "run.zoom";
    geo.popup_shown <- None;
    Popup.clear st.popup;
  )


let run_custom (st : state) (custom : Popup.custom) =
  let geo = st.geometry in

  let (module WindowUi) = Option.get st.layout in
  let module CustomUi = WindowUi.Custom () in

  Ui.nonmodal geo.ui "run.custom";

  let name = custom.name.text in
  let expr = custom.expr.text in
  let expr_color =
    Ui.(if custom.valid custom.expr.text then text_color else error_color) in

  CustomUi.Name.box ();
  let _ = CustomUi.Name.edit custom.name in
  if custom.name.focus then Edit.defocus custom.expr;

  CustomUi.Text.box ();
  let _ = CustomUi.Text.edit (expr_color geo.ui) custom.expr in
  if custom.expr.focus then Edit.defocus custom.name;

  let ok = CustomUi.Key.ok () in
  let cancel = CustomUi.Key.cancel () in

  if custom.name.text <> name || custom.expr.text <> expr then
    custom.ok custom.name.text custom.expr.text;  (* update live *)

  if ok || cancel then
  (
    geo.popup_shown <- None;
    Popup.clear st.popup;
    Library.error st.library "";
  )
  else
  (
    Ui.modal geo.ui "run.custom";
  )


let run (st : state) =
  match st.popup.kind with
  | None -> ()
  | Some (`Zoom zoom) -> run_zoom st zoom
  | Some (`Menu menu) -> run_menu st menu
  | Some (`Custom custom) -> run_custom st custom
