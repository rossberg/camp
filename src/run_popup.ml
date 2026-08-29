(* Run Popup UI *)

type state = State.t


(* Cover creation *)

let cover (st : state) cov =
  Popup.set_cover st.popup cov;
  st.geometry.popup_shown <- Some (Api.Mouse.pos (Ui.window st.geometry.ui));
  Ui.modal st.geometry.ui "popup.cover"


(* Custom Column edit creation *)

let edit_custom (st : state) (view : _ Library.view) mouse kind attrs pos name s =
  Popup.set_custom st.popup name s
    (fun s -> Result.is_ok (Query.parse_custom kind s))
    (fun name' s' ->
      let _, w = List.nth attrs pos in
      let attr' = `Custom (name', s', ref Data.Unset) in
      let attrs' = List.take pos attrs @ [attr', w] @ List.drop (pos + 1) attrs in
      view.columns <- Iarray.of_list attrs';
      Option.iter (Library.save_dir st.library) st.library.current;
    );
  st.geometry.popup_shown <- Some mouse;
  Ui.modal st.geometry.ui "popup.menu"


(* Menu creation *)

let menu' (st : state) items op =
  Popup.set_menu st.popup items op;
  st.geometry.popup_shown <- Some (Api.Mouse.pos (Ui.window st.geometry.ui));
  Ui.modal st.geometry.ui "popup.menu"


let command_menu st cmds =
  menu' st (Iarray.map fst cmds) (fun k -> snd (Iarray.get cmds k) ())


(* Header Menu creation *)

let header_menu (st : state) (view : _ Library.view) kind
  pos current_attrs unused_attrs hide =
  let mouse = Api.Mouse.pos (Ui.window st.geometry.ui) in
  let c = Ui.text_color st.geometry.ui in
  let custom =
    if pos >= Iarray.length view.columns then None else
    match Iarray.get view.columns pos with
    | `Custom (l, s, _), _ -> Some (l, s)
    | _ -> None
  in
  let removes =
    current_attrs |>
    List.map (fun a ->
      `Entry (c, "Remove " ^ Library.attr_name a, Layout.nokey, true), a)
    |> List.sort compare
  and edits =
    current_attrs |>
    List.map (fun a ->
      `Entry (c, "Edit " ^ Library.attr_name a ^ "...", Layout.nokey,
        custom <> None))
    |> List.sort compare
  and adds =
    unused_attrs |>
    List.map (fun a ->
      `Entry (c, "Add " ^ Library.attr_name a, Layout.nokey, true), a)
    |> List.sort compare
  and customs =
    [ `Entry (c, "Add Custom Column...", Layout.nokey, true) ]
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
          edit_custom st view mouse kind attrs pos l s
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
        edit_custom st view mouse kind attrs' i "" "";
      )
    )
  in
  let items2 : _ iarray =
    if hide = None then [||] else
    [|
      `Separator;
      `Entry (c, "Hide Column Headers", Layout.nokey, true)
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

  match Layout.menu geo x y menu.hscroll menu.vscroll menu.items with
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


let run_cover (st : state) (cover : Popup.cover) =
  let geo = st.geometry in
  let ctl = st.control in
  let x, y = Option.get geo.popup_shown in

  let cover_opt =
    match cover with
    | Track _ | Album _ as cover -> Some cover
    | Current -> Option.map (fun track -> Popup.Track track) ctl.current
  in
  Option.iter (fun cover ->
    let path, artist, title, year, num =
      match cover with
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
    let img = Option.value ~default: (Ui.nocover geo.ui)
      (Library.load_cover st.library (Ui.window geo.ui) path) in
    let iw, ih = Layout.cover_popup_image_size geo img in
    Layout.cover_popup geo (x, y, iw, ih);
    let text =
      artist ^ " - " ^ title ^
      (if year = "" then "" else " (" ^ year ^ ")") ^
      (if num = "" then "" else ", track " ^ num)
    in
    Layout.cover_popup_image geo img;
    Layout.cover_popup_text geo text;
  ) cover_opt;

  if cover_opt = None
  || cover = Popup.Current && Control.silent ctl &&
      List.mem (Control.status ctl) [`Stopped; `Ejected]
  || Api.Mouse.(is_released `Left || is_pressed `Right) then
  (
    Ui.nonmodal geo.ui "run.cover";
    geo.popup_shown <- None;
    Popup.clear st.popup;
  )


let run_custom (st : state) (custom : Popup.custom) =
  let geo = st.geometry in
  let x, y = Option.get geo.popup_shown in

  Layout.custom_popup geo x y;
  Layout.custom_popup_name_label geo;
  Layout.custom_popup_text_label geo;
  Layout.custom_popup_name_box geo;
  Layout.custom_popup_text_box geo;

  Ui.nonmodal geo.ui "run.custom";

  let name = custom.name.text in
  let expr = custom.expr.text in
  let expr_color =
    Ui.(if custom.valid custom.expr.text then text_color else error_color) in
  let _ = Layout.custom_popup_name_edit geo (Ui.text_color geo.ui) custom.name in
  if custom.name.focus then Edit.defocus custom.expr;
  let _ = Layout.custom_popup_text_edit geo (expr_color geo.ui) custom.expr in
  if custom.expr.focus then Edit.defocus custom.name;

  let ok = Layout.custom_popup_ok_button geo in
  let cancel = Layout.custom_popup_cancel_button geo in

  if custom.name.text <> name || custom.expr.text <> expr then
    custom.ok custom.name.text custom.expr.text;  (* update live *)

(*TODO: replace Ok/Cancel buttons with Done button, or none at all? *)
(* Minimalist: use table header/row look for edits, no labels no buttons *)
  if ok || cancel then
  (
    geo.popup_shown <- None;
    Popup.clear st.popup;
  )
  else
  (
    Ui.modal geo.ui "run.custom";
  )


let run (st : state) =
  match st.popup.kind with
  | None -> ()
  | Some (`Cover cover) -> run_cover st cover
  | Some (`Menu menu) -> run_menu st menu
  | Some (`Custom custom) -> run_custom st custom
