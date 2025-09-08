(* Run Menu UI *)

type state = State.t


(* Menu creation *)

let menu' (st : state) items op =
  st.layout.menu_shown <- true;
  Menu.set st.menu (Api.Mouse.pos (Ui.window st.layout.ui)) op items;
  Ui.modal st.layout.ui


let command_menu st cmds =
  menu' st (Array.map fst cmds) (fun k -> snd cmds.(k) ())


let header_menu (st : state) (view : _ Library.view) i current_attrs unused_attrs =
  let c = Ui.text_color st.layout.ui in
  if current_attrs = [] && unused_attrs = [] then
  (
    menu' st [|`Entry (c, "(Nothing to add)", Layout.nokey, false)|] ignore
  )
  else
  (
    let removes = current_attrs |>
      List.map (fun a ->
        `Entry (c, "Remove " ^ Library.attr_name a, Layout.nokey, true), a)
      |> List.sort compare in
    let adds = unused_attrs |>
      List.map (fun a ->
        `Entry (c, "Add " ^ Library.attr_name a, Layout.nokey, true), a)
      |> List.sort compare in
    let sep = if removes = [] || adds = [] then [] else [`Separator] in
    let items = Array.of_list List.(map fst removes @ sep @ map fst adds) in
    menu' st items (fun k ->
      let n = if removes = [] then -1 (* no sep! *) else List.length removes in
      let attrs = Array.to_list view.columns in
      let attrs' =
        match compare k n with
        | -1 ->  (* remove entry *)
          let _, attr = List.nth removes k in
          view.sorting <- List.filter (fun (a, _) -> a <> attr) view.sorting;
          List.filter (fun (a, _) -> a <> attr) attrs
        | +1 when adds <> [] ->  (* add entry *)
          let _, attr = List.nth adds (k - n - 1) in
          let i' = min (i + 1) (List.length attrs) in
          List.take i' attrs @ [attr, 40] @ List.drop i' attrs
        | _ ->  (* separator *)
          attrs
      in
      view.columns <- Array.of_list attrs';
      Option.iter (Library.save_dir st.library) st.library.current
    )
  )


(* Pop-up creation *)

let popup (st : state) track_opt =
  st.popup <- track_opt;
  st.layout.popup_shown <- Some (Api.Mouse.pos (Ui.window st.layout.ui));
  Ui.modal st.layout.ui


(* Runners *)

let run (st : state) =
  let lay = st.layout in
  let menu = st.menu in
  let x, y = menu.pos in

  match Layout.menu lay x y menu.items with
  | `None -> ()

  | `Close ->
    Ui.nonmodal lay.ui;
    lay.menu_shown <- false;
    Menu.clear menu

  | `Click k ->
    Option.get st.menu.op k;
    Ui.nonmodal lay.ui;
    lay.menu_shown <- false;
    Menu.clear menu


let parens = function "" -> "" | s -> " (" ^ s ^ ")"

let run_popup (st : state) =
  let lay = st.layout in
  let ctl = st.control in

  Option.iter (fun (x, y) ->
    let popup =
      match st.popup with
      | `Track _ | `Album _ as popup -> Some popup
      | `Current -> Option.map (fun track -> `Track track) ctl.current
    in
    Option.iter (fun popup ->
      let path, text =
        match popup with
        | `Track track ->
          let artist = Data.track_attr_string track `Artist in
          let title = Data.track_attr_string track `Title in
          let aartist = Data.track_attr_string track `AlbumArtist in
          let atitle = Data.track_attr_string track `AlbumTitle in
          let year = Data.track_attr_string track `Year in
          let num = String.trim (Data.track_attr_string track `DiscTrack) in
          let extra =
            if aartist = artist && atitle = title then year else
            if year = "" && num = "" then "" else
            if num = "" then year else
            if year = "" then "track " ^ num else
            year ^ ", track " ^ num
          in track.path, aartist ^ " - " ^ atitle ^ parens extra

        | `Album album ->
          let artist = Data.album_attr_string album `AlbumArtist in
          let title = Data.album_attr_string album `AlbumTitle in
          let year = Data.album_attr_string album `Year in
          album.path, artist ^ " - " ^ title ^ parens year
      in
      let img = Option.value ~default: (Ui.nocover lay.ui)
        (Library.load_cover st.library (Ui.window lay.ui) path) in
      let iw, ih = Layout.cover_popup_image_size lay img in
      let area = Layout.cover_popup lay x y iw ih in
      Layout.cover_popup_image lay area img;
      Layout.cover_popup_text lay area ih text;
    ) popup;

    if popup = None || Api.Mouse.(is_released `Left || is_pressed `Right) then
    (
      Ui.nonmodal lay.ui;
      lay.popup_shown <- None;
    )
  ) lay.popup_shown
