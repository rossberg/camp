(* Run Menu UI *)

type state = State.t


(* Menu creation *)

let menu' (st : state) items op =
  st.geometry.menu_shown <- true;
  Menu.set st.menu (Api.Mouse.pos (Ui.window st.geometry.ui)) op items;
  Ui.modal st.geometry.ui


let command_menu st cmds =
  menu' st (Iarray.map fst cmds) (fun k -> snd (Iarray.get cmds k) ())


let header_menu (st : state) (view : _ Library.view) i current_attrs unused_attrs =
  let c = Ui.text_color st.geometry.ui in
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
    let items = Iarray.of_list List.(map fst removes @ sep @ map fst adds) in
    menu' st items (fun k ->
      let n = if removes = [] then -1 (* no sep! *) else List.length removes in
      let attrs = Iarray.to_list view.columns in
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
      view.columns <- Iarray.of_list attrs';
      Option.iter (Library.save_dir st.library) st.library.current
    )
  )


(* Pop-up creation *)

let popup (st : state) track_opt =
  st.popup <- track_opt;
  st.geometry.popup_shown <- Some (Api.Mouse.pos (Ui.window st.geometry.ui));
  Ui.modal st.geometry.ui


(* Runners *)

let run (st : state) =
  let geo = st.geometry in
  let menu = st.menu in
  let x, y = menu.pos in

  match Layout.menu geo x y menu.items with
  | `None -> ()

  | `Close ->
    Ui.nonmodal geo.ui;
    geo.menu_shown <- false;
    Menu.clear menu

  | `Click k ->
    Option.get st.menu.op k;
    Ui.nonmodal geo.ui;
    geo.menu_shown <- false;
    Menu.clear menu


let run_popup (st : state) =
  let geo = st.geometry in
  let ctl = st.control in

  Option.iter (fun (x, y) ->
    let popup =
      match st.popup with
      | `Track _ | `Album _ as popup -> Some popup
      | `Current -> Option.map (fun track -> `Track track) ctl.current
    in
    Option.iter (fun popup ->
      let path, artist, title, year, num =
        match popup with
        | `Track track ->
          let artist = Data.track_attr_string track `Artist in
          let title = Data.track_attr_string track `Title in
          let aartist = Data.track_attr_string track `AlbumArtist in
          let atitle = Data.track_attr_string track `AlbumTitle in
          let num = String.trim (Data.track_attr_string track `DiscTrack) in
          track.path, aartist, atitle, Data.track_attr_string track `Year,
          (if aartist = artist && atitle = title then "" else num)

        | `Album (album : Data.album) ->
          album.path,
          Data.album_attr_string album `AlbumArtist,
          Data.album_attr_string album `AlbumTitle,
          Data.album_attr_string album `Year,
          ""
      in
      let img = Option.value ~default: (Ui.nocover geo.ui)
        (Library.load_cover st.library (Ui.window geo.ui) path) in
      let iw, ih = Layout.cover_popup_image_size geo img in
      let area = Layout.cover_popup geo x y iw ih in
      let text =
        artist ^ " - " ^ title ^
        (if year = "" then "" else " (" ^ year ^ ")") ^
        (if num = "" then "" else ", track " ^ num)
      in
      Layout.cover_popup_image geo area img;
      Layout.cover_popup_text geo area ih text;
    ) popup;

    if popup = None
    || st.popup = `Current && Control.silent ctl &&
        List.mem (Control.status ctl) [`Stopped; `Ejected]
    || Api.Mouse.(is_released `Left || is_pressed `Right) then
    (
      Ui.nonmodal geo.ui;
      geo.popup_shown <- None;
    )
  ) geo.popup_shown
