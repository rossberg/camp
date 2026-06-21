(* Playlist UI *)

type state = State.t


(* Helpers *)

let refresh_delay = 9

let (.$()) = Iarray.get

let start_time = Unix.gettimeofday ()
let time () = Unix.gettimeofday () -. start_time


let rec log10 n = if n < 10 then 0 else 1 + log10 (n / 10)


let fmt = Printf.sprintf

let fmt_time t =
  let t' = int_of_float (Float.trunc t) in
  fmt "%d:%02d" (t' / 60) (t' mod  60)

let _fmt_time2 t =
  let t' = int_of_float (Float.trunc t) in
  fmt "%02d:%02d" (t' / 60) (t' mod  60)

let fmt_time3 t =
  let t' = int_of_float (Float.trunc t) in
  if t' < 3600 then
    fmt_time t
  else
    fmt "%d:%02d:%02d" (t' / 3600) (t' / 60 mod 60) (t' mod  60)


(* Runner *)

let convert_sorting columns sorting =
  let index attr = Iarray.find_index (fun (a, _) -> a = attr) columns in
  List.map (fun (attr, order) -> Option.get (index attr), order) sorting


let run (st : state) =
  let pl = st.playlist in
  let geo = st.geometry in
  let win = Ui.window geo.ui in
  let tab = pl.table in
  let len = Table.length tab in
  let now = Unix.time () in

  Layout.playlist_pane geo;

  (* Playlist table *)
  let _, _, _, h = Ui.dim geo.ui (Layout.playlist_area geo) in
  let page = max 1 (int_of_float (Float.floor (float h /. float geo.text))) in
  let digits_pos = log10 (len + 1) + 1 in
  let digits_time = ref 1 in
  for i = tab.vscroll to min len (tab.vscroll + page) - 1 do
    let time = Track.time tab.entries.(i) in
    if time > 599.4 then
      digits_time := max !digits_time (if time > 5999.4 then 3 else 2)
  done;
  let font = Ui.font geo.ui geo.text in
  let s_pos = String.make digits_pos '0' ^ "." in
  let s_time = String.make !digits_time '0' ^ ":00" in
  let cw_pos = Api.Draw.text_width win geo.text font s_pos + 1 in
  let cw_time = Api.Draw.text_width win geo.text font s_time + 1 in
(*
  let cols : _ iarray = [|cw_pos, `Right; -1, `Left; cw_time, `Right|] in
*)
  let cols =
    Iarray.map (fun (attr, cw) ->
      let cw' =
        match attr with
        | `Pos -> max cw cw_pos
        | `Length -> max cw cw_time
        | `Name -> -1
        | _ -> cw
      in cw', Library.attr_align attr
    ) pl.view.columns
  in
  let headings =
    Iarray.map (fun (attr, _) -> Library.attr_name attr) pl.view.columns in

  if Api.Draw.frame win mod refresh_delay = 0 then
    Table.dirty tab;  (* to capture track updates *)

  let pp_row i : _ * _ iarray =
    let track = tab.entries.(i) in
    if now -. track.file.age > st.config.delay_track_update then
      Track.update track;
    let c1, normal =
      match track.status with
      | `Det | `Predet -> Ui.text_color geo.ui, true
      | `Undet -> Ui.semilit_color (Ui.text_color geo.ui), true
      | `Invalid -> Ui.warn_color geo.ui, false
      | `Absent -> Ui.error_color geo.ui, false
    in
    let current_path =
      match st.control.current with
      | Some dir -> dir.path
      | None -> ""
    in
    let c2 = if track.path = current_path then `White else `Gray 0xc0 in
    let c =
      if tab.pos <> Some i then c1 else
      if normal then c2 else Api.Color.mix c1 c2
    in
(*
    let time = Track.time track in
    let stime = if time = 0.0 then "" else fmt_time time in
    c, [|
      `Text (fmt "%0*d." digits_pos (i + 1));
      `Text (Track.name track);
      `Text stime
    |]
*)
    c, Iarray.map (fun (attr, _) ->
      match attr with
      | `Cover ->
        if not st.library.covers_shown then `Text "" else
        (match Library.load_cover st.library win track.path with
        | Some img -> `Image img
        | None -> `Text ""
        )
      | _ -> `Text (String.trim (Data.track_attr_string track attr))
    ) pl.view.columns
  in

  let sorting = convert_sorting pl.view.columns pl.view.sorting in
  let header = if geo.playlist_headers then Some (headings, sorting) else None in
  (match Layout.playlist_table geo cols header tab pp_row with
  | `None | `Scroll | `Sort _ -> ()

  | `Resize ws ->
    (* Column resizing: update column widths *)
    pl.view.columns <-
      Iarray.mapi (fun i (attr, w) ->
        attr, if attr = `Name then w else ws.$(i)
      ) pl.view.columns;
    State.save st;

  | `Reorder perm ->
    (* Column reordering: update columns *)
    pl.view.columns <- Data.permute perm pl.view.columns;
    State.save st;

  | `Select ->
    State.focus_playlist st;
    Playlist.refresh_total_selected pl

  | `Click (Some i, _)
    when Api.Mouse.(is_pressed `Left && is_double_click `Left) ->
    (* Double-click on track: switch to track *)
    Playlist.jump pl i;
    Control.switch st.control tab.entries.(i);
    Control.play st.control;
    Table.dirty st.library.tracks;  (* redraw for current track *)
    Table.dirty st.library.browser;

  | `Click loc ->
    (* Single-click: grab focus *)
    if Api.Mouse.is_pressed `Left then
    (
      State.focus_playlist st;
      match loc with
      | Some i, Some j when fst pl.view.columns.$(j) = `Cover ->
        (* Click on cover cell: open cover popup *)
        Run_menu.popup st (`Track tab.entries.(i));
      | _ -> ()
    );
    Playlist.refresh_total_selected pl;

  | `Move delta ->
    (* Cmd-cursor movement: move selection *)
    Playlist.move_selected pl delta;

  | `Drag (delta, motion, traj) ->
    (* Drag: move selection if inside *)
    if Api.Key.are_modifiers_down [] then
    (
      State.focus_playlist st;
      if Playlist.num_selected pl > 0 then
      (
        if motion <> `Unmoved then Run_view.set_drop_cursor st;
        match traj with
        | `Inside | `Inward -> ()
        | `Outward | `Outside ->
          Run_view.drag_on_tracks st;
          Run_library.drag_on_browser st;
      );

      (* Invariant:
       * - on Start: no undo or redo added yet
       * - when Inside: one undo for returning to original state on undo stack
       * - when Outside: one redo for creating new state on redo stack
       *)
      if motion = `Moving then
      (
        (* Start of drag & drop: remember original configuration *)
        Table.push_undo pl.table;
      );

      (match traj with
      | `Outward ->
        (* Leaving area: snap back to original state *)
        Playlist.undo pl;
        Playlist.save_playlist pl;
      | `Inward ->
        (* Reentering area: restore updated state *)
        Playlist.redo pl
      | `Inside | `Outside -> ()
      );

      (* Positional movement *)
      if delta <> 0 && Playlist.num_selected pl > 0 then
      (
        match traj with
        | `Inside | `Inward ->
          Playlist.move_selected pl delta;
          (* Erase intermediate new state *)
          Table.drop_undo pl.table;
        | `Outside | `Outward -> ()  (* ignore *)
      );
    )

  | `Drop ->
    if Api.Key.are_modifiers_down [] then
    (
      if Ui.mouse_inside geo.ui (Layout.playlist_area geo) then
      (
        (* Dropping inside playlist: drop aux undo if no change *)
        Table.clean_undo pl.table
      )
      else
      (
        (* Dropping outside playlist: drop aux redo for new state *)
        Table.drop_redo pl.table;

        let tracks = Playlist.selected pl in
        Run_view.drop_on_tracks st tracks;
        Run_library.drop_on_browser st tracks;
      )
    );

  | `Abort ->
    if Api.Key.are_modifiers_down [] then
    (
      if Ui.mouse_inside geo.ui (Layout.playlist_area geo) then
      (
        (* Aborting inside playlist: snap back to original state *)
        Playlist.undo pl;
        Playlist.save_playlist pl;
      )
      else
      (
        (* Aborting outside playlist: drop aux redo for new state *)
        Table.drop_redo pl.table;
      )
    )

  | `Menu (i_opt, _) ->
    (* Right-click on content: context menu *)
    State.focus_playlist st;
    Playlist.refresh_total_selected pl;
    let search =
      match i_opt with
      | Some i when not (Data.is_separator tab.entries.(i)) ->
        Track.split_name (Track.name tab.entries.(i))
      | _ -> []
    in
    Run_view.(edit_menu st (playlist_view st) search i_opt)

  | `HeadMenu i_opt ->
    (* Right-click on header: header menu *)
    State.focus_playlist st;
    let current_attrs = Iarray.to_list (Iarray.map fst pl.view.columns) in
    let unused_attrs = Data.diff_attrs Data.track_attrs current_attrs in
    (* Cannot remove Pos or Name *)
(*
    let used_attrs = Data.diff_attrs current_attrs (`Pos :: `Name :: unused_attrs) in
*)
    let i = Option.value i_opt ~default: (Iarray.length pl.view.columns) in
    let removable_attrs =
      if i_opt = None then [] else
      let attr = fst pl.view.columns.$(i) in
      if attr = `Pos || attr = `Name then [] else [attr]
    in
    Run_menu.header_menu st pl.view i removable_attrs unused_attrs
      (Some (fun () -> geo.playlist_headers <- false))
  );

  if geo.popup_shown <> None && Api.Mouse.is_down `Left then
  (
    match Layout.playlist_mouse geo cols tab with
    | Some (Some i, _) ->
      (* Drag with active cover popup: update cover *)
      Run_menu.popup st (`Track tab.entries.(i));
    | _ -> ()
  );

  (* Playlist file drag & drop *)
  Run_view.external_drop_on_playlist st;

  (* Playlist total *)
  if int_of_float (time ()) mod 10 = 0 then Playlist.refresh_total pl;
  let fmt_total (t, n) = fmt_time3 t ^ if n > 0 then "+" else "" in
  let s1 =
    if pl.total_selected = (0.0, 0) then "" else
    fmt_total pl.total_selected ^ "/"
  in
  let s2 = fmt_total pl.total in
  Layout.playlist_total_box geo;
  Layout.playlist_total_text geo `Regular true (s1 ^ s2)
