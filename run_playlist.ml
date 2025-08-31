(* Playlist UI *)

type state = State.t


(* Helpers *)

let refresh_delay = 9

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

let run (st : state) =
  let pl = st.playlist in
  let lay = st.layout in
  let win = Ui.window lay.ui in
  let tab = pl.table in
  let len = Table.length tab in
  let now = Unix.time () in

  Layout.playlist_pane lay;

  (* Playlist table *)
  let _, _, _, h = Ui.dim lay.ui (Layout.playlist_area lay) in
  let page = max 1 (int_of_float (Float.floor (float h /. float lay.text))) in
  let digits_pos = log10 (len + 1) + 1 in
  let digits_time = ref 1 in
  for i = tab.vscroll to min len (tab.vscroll + page) - 1 do
    let time = Track.time tab.entries.(i) in
    if time > 599.4 then
      digits_time := max !digits_time (if time > 5999.4 then 3 else 2)
  done;
  let font = Ui.font lay.ui lay.text in
  let s_pos = String.make digits_pos '0' ^ "." in
  let s_time = String.make !digits_time '0' ^ ":00" in
  let cw_pos = Api.Draw.text_width win lay.text font s_pos + 1 in
  let cw_time = Api.Draw.text_width win lay.text font s_time + 1 in
  let cols = [|cw_pos, `Right; -1, `Left; cw_time, `Right|] in

  if Api.Draw.frame win mod refresh_delay = 0 then
    Table.dirty tab;  (* to capture track updates *)

  let pp_row i =
    let track = tab.entries.(i) in
    if now -. track.file.age > st.config.delay_track_update then
      Track.update track;
    let c =
      match track.status with
      | _ when tab.pos = Some i ->
        if track.path = (Option.get st.control.current).path then `White else `Gray 0xc0
      | _ when Data.is_separator track -> Ui.text_color lay.ui
      | `Absent -> Ui.error_color lay.ui
      | `Invalid -> Ui.warn_color lay.ui
      | `Undet -> Ui.semilit_color (Ui.text_color lay.ui)
      | `Predet | `Det -> Ui.text_color lay.ui
    in
    let time = Track.time track in
    let stime = if time = 0.0 then "" else fmt_time time in
    c, [|
      `Text (fmt "%0*d." digits_pos (i + 1));
      `Text (Track.name track);
      `Text stime
    |]
  in

  (match Layout.playlist_table lay cols None tab pp_row with
  | `None | `Scroll -> ()
  | `Sort _ | `Resize _ | `Reorder _ | `HeadMenu _ -> assert false

  | `Select ->
    State.focus_playlist st;
    Playlist.refresh_total_selected pl

  | `Click (Some i) when Api.Mouse.is_doubleclick `Left ->
    (* Double-click on track: switch to track *)
    Playlist.jump pl i;
    Control.switch st.control tab.entries.(i) true;
    Table.dirty st.library.tracks;  (* redraw for current track *)
    Table.dirty st.library.browser;

  | `Click _ ->
    (* Single-click: grab focus *)
    if Api.Mouse.is_pressed `Left then State.focus_playlist st;
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
      if Ui.mouse_inside lay.ui (Layout.playlist_area lay) then
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

  | `Menu i_opt ->
    (* Right-click on content: context menu *)
    State.focus_playlist st;
    Run_view.(edit_menu st (playlist_view st) i_opt)
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
  Layout.playlist_total_box lay;
  Layout.playlist_total_text lay `Regular true (s1 ^ s2)
