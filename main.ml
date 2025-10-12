(* Main Program *)

open Audio_file


(* State *)

type state = State.t


(* Runner *)

let queue_file = "queue.m3u"

let rec run (st : state) =
  State.ok st;
  (try run' st with exn -> Storage.log_exn "internal" exn ""; exit 0);
  run st

and run' (st : state) =
  let lay = st.layout in
  let win = Ui.window lay.ui in
  if Api.Window.closed win then Run_control.quit st;

  (* App invocation with arguments *)
  let m3u = ref "" in
  Storage.load_string queue_file ((:=) m3u);
  if !m3u <> "" then
  (
    (* TODO: this could race, should lock the file *)
    Storage.save_string queue_file (fun () -> "");
    Run_view.external_queue_on_playlist st (M3u.parse !m3u);
  );

  (* Start drawing *)
  Ui.start lay.ui;

  (* Remember current geometry for later *)
  let playlist_shown = lay.playlist_shown in
  let library_shown = lay.library_shown in
  let filesel_shown = lay.filesel_shown in
  let overlay_shown = library_shown || filesel_shown in
  let menu_shown = lay.menu_shown in
  let popup_shown = lay.popup_shown <> None in
  let library_side = lay.library_side in
  let library_width = lay.library_width in

  (* Update geometry *)
  let ww, wh = Api.Window.size win in
  if playlist_shown then lay.playlist_height <- wh - Layout.control_h lay;
  if overlay_shown then lay.library_width <- ww - Layout.control_w lay;

  (* Run panes *)
  Run_control.run st;
  if not (Api.Window.is_minimized win) then
  (
    if playlist_shown then Run_playlist.run st;
    if filesel_shown then Run_filesel.run st
    else if library_shown then Run_library.run st;
    if playlist_shown || overlay_shown then Run_view.run_edit_panel st;
    Run_control.run_toggle_panel st;
    if menu_shown then Run_menu.run st;
    if popup_shown then Run_menu.run_popup st;
  );
  List.iter (fun f -> f ()) st.delayed;
  st.delayed <- [];

  (* Adjust font and grid size *)
  let text_delta =
    Bool.to_int (Layout.enlarge_text_key lay) -
    Bool.to_int (Layout.reduce_text_key lay)
  in
  Run_control.resize_text st text_delta;

  let grid_delta =
    Bool.to_int (Layout.enlarge_grid_key lay) -
    Bool.to_int (Layout.reduce_grid_key lay)
  in
  Run_control.resize_grid st grid_delta;

  let is_modal = Ui.is_modal lay.ui in
  if is_modal then Ui.nonmodal lay.ui;  (* temporarily enable keys *)
  let popup_delta =
    Bool.to_int (Layout.enlarge_popup_key lay) -
    Bool.to_int (Layout.reduce_popup_key lay)
  in
  if is_modal then Ui.modal lay.ui;  (* reenable keys *)
  lay.popup_size <- Layout.(clamp min_popup_size max_popup_size
    (lay.popup_size + 100 * popup_delta));

  let scale_delta =
    Bool.to_int (Layout.enlarge_scale_key lay) -
    Bool.to_int (Layout.reduce_scale_key lay)
  in
  let scale_old = Api.Window.scale win in
  Ui.rescale lay.ui scale_delta scale_delta;
  let scale_new = Api.Window.scale win in
  lay.scaling <-
    snd lay.scaling + (fst scale_new - fst scale_old),
    snd lay.scaling + (snd scale_new - snd scale_old);

  if Layout.lib_cover_key lay then
    Library.activate_covers st.library (not st.library.covers_shown);

  (* Adjust window size *)
  let overlay_shown' = lay.library_shown || lay.filesel_shown in
  let extra_w = if overlay_shown' then lay.library_width else 0 in
  let extra_h =
    if lay.playlist_shown then lay.playlist_height else
    if overlay_shown' then Layout.bottom_h lay else 0
  in
  Api.Window.set_size win
    (Layout.control_w lay + extra_w) (Layout.control_h lay + extra_h);

  (* Adjust window position after opening/closing library *)
  let dx =
    match overlay_shown, overlay_shown', library_side, lay.library_side with
    | false, true, _, `Left
    | true, true, `Right, `Left -> -lay.library_width  (* opened on the left *)
    | true, false, `Left, _
    | true, true, `Left, `Right -> +library_width      (* closed on the left *)
    | _ -> 0
  in
  let x, y = Api.Window.pos win in
  if dx <> 0 then Api.Window.set_pos win (x + dx) y;

  (* Finish drawing *)
  let minw, maxw =
    if overlay_shown
    then Layout.(control_w lay + library_min lay, -1)
    else Layout.(control_w lay, control_w lay)
  and minh, maxh =
    if playlist_shown
    then Layout.(control_h lay + playlist_min lay, -1)
    else Layout.(control_h lay, control_h lay)
  in
  Ui.finish lay.ui (Layout.margin lay) (minw, minh) (maxw, maxh);

  if Api.Window.is_hidden win then  (* after startup *)
    Api.Window.reveal win;

  (* Save state regularly every second *)
  State.save_after st 1.0;

  (* Trigger major GC all 600 frames (GC pacing doesn't keep up otherwise) *)
  if Api.Draw.frame win mod 600 = 0 then
  (
    let gc = Gc.stat () in  (* triggers GC *)
    (* Trigger GC compaction if worthwhile (metric suggested by KC) *)
    if Control.status st.control <> `Playing
    && gc.live_words > 1024 * 1024 * 16
    && gc.free_words / gc.live_words > 4 then
      Gc.compact ();
  )


(* Startup *)

let startup () =
  Storage.clear_temp ();
  let audio = Api.Audio.init () in
  let win = Api.Window.init 0 0 0 0 App.name in
  Api.Window.hide win;  (* hide during initialisation *)
  let ui = Ui.make win in
  let st0 = State.make ui audio in
  let success, (x, y) = State.load st0 in
  let st = if success then st0 else State.make ui audio in
  let w = Layout.control_min_w + st.layout.library_width in
  let h = Layout.control_min_h + st.layout.playlist_height in
  Api.Draw.start win `Black;
  Api.Window.set_pos win x y;
  Api.Window.set_size win w h;
  Api.Draw.finish win;
  at_exit (fun () ->
    Api.Audio.pause st.control.audio;
    State.save st;
    Storage.delete queue_file;
    Storage.clear_temp ();
  );
  st

let _main =
  try
    let paths = ref [] in
    Printexc.record_backtrace true;
    Arg.parse ["--dperf", Arg.Set App.debug_perf, "Log times"]
      (fun path -> paths := path :: !paths) "";
    let m3u = M3u.make (List.rev !paths) in
    (* Work around seeming bug in GC scheduler. *)
    Gc.(set {(get ()) with space_overhead = 10});
    if Storage.exists queue_file then
    (
      (* TODO: this could race, should lock the file *)
      Storage.save_string_append queue_file (fun () -> m3u);
      let t1 = Storage.time queue_file in
      Unix.sleepf 1.0;
      let t2 = Storage.time queue_file in
      (* If file has not been modified after 1s, assume it's a zombie. *)
      if t1 = t2 then run (startup ());
    )
    else
    (
      Storage.save_string queue_file (fun () -> m3u);
      run (startup ());
    )
  with exn ->
    Storage.log_exn "internal" exn "";
    Stdlib.exit 2
