(* Main Program *)

open Audio_file


(* State *)

type state = State.t


(* Runner *)

let queue_file = "queue.m3u"

let t_start = Unix.gettimeofday ()
let t_last = ref 0.0

let rec run (st : state) =
  State.ok st;
  (try run' st with exn -> Storage.log_exn "internal" exn ""; exit 0);
  if !App.debug_perf then
  (
    let t = Unix.gettimeofday () -. t_start in
    if t >= !t_last +. 10.0 then
    (
      t_last := t;
      let gc = Gc.quick_stat () in
      Printf.eprintf
        "[%s] GC memory %#d live, %#d total, %d collections, %d compactions\n%!"
        (Data.string_of_time t)
        gc.live_words gc.heap_words gc.major_collections gc.compactions;
    )
  );
  run st

and run' (st : state) =
  let geo = st.geometry in
  let win = Ui.window geo.ui in
  if Api.Window.closed win then Run_control.quit st;

  (* App invocation with arguments *)
  let m3u = ref "" in
  Storage.load_string queue_file ((:=) m3u);
  if !m3u <> "" then
  (
    (* TODO: this could race, should lock the file *)
    Storage.save_string queue_file (fun () -> "");
    Run_view.external_queue_on_playlist st (M3u.parse !m3u) `QueueAndJump;
  );

  (* Start drawing *)
  Ui.start geo.ui;

  (* Remember current geometry for later *)
  let playlist_shown = geo.playlist_shown in
  let library_shown = geo.library_shown in
  let filesel_shown = geo.filesel_shown in
  let overlay_shown = library_shown || filesel_shown in
  let menu_shown = geo.menu_shown in
  let popup_shown = geo.popup_shown <> None in
  let library_side = geo.library_side in

  (* Update geometry *)
  let ww, wh = Api.Window.size win in
  if playlist_shown then geo.playlist_height <- wh - Geometry.control_h geo;
  if overlay_shown then geo.library_width <- ww - Geometry.control_w geo;

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
    Bool.to_int (Layout.enlarge_text_key geo) -
    Bool.to_int (Layout.reduce_text_key geo)
  in
  Run_control.resize_text st text_delta;

  let grid_delta =
    Bool.to_int (Layout.enlarge_grid_key geo) -
    Bool.to_int (Layout.reduce_grid_key geo)
  in
  Run_control.resize_grid st grid_delta;

  let is_modal = Ui.is_modal geo.ui in
  if is_modal then Ui.nonmodal geo.ui;  (* temporarily enable keys *)
  let popup_delta =
    Bool.to_int (Layout.enlarge_popup_key geo) -
    Bool.to_int (Layout.reduce_popup_key geo)
  in
  if is_modal then Ui.modal geo.ui;  (* reenable keys *)
  geo.popup_size <- Geometry.(clamp min_popup_size max_popup_size
    (geo.popup_size + 100 * popup_delta));

  let scale_delta =
    Bool.to_int (Layout.enlarge_scale_key geo) -
    Bool.to_int (Layout.reduce_scale_key geo)
  in
  let scale_old = Api.Window.scale win in
  Ui.rescale geo.ui (scale_delta, scale_delta);
  let scale_new = Api.Window.scale win in
  let scaling' =
    fst geo.scaling + (fst scale_new - fst scale_old),
    snd geo.scaling + (snd scale_new - snd scale_old)
  in
  if scaling' <> geo.scaling then
  (
    geo.scaling <- scaling';
    Option.iter (fun wingeo ->
      let x, y = Geometry.apply_geo geo wingeo in
      Api.Window.set_pos win x y;
    ) geo.window
  );

  if Layout.lib_cover_key geo then
    Library.activate_covers st.library (not st.library.covers_shown);

  (* Adjust window size after opening/closing panes *)
  let playlist_shown' = geo.playlist_shown in
  let overlay_shown' = Geometry.overlay_shown geo in
  if playlist_shown' <> playlist_shown then
  (
    let s = if playlist_shown' then +1 else -1 in
    Ui.resize geo.ui (-1, -1) (0, s * geo.playlist_height);
  );
  if overlay_shown' <> overlay_shown then
  (
    let s = if overlay_shown' then +1 else -1 in
    let ox = if Geometry.overlay_left geo then Int.max_int else -1 in
    Ui.resize geo.ui (ox, -1) (s * geo.library_width, 0);
  )
  else if overlay_shown' && geo.library_side <> library_side then
  (
    let ox, ox' =
      if Geometry.overlay_left geo then -1, Int.max_int else Int.max_int, -1 in
    Ui.resize geo.ui (ox, -1) (- geo.library_width, 0);
    Ui.resize geo.ui (ox', -1) (+ geo.library_width, 0);
  );

  (* Finish drawing *)
  let win_min = Geometry.(win_min_w geo, win_min_h geo) in
  let win_max = Geometry.(win_max_w geo, win_max_h geo) in
  Ui.finish geo.ui (Geometry.margin geo) win_min win_max (fun scr ->
    Ui.pin geo.ui scr;
    Option.iter (fun wingeo ->
      let w, h = geo.library_width, geo.playlist_height in
      ignore (Geometry.apply_geo geo wingeo);
      let dw = if Geometry.overlay_shown geo then geo.library_width - w else 0 in
      let dh = if geo.playlist_shown then geo.playlist_height - h else 0 in
      (* Substract mouse delta to get position relative to current geometry *)
      Ui.resize geo.ui Api.Mouse.(Api.sub (pos win) (delta win)) (dw, dh)
    ) geo.window
  );

  if Api.Window.is_hidden win then  (* after startup *)
    Api.Window.reveal win;

  geo.window <- Some (Geometry.abstract_geo geo);

  (* Save state regularly every second *)
  State.save_after st 1.0


(* Startup *)

let startup () =
  Storage.clear_temp ();
  let win = Api.Window.init 0 0 0 0 App.name in
  let audio = Api.Audio.init win in
  Api.Window.hide win;  (* hide during initialisation *)
  let ui = Ui.make win in
  let st0 = State.make ui audio in
  let success, (x, y) = State.load st0 in
  let st = if success then st0 else State.make ui audio in
  let w = Geometry.win_w st.geometry in
  let h = Geometry.win_h st.geometry in
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

let args = Arg.align
[
  "-help", Arg.Unit ignore, "";
  "--debug_perf", Arg.Set App.debug_perf, "\tLog execution times";
  "--debug_layout", Arg.Set App.debug_layout, "\tPrint window layout";
]

let _main =
  try
    Printexc.record_backtrace true;
    let paths = ref [] in
    Arg.parse args (fun path -> paths := path :: !paths) "";
    let m3u = M3u.make (List.rev !paths) in
    (* Configure GC very aggressive to avoid giga bytes of memory usage *)
    Gc.(set {(get ()) with space_overhead = 20});
    (* Trigger GC compaction if worthwhile *)
    Domain.spawn (fun () ->
      Unix.sleepf 3600.0;  (* roughly once a minute, assuming 60 fps *)
      let gc = Gc.quick_stat () in
      if gc.free_words > gc.live_words then Gc.compact ()
    ) |> ignore;
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
