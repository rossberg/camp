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
    (* If we're just starting up, double-click caused it, force playing *)
    if Api.Draw.frame win <= 1 then Control.stop st.control;
    Run_view.external_queue_on_playlist st (M3u.parse !m3u) `QueueAndJump;
  );

  (* Start drawing *)
  Ui.start geo.ui;

  (* Remember current geometry for later *)
  let extension_shown_w = Geometry.extension_shown_w geo in
  let extension_shown_h = Geometry.extension_shown_h geo in
  let extension_side = geo.extension_side in
  let playlist_shown = geo.playlist_shown in
  let library_shown = geo.library_shown in
  let filesel_shown = geo.filesel_shown in
  let menu_shown = geo.menu_shown in
  let popup_shown = geo.popup_shown <> None in

  (* Update geometry *)
  let ww, wh = Api.Window.size win in
  if extension_shown_h then geo.extension_width <- ww - Geometry.control_w geo;
  if extension_shown_w then geo.extension_height <- wh - Geometry.control_h geo;
  Geometry.update_geo geo;

  (* Run panes *)
  Run_control.run st;
  if not (Api.Window.is_minimized win) then
  (
    if playlist_shown then Run_playlist.run st;
    if filesel_shown then Run_filesel.run st
    else if library_shown then Run_library.run st;
    if playlist_shown then Run_view.run_edit_panel st;
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

  if Layout.lib_cover_key geo then
    Library.activate_covers st.library (not st.library.covers_shown);

  (* Adjust window size after opening/closing panes *)
  let extension_shown_w' = Geometry.extension_shown_w geo in
  let extension_shown_h' = Geometry.extension_shown_h geo in
  if extension_shown_h' <> extension_shown_h then
  (
    let s = if extension_shown_h' then +1 else -1 in
    Ui.resize geo.ui (-1, -1) (0, s * geo.extension_height);
  );
  if extension_shown_w' <> extension_shown_w then
  (
    let s = if extension_shown_w' then +1 else -1 in
    let ox = if Geometry.extension_left geo then Int.max_int else -1 in
    Ui.resize geo.ui (ox, -1) (s * geo.extension_width, 0);
  )
  else if extension_shown_w' && geo.extension_side <> extension_side then
  (
    let ox, ox' =
      if Geometry.extension_left geo then -1, Int.max_int else Int.max_int, -1 in
    Ui.resize geo.ui (ox, -1) (- geo.extension_width, 0);
    Ui.resize geo.ui (ox', -1) (+ geo.extension_width, 0);
  );

  (* Finish drawing *)
  let shift = Api.Key.is_modifier_down `Shift in
  let flex_ctl_w = shift || not (Geometry.extension_shown_w geo) in
  let flex_ctl_h = shift || not (Geometry.extension_shown_h geo) in
  let flex_ext_w = true in  (* since browser width may grow with control *)
  let flex_ext_h = true in
  let min_w = Geometry.win_min_w flex_ctl_w flex_ext_w geo in
  let max_w = Geometry.win_max_w flex_ctl_w flex_ext_w geo in
  let min_h = Geometry.win_min_h flex_ctl_h flex_ext_h geo in
  let max_h = Geometry.win_max_h flex_ctl_h flex_ext_h geo in
  Ui.finish geo.ui (Geometry.margin geo) (min_w, min_h) (max_w, max_h)
    (fun (_dx, _dy, dw, dh) ->
      (* Window was resized *)
      if dw <> 0 && extension_shown_w' = extension_shown_w then
      (
        if flex_ctl_w then
        (
          let control_width' =
            max Geometry.control_min_w (geo.control_width + dw) in
          let dw' = control_width' - geo.control_width in
          geo.control_width <- control_width';
          geo.extension_width <- geo.extension_width + (dw - dw');
        )
        else
        (
          let extension_width' =
            max (Geometry.extension_min_w geo) (geo.extension_width + dw) in
          let dw' = extension_width' - geo.extension_width in
          geo.extension_width <- extension_width';
          geo.control_width <- geo.control_width + (dw - dw');
        )
      );
      if dh <> 0 && extension_shown_h' = extension_shown_h then
      (
        if flex_ctl_h then
        (
          let control_height' =
            max Geometry.control_min_h (geo.control_height + dh) in
          let dh' = control_height' - geo.control_height in
          geo.control_height <- control_height';
          geo.extension_height <- geo.extension_height + (dh - dh');
        )
        else
        (
          let extension_height' =
            max (Geometry.extension_min_h geo) (geo.extension_height + dh) in
          let dh' = extension_height' - geo.extension_height in
          geo.extension_height <- extension_height';
          geo.control_height <- geo.control_height + (dh - dh');
        )
      );

      if !App.debug_layout then
      (
        Printf.eprintf
          "[layout set] win=%d,%d ctl=%d,%d ext=%d,%d bw=%d vw=%d delta=%d,%d\n%!"
          (fst (Api.Window.next_size win)) (snd (Api.Window.next_size win))
          geo.control_width geo.control_height
          geo.extension_width geo.extension_height
          geo.browser_width geo.left_width dw dh;
        Printf.eprintf "[layout min] win=%d,%d ctl=%d,%d ext=%d,%d bw=%d vw=%d\n%!"
          min_w min_h
          Geometry.control_min_w Geometry.control_min_h
          (Geometry.extension_min_w geo) (Geometry.extension_min_h geo)
          (Geometry.browser_min_w geo) (Geometry.left_min_w geo);
      );

      let wx, wy = Api.Window.next_pos win in
      let ww, wh = Api.Window.next_size win in
      Geometry.update_geo' geo (wx, wy, ww, wh);

      if !App.debug_layout then
      (
        Printf.eprintf
          "[layout new] win=%d,%d ctl=%d,%d ext=%d,%d bw=%d vw=%d delta=%d,%d\n%!"
          (fst (Api.Window.size win)) (snd (Api.Window.size win))
          geo.control_width geo.control_height
          geo.extension_width geo.extension_height
          geo.browser_width geo.left_width dw dh;
        Printf.eprintf "[layout min] win=%d,%d ctl=%d,%d ext=%d,%d bw=%d vw=%d\n%!"
          min_w min_h
          Geometry.control_min_w Geometry.control_min_h
          (Geometry.extension_min_w geo) (Geometry.extension_min_h geo)
          (Geometry.browser_min_w geo) (Geometry.left_min_w geo);
      );
    )
    (fun scr ->
      (* Window moved to another screen *)
      Ui.pin geo.ui scr;
      let w, h = geo.extension_width, geo.extension_height in
      Geometry.clamp_geo geo;
      let dw =
        if Geometry.extension_shown_w geo then geo.extension_width - w else 0 in
      let dh =
        if Geometry.extension_shown_h geo then geo.extension_height - h else 0 in
      (* Substract mouse delta to get position relative to current geometry *)
      Ui.resize geo.ui Api.Mouse.(Api.sub (pos win) (delta win)) (dw, dh)
    );

  if Api.Window.is_hidden win then  (* after startup *)
    Api.Window.reveal win;

  let scale_new = Api.Window.scale win in
  let scaling' =
    fst geo.scaling + (fst scale_new - fst scale_old),
    snd geo.scaling + (snd scale_new - snd scale_old)
  in
  geo.scaling <- scaling';
  if scale_delta = 0 then
    (* Not set yet on first frame *)
    geo.window <- Geometry.abstract_geo geo
  else
  (
    (* Reset geometry when scaling was changed *)
(*
    let scale_x, scale_y = Api.Window.scale win in
    let rx = float scale_x /. (float scale_x -. float scale_delta) in
    let ry = float scale_y /. (float scale_y -. float scale_delta) in
    let dims = [ax; ay; aw; ah] in
    let scaled_dims = [rx *. ax; ry *. ay; rx *. aw; ry *. ah] in
    if List.fold_left max 0.0 dims >= 0.97 (* = 1.0 +- eps *)
    || List.fold_left max 0.0 scaled_dims > 1.0 then
*)
      Ui.reset geo.ui (Geometry.apply_geo geo geo.window);
  );

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
  let success = State.load st0 in
  let st = if success then st0 else State.make ui audio in
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
  "--debug-perf", Arg.Set App.debug_perf, "\tLog execution times";
  "--debug-layout", Arg.Set App.debug_layout, "\tPrint window layout";
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
      Storage.clear_temp ();  (* possible left-overs *)
      run (startup ());
    )
  with exn ->
    Storage.log_exn "internal" exn "";
    Stdlib.exit 2
