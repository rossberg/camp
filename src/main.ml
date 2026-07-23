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
    (* If we're just starting up, and double-click caused it, force playing *)
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

(*
  (* Update geometry *)
  let ww, wh = Api.Window.size win in
  if extension_shown_w then geo.extension_width <- ww - Geometry.control_w geo;
  if extension_shown_h then geo.extension_height <- wh - Geometry.control_h geo;
  Geometry.update_geo geo;
*)

  (* Run panes *)
  Run_control.run st;
  let (dx, dy), (dw, dh) =
    if Api.Window.is_minimized win then (0, 0), (0, 0) else
    (
      if playlist_shown then Run_playlist.run st;
      if filesel_shown then Run_filesel.run st
      else if library_shown then Run_library.run st;
      if playlist_shown then Run_view.run_edit_panel st;
      let dpos, dsize = Run_control.run_toggle_panel st in
      if menu_shown then Run_menu.run st;
      if popup_shown then Run_menu.run_popup st;
      dpos, dsize
    )
  in
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

  let dx', dw' =
    if extension_shown_w' = extension_shown_w then dx, dw else
    (
      let s = if extension_shown_w' then +1 else -1 in
      let sw, _ = Api.Window.max_size win in
      let dw' = min (sw - geo.control_width) (s * geo.extension_width) in
      let dx' = if Geometry.extension_left geo then -dw' else 0 in
  Printf.printf "[layout extend h %b] ww=%d cw=%d ew=%d sw=%d dw=%d maxew=%d\n%!"
  extension_shown_w' (fst(Api.Window.size win)+dw) geo.control_width geo.extension_width sw dw' (sw - geo.control_width);
      geo.extension_width <- abs dw';
      dx + dx', dw + dw'
    )
  and dy', dh' =
    if extension_shown_h' = extension_shown_h then dy, dh else
    (
      let s = if extension_shown_h' then +1 else -1 in
      let _, sh = Api.Window.max_size win in
      let dh' = min (sh - geo.control_height) (s * geo.extension_height) in
Printf.printf "[layout extend w %b] wh=%d ch=%d eh=%d sh=%d dh=%d maxeh=%d\n%!"
extension_shown_h' (snd(Api.Window.size win)+dh) geo.control_height geo.extension_height sh dh' (sh - geo.control_height);
      geo.extension_height <- abs dh';
      dy, dh + dh'
    )
  in
  Geometry.clamp_geo' geo Api.(add (Window.size win) (dw', dh'));

  let dx'' =
    if extension_shown_w' && geo.extension_side <> extension_side then
    (
      let sx = if Geometry.extension_left geo then -1 else +1 in
      dx' + sx * geo.extension_width;
    )
    else dx'
  in
  Ui.resize geo.ui (dx'', dy') (dw', dh');

  (* Finish drawing *)
  let shift = Api.Key.is_modifier_down `Shift in
  let cmd = Api.Key.is_modifier_down `Command in

  if not cmd then
    geo.control_ratio <- None
  else if Api.Key.(is_pressed (`Command `Left) || is_pressed (`Command `Right)) then
    geo.control_ratio <- Some (float geo.control_width /. float geo.control_height);

  let flex_ctl_w = shift || not extension_shown_w' in
  let flex_ctl_h = shift || not extension_shown_h' in
  let flex_ext_w = extension_shown_w' in
  let flex_ext_h = extension_shown_h' in
  let maxw = Geometry.win_max_w geo flex_ctl_w flex_ext_w in
  let minw = min maxw (Geometry.win_min_w geo flex_ctl_w flex_ext_w) in
  let maxh = Geometry.win_max_h geo flex_ctl_h flex_ext_h in
  let minh = min maxh (Geometry.win_min_h geo flex_ctl_h flex_ext_h) in
  Ui.finish geo.ui (Geometry.margin geo) (minw, minh) (maxw, maxh)
    (fun (dx, dy, dw, dh) (lft, top, rgt, bot) ->
      (* Window was resized *)
      let x, y = Api.Window.pos win in
      let w, h = Api.Window.size win in

      if !App.debug_layout then
      (
        Printf.eprintf
          "[win resize]\n    win=%d%+d,%d%+d,%d%+d,%d%+d min=%d,%d max=%d,%d ctl=%d,%d ext=%d,%d\n%!"
          x dx y dy w dw h dh minw minh maxw maxh geo.control_width geo.control_height geo.extension_width geo.extension_height
      );
(*
      let x, y = Api.Window.pos win in
      let w, h = Api.Window.size win in
      let x', y' = x + dx, y + dy in
      let dx, dy = dx - dx'', dy - dy' in
      let dw, dh = dw - dw', dh - dh' in

      let cw, ch = geo.control_width, geo.control_height in
      let cw' = if flex_ctl_w then cw + dw else cw in
      let ch' = if flex_ctl_h then ch + dh else ch in
      let cw'', ch'' =
        match geo.control_ratio with
        | None -> cw', ch'
        | Some ratio ->
          let adapt_w () = int_of_float (float ch' *. ratio), ch' in
          let adapt_h () = cw', int_of_float (float cw' /. ratio) in
          if not (lft || rgt) then adapt_w () else
          if not (top || bot) then adapt_h () else
          if float cw' /. float ch' < ratio then adapt_w () else adapt_h ()
      in

      let ax, ay, _, _ = geo.window in
      let sign lo hi a =
        if lo then -1 else if hi then 0 else
        if a < 1.0 then 0 else -1
      in
      let dcw, dch = cw'' - cw, ch'' - ch in
      let dx', dw' =
        if not flex_ctl_w then dx, dw else
        dx + sign lft rgt ax * (cw'' - cw'), dcw
      and dy', dh' =
        if not flex_ctl_h then dy, dh else
        dy + sign top bot ay * (ch'' - ch'), dch
      in
*)
      let dcw = if flex_ctl_w then dw else 0 in
      let dch = if flex_ctl_h then dh else 0 in
      let focus_w = if lft then `Lft else if rgt then `Rgt else `None in
      let focus_h = if top then `Top else if bot then `Bot else `None in
      let dx', dy', dw', dh' =
        Geometry.change_geo geo dx dy dw dh dcw dch
          focus_w focus_h flex_ctl_w flex_ctl_h
      in
      Ui.resize geo.ui (dx' - dx, dy' - dy) (dw' - dw, dh' - dh);

      if !App.debug_layout then
      (
        Printf.eprintf
          "  [layout set] win=%d,%d ctl=%d,%d ext=%d,%d bw=%d vw=%d\n%!"
          (w + dw') (h + dh')
          geo.control_width geo.control_height
          geo.extension_width geo.extension_height
          geo.browser_width geo.left_width;
        Printf.eprintf
          "  [layout min] win=%d,%d ctl=%d,%d ext=%d,%d bw=%d vw=%d\n%!"
          minw minh
          Geometry.control_min_w Geometry.control_min_h
          (Geometry.extension_min_w geo) (Geometry.extension_min_h geo)
          (Geometry.browser_min_w geo) (Geometry.left_min_w geo);
      );

      let x', y' = x + dx', y + dy' in
      let w', h' = w + dw', h + dh' in
      Geometry.update_geo' geo (x', y', w', h');

      if !App.debug_layout then
      (
        Printf.eprintf
          "  [layout new] win=%d,%d ctl=%d,%d ext=%d,%d bw=%d vw=%d\n%!"
          w' h'
          geo.control_width geo.control_height
          geo.extension_width geo.extension_height
          geo.browser_width geo.left_width;
      )
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
      Ui.resize_from geo.ui Api.Mouse.(Api.sub (pos win) (delta win)) (dw, dh)
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

  (* Save state regularly every 3 seconds *)
  State.save_after st 3.0


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
  if !App.debug_layout then State.dump st [];
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
    let m3u = if !paths = [] then "" else M3u.make (List.rev !paths) in
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
