(* Main Runner *)

open Audio_file

type state = State.t


(* Extension Dividers *)

let run_dividers (st : state) =
  let geo = st.geometry in

  let shift = Api.Key.is_modifier_down `Shift in
  let cmd = Api.Key.is_modifier_down `Command in

  if not cmd then
    geo.control_ratio <- None
  else if Api.Key.(is_pressed (`Command `Left) || is_pressed (`Command `Right)) then
    geo.control_ratio <- Some (float geo.control_width /. float geo.control_height);

  let flex_ext_w = Geometry.extension_shown_w geo in
  let flex_ext_h = Geometry.extension_shown_h geo in
  let flex_ctl_w = shift || not flex_ext_w in
  let flex_ctl_h = shift || not flex_ext_h in
  let win_minw = Geometry.win_min_w geo flex_ctl_w flex_ext_w in
  let win_maxw = Geometry.win_max_w geo flex_ctl_w flex_ext_w in
  let win_minh = Geometry.win_min_h geo flex_ctl_h flex_ext_h in
  let win_maxh = Geometry.win_max_h geo flex_ctl_h flex_ext_h in
  let ctl_w, ctl_h = geo.control_width, geo.control_height in
  let ext_w, ext_h = geo.extension_width, geo.extension_height in
  let ctl_minh, ctl_maxh = Geometry.(control_min_h, control_max_h geo) in

  let win = Ui.window geo.ui in
  let scr = Api.Window.screen win in  (* snap relative to window's screen *)
  let win_x, win_y = Api.Window.pos win in
  let win_w, win_h = (*Api.Window.size win*)Geometry.(win_w geo, win_h geo) in
  let scr_x, scr_y = Api.Screen.min_pos scr in
  let scr_w, scr_h = Api.Screen.max_size scr in

  let ctl_w1', ctl_h1', win_dw, win_dx, focus_w1, focus_h1 =
    if not (Geometry.extension_shown_h geo) then
      ctl_w, ctl_h, 0, 0, `None, `None
    else
    (
      Layout.extension_divider_h_pane geo;

      let ctl_midh', mid =
        Layout.extension_divider_h geo ctl_h ctl_minh ctl_maxh in
      let win_lftw = win_maxw - win_w in
      let (win_lftw', ctl_lfth'), lft =
        if Geometry.(extension_shown_w geo && extension_left geo) then
          (win_lftw, ctl_h), false
        else
          Layout.extension_divider_wh_left geo (win_lftw, ctl_h)
            (win_maxw - win_maxw, ctl_minh) (win_maxw - win_minw, ctl_maxh)
            (win_maxw - win_w - win_x + scr_x, -1) (-1, -1)
      in
      let win_rgtw = win_w in
      let (win_rgtw', ctl_rgth'), rgt =
        if Geometry.(extension_shown_w geo && not (extension_left geo)) then
          (win_rgtw, ctl_h), false
        else
          Layout.extension_divider_wh_right geo (win_rgtw, ctl_h)
            (win_minw, ctl_minh) (win_maxw, ctl_maxh)
            (-1, -1) (scr_w + scr_x - win_x, -1)
      in

      let win_dw, win_sx =
        if win_lftw' <> win_lftw then win_lftw - win_lftw', -1 else
        if win_rgtw' <> win_rgtw then win_rgtw' - win_rgtw, 0 else
        0, 0
      in
      let ctl_w1' = if flex_ctl_w then ctl_w + win_dw else ctl_w in
      let ctl_h1' = ctl_lfth' + ctl_midh' + ctl_rgth' - 2 * ctl_h in
      ctl_w1', ctl_h1', win_dw, win_sx * win_dw,
      (if lft then `Lft else if rgt then `Rgt else `None),
      (if lft || mid || rgt then `Hor else `None)
    )
  in

  let ctl_w', ctl_h', win_dh, win_dy, focus_w, focus_h =
    if not (Geometry.extension_shown_w geo) then
      ctl_w1', ctl_h1', 0, 0, focus_w1, focus_h1
    else
    (
      Layout.extension_divider_w_pane geo;

      let w, minw, maxw =
        if Geometry.extension_left geo
        then Geometry.(ext_w, extension_min_w geo, extension_max_w geo)
        else Geometry.(ctl_w, control_min_w, control_max_w geo)
      in

      let uppw', upp = Layout.extension_divider_w_upper geo w minw maxw in
      let loww', low = Layout.extension_divider_w_lower geo w minw maxw in
      let (midw', ctl_midh'), mid =
        Layout.extension_divider_wh_mid geo (w, ctl_h)
          (minw, ctl_minh) (maxw, ctl_maxh) (-1, -1) (-1, -1)
      in
      let win_toph = win_maxh - win_h in
      let (topw', win_toph'), top =
        Layout.extension_divider_wh_top geo (w, win_toph)
          (minw, win_maxh - win_maxh) (maxw, win_maxh - win_minh)
          (-1, win_maxh - win_h - win_y + scr_y) (-1, -1)
      in
      let win_both = win_h in
      let (botw', win_both'), bot =
        Layout.extension_divider_wh_bot geo (w, win_both)
          (minw, win_minh) (maxw, win_maxh)
          (-1, -1) (-1, scr_h + scr_y - win_y)
      in

      let win_dh, win_sy =
        if win_toph' <> win_toph then win_toph - win_toph', -1 else
        if win_both' <> win_both then win_both' - win_both, 0 else
        0, 0
      in

      let w' = topw' + uppw' + midw' + loww' + botw' - 4 * w in
      let ctl_dw2' = if Geometry.extension_left geo then w - w' else w' - w in
      let ctl_w' = ctl_w1' + ctl_dw2' in
      let ctl_both' = if flex_ctl_h then ctl_h + win_dh else ctl_h in
      let ctl_h' = ctl_h1' + ctl_midh' + ctl_both' - 2 * ctl_h in
      ctl_w', ctl_h', win_dh, win_sy * win_dh,
      (if top || upp || mid || low || bot then `Ver else focus_w1),
      (if top then `Top else if mid then `Hor else if bot then `Bot else focus_h1)
    )
  in

  assert (focus_w <> `None || ctl_w' = ctl_w);
  assert (focus_h <> `None || ctl_h' = ctl_h);
  assert (focus_w = `Lft || focus_w = `Rgt || win_dw = 0);
  assert (focus_h = `Top || focus_h = `Bot || win_dh = 0);
  assert (focus_w = `Lft || win_dx = 0);
  assert (focus_h = `Top || win_dy = 0);

  let ctl_dw, ctl_dh = ctl_w' - ctl_w, ctl_h' - ctl_h in
  (win_dx, win_dy), (win_dw, win_dh), (ctl_dw, ctl_dh), focus_w, focus_h


(* Main runner *)

let queue_file = "queue.m3u"

let t_start = Unix.gettimeofday ()
let t_last = ref 0.0

let rec run (st : state) r =
  State.ok st;
  match run' st r with
  | exception exn ->
    Storage.log_exn "internal" exn ""; exit 0
  | r' ->
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
    run st r'

and run' (st : state) (x, y, w, h as r) =
  let geo = st.geometry in
  let win = Ui.window geo.ui in
  if Api.Window.closed win then Run_control.quit st;

  (* Not set yet on first frame *)
  if Api.Draw.frame win <= 1 then geo.window <- Geometry.abstract_geo geo r;

  (* Save state regularly every 3 seconds *)
  State.save_after st 3.0;

  (* App invocation with arguments *)
  let m3u = ref "" in
  Storage.load_string queue_file ((:=) m3u);
  (* Touch queue file to mark it as read. *)
  (* TODO: this could race, should lock the file *)
  if !m3u <> "" || Api.Draw.frame win mod 15 = 0 then
    Storage.save_string queue_file (fun () -> "");
  if !m3u <> "" then
  (
    (* If we're just starting up, and double-click caused it, force playing *)
    if Api.Draw.frame win <= 1 then Control.stop st.control;
    Run_view.external_queue_on_playlist st (M3u.parse !m3u) `QueueAndJump;
  );

  (* Start drawing *)
  Ui.start geo.ui r;

  (* Remember current geometry for later *)
  let extension_shown_w = Geometry.extension_shown_w geo in
  let extension_shown_h = Geometry.extension_shown_h geo in
  let extension_side = geo.extension_side in
  let menu_shown = geo.menu_shown in
  let popup_shown = geo.popup_shown <> None in

  (* Global keys *)
  if Layout.settings_key geo then
  (
    geo.settings_shown <- not geo.settings_shown;
    if geo.settings_shown then
      Run_settings.init st
    else
      State.defocus_all st;
  );

  let focus_change =
    if Layout.focus_next_key geo then (State.focus_next st; true) else
    if Layout.focus_prev_key geo then (State.focus_prev st; true) else false
  in

  (* Run panes *)
  Run_control.run st;
  if not (Api.Window.is_minimized win) then
  (
    if geo.settings_shown then Run_settings.run st focus_change
    else if geo.playlist_shown then Run_playlist.run st;
    if geo.filesel_shown then Run_filesel.run st
    else if geo.library_shown then Run_library.run st;
    if geo.playlist_shown && not geo.settings_shown then
      Run_view.run_edit_panel st;
    Run_control.run_toggle_panel st;
    if menu_shown then Run_menu.run st;
    if popup_shown then Run_menu.run_popup st;
  );
  List.iter (fun f -> f ()) st.delayed;
  st.delayed <- [];

  let extension_shown_w' = Geometry.extension_shown_w geo in
  let extension_shown_h' = Geometry.extension_shown_h geo in
  let extension_change =
    extension_shown_w' <> extension_shown_w ||
    extension_shown_h' <> extension_shown_h
  in

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
  if is_modal then Ui.modal geo.ui;  (* redisable keys *)
  geo.popup_size <- Geometry.(clamp min_popup_size max_popup_size
    (geo.popup_size + 100 * popup_delta));

  if Layout.lib_cover_key geo then
    Library.activate_covers st.library (not st.library.covers_shown);

  (* Scaling keys (ignore when extension was changed) *)
  let scale_delta =
    if extension_change then 0 else
    Bool.to_int (Layout.enlarge_scale_key geo) -
    Bool.to_int (Layout.reduce_scale_key geo)
  in
  let scale_old = Api.Window.scale win in
  let scale_new = Api.Window.scale win in
  let scaling' =
    fst geo.scaling + (fst scale_new - fst scale_old),
    snd geo.scaling + (snd scale_new - snd scale_old)
  in
  geo.scaling <- scaling';

  (* Finish drawing *)
  let shift = Api.Key.is_modifier_down `Shift in
  let cmd = Api.Key.is_modifier_down `Command in

  if not cmd then
    geo.control_ratio <- None
  else if Api.Key.(is_pressed (`Command `Left) || is_pressed (`Command `Right)) then
    geo.control_ratio <- Some (float geo.control_width /. float geo.control_height);

  let scr = Api.Window.screen win in
  let (x', y', w', h'), (lft, top, rgt, bot), screen_resized =
    Ui.finish geo.ui (Geometry.margin geo) (true, true) in
  let x', y', w', h' =
    if not extension_change then x', y', w', h' else
    (Ui.pin geo.ui scr; x, y, w, h)  (* undo move or resize *)
  in
  let scr' = Api.Window.screen win in
  if Api.Window.is_hidden win then  (* after startup *)
    Api.Window.reveal win;

  (* Compute new window geometry *)
  if screen_resized then
  (
    Geometry.apply_geo geo geo.window
  )
  else if scr' <> scr then
  (
    (* Window was dragged to another screen: adapt size *)
    assert ((w', h') = (w, h));  (* can only happen on move *)

    let _, _, w'', h'' = Geometry.apply_geo geo geo.window in
    (* Subtract mouse delta to get position relative to current geometry *)
    let origin = Api.Mouse.(Api.sub (pos win) (delta win)) in
    let dx, dy = Ui.resize_repos geo.ui origin (w'' - w, h'' - h) in
    let x'', y'' = x' + dx, y' + dy in

    if !App.debug_layout then
    (
      let sx, sy = Api.Screen.pos scr in
      let sw, sh = Api.Screen.size scr in
      let sx', sy' = Api.Screen.pos scr' in
      let sw', sh' = Api.Screen.size scr' in
      Printf.eprintf
        "[win screen] %d,%d,%d,%d @ %d,%d,%d,%d -> %d,%d,%d,%d @ %d,%d,%d,%d\n%!"
        x y w h sx sy sw sh x'' y'' w'' h'' sx' sy' sw' sh';
    );

    x'', y'', w'', h''
  )
  else if scale_delta <> 0 then
  (
    (* Scaling was changed: adapt geometry, ignore movement or resize *)
    assert (not extension_change);
(*
    let scale_x, scale_y = Api.Window.scale win in
    let rx = float scale_x /. (float scale_x -. float scale_delta) in
    let ry = float scale_y /. (float scale_y -. float scale_delta) in
    let dims = [ax; ay; aw; ah] in
    let scaled_dims = [rx *. ax; ry *. ay; rx *. aw; ry *. ah] in
    if List.fold_left max 0.0 dims >= 0.97 (* = 1.0 +- eps *)
    || List.fold_left max 0.0 scaled_dims > 1.0 then
*)
    Ui.rescale geo.ui (scale_delta, scale_delta);
    Geometry.apply_geo geo geo.window
  )
  else
  (
    (* Window was possibly moved or resized: clamp geometry *)
    let flex_ctl_w = shift || not extension_shown_w' in
    let flex_ctl_h = shift || not extension_shown_h' in

    let (win_dx, win_dy), (win_dw, win_dh) = (x' - x, y' - y), (w' - w, h' - h) in
    let win_dcw = if flex_ctl_w then win_dw else 0 in
    let win_dch = if flex_ctl_h then win_dh else 0 in
    let win_focusw = if lft then `Lft else if rgt then `Rgt else `None in
    let win_focush = if top then `Top else if bot then `Bot else `None in

    let (div_dx, div_dy), (div_dw, div_dh), (div_dcw, div_dch), div_focusw, div_focush =
      if extension_shown_w' = extension_shown_w
      && extension_shown_h' = extension_shown_h then
        run_dividers st
      else
        (0, 0), (0, 0), (0, 0), `None, `None
    in

    let dx, dy = win_dx + div_dx, win_dy + div_dy in
    let dw, dh = win_dw + div_dw, win_dh + div_dh in
    let dcw, dch = win_dcw + div_dcw, win_dch + div_dch in

    let (x'', y'', w'', h'') =
      if (dw, dh, dcw, dch) = (0, 0, 0, 0) then (x + dx, y + dy, w, h) else
      (
        (* Window was resized or a divider used *)
        if !App.debug_layout then
        (
          Printf.eprintf "[win change]\n%!";
          Printf.eprintf
            "    win=%d%+d,%d%+d,%d%+d,%d%+d ctl=%d,%d ext=%d,%d\n%!"
            x dx y dy w dw h dh
            geo.control_width geo.control_height
            geo.extension_width geo.extension_height
        );

        let dx', dy', dw', dh' =
          Geometry.change_geo geo dx dy dw dh dcw dch
            (if win_focusw = `None then div_focusw else win_focusw)
            (if win_focush = `None then div_focush else win_focush)
            flex_ctl_w flex_ctl_h
        in

        if !App.debug_layout then
        (
          Printf.eprintf
            "  [geo set] win=%d,%d ctl=%d,%d ext=%d,%d bw=%d vw=%d\n%!"
            (w + dw') (h + dh')
            geo.control_width geo.control_height
            geo.extension_width geo.extension_height
            geo.browser_width geo.left_width;
          Printf.eprintf
            "  [geo min] win=%d,%d ctl=%d,%d ext=%d,%d bw=%d vw=%d\n%!"
            (Geometry.win_min_w geo flex_ctl_w flex_ctl_h)
            (Geometry.win_min_h geo flex_ctl_w flex_ctl_h)
            Geometry.control_min_w Geometry.control_min_h
            (Geometry.extension_min_w geo) (Geometry.extension_min_h geo)
            (Geometry.browser_min_w geo) (Geometry.left_min_w geo);
        );

        x + dx', y + dy', w + dw', h + dh'
      )
    in

    (* Adjust window size after opening/closing panes *)
    let ext_dx, ext_dw =
      if extension_shown_w' = extension_shown_w then 0, 0 else
      (
        let s = if extension_shown_w' then +1 else -1 in
        let sw, _ = Api.Window.max_size win in
        let dw' = min (sw - geo.control_width) (s * geo.extension_width) in
        let dx' = if Geometry.extension_left geo then -dw' else 0 in
        geo.extension_width <- abs dw';
        dx', dw'
      )
    and ext_dy, ext_dh =
      if extension_shown_h' = extension_shown_h then 0, 0 else
      (
        let s = if extension_shown_h' then +1 else -1 in
        let _, sh = Api.Window.max_size win in
        let dh' = min (sh - geo.control_height) (s * geo.extension_height) in
        geo.extension_height <- abs dh';
        0, dh'
      )
    in
    let ext_dx =
      if ext_dw = 0
      && extension_shown_w' && geo.extension_side <> extension_side then
      (
        let sx = if Geometry.extension_left geo then -1 else +1 in
        ext_dx + sx * geo.extension_width;
      )
      else ext_dx
    in

    let r''' = x'' + ext_dx, y'' + ext_dy, w'' + ext_dw, h'' + ext_dh in

    Geometry.clamp_geo geo;

    if (ext_dw, ext_dh, scale_delta) <> (0, 0, 0) then
    (
      (* When a window gets resized, a scaled version of the old content is
       * visible for one frame. When opening/closing extensions, this creates
       * a very ugly flicker artefact.
       * To work around that, we draw an empty window in old size for a few
       * frames to clear the frame buffers, then draw an empty window in new
       * size. The latter seems necessary, otherwise we still see the artefact
       * occasionally.
       * Below is the best result of experimentation, who knows why...
       *)
      for _ = 1 to 2 do
        Ui.start geo.ui r;
        ignore (Ui.finish geo.ui 0 (false, false));
      done;
      for _ = 1 to 1 do
        Ui.start geo.ui r''';
        ignore (Ui.finish geo.ui 0 (false, false));
      done;
    );

    r'''
  )


(* Startup *)

let start' () =
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
  let x, y = Api.Window.pos win in
  let w, h = Api.Window.size win in
  run st (x, y, w, h)


let start paths =
  let m3u = if paths = [] then "" else M3u.make paths in
  if Storage.exists queue_file then
  (
    (* TODO: this could race, should lock the file *)
    Storage.save_string_append queue_file (fun () -> m3u);
    let t1 = Storage.time queue_file in
    Unix.sleepf 1.0;
    let t2 = Storage.time queue_file in
    (* If file has not been modified after 1s, assume it's a zombie. *)
    if t1 = t2 then start' ();
  )
  else
  (
    Storage.save_string queue_file (fun () -> m3u);
    Storage.clear_temp ();  (* possible left-overs *)
    start' ();
  )
