(* Run Control UI *)

open Audio_file

type state = State.t


(* Helpers *)

let float_of_bool b = float (Bool.to_int b)


let start_time = Unix.gettimeofday ()

let time () = Unix.gettimeofday () -. start_time


let fmt = Printf.sprintf

let fmt_time t =
  let t' = int_of_float (Float.trunc t) in
  fmt "%d:%02d" (t' / 60) (t' mod  60)


(* Display operations *)

let quit _st =
  exit 0

let minimize (st : state) =
  Api.Window.minimize (Ui.window st.layout.ui)

let toggle_playlist (st : state) =
  let lay = st.layout in
  lay.playlist_shown <- not lay.playlist_shown;
  if not lay.playlist_shown then Playlist.defocus st.playlist

let toggle_library (st : state) =
  let lay = st.layout in
  if lay.library_shown then
  (
    lay.library_shown <- false;
    Library.defocus st.library;
  )
  else
  (
    lay.library_shown <- true;
    (* Switch side if window exceeds respective border *)
    if not lay.filesel_shown then
    (
      let win = Ui.window lay.ui in
      let wx, _ = Api.Window.pos win in
      let sx, _ = Api.Window.min_pos win in
      let sw, _ = Api.Window.max_size win in
      if lay.library_side = `Left && wx <= sx then
        lay.library_side <- `Right;
      if lay.library_side = `Right && wx + Layout.control_w lay >= sx + sw then
        lay.library_side <- `Left;
    )
  );
  if not (lay.library_shown || lay.filesel_shown) then State.focus_playlist st

let toggle_side (st : state) =
  let lay = st.layout in
  lay.library_side <- if lay.library_side = `Left then `Right else `Left

let toggle_cover (st : state) =
  let ctl = st.control in
  ctl.cover <- not ctl.cover

let toggle_fps (st : state) =
  let ctl = st.control in
  ctl.fps <- not ctl.fps

let dirty_all (st : state) =
  Table.dirty st.playlist.table;
  Table.dirty st.library.browser;
  Table.dirty st.library.artists;
  Table.dirty st.library.albums;
  Table.dirty st.library.tracks;
  Table.dirty st.filesel.dirs;
  Table.dirty st.filesel.files

let toggle_sdf (st : state) =
  let lay = st.layout in
  Ui.font_sdf lay.ui (not (Ui.font_is_sdf lay.ui));
  dirty_all st

let cycle_color (st : state) d =
  let lay = st.layout in
  let n = Ui.num_palette lay.ui in
  Ui.set_palette lay.ui ((Ui.get_palette lay.ui + d + n) mod n);
  dirty_all st

let clamp_text = Layout.(clamp min_text_size max_text_size)

let resize_text_avail (st : state) delta =
  clamp_text (st.layout.text + delta) <> st.layout.text

let resize_text (st : state) delta =
  st.layout.text <- clamp_text (st.layout.text + delta)

let clamp_grid = Layout.(clamp min_grid_size max_grid_size)

let resize_grid_avail (st : state) delta =
  match st.library.current with
  | None -> false
  | Some (dir : Library.dir) ->
    let lay = st.layout in
    dir.view.albums.shown = Some `Grid &&
      clamp_grid (lay.album_grid + delta) <> lay.album_grid ||
    dir.view.tracks.shown = Some `Grid &&
      clamp_grid (lay.track_grid + delta) <> lay.track_grid

let resize_grid (st : state) delta =
  Option.iter (fun (dir : Library.dir) ->
    let inc n =
      n + delta *
      if n <= 20 then 2 else
      if n <= 60 then 4 else
      if n <= 140 then 8 else
      if n <= 300 then 16 else 32
    in
    let lay = st.layout in
    if dir.view.albums.shown = Some `Grid then
      lay.album_grid <- clamp_grid (inc lay.album_grid);
    if dir.view.tracks.shown = Some `Grid then
      lay.track_grid <- clamp_grid (inc lay.track_grid);
  ) st.library.current


(* Runner *)

let run (st : state) =
  let ctl = st.control in
  let pl = st.playlist in
  let lay = st.layout in
  let win = Ui.window lay.ui in

  Layout.control_pane lay;

  (* Exit button *)
  (* This has to come first, otherwise Raylib crashes? *)
  let modal = Ui.is_modal lay.ui in
  Ui.nonmodal lay.ui;  (* always allow Quit *)
  if not (Layout.power_button lay (Some true))
  && not (Api.Key.is_modifier_down `Shift) then
  (
    (* Power button clicked: quit *)
    quit st
  );
  if modal then Ui.modal lay.ui;
  Layout.power_label lay;

  (* Current status *)
  let status = Control.status ctl in
  let playing = (status = `Playing) in
  let paused = (status = `Paused) in
  let stopped = (status = `Stopped || status = `Ejected) in
  let ejected = (status = `Ejected) in
  let length = Control.length ctl in
  let elapsed = Control.elapsed ctl in
  let remaining = length -. elapsed in
  let focus =
    pl.table.focus ||
    not (lay.library_shown || lay.filesel_shown || lay.menu_shown)
  in

  (* LCD *)
  Layout.info_box lay;
  let sign, d1, d2, d3, d4 =
    if paused && int_of_float (time ()) mod 2 = 0 then
      '+', ' ', ' ', ' ', ' ' else
    let sign, time =
      match ctl.timemode with
      | `Elapse -> '+', elapsed
      | `Remain -> '-', remaining
    in
    Layout.lcd_colon lay ':';
    let seconds = int_of_float (Float.round time) in
    sign,
    (Char.chr (Char.code '0' + seconds mod 6000 / 600)),
    (Char.chr (Char.code '0' + seconds mod 600 / 60)),
    (Char.chr (Char.code '0' + seconds mod 60 / 10)),
    (Char.chr (Char.code '0' + seconds mod 10))
  in
  Layout.lcd_sign lay sign;
  Layout.lcd_min1 lay d1;
  Layout.lcd_min2 lay d2;
  Layout.lcd_sec1 lay d3;
  Layout.lcd_sec2 lay d4;

  if Layout.lcd_button lay then
  (
    (* Click on time LCD: toggle time mode *)
    ctl.timemode <-
      match ctl.timemode with
      | `Elapse -> `Remain
      | `Remain -> `Elapse
  );

  if Layout.color_button lay then
  (
    (* Click on color button: cycle color palette *)
    cycle_color st (if Api.Key.is_modifier_down `Shift then -1 else +1)
  );

  (* Cover *)
  if ctl.cover then
  (
    Option.iter (fun (track : Data.track) ->
      Option.iter (Layout.cover lay)
        (Library.load_cover st.library win track.path)
    ) ctl.current
  );
  if Layout.cover_key lay then toggle_cover st;

  (* FPS *)
  if ctl.fps then
    Layout.fps_text lay `Regular true (fmt "%d FPS" (Api.Window.fps win));
  (* Press of FPS key: toggle FPS display *)
  if Layout.fps_key lay then toggle_fps st;

  if Layout.sdf_key lay then toggle_sdf st;

  (* Audio properties *)
  if not ejected then
  (
    let track = Option.get ctl.current in
    let ext = File.extension track.path in
    let format = if ext = "" || ext.[0] <> '.' then "???" else
      String.uppercase_ascii (String.sub ext 1 (String.length ext - 1)) in
    let bitrate = Control.bitrate ctl in
    let rate = Control.rate ctl in
    let channels = Control.channels ctl in
    let depth = bitrate /. float rate /. float channels in
    Layout.prop_text lay `Regular true
      (fmt "%s  %.0f KBPS  %.1f KHZ  %s BIT  %s"
        format (bitrate /. 1000.0) (float rate /. 1000.0)
        (fmt (if depth = Float.round depth then "%.0f" else "%.1f") depth)
        (match channels with
        | 1 -> "MONO"
        | 2 -> "STEREO"
        | n -> fmt "%d CHAN" n
        )
      );
  );

  (* Title info *)
  let name =
    match ctl.current with
    | Some track when not (Data.is_separator track) ->
      Track.name track ^ " - " ^ fmt_time (Track.time track)
    | _ -> App.(name ^ " " ^ version)
  in
  Layout.title_ticker lay name;

  (* Volume control *)
  let shift_volume (st : state) delta =
    if delta <> 0.0 then
      Control.volume st.control (ctl.volume +. 0.05 *. delta)
  in
  let vol_mouse = Layout.volume_bar lay ctl.volume in
  (* Hack to overlap volume bar with mute button. *)
  let mute_mouse = Ui.mouse_inside lay.ui (Layout.mute_area lay) in
  if not mute_mouse && Layout.mute_drag lay (0, 0) = `None
  && vol_mouse <> ctl.volume then
  (
    (* Click or drag on volume bar: adjust volume *)
    Control.volume ctl vol_mouse;
  );
  let vol_delta =
    snd (Layout.volume_wheel lay) +.
    float_of_bool (Layout.volup_key lay focus) -.
    float_of_bool (Layout.voldown_key lay focus)
  in
  (* Volume key pressed or mouse wheel used: shift volume *)
  shift_volume st vol_delta;

  let toggle_mute (st : state) =
    Control.mute st.control (not st.control.mute)
  in
  Layout.mute_text lay (Ui.error_color lay.ui) `Inverted ctl.mute "MUTE";
  if Layout.mute_button lay then
  (
    (* Click on mute label: toggle muting *)
    toggle_mute st;
  );

  (* Seek bar *)
  let seek (st : state) delta =
    if delta <> 0.0 then
      Control.seek st.control (st.control.progress +. 0.05 *. delta)
  in
  let progress = if length > 0.0 then elapsed /. length else 0.0 in
  let progress' = Layout.seek_bar lay progress in
  if (progress' <> ctl.progress || Api.Mouse.is_pressed `Left)
  && progress' <> progress && length > 0.0 then
  (
    (* Click or drag on seek bar at new position: reposition audio *)
    Control.seek ctl progress'
  );
  let seek_delta =
    float_of_bool (Layout.ff_key lay focus) -.
    float_of_bool (Layout.rw_key lay focus)
  in
  (* Seek key pressed: seek *)
  seek st seek_delta;

  (* Mouse reflection *)
  Layout.info_refl lay;

  (* Looping *)
  (match ctl.loop with
  | `AB (t1, t2) when playing && t2 < elapsed ->
    (* End of loop reached: jump back to start *)
    Control.seek ctl (t1 /. length);
  | _ -> ()
  );

  (* Play controls *)
  let len = Playlist.length pl in
  let _, _, _, h = Ui.dim lay.ui (Layout.playlist_area lay) in
  let page = max 1 (int_of_float (Float.floor (float h /. float lay.text))) in

  let skip (st : state) delta =
    if delta <> 0 then
    (
      let ctl = st.control in
      let more = Playlist.skip st.playlist delta (ctl.repeat <> `None) in
      Control.switch ctl (Playlist.current st.playlist) more;
      Playlist.adjust_scroll st.playlist page;
      Table.dirty st.library.tracks;
      Table.dirty st.library.browser;
    );
  in
  let bwd = Layout.bwd_button lay focus (Some false) in
  let fwd = Layout.fwd_button lay focus (Some false) in
  skip st (Bool.to_int fwd - Bool.to_int bwd);

  let play (st : state) =
    if stopped && len > 0 then
    (
      (* Click on play button: start track *)
      Control.switch st.control (Playlist.current st.playlist) true;
      Playlist.adjust_scroll st.playlist page;
      Table.dirty st.library.tracks;
      Table.dirty st.library.browser;
    )
  in
  let playing' = Layout.play_button lay focus (Some playing) in
  if playing' && not playing then
  (
    (* Click on play button: start track *)
    play st
  );

  let pause (_st : state) b =
    if playing' && b then
      Control.pause ctl
    else if (not stopped && not b || stopped && b) && length > 0.0 then
      Control.resume ctl
  in
  let paused' = Layout.pause_button lay focus (Some paused) in
  if paused <> paused' then
  (
    (* Click on pause button when playing: pause track *)
    pause st paused'
  );

  let stop (st : state) =
    if not stopped then
    (
      (match Playlist.current_opt st.playlist with
      | None -> Control.eject st.control
      | Some track -> Control.switch st.control track false
      );
      Playlist.adjust_scroll st.playlist page;
      Table.dirty st.library.tracks;
      Table.dirty st.library.browser;
    )
  in
  if Layout.stop_button lay focus (Some false) then
  (
    (* Click on stop button when playing: stop track *)
    stop st
  );

  let eject (st : state) =
    Control.eject st.control;
    Playlist.remove_all st.playlist;
    Table.dirty st.library.tracks;
    Table.dirty st.library.browser
  in
  if Layout.eject_button lay focus (Some false) then
  (
    (* Click on eject button: stop and clear playlist *)
    eject st
  );

  let start_stop (st : state) =
    let ctl = st.control in
    if playing then
      Control.pause ctl
    else if paused then
      Control.resume ctl
    else if stopped && len > 0 then
    (
      Control.switch ctl (Playlist.current st.playlist) true;
      Table.dirty st.library.tracks;
      Table.dirty st.library.browser;
    );
    Playlist.adjust_scroll st.playlist page
  in
  if Layout.start_stop_key lay focus then
  (
    (* Press of space key: pause or resume *)
    start_stop st
  );

  (* End of track *)
  (* Check must occur after possible Control.resume above,
   * otherwise the last track would be restarted. *)
  if playing && remaining < 0.2 then
  (
    (* Close to end: switch to next track *)
    let more =
      match ctl.repeat with
      | `One -> true
      | `All -> Playlist.skip pl (+1) true
      | `None -> Playlist.skip pl (+1) false
    in
    let next_track =
      if pl.table.pos = None
      then Option.get ctl.current
      else Playlist.current pl
    in
    Control.switch ctl next_track more;
    Playlist.adjust_scroll pl page;
    Table.dirty st.library.tracks;
    Table.dirty st.library.browser;
  );

  (* Play modes *)
  let toggle_shuffle (st : state) =
    let pl = st.playlist in
    if pl.shuffle = None then
    (
      Playlist.shuffle pl (if stopped then None else pl.table.pos);
      if stopped && pl.table.pos <> None then
      (
        Control.switch ctl (Playlist.current pl) false;
        Table.dirty st.library.tracks;
        Table.dirty st.library.browser;
      );
      Playlist.adjust_scroll pl page;
    )
    else
      Playlist.unshuffle pl
  in
  let shuffle = pl.shuffle <> None in
  Layout.shuffle_label lay;
  Layout.shuffle_indicator lay shuffle;
  let shuffle' = Layout.shuffle_button lay focus (Some shuffle) in
  if shuffle' <> shuffle then
  (
    (* Click on Shuffle button: toggle shuffle *)
    toggle_shuffle st
  );

  let cycle_repeat (st : state) =
    st.control.repeat <-
      match st.control.repeat with
      | `None -> `One
      | `One -> `All
      | `All -> `None
  in
  Layout.repeat_label lay;
  Layout.repeat_indicator1 lay (ctl.repeat <> `None);
  Layout.repeat_indicator2 lay (ctl.repeat = `All);
  if Layout.repeat_button lay focus (Some false) then
  (
    (* Click on Repeat button: cycle repeat mode *)
    cycle_repeat st
  );

  let cycle_loop (st : state) =
    let t = Control.elapsed st.control in
    st.control.loop <-
      match st.control.loop with
      | `None -> `A t
      | `A t1 when t1 > t -> `A t
      | `A t1 -> `AB (t1, t)
      | `AB _ -> `None
  in
  Layout.loop_label lay;
  Layout.loop_indicator1 lay (ctl.loop <> `None);
  Layout.loop_indicator2 lay
    (match ctl.loop with `AB _ -> true | _ -> false);
  if Layout.loop_button lay focus (Some false) then
  (
    (* Click on Loop button: cycle loop mode *)
    cycle_loop st
  );

  (* Context menu *)

  if Layout.(control_context lay || seek_context lay || volume_context lay) then
  (
    let c = Ui.text_color lay.ui in
    let unpause x = if x then "Unpause" else "Pause" in
    let shuffle s x = s ^ (if x = None then " On " else " Off ") in
    let repeat s x = s ^ (match x with `None -> " One" | `One -> " All" | `All -> " Off") in
    let loop s x = s ^ (match x with `None -> " Start" | `A _ -> " End" | `AB _ -> " Off") in
    let unmute x = if x then "Unmute" else "Mute" in
    Run_menu.command_menu st [|
      `Entry (c, "Start/Stop", Layout.key_startstop, paused || len > 0),
        (fun () -> start_stop st);
      `Entry (c, "Play", Layout.key_play, stopped && len > 0),
        (fun () -> play st);
      `Entry (c, unpause (not playing), Layout.key_pause, playing || paused),
        (fun () -> pause st (not paused));
      `Entry (c, "Stop", Layout.key_stop, not stopped),
        (fun () -> stop st);
      `Entry (c, "Next", Layout.key_fwd, len > 0),
        (fun () -> skip st (+1));
      `Entry (c, "Previous", Layout.key_bwd, len > 0),
        (fun () -> skip st (-1));
      `Entry (c, "Eject", Layout.key_eject, len <> 0 || ctl.current <> None),
        (fun () -> eject st);
      `Separator, ignore;
      `Entry (c, "Seek Backwards", Layout.key_rw, ctl.progress > 0.0),
        (fun () -> seek st (-1.0));
      `Entry (c, "Seek Forwards", Layout.key_ff, length > 0.0 && ctl.progress < 1.0),
        (fun () -> seek st (+1.0));
      `Separator, ignore;
      `Entry (c, shuffle "Shuffle" pl.shuffle, Layout.key_shuffle, true),
        (fun () -> toggle_shuffle st);
      `Entry (c, repeat "Repeat" ctl.repeat, Layout.key_repeat, true),
        (fun () -> cycle_repeat st);
      `Entry (c, loop "Loop" ctl.loop, Layout.key_loop, true),
        (fun () -> cycle_loop st);
      `Separator, ignore;
      `Entry (c, unmute ctl.mute, Layout.key_mute, true),
        (fun () -> toggle_mute st);
      `Entry (c, "Volume Up", Layout.key_volup, ctl.volume < 1.0),
        (fun () -> shift_volume st (+1.0));
      `Entry (c, "Volume Down", Layout.key_voldn, ctl.volume > 0.0),
        (fun () -> shift_volume st (-1.0));
    |]
  )


(* Pane Activation Runner *)

let run_toggle_panel (st : state) =
  let lay = st.layout in

  Layout.playlist_label lay;
  Layout.playlist_indicator lay lay.playlist_shown;
  let playlist_shown' = Layout.playlist_button lay (Some lay.playlist_shown) in
  (* Click on playlist activation button: toggle playlist *)
  lay.playlist_shown <- playlist_shown';
  if not playlist_shown' then Playlist.defocus st.playlist;

  Layout.library_label lay;
  Layout.library_indicator lay lay.library_shown;
  let library_shown' = Layout.library_button lay (Some lay.library_shown) in
  (* Click on library activation button: toggle library *)
  if library_shown' <> lay.library_shown then
  (
    if Api.Key.is_modifier_down `Shift then
      (* Shift-click: switch sides for library pane *)
      toggle_side st
    else
      toggle_library st
  )
  else if Layout.library_side_key lay then
  (
    (* Library side toggle key pressed: switch sides for library pane *)
    toggle_side st
  );

  (* Minimize button *)
  if Layout.minimize_button lay then
  (
    (* Right-click on power button: minimize window *)
    minimize st
  );

  (* Context menu *)
  if Layout.(info_context lay || shown_context lay) then
  (
    let c = Ui.text_color lay.ui in
    let show s b = (if b then "Show " else "Hide ") ^ s in
    let side s d = s ^ (match d with `Left -> " Right" | `Right -> " Left") in
    Run_menu.command_menu st [|
      `Entry (c, "Quit", Layout.key_quit, true),
        (fun () -> quit st);
      `Entry (c, "Minimize", Layout.key_min, true),
        (fun () -> minimize st);
      `Separator, ignore;
      `Entry (c, show "Playlist" (not lay.playlist_shown), Layout.key_pl, true),
        (fun () -> toggle_playlist st);
      `Entry (c, show "Library" (not lay.library_shown), Layout.key_lib, true),
        (fun () -> toggle_library st);
      `Entry (c, side "Expand to" lay.library_side, Layout.key_side, true),
        (fun () -> toggle_side st);
      `Separator, ignore;
      `Entry (c, "Cycle Color", Layout.key_color, true),
        (fun () -> cycle_color st (+1));
      `Entry (c, show "Cover" (not lay.playlist_shown), Layout.key_cover, true),
        (fun () -> toggle_cover st);
      `Entry (c, show "FPS" (not lay.playlist_shown), Layout.key_fps, true),
        (fun () -> toggle_fps st);
      `Separator, ignore;
      `Entry (c, "Increase Text Size", Layout.key_textup, resize_text_avail st (+1)),
        (fun () -> resize_text st (+1));
      `Entry (c, "Decrease Text Size", Layout.key_textdn, resize_text_avail st (-1)),
        (fun () -> resize_text st (-1));
      `Entry (c, "Increase Cover Size", Layout.key_gridup, resize_grid_avail st (+1)),
        (fun () -> resize_grid st (+1));
      `Entry (c, "Decrease Cover Size", Layout.key_griddn, resize_grid_avail st (-1)),
        (fun () -> resize_grid st (-1));
    |]
  )
