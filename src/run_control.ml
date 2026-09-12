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
  State.delay st (fun () ->
    Api.Window.minimize (Ui.window st.geometry.ui)
  )

let toggle_playlist (st : state) =
  State.delay st (fun () ->
    let geo = st.geometry in
    geo.playlist_shown <- not geo.playlist_shown;
    if not geo.playlist_shown then
    (
      Playlist.defocus st.playlist;
      geo.library_shown <- false;  (* force closed to avoid nonsensical layout *)
      Library.defocus st.library;
    )
  )

let toggle_library (st : state) =
  State.delay st (fun () ->
    let geo = st.geometry in
    if geo.library_shown then
    (
      geo.library_shown <- false;
      if not geo.filesel_shown then State.focus_playlist st;
    )
    else
    (
      geo.playlist_shown <- true;  (* force open to avoid nonsensical layout *)
      geo.library_shown <- true;
      if not geo.filesel_shown then
      (
        State.focus_edit st st.library.search;
        (* Switch side if window exceeds respective border *)
        let win = Ui.window geo.ui in
        let wx, _ = Api.Window.pos win in
        let sx, _ = Api.Window.min_pos win in
        let sw, _ = Api.Window.max_size win in
        if geo.extension_side = `Left && wx <= sx then
          geo.extension_side <- `Right;
        if geo.extension_side = `Right && wx + Geometry.control_w geo >= sx + sw then
          geo.extension_side <- `Left;
      )
    )
  )

let toggle_side (st : state) =
  State.delay st (fun () ->
    let geo = st.geometry in
    geo.extension_side <- if geo.extension_side = `Left then `Right else `Left
  )


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
  let geo = st.geometry in
  Ui.font_sdf geo.ui (not (Ui.font_is_sdf geo.ui));
  dirty_all st

let cycle_color (st : state) d =
  let geo = st.geometry in
  let n = Ui.num_palette geo.ui in
  Ui.set_palette geo.ui ((Ui.get_palette geo.ui + d + n) mod n);
  dirty_all st

let clamp_text = Geometry.(clamp min_text_size max_text_size)
let clamp_pad = Geometry.(clamp min_pad_size max_pad_size)

let resize_text_avail (st : state) delta =
  clamp_text (st.geometry.text + delta) <> st.geometry.text

let resize_text (st : state) delta =
  st.geometry.text <- clamp_text (st.geometry.text + delta)

let resize_pad_avail (st : state) delta =
  clamp_pad (st.geometry.pad_y + delta) <> st.geometry.pad_y

let resize_pad (st : state) delta =
  st.geometry.pad_y <- clamp_pad (st.geometry.pad_y + delta)

let clamp_grid = Geometry.(clamp min_grid_size max_grid_size)

let resize_grid_avail (st : state) delta =
  match st.library.current with
  | None -> false
  | Some (dir : Library.dir) ->
    let geo = st.geometry in
    clamp_grid (geo.grid + delta) <> geo.grid

let resize_grid (st : state) delta =
  Option.iter (fun (dir : Library.dir) ->
    let inc n =
      n + delta *
      if n <= 20 then 2 else
      if n <= 60 then 4 else
      if n <= 140 then 8 else
      if n <= 300 then 16 else 32
    in
    let geo = st.geometry in
    geo.grid <- clamp_grid (inc geo.grid);
  ) st.library.current

let clamp_zoom = Geometry.(clamp min_zoom_size max_zoom_size)

let resize_zoom_avail (st : state) delta =
  clamp_zoom (st.geometry.zoom_size + 100 * delta) <> st.geometry.zoom_size

let resize_zoom (st : state) delta =
  st.geometry.zoom_size <- st.geometry.zoom_size + 100 * delta


(* Runner *)

let run (st : state) =
  let ctl = st.control in
  let pl = st.playlist in
  let geo = st.geometry in
  let win = Ui.window geo.ui in

  let (module WindowUi) = Option.get st.layout in
  let module ControlUi = WindowUi.Control in

  (* Exit button *)
  (* This has to come first, otherwise Raylib crashes? *)
  let modal = Ui.is_modal geo.ui in
  Ui.except_modal geo.ui "ctl.run/power" (fun () ->  (* always allow Quit key *)
    ControlUi.Power.shadow ();
    if not modal && ControlUi.Power.button () || ControlUi.Power.key () then
    (
      (* Power button clicked: quit *)
      quit st
    )
  );
  ControlUi.Power.label ();

  (* Current status *)
  let status = Control.status ctl in
  let playing = (status = `Playing) in
  let paused = (status = `Paused) in
  let stopped = (status = `Stopped || status = `Ejected) in
  let silent = Control.silent ctl in
  let length = Control.length ctl in
  let elapsed = Control.elapsed ctl in
  let remaining = length -. elapsed in
  let focus =
    pl.table.focus ||
    not Geometry.(extension_shown_w geo || extension_shown_h geo)
  in

  (* LCD *)
  ControlUi.Info.box ();
  let sign, d1, d2, d3, d4 =
    if paused && int_of_float (time ()) mod 2 = 0 then
      '+', ' ', ' ', ' ', ' ' else
    let sign, time =
      match ctl.timemode with
      | `Elapse -> '+', elapsed
      | `Remain -> '-', remaining
    in
    ControlUi.Info.Lcd.colon ':';
    let seconds = int_of_float (Float.round time) in
    sign,
    (Char.chr (Char.code '0' + seconds mod 6000 / 600)),
    (Char.chr (Char.code '0' + seconds mod 600 / 60)),
    (Char.chr (Char.code '0' + seconds mod 60 / 10)),
    (Char.chr (Char.code '0' + seconds mod 10))
  in
  ControlUi.Info.Lcd.sign sign;
  ControlUi.Info.Lcd.min1 d1;
  ControlUi.Info.Lcd.min2 d2;
  ControlUi.Info.Lcd.sec1 d3;
  ControlUi.Info.Lcd.sec2 d4;

  if ControlUi.Info.Lcd.button () then
  (
    (* Click on time LCD: toggle time mode *)
    ctl.timemode <-
      match ctl.timemode with
      | `Elapse -> `Remain
      | `Remain -> `Elapse
  );

  if ControlUi.Info.Button.color () then
  (
    (* Click on color button: cycle color palette *)
    cycle_color st (if Api.Key.is_modifier_down `Shift then -1 else +1)
  );

  (* Visual *)
  let old_visual = ctl.visual in
  let visual_button = ControlUi.Info.Visual.button () in
  Ui.except_modal geo.ui "zoom" (fun () ->
    if visual_button || ControlUi.Info.Visual.key () then
      Control.(if Geometry.popup_shown geo then cycle_zoom else cycle_visual)
        ctl;
    (*Option.iter (Layout.visual_indicator geo) (idx_visual st);*)
  );

  let vis_area =
    ControlUi.Info.Visual.(if ctl.visual = `Cover then cover_area else area) in
  let img_opt =
    Option.map (fun (track : Data.track) ->
      Library.load_cover st.library win track.path
    ) ctl.current |> Option.join
  in

  Run_visualization.run st vis_area ctl.visual img_opt;

  (* FPS *)
  if ctl.fps then
    ControlUi.Info.Visual.fps (fmt "%d FPS" (Api.Window.fps win));
  (* Press of FPS key: toggle FPS display *)
  if ControlUi.Info.Button.fps () then toggle_fps st;

  (* Font *)
  if ControlUi.Info.Button.sdf () then toggle_sdf st;

  (* Audio properties *)
  if not silent then
  (
    let track = Option.get ctl.current in
    let ext = File.extension track.path in
    let format = if ext = "" || ext.[0] <> '.' then "???" else
      String.uppercase_ascii (String.drop_first 1 ext) in
    let bitrate = Control.bitrate ctl in
    let rate = Control.rate ctl in
    let channels = Control.channels ctl in
    let depth = Control.depth ctl in
    ControlUi.Info.Prop.text
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
  ControlUi.Info.Ticker.title name;

  (* Volume control *)
  let toggle_mute (st : state) =
    Control.mute st.control (not st.control.mute)
  in
  if ControlUi.Info.Mute.button () then
  (
    (* Click on mute label: toggle muting *)
    toggle_mute st;
  );

  let shift_volume (st : state) delta =
    if delta <> 0.0 then
      Control.volume st.control (ctl.volume +. 0.05 *. delta)
  in
  let vol_mouse = ControlUi.Info.Volume.bar ctl.volume in
  (* Hack to overlap volume bar with mute button. *)
  let mute_mouse = Ui.mouse_inside geo.ui ControlUi.Info.Mute.area in
  if not mute_mouse && ControlUi.Info.Mute.drag () = `None
  && vol_mouse <> ctl.volume then
  (
    (* Click or drag on volume bar: adjust volume *)
    Control.volume ctl vol_mouse;
  );
  let vol_delta =
    snd (ControlUi.Info.Volume.wheel ()) +.
    float_of_bool (ControlUi.Info.Volume.Key.up focus) -.
    float_of_bool (ControlUi.Info.Volume.Key.down focus)
  in
  (* Volume key pressed or mouse wheel used: shift volume *)
  shift_volume st vol_delta;

  ControlUi.Info.Mute.text ctl.mute;

  (* Seek bar *)
  let tip v = Data.string_of_time (v *. length), ControlUi.Info.Prop.h, `White in
  let seek (st : state) delta =
    if delta <> 0.0 then
      Control.seek st.control (st.control.progress +. 0.05 *. delta)
  in
  let progress = if length > 0.0 then elapsed /. length else 0.0 in
  let progress' =
    ControlUi.Info.Seek.bar (if silent then None else Some tip) progress in
  if (progress' <> ctl.progress || Api.Mouse.is_pressed `Left)
  && progress' <> progress && ctl.current <> None then
  (
    (* Click or drag on seek bar at new position: reposition audio *)
    Control.seek ctl progress'
  );
  let seek_delta =
    float_of_bool (ControlUi.Nav.Key.ff focus) -.
    float_of_bool (ControlUi.Nav.Key.rw focus)
  in
  (* Seek key pressed: seek *)
  seek st seek_delta;

  (* Mouse reflection *)
  ControlUi.Info.refl ();

  (* Looping *)
  (match ctl.loop with
  | `AB (t1, t2) when playing && t2 < elapsed ->
    (* End of loop reached: jump back to start *)
    Control.seek ctl (t1 /. length);
  | _ -> ()
  );

  (* Selection repeat *)
  if ctl.repeat = `Marked && Table.num_marked pl.table = 0 then
    ctl.repeat <- `None;

  (* Play controls *)
  let len = Playlist.length pl in
  let _, _, _, h = Ui.dim geo.ui WindowUi.Playlist.area in
  let rh = Geometry.text_h geo + 2 * Geometry.pad_h geo in
  let page = max 1 (int_of_float (Float.floor (float h /. float rh))) in

  ControlUi.Nav.shadow ();

  let last_pos = st.playlist.table.pos in
  let rec skip (st : state) delta =
    if delta <> 0 && st.playlist.table.pos <> None then
    (
      let ctl = st.control in
      let more = Playlist.skip st.playlist delta
        (if ctl.repeat = `One then `All else ctl.repeat) in
      Control.switch ctl (Playlist.current st.playlist);
      if more then Control.play ctl;
      (* Unless repeat mode is One, back-skip over silent tracks (separators,
       * missing songs), but make sure not to loop infinitely. *)
      if Control.silent ctl && delta < 0 && more && ctl.repeat <> `One
      && st.playlist.table.pos <> last_pos then
        skip st (delta / abs delta)
      else
      (
        Playlist.adjust_scroll st.playlist page;
        Table.dirty st.library.tracks;
        Table.dirty st.library.browser;
      )
    );
  in
  let bwd = ControlUi.Nav.bwd focus (Some false) in
  let fwd = ControlUi.Nav.fwd focus (Some false) in
  skip st (Bool.to_int fwd - Bool.to_int bwd);

  let play (st : state) =
    if stopped && (len > 0 || st.control.current <> None) then
    (
      (* Click on play button: start track *)
      let track =
        match Playlist.current_opt st.playlist with
        | Some track -> track
        | None -> Option.get st.control.current
      in
      Control.switch st.control track;
      Control.play st.control;
      Playlist.adjust_scroll st.playlist page;
      Table.dirty st.library.tracks;
      Table.dirty st.library.browser;
    )
  in
  let playing' = ControlUi.Nav.play focus (Some playing) in
  if playing' && not playing then
  (
    (* Click on play button: start track *)
    play st
  );

  let pause (_st : state) b =
    if playing' && b then
      Control.pause ctl
    else if stopped && b && length > 0.0 then
      Control.play ctl
    else if not stopped && not b && length > 0.0 then
      Control.resume ctl
  in
  let paused' = ControlUi.Nav.pause focus (Some paused) in
  if paused <> paused' then
  (
    (* Click on pause button when playing: pause track *)
    pause st paused'
  );

  let stop (st : state) =
    if not stopped then
    (
      Control.stop st.control;
      Playlist.adjust_scroll st.playlist page;
      Table.dirty st.library.tracks;
      Table.dirty st.library.browser;
    )
  in
  if ControlUi.Nav.stop focus (Some false) then
  (
    (* Click on stop button when playing: stop track *)
    stop st;
  );

  let eject (st : state) =
    Control.eject st.control;
    Playlist.remove_all st.playlist;
    Table.dirty st.library.tracks;
    Table.dirty st.library.browser
  in
  if ControlUi.Nav.eject focus (Some false) then
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
      Control.switch ctl (Playlist.current st.playlist);
      Control.play st.control;
      Table.dirty st.library.tracks;
      Table.dirty st.library.browser;
    );
    Playlist.adjust_scroll st.playlist page
  in
  if ControlUi.Nav.Key.start_stop focus then
  (
    (* Press of space key: pause or resume *)
    start_stop st
  );

  (* End of track *)
  (* Check must occur after possible Control.resume above,
   * otherwise the last track would be restarted. *)
  let length = Control.length ctl in
  let elapsed = Control.elapsed ctl in
  let remaining = length -. elapsed in
  if Control.status ctl = `Playing && remaining < 0.2 then
  (
    (* Close to end: switch to next track *)
    let more = Playlist.skip pl (+1) ctl.repeat in
    let next_track =
      if pl.table.pos = None
      then Option.get ctl.current
      else Playlist.current pl
    in
    Control.switch ctl next_track;
    if more then Control.play st.control;
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
        Control.switch ctl (Playlist.current pl);
        Table.dirty st.library.tracks;
        Table.dirty st.library.browser;
      );
      Playlist.adjust_scroll pl page;
    )
    else
      Playlist.unshuffle pl
  in
  let shuffle = pl.shuffle <> None in
  ControlUi.Mode.Shuffle.label ();
  ControlUi.Mode.Shuffle.indicator shuffle;
  ControlUi.Mode.Shuffle.shadow ();
  let shuffle' = ControlUi.Mode.Shuffle.button focus (Some shuffle) in
  if shuffle' <> shuffle then
  (
    (* Click on Shuffle button: toggle shuffle *)
    toggle_shuffle st
  );

  let cycled_repeat = function
    | `None -> `One
    | `One -> `All
    | `All -> if Table.num_selected pl.table > 1 then `Marked else `None
    | `Marked -> `None
  in
  let cycle_repeat (st : state) =
    st.control.repeat <- cycled_repeat st.control.repeat;
    if st.control.repeat = `Marked then
      Table.mark_selected pl.table
    else
      Table.unmark_all pl.table
  in
  let select_repeat (st : state) =
    let pl = st.playlist in
    Playlist.deselect_all pl;
    match st.control.repeat with
    | `None -> ()
    | `One -> let i = Option.get pl.table.pos in Playlist.select pl i i
    | `All -> Playlist.select_all pl
    | `Marked -> Playlist.select_marked pl
  in
  ControlUi.Mode.Repeat.label ();
  ControlUi.Mode.Repeat.indicator1 (ctl.repeat = `One || ctl.repeat = `Marked);
  ControlUi.Mode.Repeat.indicator2 (ctl.repeat = `All || ctl.repeat = `Marked);
  ControlUi.Mode.Repeat.shadow ();
  if ControlUi.Mode.Repeat.button focus (Some false) then
  (
    if Api.Key.are_modifiers_down [`Shift] then
      (* Shift-Click on Repeat button: select repeat set *)
      select_repeat st
    else
      (* Click on Repeat button: cycle repeat mode *)
      cycle_repeat st
  );

  let cycled_loop =
    let t = Control.elapsed st.control in
    function
    | `None -> `A t
    | `A t1 -> if t1 > t then `A t else `AB (t1, t)
    | `AB _ -> `None
  in
  let cycle_loop (st : state) =
    st.control.loop <- cycled_loop st.control.loop
  in
  ControlUi.Mode.Loop.label ();
  ControlUi.Mode.Loop.indicator1 (ctl.loop <> `None);
  ControlUi.Mode.Loop.indicator2
    (match ctl.loop with `AB _ -> true | _ -> false);
  ControlUi.Mode.Loop.shadow ();
  if ControlUi.Mode.Loop.button focus (Some false) then
  (
    (* Click on Loop button: cycle loop mode *)
    cycle_loop st
  );

  (* Pop-ups *)

  if ControlUi.Context.(nav || seek || volume) then
  (
    let c = Ui.text_color geo.ui in
    let unpause x = if x then "Unpause" else "Pause" in
    let shuffle s x = s ^ (if x = None then " On " else " Off ") in
    let repeat s x = s ^ (match cycled_repeat x with `None -> " None" | `One -> " One" | `All -> " All" | `Marked -> " Selection") in
    let loop s x = s ^ (match cycled_loop x with `None -> " Off" | `A _ -> " Start" | `AB _ -> " End") in
    let unmute x = if x then "Unmute" else "Mute" in
    Run_popup.command_menu st (Iarray.append [|
      `Entry (c, "Start/Stop", Layout.KeyBind.start_stop, paused || len > 0),
        (fun () -> start_stop st);
      `Entry (c, "Play", Layout.KeyBind.play, stopped && len > 0),
        (fun () -> play st);
      `Entry (c, unpause (not playing), Layout.KeyBind.pause, playing || paused),
        (fun () -> pause st (not paused));
      `Entry (c, "Stop", Layout.KeyBind.stop, not stopped),
        (fun () -> stop st);
      `Entry (c, "Next", Layout.KeyBind.fwd, len > 0),
        (fun () -> skip st (+1));
      `Entry (c, "Previous", Layout.KeyBind.bwd, len > 0),
        (fun () -> skip st (-1));
      `Entry (c, "Eject", Layout.KeyBind.eject, len <> 0 || ctl.current <> None),
        (fun () -> eject st);
    |] (if not geo.playlist_shown then [||] else [|
      `Separator, ignore;
      `Entry (c, "Seek Backwards", Layout.KeyBind.rw, ctl.progress > 0.0),
        (fun () -> seek st (-1.0));
      `Entry (c, "Seek Forwards", Layout.KeyBind.ff, length > 0.0 && ctl.progress < 1.0),
        (fun () -> seek st (+1.0));
      `Separator, ignore;
      `Entry (c, shuffle "Shuffle" pl.shuffle, Layout.KeyBind.shuffle, true),
        (fun () -> toggle_shuffle st);
      `Entry (c, repeat "Repeat" ctl.repeat, Layout.KeyBind.repeat, true),
        (fun () -> cycle_repeat st);
      `Entry (c, loop "Loop" ctl.loop, Layout.KeyBind.loop, true),
        (fun () -> cycle_loop st);
      `Separator, ignore;
      `Entry (c, unmute ctl.mute, Layout.KeyBind.mute, true),
        (fun () -> toggle_mute st);
      `Entry (c, "Volume Up", Layout.KeyBind.vol_up, ctl.volume < 1.0),
        (fun () -> shift_volume st (+1.0));
      `Entry (c, "Volume Down", Layout.KeyBind.vol_dn, ctl.volume > 0.0),
        (fun () -> shift_volume st (-1.0));
    |]))
  )
  else if ctl.visual <> `Oscilloscope && old_visual = ctl.visual && not (Control.silent ctl)
    && ControlUi.Info.Button.zoom () then
  (
    Run_popup.zoom st Popup.Current
  )


(* Pane Activation Runner *)

let run_toggle_panel (st : state) =
  let geo = st.geometry in
  let ctl = st.control in

  let (module WindowUi) = Option.get st.layout in
  let module ControlUi = WindowUi.Control in
  let module ToggleUi = ControlUi.Shown in

  ToggleUi.Playlist.label ();
  ToggleUi.Playlist.shadow ();
  ToggleUi.Playlist.indicator geo.playlist_shown;
  let playlist_shown' = ToggleUi.Playlist.button (Some geo.playlist_shown) in
  (* Click on playlist activation button: toggle playlist *)
  if playlist_shown' <> geo.playlist_shown then
    toggle_playlist st;

  ToggleUi.Library.label ();
  ToggleUi.Library.shadow ();
  ToggleUi.Library.indicator geo.library_shown;
  let library_shown' = ToggleUi.Library.button (Some geo.library_shown) in
  (* Click on library activation button: toggle library *)
  if library_shown' <> geo.library_shown then
  (
    if Api.Key.is_modifier_down `Shift then
      (* Shift-click: switch sides for library pane *)
      toggle_side st
    else
      toggle_library st
  )
  else if ToggleUi.Key.side () then
  (
    (* Library side toggle key pressed: switch sides for library pane *)
    toggle_side st
  );

  (* Minimize button *)
  if ControlUi.Power.minimize () then
  (
    (* Right-click on power button: minimize window *)
    minimize st
  );

  (* Context menu *)
  if ControlUi.Context.(info || shown) then
  (
    let c = Ui.text_color geo.ui in
    let show s b = (if b then "Hide " else "Show ") ^ s in
    let side s d = s ^ (match d with `Left -> " Right" | `Right -> " Left") in
(*
    let next_vis = next_visual ctl.visual in
*)
    let vis_entry name vis =
      `Entry (c, "Show " ^ name,
        Layout.KeyBind.na (*Layout.KeyBind.(if next_vis = vis then visual else na)*),
        ctl.visual <> vis),
        (fun () -> Control.set_visual ctl vis)
    in
    Run_popup.command_menu st (Iarray.append [|
      `Entry (c, "Quit", Layout.KeyBind.quit, true),
        (fun () -> quit st);
      `Entry (c, "Minimize", Layout.KeyBind.min, true),
        (fun () -> minimize st);
      `Separator, ignore;
      `Entry (c, show "Playlist" geo.playlist_shown, Layout.KeyBind.pl, true),
        (fun () -> toggle_playlist st);
      `Entry (c, show "Library" geo.library_shown, Layout.KeyBind.lib, true),
        (fun () -> toggle_library st);
      `Entry (c, side "Expand to" geo.extension_side, Layout.KeyBind.side, true),
        (fun () -> toggle_side st);
      `Separator, ignore;
      vis_entry "Cover" `Cover;
      vis_entry "Turntable" `Turntable;
      vis_entry "Spectrum" `Spectrum;
      vis_entry "Waveform" `Waveform;
      vis_entry "Oscilloscope" `Oscilloscope;
      `Separator, ignore;
      `Entry (c, show "Settings" geo.settings_shown, Layout.KeyBind.settings, true),
        (fun () ->
          Playlist.defocus st.playlist;
          geo.settings_shown <- not geo.settings_shown;
          if geo.settings_shown then
            Run_settings.init st
          else
            Settings.defocus st.settings;
        );
    |] [||])
(*
    |] (if not geo.playlist_shown then [||] else [|
      `Separator, ignore;
      `Entry (c, "Cycle Color", Layout.KeyBind.color, true),
        (fun () -> cycle_color st (+1));
      `Entry (c, "Cycle Visual", Layout.KeyBind.visual, true),
        (fun () -> cycle_visual st);
      `Entry (c, show "FPS" (not st.control.fps), Layout.key_fps, true),
        (fun () -> toggle_fps st);
      `Separator, ignore;
      `Entry (c, "Increase Text Size", Layout.KeyBind.text_up, resize_text_avail st (+1)),
        (fun () -> resize_text st (+1));
      `Entry (c, "Decrease Text Size", Layout.KeyBind.text_dn, resize_text_avail st (-1)),
        (fun () -> resize_text st (-1));
      `Entry (c, "Increase Text Padding", Layout.KeyBind.pad_up, resize_pad_avail st (+1)),
        (fun () -> resize_pad st (+1));
      `Entry (c, "Decrease Text Padding", Layout.KeyBind.pad_dn, resize_pad_avail st (-1)),
        (fun () -> resize_pad st (-1));
      `Entry (c, "Increase Grid Cover Size", Layout.KeyBind.grid_up, resize_grid_avail st (+1)),
        (fun () -> resize_grid st (+1));
      `Entry (c, "Decrease Grid Cover Size", Layout.KeyBind.grid_dn, resize_grid_avail st (-1)),
        (fun () -> resize_grid st (-1));
      `Entry (c, "Increase Popup Cover Size", Layout.KeyBind.zoom_up, resize_zoom_avail st (+1)),
        (fun () -> resize_zoom st (+1));
      `Entry (c, "Decrease Popup Cover Size", Layout.KeyBind.zoom_dn, resize_zoom_avail st (-1)),
        (fun () -> resize_zoom st (-1));
    |]))
*)
  )
