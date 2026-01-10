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
    if not geo.playlist_shown then Playlist.defocus st.playlist
  )

let toggle_library (st : state) =
  State.delay st (fun () ->
    let geo = st.geometry in
    if geo.library_shown then
    (
      geo.library_shown <- false;
      Library.defocus st.library;
    )
    else
    (
      geo.library_shown <- true;
      (* Switch side if window exceeds respective border *)
      if not geo.filesel_shown then
      (
        let win = Ui.window geo.ui in
        let scr = Api.Window.screen win in
        let wx, _ = Api.Window.pos win in
        let sx, _ = Api.Screen.min_pos scr in
        let sw, _ = Api.Screen.max_size scr in
        if geo.library_side = `Left && wx <= sx then
          geo.library_side <- `Right;
        if geo.library_side = `Right && wx + Geometry.control_w geo >= sx + sw then
          geo.library_side <- `Left;
      )
    );
    if not (geo.library_shown || geo.filesel_shown) then State.focus_playlist st
  )

let toggle_side (st : state) =
  State.delay st (fun () ->
    let geo = st.geometry in
    geo.library_side <- if geo.library_side = `Left then `Right else `Left
  )

let cycle_visual (st : state) =
  let ctl = st.control in
  ctl.raw <- [||];
  ctl.data <- [||];
  Control.set_visual ctl
    (match ctl.visual with
    | `None -> `Cover
    | `Cover -> `Spectrum
    | `Spectrum -> `Wave
    | `Wave -> `Oscilloscope
    | `Oscilloscope -> `None
    )

(*
let idx_visual (st : state) =
  match st.control.visual with
  | `None -> None
  | `Cover -> Some 0
  | `Spectrum -> Some 1
  | `Wave -> Some 2
  | `Oscilloscope -> Some 3
*)


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
    dir.view.albums.shown = Some `Grid &&
      clamp_grid (geo.album_grid + delta) <> geo.album_grid ||
    dir.view.tracks.shown = Some `Grid &&
      clamp_grid (geo.track_grid + delta) <> geo.track_grid

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
    if dir.view.albums.shown = Some `Grid then
      geo.album_grid <- clamp_grid (inc geo.album_grid);
    if dir.view.tracks.shown = Some `Grid then
      geo.track_grid <- clamp_grid (inc geo.track_grid);
  ) st.library.current

let clamp_popup = Geometry.(clamp min_popup_size max_popup_size)

let resize_popup_avail (st : state) delta =
  clamp_popup (st.geometry.popup_size + 100 * delta) <> st.geometry.popup_size

let resize_popup (st : state) delta =
  st.geometry.popup_size <- st.geometry.popup_size + 100 * delta


(*
(* Sine wave generator *)

let sine_freq = ref 440.0
let sine_off = ref 0.0

let sine_wave_callback buf len =
  let d = !sine_freq /. 44100.0 in
  let n = Unsigned.UInt.to_int len in
  let a = Ctypes.(CArray.from_ptr (from_voidp short buf) n) in
  for i = 0 to n - 1 do
    Ctypes.CArray.unsafe_set a i
      (int_of_float (32000.0 *. sin (2.0 *. Float.pi *. !sine_off)));
    sine_off := !sine_off +. d;
    if !sine_off > 1.0 then sine_off := !sine_off -. 1.0
  done

let sine_stream = ref None

let start_sine_wave () =
  Printf.printf "[start sine]\n%!";
  Raylib.set_audio_stream_buffer_size_default 4096;
  let stream = Raylib.load_audio_stream 44100 16 1 in
  Raylib_ocaml.Callbacks.set_audio_stream_callback stream sine_wave_callback;
  Raylib.play_audio_stream stream;
  sine_stream := Some stream

let stop_sine_wave () =
  Printf.printf "[stop sine]\n%!";
  Option.iter Raylib.unload_audio_stream !sine_stream;
  sine_stream := None

let toggle_sine_wave () =
  (if !sine_stream = None then start_sine_wave else stop_sine_wave) ()

let tweak_sine_wave c =
  sine_freq := max 6.875 (min 28160.0 (!sine_freq *. Float.pow 2.0 (c /. 12.0)));
  Printf.printf "[tweak sine %+.0f] freq=%f\n%!" c !sine_freq

let run_sine_wave () =
  if Api.Key.is_pressed (`Char '/') then toggle_sine_wave ();
  if Api.Key.is_pressed_or_repeated (`Char ',') then tweak_sine_wave (-1.0);
  if Api.Key.is_pressed_or_repeated (`Char '.') then tweak_sine_wave (+1.0)
*)


(* Runner *)

let run (st : state) =
  let ctl = st.control in
  let pl = st.playlist in
  let geo = st.geometry in
  let win = Ui.window geo.ui in

(*
  run_sine_wave ();
*)

  Layout.control_pane geo;

  (* Exit button *)
  (* This has to come first, otherwise Raylib crashes? *)
  let modal = Ui.is_modal geo.ui in
  Ui.nonmodal geo.ui;  (* always allow Quit *)
  Layout.power_shadow geo;
  if not (Layout.power_button geo (Some true))
  && not (Api.Key.is_modifier_down `Shift) then
  (
    (* Power button clicked: quit *)
    quit st
  );
  if modal then Ui.modal geo.ui;
  Layout.power_label geo;

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
    not (geo.library_shown || geo.filesel_shown || geo.menu_shown)
  in

  (* LCD *)
  Layout.info_box geo;
  let sign, d1, d2, d3, d4 =
    if paused && int_of_float (time ()) mod 2 = 0 then
      '+', ' ', ' ', ' ', ' ' else
    let sign, time =
      match ctl.timemode with
      | `Elapse -> '+', elapsed
      | `Remain -> '-', remaining
    in
    Layout.lcd_colon geo ':';
    let seconds = int_of_float (Float.round time) in
    sign,
    (Char.chr (Char.code '0' + seconds mod 6000 / 600)),
    (Char.chr (Char.code '0' + seconds mod 600 / 60)),
    (Char.chr (Char.code '0' + seconds mod 60 / 10)),
    (Char.chr (Char.code '0' + seconds mod 10))
  in
  Layout.lcd_sign geo sign;
  Layout.lcd_min1 geo d1;
  Layout.lcd_min2 geo d2;
  Layout.lcd_sec1 geo d3;
  Layout.lcd_sec2 geo d4;

  if Layout.lcd_button geo then
  (
    (* Click on time LCD: toggle time mode *)
    ctl.timemode <-
      match ctl.timemode with
      | `Elapse -> `Remain
      | `Remain -> `Elapse
  );

  if Layout.color_button geo then
  (
    (* Click on color button: cycle color palette *)
    cycle_color st (if Api.Key.is_modifier_down `Shift then -1 else +1)
  );

  (* Visual *)
  let old_visual = ctl.visual in
  if Layout.visual_key geo || Layout.visual_button geo
  || ctl.visual = `None && Layout.novisual_button geo then
    cycle_visual st;
  (*Option.iter (Layout.visual_indicator geo) (idx_visual st);*)

  (match ctl.visual with
  | `None -> ()

  | `Cover ->
    Option.iter (fun (track : Data.track) ->
      Option.iter (Layout.cover geo)
        (Library.load_cover st.library win track.path)
    ) ctl.current

  | `Spectrum ->
    let raw = ctl.raw in
    let len = Array.length raw in
    let lim = Spectrum.fft_samples in
    if len >= lim then
    (
      (* This could race, but that's okay *)
      let wave = Array.sub raw 0 lim in
      let rest = Array.sub raw lim (len - lim) in
      ctl.raw <- rest;
      ctl.data <- Spectrum.bands wave ctl.spec_bands;
    );
    let bands = ctl.data in
    let n = ctl.spec_bands in
    let n' = Array.length bands in
    (* Buffer may be off right after switching visuals *)
    let bands = if n' = n then bands else Array.make n 0.0 in

    let x, y, w, h = Ui.dim geo.ui (Layout.graph_area geo) in
    let y, h = y + 2, h - 4 in
    let wbar = (w + 1) / n in
    let wsep = if wbar <= 4 then 1 else if n <= 10 then 2 else 3 in
    let w' = wbar - wsep in
    let win = Ui.window geo.ui in
    let green = Ui.text_color geo.ui in
    let yellow = Ui.warn_color geo.ui in
    let red = Ui.error_color geo.ui in

    for i = 0 to n - 1 do
      let x' = x + i * wbar in
      Api.Draw.fill win x' y w' h (Ui.unlit_color red);
      let hy = 10 * h / 12 in
      Api.Draw.fill win x' (y + h - hy) w' hy (Ui.unlit_color yellow);
      let hg = 8 * h / 12 in
      Api.Draw.fill win x' (y + h - hg) w' hg (Ui.unlit_color green);
      let hr = min (int_of_float (bands.(i) /. 5.0 *. float h)) h in
      Api.Draw.fill win x' (y + h - hr) w' hr red;
      let hy = min hr hy in
      Api.Draw.fill win x' (y + h - hy) w' hy yellow;
      let hg = min hr hg in
      Api.Draw.fill win x' (y + h - hg) w' hg green;
      for j = 0 to h/2 - 1 do
        Api.Draw.fill win x (y + 2 * j) w 1 `Black;
      done
    done

  | `Wave ->
    let data = if ctl.raw = [||] then ctl.data else ctl.raw in
    ctl.raw <- [||];
    ctl.data <- data;

    let x, y, w, h = Ui.dim geo.ui (Layout.graph_area geo) in
    let win = Ui.window geo.ui in
    for i = 0 to w / 2 - 1 do
      let i = 2 * i in
      let v = if i < Array.length data then data.(i) else 0.0 in
      let v' = v *. float h /. 1.5 in
      let x, y = x + i, y + h/2 - int_of_float v' in
      Api.Draw.fill win x y 1 1 `White;
    done;

  | `Oscilloscope ->
    let data = if ctl.raw = [||] then ctl.data else ctl.raw in
    let len = Array.length data in
    ctl.raw <- [||];
    ctl.data <- data;

    if len > 0 then
    (
      let x, y, w, h = Ui.dim geo.ui (Layout.graph_area geo) in
      let win = Ui.window geo.ui in

      (match Layout.graph_drag geo (1, 1) with
      | `None | `Click | `Drop -> ()
      | `Take ->
        (* Dobule-click on oscilloscope: reset *)
        if Api.Mouse.is_double_click `Left then
          Control.reset_osc ctl
      | `Drag ((dx, dy), _, _) ->
        (* Drag on oscilloscope: adjust scaling *)
        let dx, dy = if abs dx > abs dy then dx, 0 else 0, dy in
        let mx, my = Api.Mouse.pos win in
        let ox, oy = mx - dx, my - dy in
        let mx', my' = max (x + 1) mx, min (y + h - 1) my in
        let ox', oy' = max (x + 1) ox, min (y + h - 1) oy in
        let sx = float (mx' - x) /. float (ox' - x) in
        let sy = float (y + h - my') /. float (y + h - oy') in
        Control.set_osc ctl (ctl.osc_x *. sx) (ctl.osc_y *. sy)
      );

      let sx = max (float w /. float len *. 0.8) ctl.osc_x in
      let n = min len (int_of_float (Float.ceil (float w /. sx))) in
      let ps = Array.make (2 * n) 0.0 in
      for i = 0 to n - 1 do
        let v = if i < len then data.(i) else 0.0 in
        ps.(2 * i) <- float x +. sx *. float i;
        ps.(2 * i + 1) <- float y +. (ctl.osc_y *. v +. 1.0) *. float h /. 2.0;
      done;
      if ctl.osc_x < 1.0 || ctl.osc_y > 1.0 then Api.Draw.clip win x y w h;
      Api.Draw.spline win ps 0.5 `White;
      if ctl.osc_x < 1.0 || ctl.osc_y > 1.0 then Api.Draw.unclip win;
(*
      let array = Ctypes.CArray.make Raylib.Vector2.t w in
      for i = 0 to min w (Array.length data) - 1 do
        let v = data.(i) *. float h /. 2.0 in
        let vec = Raylib.Vector2.create (float (x + i)) (float (y + h/2) -. v) in
        Ctypes.CArray.unsafe_set array i vec;
      done;
      for i = min w (Array.length data) to w - 1 do
        let vec = Raylib.Vector2.create (float (x + i)) (float (y + h/2)) in
        Ctypes.CArray.unsafe_set array i vec;
      done;
      Raylib.draw_spline_linear (Ctypes.CArray.start array) w
        0.5 Raylib.Color.white;
*)
    )
  );

  (* FPS *)
  if ctl.fps then
    Layout.fps_text geo `Regular true (fmt "%d FPS" (Api.Window.fps win));
  (* Press of FPS key: toggle FPS display *)
  if Layout.fps_key geo then toggle_fps st;

  if Layout.sdf_key geo then toggle_sdf st;

  (* Audio properties *)
  if not silent then
  (
    let track = Option.get ctl.current in
    let ext = File.extension track.path in
    let format = if ext = "" || ext.[0] <> '.' then "???" else
      String.uppercase_ascii (String.sub ext 1 (String.length ext - 1)) in
    let bitrate = Control.bitrate ctl in
    let rate = Control.rate ctl in
    let channels = Control.channels ctl in
    let depth = bitrate /. float rate /. float channels in
    Layout.prop_text geo `Regular true
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
  Layout.title_ticker geo name;

  (* Volume control *)
  let shift_volume (st : state) delta =
    if delta <> 0.0 then
      Control.volume st.control (ctl.volume +. 0.05 *. delta)
  in
  let vol_mouse = Layout.volume_bar geo ctl.volume in
  (* Hack to overlap volume bar with mute button. *)
  let mute_mouse = Ui.mouse_inside geo.ui (Layout.mute_area geo) in
  if not mute_mouse && Layout.mute_drag geo (0, 0) = `None
  && vol_mouse <> ctl.volume then
  (
    (* Click or drag on volume bar: adjust volume *)
    Control.volume ctl vol_mouse;
  );
  let vol_delta =
    snd (Layout.volume_wheel geo) +.
    float_of_bool (Layout.volup_key geo focus) -.
    float_of_bool (Layout.voldown_key geo focus)
  in
  (* Volume key pressed or mouse wheel used: shift volume *)
  shift_volume st vol_delta;

  let toggle_mute (st : state) =
    Control.mute st.control (not st.control.mute)
  in
  Layout.mute_text geo (Ui.error_color geo.ui) `Inverted ctl.mute "MUTE";
  if Layout.mute_button geo then
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
  let progress' = Layout.seek_bar geo progress in
  if (progress' <> ctl.progress || Api.Mouse.is_pressed `Left)
  && progress' <> progress && ctl.current <> None then
  (
    (* Click or drag on seek bar at new position: reposition audio *)
    Control.seek ctl progress'
  );
  let seek_delta =
    float_of_bool (Layout.ff_key geo focus) -.
    float_of_bool (Layout.rw_key geo focus)
  in
  (* Seek key pressed: seek *)
  seek st seek_delta;

  (* Mouse reflection *)
  Layout.info_refl geo;

  (* Looping *)
  (match ctl.loop with
  | `AB (t1, t2) when playing && t2 < elapsed ->
    (* End of loop reached: jump back to start *)
    Control.seek ctl (t1 /. length);
  | _ -> ()
  );

  (* Play controls *)
  let len = Playlist.length pl in
  let _, _, _, h = Ui.dim geo.ui (Layout.playlist_area geo) in
  let rh = geo.text + 2 * geo.pad_y in
  let page = max 1 (int_of_float (Float.floor (float h /. float rh))) in

  Layout.button_shadow geo;

  let last_pos = st.playlist.table.pos in
  let rec skip (st : state) delta =
    if delta <> 0 && st.playlist.table.pos <> None then
    (
      let ctl = st.control in
      let more = Playlist.skip st.playlist delta (ctl.repeat <> `None) in
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
  let bwd = Layout.bwd_button geo focus (Some false) in
  let fwd = Layout.fwd_button geo focus (Some false) in
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
  let playing' = Layout.play_button geo focus (Some playing) in
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
  let paused' = Layout.pause_button geo focus (Some paused) in
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
  if Layout.stop_button geo focus (Some false) then
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
  if Layout.eject_button geo focus (Some false) then
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
  if Layout.start_stop_key geo focus then
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
  Layout.shuffle_label geo;
  Layout.shuffle_indicator geo shuffle;
  Layout.shuffle_shadow geo;
  let shuffle' = Layout.shuffle_button geo focus (Some shuffle) in
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
  Layout.repeat_label geo;
  Layout.repeat_indicator1 geo (ctl.repeat = `One);
  Layout.repeat_indicator2 geo (ctl.repeat = `All);
  Layout.repeat_shadow geo;
  if Layout.repeat_button geo focus (Some false) then
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
  Layout.loop_label geo;
  Layout.loop_indicator1 geo (ctl.loop <> `None);
  Layout.loop_indicator2 geo
    (match ctl.loop with `AB _ -> true | _ -> false);
  Layout.loop_shadow geo;
  if Layout.loop_button geo focus (Some false) then
  (
    (* Click on Loop button: cycle loop mode *)
    cycle_loop st
  );

  (* Pop-ups *)

  if Layout.(control_context geo || seek_context geo || volume_context geo) then
  (
    let c = Ui.text_color geo.ui in
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
  else if ctl.visual = `Cover && old_visual = `Cover && not (Control.silent ctl)
    && Layout.cover_popup_open geo then
  (
    Run_menu.popup st `Current
  )


(* Pane Activation Runner *)

let run_toggle_panel (st : state) =
  let geo = st.geometry in

  Layout.playlist_label geo;
  Layout.playlist_shadow geo;
  Layout.playlist_indicator geo geo.playlist_shown;
  let playlist_shown' = Layout.playlist_button geo (Some geo.playlist_shown) in
  (* Click on playlist activation button: toggle playlist *)
  geo.playlist_shown <- playlist_shown';
  if not playlist_shown' then Playlist.defocus st.playlist;

  Layout.library_label geo;
  Layout.library_shadow geo;
  Layout.library_indicator geo geo.library_shown;
  let library_shown' = Layout.library_button geo (Some geo.library_shown) in
  (* Click on library activation button: toggle library *)
  if library_shown' <> geo.library_shown then
  (
    if Api.Key.is_modifier_down `Shift then
      (* Shift-click: switch sides for library pane *)
      (if geo.library_shown then toggle_side st)
    else
      toggle_library st
  )
  else if Layout.library_side_key geo then
  (
    (* Library side toggle key pressed: switch sides for library pane *)
    toggle_side st
  );

  (* Minimize button *)
  if Layout.minimize_button geo then
  (
    (* Right-click on power button: minimize window *)
    minimize st
  );

  (* Context menu *)
  if Layout.(info_context geo || shown_context geo) then
  (
    let c = Ui.text_color geo.ui in
    let show s b = (if b then "Show " else "Hide ") ^ s in
    let side s d = s ^ (match d with `Left -> " Right" | `Right -> " Left") in
    Run_menu.command_menu st [|
      `Entry (c, "Quit", Layout.key_quit, true),
        (fun () -> quit st);
      `Entry (c, "Minimize", Layout.key_min, true),
        (fun () -> minimize st);
      `Separator, ignore;
      `Entry (c, show "Playlist" (not geo.playlist_shown), Layout.key_pl, true),
        (fun () -> toggle_playlist st);
      `Entry (c, show "Library" (not geo.library_shown), Layout.key_lib, true),
        (fun () -> toggle_library st);
      `Entry (c, side "Expand to" geo.library_side, Layout.key_side, true),
        (fun () -> toggle_side st);
      `Separator, ignore;
      `Entry (c, "Cycle Color", Layout.key_color, true),
        (fun () -> cycle_color st (+1));
      `Entry (c, "Cycle Visual", Layout.key_visual, true),
        (fun () -> cycle_visual st);
      `Entry (c, show "FPS" (not st.control.fps), Layout.key_fps, true),
        (fun () -> toggle_fps st);
      `Separator, ignore;
      `Entry (c, "Increase Text Size", Layout.key_textup, resize_text_avail st (+1)),
        (fun () -> resize_text st (+1));
      `Entry (c, "Decrease Text Size", Layout.key_textdn, resize_text_avail st (-1)),
        (fun () -> resize_text st (-1));
      `Entry (c, "Increase Text Padding", Layout.key_padup, resize_pad_avail st (+1)),
        (fun () -> resize_pad st (+1));
      `Entry (c, "Decrease Text Padding", Layout.key_paddn, resize_pad_avail st (-1)),
        (fun () -> resize_pad st (-1));
      `Entry (c, "Increase Grid Cover Size", Layout.key_gridup, resize_grid_avail st (+1)),
        (fun () -> resize_grid st (+1));
      `Entry (c, "Decrease Grid Cover Size", Layout.key_griddn, resize_grid_avail st (-1)),
        (fun () -> resize_grid st (-1));
      `Entry (c, "Increase Popup Cover Size", Layout.key_popupup, resize_popup_avail st (+1)),
        (fun () -> resize_popup st (+1));
      `Entry (c, "Decrease Popup Cover Size", Layout.key_popupdn, resize_popup_avail st (-1)),
        (fun () -> resize_popup st (-1));
    |]
  )
