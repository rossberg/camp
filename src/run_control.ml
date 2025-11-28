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
    Api.Window.minimize (Ui.window st.layout.ui)
  )

let toggle_playlist (st : state) =
  State.delay st (fun () ->
    let lay = st.layout in
    lay.playlist_shown <- not lay.playlist_shown;
    if not lay.playlist_shown then Playlist.defocus st.playlist
  )

let toggle_library (st : state) =
  State.delay st (fun () ->
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
  )

let toggle_side (st : state) =
  State.delay st (fun () ->
    let lay = st.layout in
    lay.library_side <- if lay.library_side = `Left then `Right else `Left
  )

let cycle_visual (st : state) =
  let ctl = st.control in
  Control.set_visual ctl
    (match ctl.visual with
    | `None -> `Cover
    | `Cover -> `Spectrum
    | `Spectrum -> `Wave
    | `Wave -> `Oscilloscope
    | `Oscilloscope -> `None
    )

let idx_visual (st : state) =
  match st.control.visual with
  | `None -> None
  | `Cover -> Some 0
  | `Spectrum -> Some 1
  | `Wave -> Some 2
  | `Oscilloscope -> Some 3


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
let clamp_pad = Layout.(clamp min_pad_size max_pad_size)

let resize_text_avail (st : state) delta =
  clamp_text (st.layout.text + delta) <> st.layout.text

let resize_text (st : state) delta =
  st.layout.text <- clamp_text (st.layout.text + delta)

let resize_pad_avail (st : state) delta =
  clamp_pad (st.layout.pad_y + delta) <> st.layout.pad_y

let resize_pad (st : state) delta =
  st.layout.pad_y <- clamp_pad (st.layout.pad_y + delta)

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

let clamp_popup = Layout.(clamp min_popup_size max_popup_size)

let resize_popup_avail (st : state) delta =
  clamp_popup (st.layout.popup_size + 100 * delta) <> st.layout.popup_size

let resize_popup (st : state) delta =
  st.layout.popup_size <- st.layout.popup_size + 100 * delta


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
  let lay = st.layout in
  let win = Ui.window lay.ui in

(*
  run_sine_wave ();
*)

  Layout.control_pane lay;

  (* Exit button *)
  (* This has to come first, otherwise Raylib crashes? *)
  let modal = Ui.is_modal lay.ui in
  Ui.nonmodal lay.ui;  (* always allow Quit *)
  Layout.power_shadow lay;
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
  let silent = Control.silent ctl in
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

  (* Visual *)
  let old_visual = ctl.visual in
  if Layout.visual_key lay || Layout.visual_button lay
  || ctl.visual = `None && Layout.novisual_button lay then
    cycle_visual st;
  Option.iter (Layout.visual_indicator lay) (idx_visual st);

  (match ctl.visual with
  | `None -> ()

  | `Cover ->
    Option.iter (fun (track : Data.track) ->
      Option.iter (Layout.cover lay)
        (Library.load_cover st.library win track.path)
    ) ctl.current

  | `Spectrum ->
    let len = Array.length ctl.raw in
    let lim = Spectrum.fft_samples in
    if len >= lim then
    (
      (* This could race, but that's okay *)
      let raw = ctl.raw in
      let wave, rest =
        if len = lim then raw, [||] else
        Array.sub raw 0 lim, Array.sub raw lim (len - lim)
      in
      ctl.raw <- rest;
      ctl.data <- Spectrum.bands wave ctl.spec_bands;
    );
    let data = ctl.data in

    let x, y, w, h = Ui.dim lay.ui (Layout.graph_area lay) in
    let y, h = y + 2, h - 4 in
    let win = Ui.window lay.ui in
    let green = Ui.text_color lay.ui in
    let yellow = Ui.warn_color lay.ui in
    let red = Ui.error_color lay.ui in

    for i = 0 to Array.length data - 1 do
      let wsep =
        if ctl.spec_bands <= 12 then 3 else
        if ctl.spec_bands <= 32 then 2 else 1
      in
      let w' = w / ctl.spec_bands - wsep in
      let x' = x + i * (w' + wsep) in
      Api.Draw.fill win x' y w' h (Ui.unlit_color red);
      let hy = 10 * h / 12 in
      Api.Draw.fill win x' (y + h - hy) w' hy (Ui.unlit_color yellow);
      let hg = 8 * h / 12 in
      Api.Draw.fill win x' (y + h - hg) w' hg (Ui.unlit_color green);
      let hr = min (int_of_float (data.(i) /. 5.0 *. float h)) h in
      Api.Draw.fill win x' (y + h - hr) w' hr red;
      let hy = min hr hy in
      Api.Draw.fill win x' (y + h - hy) w' hy yellow;
      let hg = min hr hg in
      Api.Draw.fill win x' (y + h - hg) w' hg green;
      for j = 0 to h/2 - 1 do
        Api.Draw.fill win x (y + 2 * j) w 1 `Black;
      done
    done;

  | `Wave ->
    let data = if ctl.raw = [||] then ctl.data else ctl.raw in
    ctl.raw <- [||];
    ctl.data <- data;

    let x, y, w, h = Ui.dim lay.ui (Layout.graph_area lay) in
    let win = Ui.window lay.ui in
    for i = 0 to (min w (Array.length data))/2 - 1 do
      let i = 2 * i in
      let v = data.(i) *. float h /. 1.5 in
      let x, y = x + i, y + h/2 - int_of_float v in
      Api.Draw.fill win x y 1 1 `White;
    done;

  | `Oscilloscope ->
    let data = if ctl.raw = [||] then ctl.data else ctl.raw in
    ctl.raw <- [||];
    ctl.data <- data;

    let x, y, w, h = Ui.dim lay.ui (Layout.graph_area lay) in
    let win = Ui.window lay.ui in

    (match Layout.graph_drag lay (1, 1) with
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

    let len = Array.length data in
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
  );

  (* FPS *)
  if ctl.fps then
    Layout.fps_text lay `Regular true (fmt "%d FPS" (Api.Window.fps win));
  (* Press of FPS key: toggle FPS display *)
  if Layout.fps_key lay then toggle_fps st;

  if Layout.sdf_key lay then toggle_sdf st;

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
  && progress' <> progress && ctl.current <> None then
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
  let rh = lay.text + 2 * lay.pad_y in
  let page = max 1 (int_of_float (Float.floor (float h /. float rh))) in

  Layout.button_shadow lay;

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
  let bwd = Layout.bwd_button lay focus (Some false) in
  let fwd = Layout.fwd_button lay focus (Some false) in
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
  let playing' = Layout.play_button lay focus (Some playing) in
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
  let paused' = Layout.pause_button lay focus (Some paused) in
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
  if Layout.stop_button lay focus (Some false) then
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
      Control.switch ctl (Playlist.current st.playlist);
      Control.play st.control;
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
  Layout.shuffle_label lay;
  Layout.shuffle_indicator lay shuffle;
  Layout.shuffle_shadow lay;
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
  Layout.repeat_indicator1 lay (ctl.repeat = `One);
  Layout.repeat_indicator2 lay (ctl.repeat = `All);
  Layout.repeat_shadow lay;
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
  Layout.loop_shadow lay;
  if Layout.loop_button lay focus (Some false) then
  (
    (* Click on Loop button: cycle loop mode *)
    cycle_loop st
  );

  (* Pop-ups *)

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
  else if ctl.visual = `Cover && old_visual = `Cover && not (Control.silent ctl)
    && Layout.cover_popup_open lay then
  (
    Run_menu.popup st `Current
  )


(* Pane Activation Runner *)

let run_toggle_panel (st : state) =
  let lay = st.layout in

  Layout.playlist_label lay;
  Layout.playlist_shadow lay;
  Layout.playlist_indicator lay lay.playlist_shown;
  let playlist_shown' = Layout.playlist_button lay (Some lay.playlist_shown) in
  (* Click on playlist activation button: toggle playlist *)
  lay.playlist_shown <- playlist_shown';
  if not playlist_shown' then Playlist.defocus st.playlist;

  Layout.library_label lay;
  Layout.library_shadow lay;
  Layout.library_indicator lay lay.library_shown;
  let library_shown' = Layout.library_button lay (Some lay.library_shown) in
  (* Click on library activation button: toggle library *)
  if library_shown' <> lay.library_shown then
  (
    if Api.Key.is_modifier_down `Shift then
      (* Shift-click: switch sides for library pane *)
      (if lay.library_shown then toggle_side st)
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
