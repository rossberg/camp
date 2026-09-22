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
  Printf.eprintf "[start sine]\n%!";
  Raylib.set_audio_stream_buffer_size_default 4096;
  let stream = Raylib.load_audio_stream 44100 16 1 in
  Raylib_ocaml.Callbacks.set_audio_stream_callback stream sine_wave_callback;
  Raylib.play_audio_stream stream;
  sine_stream := Some stream

let stop_sine_wave () =
  Printf.eprintf "[stop sine]\n%!";
  Option.iter Raylib.unload_audio_stream !sine_stream;
  sine_stream := None

let toggle_sine_wave () =
  (if !sine_stream = None then start_sine_wave else stop_sine_wave) ()

let tweak_sine_wave c =
  sine_freq := max 6.875 (min 28160.0 (!sine_freq *. Float.pow 2.0 (c /. 12.0)));
  Printf.eprintf "[tweak sine %+.0f] freq=%f\n%!" c !sine_freq

let run_sine_wave () =
  if Api.Key.is_pressed (`Char '/') then toggle_sine_wave ();
  if Api.Key.is_pressed_or_repeated (`Char ',') then tweak_sine_wave (-1.0);
  if Api.Key.is_pressed_or_repeated (`Char '.') then tweak_sine_wave (+1.0)
*)


(* Runner *)

let run (st : State.t) area vis img_opt =
  let geo = st.geometry in
  let ctl = st.control in
  let win = Ui.window geo.ui in
  let x, y, w, h = Ui.dim geo.ui area in

  let sx x = max 1 (x * w / 130) in
  let sy y = max 1 (y * h / 60) in
  let smin v = min (sx v) (sy v) in

(*
  run_sine_wave ();
*)

  (match vis with
  | `Cover ->
    Option.iter (fun img ->
      Ui.image geo.ui area (`Crop `Vertical) img;
    ) img_opt

  | `Turntable ->
    Option.iter (fun img ->
      let iw, ih = Api.Image.size img in
      let w', h' = w, w in
      let time = Api.Audio.played ctl.audio in
      let rot = time *. ctl.turn_rpm /. 60.0 *. 360.0 in
      let a = rot /. 360.0 *. 2.0 *. Float.pi in
      let f, sin, cos = float, Float.sin, Float.cos in
      let dx = int_of_float ((f w -. f w' *. cos a +. f h' *. sin a) /. 2.0) in
      let dy = int_of_float ((f w -. f h' *. cos a -. f w' *. sin a) /. 2.0) in
      Api.Draw.clip win x y w h;
      Api.Draw.image_part win (x + dx) (y + dy) w' h' 0 0 iw ih rot img;
      let fat = int_of_float (Float.sqrt 2.0 *. float w) + 3 in
      Api.Draw.fill_ring win (x - fat) (y - fat) (w + 2*fat) fat `Black;
      Api.Draw.unclip win;
    ) img_opt

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

    Api.Draw.fill_rect win x y w h `Black;

    let l = smin 1 in
    let y, h = y + sy 2, (h - sy 4) / l * l in
    let wbar = (w - sx 3) / n in
    let wsep = sx (if wbar <= 4 then 1 else if n <= 10 then 2 else 3) in
    let w' = wbar - wsep in
    let x, w = x + (w - 3 - n*wbar + wsep)/2, n*wbar - wsep + 4 in
    let win = Ui.window geo.ui in
    let green = Ui.text_color geo.ui in
    let yellow = Ui.warn_color geo.ui in
    let red = Ui.error_color geo.ui in

    for i = 0 to n - 1 do
      let x' = x + 2 + i * wbar in
      Api.Draw.fill_rect win x' y w' h (Ui.unlit_color red);
      let hy = (10 * h / 12) / l * l in
      Api.Draw.fill_rect win x' (y + h - hy) w' hy (Ui.unlit_color yellow);
      let hg = (8 * h / 12) / l * l in
      Api.Draw.fill_rect win x' (y + h - hg) w' hg (Ui.unlit_color green);
      let hr = min h ((int_of_float (bands.(i) /. 5.0 *. float h) + l/2) / l * l) in
      Api.Draw.fill_rect win x' (y + h - hr) w' hr red;
      let hy = min hr hy in
      Api.Draw.fill_rect win x' (y + h - hy) w' hy yellow;
      let hg = min hr hg in
      Api.Draw.fill_rect win x' (y + h - hg) w' hg green;
      for j = 0 to (h + 1) / l / 2 - 1 do
        Api.Draw.fill_rect win x (y + (2 * j + 1)*l) w l `Black;
      done
    done

  | `Waveform ->
    let data = if ctl.raw = [||] then ctl.data else ctl.raw in
    ctl.raw <- [||];
    ctl.data <- data;

    Api.Draw.fill_rect win x y w h `Black;
    let l = max 1 (smin 1 / 2) in
    for i = 0 to w / l / 2 - 1 do
      let i = 2 * i in
      let v = if i < Array.length data then data.(i) else 0.0 in
      let v' = v *. float h /. float l /. 1.5 in
      let x, y = x + l * i, y + h/2 - l * int_of_float v' in
      Api.Draw.fill_rect win x y l l `White;
    done;

  | `Oscilloscope ->
    let data = if ctl.raw = [||] then ctl.data else ctl.raw in
    let len = Array.length data in
    ctl.raw <- [||];
    ctl.data <- data;

    if w = h then
      Api.Draw.fill_circ win x y w h `Black
    else
      Api.Draw.fill_rect win x y w h `Black;

    if len > 0 then
    (
      (match Ui.drag geo.ui "osc_drag" area (1, 1) with
      | `None | `Click | `Drop | `Abort -> ()
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
