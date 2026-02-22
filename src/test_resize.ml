let w1 = 800
let w2 = 400
let w = w1 + w2
let h = 800
let x1 left = if left then 400 else 400 + w2
let x2 left = if left then 400 + w1 else 400
let x left = if left then x1 left else x2 left
let y = 200
let m = 50
let o1 left = if left then 0 else w2
let o2 left = if left then w1 else 0

let _main =
  Raylib.set_trace_log_level Raylib.TraceLogLevel.Warning;
  Raylib.set_config_flags [Raylib.ConfigFlags.Window_undecorated];
  Raylib.init_window w h "Test Resizing";
  Raylib.set_window_position (x true) y;

  let buf = Raylib.load_render_texture w h in
  (* Override texture format to not use alpha channel *)
  Raylib.unload_texture (Raylib.RenderTexture.texture buf);
  let format = Raylib.PixelFormat.(to_int Uncompressed_r8g8b8) in
  let id = Raylib.Rlgl.load_texture Ctypes.null w h format 1 in
  let tex = Raylib.Texture.create id w h 1 Raylib.PixelFormat.Uncompressed_r8g8b8 in
  Raylib.RenderTexture.set_texture buf tex;
  (* Mirror Raylib LoadRenderTexture: *)
  Raylib.Rlgl.framebuffer_attach (Raylib.RenderTexture.id buf) id
    0 (* = RL_ATTACHMENT_COLOR0 *) 100 (* = RL_ATTACHMENT_TEXTURE2D *) 0;

  let large = ref true in
  let left = ref true in
  while not (Raylib.window_should_close ()) do
    Raylib.begin_drawing ();
    Unix.sleepf 0.01;  (* emulate more computation *)

    let x1', y1', w1', h1' = (if !large then o1 !left + m else m + w2), m, w1 - 2*m, h - 2*m in
    let x2', y2', w2', h2' = (if !large then o2 !left + m else m), m, w2/4, h - 2*m in
    let mv = Raylib.get_mouse_position () in
    let mx, my = Raylib.Vector2.(int_of_float (x mv), int_of_float (y mv)) in
    let m_in1 = x1' <= mx && mx < x1' + w1' && y1' <= my && my < y1' + h1' in
    let m_in2 = x2' <= mx && mx < x2' + w2' && y2' <= my && my < y2' + h2' in
    if Raylib.is_mouse_button_down Raylib.MouseButton.Right then
    (
      if m_in1 then left := not !left;
      if m_in2 then large := not !large;
    );
    if Raylib.is_mouse_button_pressed Raylib.MouseButton.Left then
    (
      if m_in1 then left := not !left else
      if m_in2 then large := not !large else exit 0
    );
    let resizing = !large <> (Raylib.get_screen_width () = w) in

    Raylib.begin_texture_mode buf;
    Raylib.clear_background Raylib.Color.black;
    if !large then
    (
      Raylib.draw_rectangle x1' y1' w1' (h1'/2) Raylib.Color.red;
      Raylib.draw_rectangle x1' (y1' + h1'/2) w1' (h1'/2) Raylib.Color.blue;
      Raylib.draw_text "toggle side" x1' (y1' + h1') 10 Raylib.Color.white;
    );
    Raylib.draw_rectangle x2' y2' w2' h2' Raylib.Color.yellow;
    Raylib.draw_text "open/close" x2' (y2' + h2') 10 Raylib.Color.white;
    Raylib.draw_text "exit" (x2' + w2' + m) (y2' + h2') 10 Raylib.Color.white;
    Raylib.draw_text "L: click, R: hold" ((if !large then w else w2)/2) (h - 10) 10 Raylib.Color.white;
    Raylib.end_texture_mode ();

    if resizing then Raylib.set_window_size (if !large then w else w2) h;

    let w = if !large then w else w2 in
    let r' = Raylib.Rectangle.create 0.0 0.0 (float w) (-. float h) in
    let r = Raylib.Rectangle.create 0.0 0.0 (float w) (float h) in
    let v = Raylib.Vector2.create 0.0 0.0 in
    let img = Raylib.RenderTexture.texture buf in
    Raylib.draw_texture_pro img r' r v 0.0 Raylib.Color.white;

    if resizing then Raylib.set_window_position (if !large then x !left else x2 !left) y;

    Raylib.end_drawing ();
  done
