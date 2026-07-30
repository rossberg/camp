let w1 = 800
let w2 = 400
let w = w1 + w2
let h = 800
let x1 left = if left then 200 else 200 + w2
let x2 left = if left then 200 + w1 else 200
let x left = if left then x1 left else x2 left
let y = 100
let m = 50
let o1 left = if left then 0 else w2
let o2 left = if left then w1 else 0

let _main =
  Raylib.set_trace_log_level Raylib.TraceLogLevel.Warning;
  Raylib.set_config_flags Raylib.ConfigFlags.(window_undecorated (*+ vsync_hint*));
  Raylib.init_window w h "Test Resizing";
  Raylib.set_window_position (x true) y;
  Raylib.set_target_fps max_int;

  let buf = Raylib.load_render_texture w h in
  let id = Raylib.Texture.id (Raylib.RenderTexture.texture buf) in
  Raylib.Rlgl.framebuffer_attach (Raylib.RenderTexture.id buf) id
    Raylib.Rlgl.FramebufferAttachType.Color_channel0
    Raylib.Rlgl.FramebufferAttachTextureType.Texture2d 0;

  let large = ref true in
  let left = ref true in
  let mouse = ref (-1, -1) in
  let resizing = ref false in
  while not (Raylib.window_should_close ()) do
    if not !resizing then
    (
      let x1', y1', w1', h1' = (if !large then o1 !left + m else m + w), m, w1 - 2*m, h - 2*m in
      let x2', y2', w2', h2' = (if !large then o2 !left + m else m), m, w2/4, h - 2*m in
      let mx, my = !mouse in
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
    );
    resizing := !large <> (Raylib.get_screen_width () = w);

    let x1', y1', w1', h1' = (if !large then o1 !left + m else m + w), m, w1 - 2*m, h - 2*m in
    let x2', y2', w2', h2' = (if !large then o2 !left + m else m), m, w2/4, h - 2*m in

    (* Always paint buffer image for new size *)
    Raylib.begin_drawing ();
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

    Unix.sleepf 0.01;  (* emulate more computation *)

    (*if !resizing then Raylib.set_window_size (if !large then w else w2) h;*)
    if !resizing then
    (
      let w' = if !large then w else w2 in
      let r' = Raylib.Rectangle.create 0.0 0.0 (float w') (-. float h) in
      let r = Raylib.Rectangle.create 0.0 0.0 (float w') (float h) in
      let v = Raylib.Vector2.create 0.0 0.0 in
      let img = Raylib.RenderTexture.texture buf in
      Raylib.draw_texture_pro img r' r v 0.0 Raylib.Color.white;
      Raylib.end_drawing ();
      Raylib.set_window_size (if !large then w else w2) h;
      Raylib.set_window_position (if !large then x !left else x2 !left) y;
      Raylib.begin_drawing ();
    );

    let w' = if !large then w else w2 in
    let r' = Raylib.Rectangle.create 0.0 0.0 (float w') (-. float h) in
    let r = Raylib.Rectangle.create 0.0 0.0 (float w') (float h) in
    let v = Raylib.Vector2.create 0.0 0.0 in
    let img = Raylib.RenderTexture.texture buf in
    Raylib.draw_texture_pro img r' r v 0.0 Raylib.Color.white;

    Raylib.end_drawing ();

    (*if !resizing then Raylib.set_window_position (if !large then x !left else x2 !left) y;*)

    let mv = Raylib.get_mouse_position () in
    let mx, my = Raylib.Vector2.(int_of_float (x mv), int_of_float (y mv)) in
    let dx = if not !resizing then 0 else x !left - x2 !left in
    let sx = if !large then -1 else +1 in
    mouse := (mx + 0*sx*dx, my);

(*
let wv = Raylib.get_window_position () in
let wx = int_of_float (Raylib.Vector2.x wv) in
let wv' = Raylib.get_window_position () in
let wx' = int_of_float (Raylib.Vector2.x wv') in
if resizing then Printf.printf "[] wx=%d wx'=%d d=%+d dm=%+d\n%!" wx wx' (wx'-wx) (sx*dx);
*)
  done
