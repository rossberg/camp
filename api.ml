(* Graphics/sound API abstraction *)

open Audio_file


(* Base types *)

type path = File.path
type time = File.time


(* Geometry helpers *)

type point = int * int
type size = int * int
type rect = int * int * int * int

type side = [`Left | `Right]
type face = [`Up | `Down]
type dir = [side | face]
type orientation = [`Horizontal | `Vertical]
type corner = [`NW | `NE | `SW | `SE]

let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
let mul (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)

let inside (x, y) (x', y', w, h) =
  x' <= x && x < x' + w && y' <= y && y < y' + h

let point_of_vec2 v =
  Raylib.Vector2.(int_of_float (x v), int_of_float (y v))

let vec2_of_point (x, y) =
  Raylib.Vector2.create (float x) (float y)

let floats_of_vec2 v =
  Raylib.Vector2.(x v, y v)


(* OS-specific Nonsense *)

let is_mac =
  Sys.unix &&
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  close_in ic;
  uname = "Darwin"


(* Per-frame Actions *)

let after_frame_start = ref []
let before_frame_finish = ref []
let after_frame_finish = ref []


(* Window *)

type window = unit
type icon = Raylib.Image.t

module Window =
struct
  let min_pos = ref (0, 0)
  let max_size = ref (0, 0)

  let current_pos = ref (0, 0)   (* buffered during minimization *)
  let current_size = ref (0, 0)
  let next_pos = ref None        (* defer window changes to frame end *)
  let next_size = ref None

  let update () =
    if not (Raylib.is_window_minimized ()) then
    (
      current_pos := point_of_vec2 (Raylib.get_window_position ());
      current_size := (Raylib.get_screen_width (), Raylib.get_screen_height ());
    )

  (* We have to set the window position before events are processed,
   * and the window size after. Otherwise we're seeing strange bobbing. *)
  let _ = after_frame_start := update :: !after_frame_start
  let _ = before_frame_finish :=
    (fun () ->
      Option.iter (fun (x, y) -> Raylib.set_window_position x y) !next_pos;
      next_pos := None;
    ) :: !before_frame_finish
  let _ = after_frame_finish :=
    (fun () ->
      Option.iter (fun (w, h) -> Raylib.set_window_size w h) !next_size;
      next_size := None;
      Raylib.(set_exit_key Key.Null);  (* seems to be reset somehow? *)
    ) :: !after_frame_finish

  let init x y w h s =
    Raylib.(set_trace_log_level TraceLogLevel.Warning);
    Raylib.(set_exit_key Key.Null);

    (* Discover screen geometry by opening a dummy window and maximise it. *)
    Raylib.(set_config_flags
      ConfigFlags.[Window_undecorated; Window_resizable]);
    Raylib.init_window 8000 4000 "";
    Raylib.maximize_window ();
    update ();
    Raylib.close_window ();
    min_pos := !current_pos;
    max_size := !current_size;

    Raylib.(set_config_flags
      ConfigFlags.[Window_undecorated; Window_always_run;
        (*Window_transparent;*) Vsync_hint; Msaa_4x_hint]);
    Raylib.init_window w h s;
    Raylib.set_window_position x y;
    update ()

  let closed () = Raylib.window_should_close ()

  let pos () = !current_pos
  let size () = !current_size
  let set_pos () x y = next_pos := Some (x, y)
  let set_size () w h = next_size := Some (w, h)
  let set_icon () img = if not is_mac then Raylib.set_window_icon img

  let min_pos () = !min_pos
  let max_size () = !max_size

  let minimize () = Raylib.minimize_window ()
  let restore () = Raylib.restore_window ()
  let is_minimized () = Raylib.is_window_minimized ()

  let hide () = Raylib.set_window_state [Raylib.ConfigFlags.Window_hidden]
  let reveal () = Raylib.clear_window_state [Raylib.ConfigFlags.Window_hidden]
  let is_hidden () = Raylib.is_window_hidden ()

  let screen_size () =
    let mon = Raylib.get_current_monitor () in
    Raylib.get_monitor_width mon, Raylib.get_monitor_height mon

  let fps () = Raylib.get_fps ()
end


(* Colors *)

type color =
[
  | `Blank | `Black | `White
  | `Red | `Orange | `Yellow | `Green | `Blue
  | `Gray of int
  | `RGB of int
  | `Trans of color * int
]

let rec color = function
  | `Blank -> Raylib.Color.blank
  | `Black -> Raylib.Color.black
  | `White -> Raylib.Color.white
  | `Red -> Raylib.Color.red
  | `Orange -> Raylib.Color.orange
  | `Yellow -> Raylib.Color.yellow
  | `Green -> Raylib.Color.green
  | `Blue -> Raylib.Color.blue
  | `Gray n -> Raylib.Color.create n n n 0xff
  | `RGB x ->
    Raylib.Color.create (x lsr 16 land 0xff) (x lsr 8 land 0xff) (x land 0xff) 0xff
  | `Trans (col, x) ->
    let c = color col in
    let r, g, b, a = Raylib.Color.(r c, g c, b c, a c) in
    Raylib.Color.create r g b (x * a / 0xff)

module Color =
struct
  let darken f col =
  let c = color col in
  let r, g, b, a = Raylib.Color.(r c, g c, b c, a c) in
  let r', g', b' = f * r / 0x100, f * g / 0x100, f * b / 0x100 in
  let col' = `RGB (r' lsl 16 + g' lsl 8 + b') in
  if a = 0xff then col' else `Trans (col', a)
end


(* Fonts *)

type font = Raylib.Font.t (* * Raylib.Shader.t *)

module Font =
struct
  let load () path min max size =
    let glyphs = Ctypes.(CArray.make int (max - min)) in
    for i = min to max - 1 do Ctypes.CArray.set glyphs (i - min) i done;
    let font = Raylib.load_font_ex path size (Some glyphs) in
    (*Raylib.(set_texture_filter (Font.texture font) TextureFilter.Point);*)
    font

(* Can't get SDF to work. Translated from https://github.com/raysan5/raylib/blob/master/examples/text/text_font_sdf.c
  let load () path size =
    let font = Raylib.load_font_ex path size None in
    Raylib.Font.set_base_size font size;
    Raylib.Font.set_glyph_padding font 0;
    let data = File.load `Bin path in
    let glyphs = Raylib.load_font_data data (String.length data) size
      Ctypes.(from_voidp int null) 0 Raylib.FontType.(to_int Sdf) in
    Raylib.Font.set_glyphs font (Ctypes.CArray.from_ptr glyphs 95);
    let atlas = Raylib.gen_image_font_atlas
      (Ctypes.CArray.start (Raylib.Font.glyphs font))
      (Ctypes.allocate (Ctypes.ptr Raylib.Rectangle.t) (Raylib.Font.recs font))
      95 size 0 1 in
    Raylib.Font.set_texture font (Raylib.load_texture_from_image atlas);
    Raylib.unload_image atlas;
    let shader = Raylib.load_shader "vertex.fs" "sdf.fs" in
    Raylib.set_texture_filter (Raylib.Font.texture font)
      Raylib.TextureFilter.Bilinear;
    font, shader
*)
end


(* Image *)

type image = Raylib.Texture.t

module Image =
struct
  type raw = Raylib.Image.t
  type prepared = image

  let load_raw path = Raylib.load_image path

  let mime_prefix = "image/"
  let load_raw_from_memory mime data =
    if not (String.starts_with ~prefix: mime_prefix mime) then
      failwith "Image.load_from_memory";
    let n = String.length mime_prefix in
    let ext = "." ^ String.sub mime n (String.length mime - n) in
    Raylib.load_image_from_memory ext data (String.length data)

  let extract img x y w h =
    Raylib.image_from_image img
      (Raylib.Rectangle.create (float x) (float y) (float w) (float h))

  let prepare () raw =
    let img = Raylib.load_texture_from_image raw in
    Raylib.set_texture_filter img Raylib.TextureFilter.Bilinear;
    img

  let load () path = prepare () (load_raw path)
  let load_from_memory () mime data =
    prepare () (load_raw_from_memory mime data)

  let size img = Raylib.Texture.(width img, height img)
end


(* Drawing *)

type buffer = Raylib.RenderTexture.t

module Buffer =
struct
  let create w h =
    let buf = Raylib.load_render_texture w h in
    (* Override texture format to not use alpha channel *)
    Raylib.unload_texture (Raylib.RenderTexture.texture buf);
    let format = Raylib.PixelFormat.(to_int Uncompressed_r8g8b8) in
    let id' = Raylib.Rlgl.load_texture Ctypes.null w h format 1 in
    let open Raylib.Texture in
    let tex' = create id' w h 1 Raylib.PixelFormat.Uncompressed_r8g8b8 in
    Raylib.RenderTexture.set_texture buf tex';
    (* Mirror Raylib LoadRenderTexture: *)
    Raylib.Rlgl.framebuffer_attach (Raylib.RenderTexture.id buf) id'
      0 (* = RL_ATTACHMENT_COLOR0 *) 100 (* = RL_ATTACHMENT_TEXTURE2D *) 0;
    buf

  let dispose = Raylib.unload_render_texture
  let size buf = Image.size (Raylib.RenderTexture.texture buf)
end


module Draw =
struct
  let frame = ref 0

  let start () c =
    Raylib.begin_drawing ();
    Raylib.clear_background (color c);
(* TODO: Raylib OCaml is missing set_blend_factors_separate
    Raylib.(begin_blend_mode BlendMode.Custom_separate);
    let rl_func_add = 0x8006 in
    let rl_max = 0x8008 in
    Raylib.Rlgl.set_blend_factors_separate 1 1 1 1 rl_func_add rl_max;
*)
    List.iter (fun f -> f ()) !after_frame_start

  let finish () =
    List.iter (fun f -> f ()) !before_frame_finish;
    incr frame;
    Raylib.end_drawing ();  (* polls input events *)
    List.iter (fun f -> f ()) !after_frame_finish

  let buffered () buf = Raylib.begin_texture_mode buf
  let unbuffered () = Raylib.end_texture_mode ()

  let clip () x y w h = Raylib.begin_scissor_mode x y w h
  let unclip () = Raylib.end_scissor_mode ()

  let frame () = !frame

  let line () x y x' y' c =
    Raylib.draw_line x y x' y' (color c)

  let fill () x y w h c =
    Raylib.draw_rectangle x y w h (color c)

  let rect () x y w h c =
    Raylib.draw_rectangle_lines x y w h (color c)

  let gradient () x y w h c1 o c2 =
    (match o with
    | `Horizontal -> Raylib.draw_rectangle_gradient_h
    | `Vertical -> Raylib.draw_rectangle_gradient_v
    ) x y w h (color c1) (color c2)

  let fill_circ () x y w h c =
    Raylib.draw_ellipse (x + w/2) (y + h/2) (float w /. 2.0) (float h /. 2.0) (color c)

  let gradient_circ () x y w h c1 c2 =
    Raylib.draw_circle_gradient (x + w/2) (y + h/2) (float (w + h) /. 4.0) (color c1) (color c2)

  let circ () x y w h c =
    Raylib.draw_ellipse_lines (x + w/2) (y + h/2) (float w /. 2.0) (float h /. 2.0) (color c)

  let tri () x y w h c corner =
    let x', y' = x + w, y + h in
    let vs = List.map vec2_of_point [x, y; x, y'; x', y'; x', y] in
    let drop = match corner with `NW -> 2 | `NE -> 1 | `SW -> 3 | `SE -> 0 in
    let vs' = Array.of_list (List.filteri (fun i _ -> i <> drop) vs) in
    Raylib.draw_triangle vs'.(0) vs'.(1) vs'.(2) (color c)

  let arrow () x y w h c dir =
    let vs = Array.map vec2_of_point
      (match dir with
      | `Up -> [|x + w/2, y; x, y + h; x + w, y + h|]
      | `Down -> [|x + w/2, y + h; x + w, y; x, y|]
      | `Left -> [|x, y + h/2; x + w, y + h; x + w, y|]
      | `Right -> [|x + w, y + h/2; x, y; x, y + h|]
      )
    in
    Raylib.draw_triangle vs.(0) vs.(1) vs.(2) (color c)

  let text () x y h c f s =
    Raylib.draw_text_ex f s (vec2_of_point (x, y)) (float h) 1.0 (color c)
(*
    Raylib.begin_shader_mode (snd f);
    Raylib.draw_text_ex (fst f) s (vec2_of_point (x, y)) (float h) 0.0 (color c);
    Raylib.end_shader_mode ()
*)

  let text_width () h f s =
    fst (point_of_vec2 (Raylib.measure_text_ex f s (float h) 1.0))

  let text_spacing () _h _f = 1

  let image () x y scale img =
    let v = vec2_of_point (x, y) in
    Raylib.draw_texture_ex img v 0.0 scale Raylib.Color.white

  let image_part () x y w h x' y' w' h' img =
    let r' = Raylib.Rectangle.create (float x') (float y') (float w') (float h') in
    let r = Raylib.Rectangle.create (float x) (float y) (float w) (float h) in
    let v = vec2_of_point (0, 0) in
    Raylib.draw_texture_pro img r' r v 0.0 Raylib.Color.white

  let buffer () x y buf =
    let w, h = Buffer.size buf in
    let r = Raylib.Rectangle.create 0.0 0.0 (float w) (-. float h) in
    let v = vec2_of_point (x, y) in
    let img = Raylib.RenderTexture.texture buf in
    Raylib.draw_texture_rec img r v Raylib.Color.white
end


(* Input devices *)

type key =
[
  | `None
  | `Char of char
  | `Arrow of dir
  | `Page of face
  | `End of face
  | `Return
  | `Enter
  | `Tab
  | `Escape
  | `Backspace
  | `Delete
  | `Insert
  | `F of int
  | `Shift of side
  | `Command of side
  | `Alt of side
  | `Caps
]

type modifier = [`Shift | `Command | `Alt]

type resize = [`N_S | `E_W | `NE_SW | `NW_SE | `All]
type cursor =
[
  | `Default
  | `Arrow
  | `Busy
  | `Blocked
  | `Beam
  | `Crosshair
  | `Point
  | `Resize of resize
]

module Mouse =
struct
  let last_win_pos = ref (0, 0) (* store to work around Raylib not updating relative mouse pos on window move *)
  let current_pos = ref (0, 0)  (* work around Raylib mouse pos bug *)
  let last_pos = ref (0, 0)     (* implement our own mouse delta, since Raylib's is off as well *)
  let last_press_pos = ref (min_int, min_int)
  let last_press_left = ref 0.0
  let last_press_right = ref 0.0
  let is_double_left = ref false
  let is_double_right = ref false
  let is_drag_left = ref false
  let is_drag_right = ref false
  let next_cursor = ref Raylib.MouseCursor.Default
  let last_screen_pos = ref (0, 0)

  let pos () = !current_pos
  let delta () = sub !current_pos !last_pos
  let wheel () = floats_of_vec2 (Raylib.get_mouse_wheel_move_v ())

  let screen_pos () = add (pos ()) (Window.pos ())
  let screen_delta () = sub (screen_pos ()) !last_screen_pos

  let button = function
    | `Left -> Raylib.MouseButton.Left
    | `Right -> Raylib.MouseButton.Right

  let is_down but = Raylib.is_mouse_button_down (button but)
  let is_pressed but = Raylib.is_mouse_button_pressed (button but)
  let is_released but = Raylib.is_mouse_button_released (button but)

  let is_doubleclick = function
    | `Left -> !is_double_left
    | `Right -> !is_double_right

  let is_drag = function
    | `Left -> !is_drag_left
    | `Right -> !is_drag_right

  let set_cursor () cursor =
    next_cursor :=
      let open Raylib.MouseCursor in
      match cursor with
      | `Default -> Default
      | `Arrow -> Arrow
      | `Busy -> Arrow  (* not supported by Raylib? *)
      | `Blocked -> Not_allowed
      | `Beam -> Ibeam
      | `Crosshair -> Crosshair
      | `Point -> Pointing_hand
      | `Resize `N_S -> Resize_ns
      | `Resize `E_W -> Resize_ew
      | `Resize `NE_SW -> Resize_nesw
      | `Resize `NW_SE -> Resize_nwse
      | `Resize `All -> Resize_all

  let _ = after_frame_start :=
    (fun () ->
      (* Work around Raylib issue: if window was moved but mouse hasn't, then
       * mouse pos is off; detect and correct by adding window delta. *)
      current_pos := point_of_vec2 (Raylib.get_mouse_position ());
      let win_pos = point_of_vec2 (Raylib.get_window_position ()) in
      let mouse_delta = point_of_vec2 (Raylib.get_mouse_delta ()) in
      let win_delta = sub win_pos !last_win_pos in
      if not is_mac || mouse_delta <> (0, 0) then
        last_win_pos := win_pos  (* true mouse location caught up *)
      else if is_mac && win_delta <> (0, 0) then
        current_pos := sub !current_pos win_delta;

      (* Detect double click *)
      let left = is_pressed `Left in
      let right = is_pressed `Right in
      is_double_left := false;
      is_double_right := false;
      if left || right then
      (
        if left then last_press_right := 0.0;
        if right then last_press_left := 0.0;
        let now = Unix.gettimeofday () in
        let mx, my = !last_press_pos in
        let (mx', my') as m' = pos () in
        let unmoved = abs (mx' - mx) < 16 && abs (my' - my) < 16 in
        is_double_left := left && unmoved && now -. !last_press_left < 0.5;
        is_double_right := right && unmoved && now -. !last_press_right < 0.5;
        last_press_left := now;
        last_press_right := now;
        last_press_pos := m';
      );

      (* Detect dragging *)
      let moved = screen_delta () <> (0, 0) in
      if is_down `Left then
        is_drag_left := !is_drag_left || moved
      else if not (is_released `Left) then
        is_drag_left := false;
      if is_down `Right then
        is_drag_right := !is_drag_right || moved
      else if not (is_released `Right) then
        is_drag_right := false;

      (* Deferred update of mouse cursor *)
      Raylib.set_mouse_cursor !next_cursor;
      next_cursor := Raylib.MouseCursor.Default;
    ) :: !after_frame_start

  let _ = before_frame_finish :=
    (fun () ->
      last_pos := !current_pos;
      last_screen_pos := screen_pos ();
    ) :: !before_frame_finish
end

module Key =
struct
  let key = function
    | `None -> Raylib.Key.Null
    | `Char '-' -> Raylib.Key.Minus
    | `Char '+' -> Raylib.Key.Equal
    | `Char c -> Raylib.Key.of_int (Char.code (Char.uppercase_ascii c))
    | `Arrow `Left -> Raylib.Key.Left
    | `Arrow `Right -> Raylib.Key.Right
    | `Arrow `Up -> Raylib.Key.Up
    | `Arrow `Down -> Raylib.Key.Down
    | `Page `Up -> Raylib.Key.Page_up
    | `Page `Down -> Raylib.Key.Page_down
    | `End `Up -> Raylib.Key.Home
    | `End `Down -> Raylib.Key.End
    | `Return -> Raylib.Key.Enter
    | `Enter -> Raylib.Key.Kp_enter
    | `Tab -> Raylib.Key.Tab
    | `Escape -> Raylib.Key.Escape
    | `Backspace -> Raylib.Key.Backspace
    | `Delete -> Raylib.Key.Delete
    | `Insert -> Raylib.Key.Insert
    | `F 1 -> Raylib.Key.F1
    | `F 2 -> Raylib.Key.F2
    | `F 3 -> Raylib.Key.F3
    | `F 4 -> Raylib.Key.F4
    | `F 5 -> Raylib.Key.F5
    | `F 6 -> Raylib.Key.F6
    | `F 7 -> Raylib.Key.F7
    | `F 8 -> Raylib.Key.F8
    | `F 9 -> Raylib.Key.F9
    | `F 10 -> Raylib.Key.F10
    | `F 11 -> Raylib.Key.F11
    | `F 12 -> Raylib.Key.F12
    | `F _ -> failwith "Api.Key.key"
    | `Shift `Left -> Raylib.Key.Left_shift
    | `Shift `Right -> Raylib.Key.Right_shift
    | `Command `Left -> Raylib.Key.(if is_mac then Left_super else Left_control)
    | `Command `Right -> Raylib.Key.(if is_mac then Right_super else Right_control)
    | `Alt `Left -> Raylib.Key.Left_alt
    | `Alt `Right -> Raylib.Key.Right_alt
    | `Caps -> Raylib.Key.Caps_lock

  let is_down k = Raylib.is_key_down (key k)
  let is_pressed k = Raylib.is_key_pressed (key k)
  let is_released k = Raylib.is_key_released (key k)
  let is_repeated k = Raylib.is_key_pressed_repeat (key k)
  let is_pressed_or_repeated k =
    Raylib.(is_key_pressed (key k) || is_key_pressed_repeat (key k))

  let shift = [`Shift `Left; `Shift `Right]
  let alt = [`Alt `Left; `Alt `Right]
  let control = [`Command `Left; `Command `Right]

  let some_down = List.exists is_down
  let is_modifier_down = function
    | `Shift -> some_down shift
    | `Alt -> some_down alt
    | `Command -> some_down control

  let all_modifiers = [|`Shift ; `Alt ; `Command|]
  let are_modifiers_down modifiers =
    Array.for_all (fun key ->
      is_modifier_down key = List.mem key modifiers
    ) all_modifiers

  let char () = Raylib.get_char_pressed ()

  (* Test string: "␣−←→↑↓⤒↟⤓↡⇤⇱⇥⇲⏎⌤⇆↹⎋⌫⌦⎀⁁⇪⇧⌥⎇⌘" *)

  let key_name = function
    | `None -> ""
    | `Char ' ' -> "Space"  (* "␣" *)
    | `Char '-' -> "−"
    | `Char c -> String.make 1 c
    | `Arrow `Left -> "←"
    | `Arrow `Right -> "→"
    | `Arrow `Up -> "↑"
    | `Arrow `Down -> "↓"
    | `Page `Up -> "PgUp"  (* "⤒" *) (* "↟" *)
    | `Page `Down -> "PgDn"  (* "⤓" *) (* "↡" *)
    | `End `Up -> "Home"  (* "⇤" *) (* "⇱" *)
    | `End `Down -> "End"  (* "⇥" *) (* "⇲" *)
    | `Return -> "Return"  (* "⏎" *)
    | `Enter -> "Enter"  (* "⌤" *)
    | `Tab -> "Tab"  (* "⇆" *) (* "↹" *)
    | `Escape -> "Esc"  (* "⎋" *)
    | `Backspace -> "Back"  (* "⌫" *)
    | `Delete -> "Del"  (* "⌦" *)
    | `Insert -> "Ins"  (* "⎀" *) (* "⁁" *)
    | `F n -> "F" ^ string_of_int n
    | `Shift `Left -> "LShift"  (* "L⇧" *)
    | `Shift `Right -> "RShift"  (* "R⇧" *)
    | `Command `Left -> if is_mac then "LCmd" (* "L⌘" *) else "LCtrl" (* "L^" *)
    | `Command `Right -> if is_mac then "RCmd" (* "R⌘" *) else "RCtrl" (* "R^" *)
    | `Alt `Left -> if is_mac then "LOpt" (* "L⌥" *) else "LAlt" (* "L⎇" *)
    | `Alt `Right -> if is_mac then "ROpt" (* "R⌥" *) else "RAlt" (* "R⎇" *)
    | `Caps -> "Caps"  (* "⇪" *)

  let modifier_name = function
    | `Shift -> "Shift"  (* "⇧" *)
    | `Alt -> if is_mac then "Opt" (* "⌥" *) else "Alt" (* "⎇" *)
    | `Command -> if is_mac then "Cmd" (* "⌘" *) else "Ctrl" (* "^" *)
end


(* Audio *)

type sound = {music : Raylib.Music.t; format : Format.t; temp : path option (* for UTF-8 workaround *)}
type audio = {mutex : Mutex.t; mutable sound : sound}

module Audio =
struct
  let silent = ref None
  let silence _ =
    match !silent with
    | Some sound -> sound
    | None ->
      let assets = File.(dir Sys.argv.(0) // "assets") in
      let music = Raylib.load_music_stream File.(assets // "silence.mp3") in  (* TODO *)
      let sound = {music; format = Format.unknown; temp = None} in
      silent := Some sound;
      sound

  let init () =
    Raylib.(set_trace_log_level TraceLogLevel.Warning);
    Raylib.init_audio_device ();
    let audio = {mutex = Mutex.create (); sound = silence ()} in

    let rec refill () =
      Mutex.protect audio.mutex (fun () ->
        if audio.sound != silence ()
        && Raylib.is_music_stream_playing audio.sound.music then
          Raylib.update_music_stream audio.sound.music;
      );
      Unix.sleepf 0.01;
      refill ()
    in ignore (Domain.spawn refill);

    audio

  let retain = ref []

  let load _ path =
    if not (File.exists path) then silence () else
    (* Raylib can't handle UTF-8 file paths, so copy those to temp file. *)
    (* Also, FLAC codec appears to keep files locked indefinitely on Windows. *)
    (* (Raylib.load_music_stream_from_memory is broken and segfaults.) *)
    let path' =
      if String.uppercase_ascii (File.extension path) = ".FLAC"
      || not (Unicode.is_ascii path)
      then Storage.copy_to_temp path
      else path
    in
    let format = try Format.read path' with _ -> Format.unknown in
    let music = Raylib.load_music_stream path' in
    (* TODO: This is a work-around for a bug in Raylib < 5.5.
     * We intentionally leak failed streams in order to avoid a double free
     * segfault in Raylib. (See https://github.com/raysan5/raylib/issues/3889
     * and https://github.com/raysan5/raylib/pull/3966). *)
    if not (Raylib.Music.looping music) (* test failure in Raylib < 5.5 *) then
    (
      Raylib.Music.set_ctx_type music 0;  (* failure in Raylib >= 5.5 *)
      retain := music :: !retain;
    );
    (* End of work-around. *)
    if Raylib.Music.ctx_type music = 0 then silence () else
    (
      (*Raylib.Music.set_looping music false;*)  (* TODO *)
      {music; format; temp = if path' = path then None else Some path'}
    )

  let free a sound =
    Mutex.protect a.mutex (fun () ->
      Raylib.stop_music_stream sound.music;
      Option.iter Storage.delete_temp sound.temp;
      Raylib.unload_music_stream sound.music
    )

  let play a sound =
    Mutex.protect a.mutex (fun () ->
      Raylib.play_music_stream sound.music;
      a.sound <- sound;
    )

  let stop a =
    Mutex.protect a.mutex (fun () ->
      Raylib.stop_music_stream a.sound.music;
      a.sound <- silence ();
    )

  let pause a =
    Mutex.protect a.mutex (fun () ->
      Raylib.pause_music_stream a.sound.music;
    )

  let resume a =
    Mutex.protect a.mutex (fun () ->
      Raylib.resume_music_stream a.sound.music;
      Raylib.update_music_stream a.sound.music;
    )

  let seek a t =
    Mutex.protect a.mutex (fun () ->
      Raylib.seek_music_stream a.sound.music t;
      Raylib.update_music_stream a.sound.music;
    )

  let volume a x =
    Mutex.protect a.mutex (fun () ->
      Raylib.set_music_volume a.sound.music x;
    )

  let protect a f = Mutex.protect a.mutex (fun () -> f a.sound.music)
  let is_playing a = protect a Raylib.is_music_stream_playing
  let length a = protect a Raylib.get_music_time_length
  let played a = protect a Raylib.get_music_time_played

  let channels _ sound = sound.format.channels
  let rate _ sound = sound.format.rate
  let depth _ sound = sound.format.depth
  let bitrate _ sound = sound.format.bitrate
end


(* Files *)

module Files =
struct
  let paths = ref []

  let dropped () =
    if !paths <> [] then !paths else
    if not (Raylib.is_file_dropped ()) then [] else
    let list = Raylib.load_dropped_files () in
    paths := Raylib.FilePathList.files list;
    Raylib.unload_dropped_files list;
    !paths

  let _ = before_frame_finish := (fun () -> paths := []) :: !before_frame_finish
end


(* Clipboard *)

module Clipboard =
struct
  let read () = Raylib.get_clipboard_text ()
  let write () s = Raylib.set_clipboard_text s
end
