(* Graphics/sound API abstraction *)

open Audio_file


(* Base types *)

type path = string
type time = float


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

let inside (x, y) (x', y', w, h) =
  x' <= x && x <= x' + w && y' <= y && y <= y' + h

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


(* Window *)

type window = unit
type icon = Raylib.Image.t

module Window =
struct
  let init x y w h s =
    Raylib.(set_trace_log_level TraceLogLevel.Warning);
    Raylib.(set_exit_key Key.Null);
    Raylib.(set_config_flags
      ConfigFlags.[Window_undecorated; Window_always_run; (*Window_transparent;*) Vsync_hint]);
    Raylib.init_window w h s;
    Raylib.set_window_position x y

  let closed () = Raylib.window_should_close ()

  let pos () = point_of_vec2 (Raylib.get_window_position ())
  let size () = Raylib.get_screen_width (), Raylib.get_screen_height ()
  let set_pos () x y = Raylib.set_window_position x y
  let set_size () w h = Raylib.set_window_size w h
  let set_icon () img = Raylib.set_window_icon img

  let minimize () = Raylib.minimize_window ()
  let restore () = Raylib.restore_window ()
  let is_minimized () = Raylib.is_window_minimized ()

  let screen_size () =
    let mon = Raylib.get_current_monitor () in
    Raylib.get_monitor_width mon, Raylib.get_monitor_height mon
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
    let r, g, b = Raylib.Color.(r c, g c, b c) in
    Raylib.Color.create r g b x


(* Fonts *)

type font = Raylib.Font.t (* * Raylib.Shader.t *)

module Font =
struct
  let load () path min max size =
    let glyphs = Ctypes.(CArray.make int (max - min)) in
    for i = min to max - 1 do Ctypes.CArray.set glyphs (i - min) i done;
    let font = Raylib.load_font_ex path size (Some glyphs) in
    Raylib.(set_texture_filter (Font.texture font) TextureFilter.Bilinear);
    font

(* Can't get SDF to work. Translated from https://github.com/raysan5/raylib/blob/master/examples/text/text_font_sdf.c
  let load () path size =
    let font = Raylib.load_font_ex path size None in
    Raylib.Font.set_base_size font size;
    Raylib.Font.set_glyph_padding font 0;
    let data = In_channel.(with_open_bin path input_all) in
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

  let extract img x y w h =
    Raylib.image_from_image img
      (Raylib.Rectangle.create (float x) (float y) (float w) (float h))

  let prepare () img = Raylib.load_texture_from_image img

  let load () path = prepare () (load_raw path)

  let size img = Raylib.Texture.(width img, height img)
end


(* Drawing *)

module Draw =
struct
  let frame = ref 0
  let updates = ref []

  let start () c =
    Raylib.begin_drawing ();
    Raylib.clear_background (color c);
(* TODO: Raylib OCaml is missing set_blend_factors_separate
    Raylib.(begin_blend_mode BlendMode.Custom_separate);
    let rl_func_add = 0x8006 in
    let rl_max = 0x8008 in
    Raylib.Rlgl.set_blend_factors_separate 1 1 1 1 rl_func_add rl_max;
*)
    List.iter (fun f -> f ()) !updates

  let finish () =
    incr frame;
    Raylib.end_drawing ()

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

  let image () x y scale img =
    let v = vec2_of_point (x, y) in
    Raylib.draw_texture_ex img v 0.0 (float scale) Raylib.Color.white
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
  | `Resize of resize
  | `Link
]

module Mouse =
struct
  let last_press_pos = ref (min_int, min_int)
  let last_press_left = ref 0.0
  let last_press_right = ref 0.0
  let is_double_left = ref false
  let is_double_right = ref false

  let pos () = point_of_vec2 (Raylib.get_mouse_position ())
  let delta () = point_of_vec2 (Raylib.get_mouse_delta ())
  let wheel () = floats_of_vec2 (Raylib.get_mouse_wheel_move_v ())

  let button = function
    | `Left -> Raylib.MouseButton.Left
    | `Right -> Raylib.MouseButton.Right

  let is_down but = Raylib.is_mouse_button_down (button but)
  let is_pressed but = Raylib.is_mouse_button_pressed (button but)
  let is_released but = Raylib.is_mouse_button_released (button but)

  let is_doubleclick = function
    | `Left -> !is_double_left
    | `Right -> !is_double_right

  let _ = Draw.updates :=
    (fun () ->
      let left = Raylib.(is_mouse_button_pressed MouseButton.Left) in
      let right = Raylib.(is_mouse_button_pressed MouseButton.Right) in
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
      )
    ) :: !Draw.updates

  let set_cursor () cursor =
    Raylib.set_mouse_cursor (
      let open Raylib.MouseCursor in
      match cursor with
      | `Default -> Default
      | `Arrow -> Arrow
      | `Busy -> Arrow  (* not supported by Raylib? *)
      | `Blocked -> Not_allowed
      | `Beam -> Ibeam
      | `Crosshair -> Crosshair
      | `Resize `N_S -> Resize_ns
      | `Resize `E_W -> Resize_ew
      | `Resize `NE_SW -> Resize_nesw
      | `Resize `NW_SE -> Resize_nwse
      | `Resize `All -> Resize_all
      | `Link -> Pointing_hand
    )
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
  let is_repeated k = Raylib.is_key_pressed_repeat (key k)
  let is_released k = Raylib.is_key_released (key k)

  let shift = [`Shift `Left; `Shift `Right]
  let control = [`Command `Left; `Command `Right]
  let alt = [`Alt `Left; `Alt `Right]

  let some_down = List.exists is_down
  let is_modifier_down = function
    | `Shift -> some_down shift
    | `Command -> some_down control
    | `Alt -> some_down alt

  let all_modifiers = [|`Shift ; `Command ; `Alt|]
  let are_modifiers_down modifiers =
    Array.for_all (fun key ->
      is_modifier_down key = List.mem key modifiers
    ) all_modifiers
end


(* Audio *)

type audio = unit
type sound = {music : Raylib.Music.t; format : Format.t; temp : path option (* for UTF-8 workaround *)}

module Audio =
struct
  let init () = Raylib.init_audio_device ()

  let silent = ref None
  let silence () =
    match !silent with
    | Some sound -> sound
    | None ->
      let (//) = Filename.concat in
      let assets = Filename.dirname Sys.argv.(0) // "assets" in
      let music = Raylib.load_music_stream (assets // "silence.mp3") in  (* TODO *)
      let sound = {music; format = Format.unknown; temp = None} in
      silent := Some sound;
      sound

  let retain = ref []

  let load () path =
    if not (Sys.file_exists path) then silence () else
    (* Raylib can't handle UTF-8 file paths, so copy those to temp file. *)
    let path' = if Unicode.is_ascii path then path else Storage.copy_to_temp path in
    let format = try Format.read path' with _ -> Format.unknown in
    let music = Raylib.load_music_stream path' in
    (* TODO: This is a work-around for a bug in Raylib < 5.5.
     * We intentionally leak the music stream in order to avoid a double free
     * segfault in Raylib. (See https://github.com/raysan5/raylib/issues/3889
     * and https://github.com/raysan5/raylib/pull/3966). *)
    if not (Raylib.Music.looping music) (* failure in Raylib < 5.5 *) then
    (
      Raylib.Music.set_ctx_type music 0;
      retain := music :: !retain;
    );
    (* End of work-around. *)
    if Raylib.Music.ctx_type music = 0 then silence () else
    (
      (*Raylib.Music.set_looping music false;*)  (* TODO *)
      {music; format; temp = if path' = path then None else Some path'}
    )

  let free () sound =
    assert (sound != silence ());
    Raylib.stop_music_stream sound.music;
    Option.iter Storage.remove_temp sound.temp;
    Raylib.unload_music_stream sound.music

  let play () sound = Raylib.play_music_stream sound.music
  let stop () sound = Raylib.stop_music_stream sound.music
  let pause () sound = Raylib.pause_music_stream sound.music
  let resume () sound =
    Raylib.resume_music_stream sound.music;
    Raylib.update_music_stream sound.music

  let is_playing () sound = Raylib.is_music_stream_playing sound.music
  let volume () sound x = Raylib.set_music_volume sound.music x
  let length () sound = Raylib.get_music_time_length sound.music
  let played () sound = Raylib.get_music_time_played sound.music
  let seek () sound t = Raylib.seek_music_stream sound.music t

  let channels () sound = sound.format.channels
  let rate () sound = sound.format.rate
  let depth () sound = sound.format.depth
  let bitrate () sound = sound.format.bitrate
end


(* Files *)

module File =
struct
  let dropped () =
    if not (Raylib.is_file_dropped ()) then [] else
    let list = Raylib.load_dropped_files () in
    let paths = Raylib.FilePathList.files list in
    Raylib.unload_dropped_files list;
    paths
end


(* Clipboard *)

module Clipboard =
struct
  let read () = Raylib.get_clipboard_text ()
  let write () s = Raylib.set_clipboard_text s
end
