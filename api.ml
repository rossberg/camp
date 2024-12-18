(* Graphics/sound API abstraction *)

open Audio_file


(* Base types *)

type path = string
type time = float


(* Geometry helpers *)

type point = int * int
type size = int * int
type rect = int * int * int * int
type orientation = [`Horizontal | `Vertical]

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


(* Window *)

type window = unit

module Window =
struct
  let init x y w h s =
    Raylib.(set_trace_log_level TraceLogLevel.Warning);
    Raylib.(set_exit_key Key.Null);
    Raylib.init_window w h s;
    Raylib.set_window_position x y;
    Raylib.(set_window_state ConfigFlags.[Window_undecorated; Vsync_hint])

  let pos () = point_of_vec2 (Raylib.get_window_position ())
  let size () = Raylib.get_screen_width (), Raylib.get_screen_height ()
  let set_pos () x y = Raylib.set_window_position x y
  let set_size () w h = Raylib.set_window_size w h

  let screen_size () =
    let mon = Raylib.get_current_monitor () in
    Raylib.get_monitor_width mon, Raylib.get_monitor_height mon
end


(* Colors *)

type color =
[
  | `Black | `White
  | `Red | `Orange | `Yellow | `Green | `Blue
  | `Gray of int
  | `RGB of int
  | `Trans of color * int
]

let rec color = function
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


(* Drawing *)

module Draw =
struct
  let frame = ref 0

  let start () c =
    Raylib.begin_drawing ();
    Raylib.clear_background (color c)

  let finish () =
    incr frame;
    Raylib.end_drawing ()

  let frame () = !frame

  let clip () (x, y, w, h) = Raylib.begin_scissor_mode x y w h
  let unclip () = Raylib.end_scissor_mode ()

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

  let text () x y h c f s =
    Raylib.draw_text_ex f s (vec2_of_point (x, y)) (float h) 0.0 (color c)
(*
    Raylib.begin_shader_mode (snd f);
    Raylib.draw_text_ex (fst f) s (vec2_of_point (x, y)) (float h) 0.0 (color c);
    Raylib.end_shader_mode ()
*)

  let text_width () h f s =
    fst (point_of_vec2 (Raylib.measure_text_ex f s (float h) 0.0))
end


(* Input devices *)

type side = [`Left | `Right]
type face = [`Up | `Down]
type dir = [side | face]
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
  | `Control of side
  | `Alt of side
  | `Caps
]

type modifier = [`Plain | `Shift | `Control | `Alt]

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
  let pos () = point_of_vec2 (Raylib.get_mouse_position ())
  let delta () = point_of_vec2 (Raylib.get_mouse_delta ())
  let wheel () = floats_of_vec2 (Raylib.get_mouse_wheel_move_v ())

  let button = function
    | `Left -> Raylib.MouseButton.Left
    | `Right -> Raylib.MouseButton.Right

  let is_down but = Raylib.is_mouse_button_down (button but)
  let is_released but = Raylib.is_mouse_button_released (button but)

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
    | `Control `Left -> Raylib.Key.Left_control
    | `Control `Right -> Raylib.Key.Right_control
    | `Alt `Left -> Raylib.Key.Left_alt
    | `Alt `Right -> Raylib.Key.Right_alt
    | `Caps -> Raylib.Key.Caps_lock

  let is_down k = Raylib.is_key_down (key k)
  let is_released k = Raylib.is_key_released (key k)

  let shift = [`Shift `Left; `Shift `Right]
  let control = [`Control `Left; `Control `Right]
  let alt = [`Alt `Left; `Alt `Right]
  let non_shift = control @ alt
  let non_control = shift @ alt
  let non_alt = shift @ control
  let all = shift @ control @ alt

  let some_down = List.exists is_down
  let is_modifier_down = function
    | `Plain -> not (some_down all)
    | `Shift -> some_down shift && not (some_down non_shift)
    | `Control -> some_down control && not (some_down non_control)
    | `Alt -> some_down alt && not (some_down non_alt)
end


(* Audio *)

type audio = unit
type sound = {music : Raylib.Music.t; temp : path option (* for UTF-8 workaround *)}

module Audio =
struct
  let init () = Raylib.init_audio_device ()

  let silent = ref None
  let silence () =
    match !silent with
    | Some sound -> sound
    | None ->
      let music = Raylib.load_music_stream "silence.mp3" in
      let sound = {music; temp = None} in
      silent := Some sound;
      sound

  let retain = ref []

  let load () path =
    if not (Sys.file_exists path) then silence () else
    (* Raylib can't handle UTF-8 file paths, so copy those to temp file. *)
    let path' = if Unicode.is_ascii path then path else File.copy_to_temp path in
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
      {music; temp = if path' = path then None else Some path'}
    )

  let free () sound =
    assert (sound != silence ());
    Raylib.stop_music_stream sound.music;
    Option.iter File.remove_temp sound.temp;
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
