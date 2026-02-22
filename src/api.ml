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


(* Screen *)

type screen = int

module Screen =
struct
  type monitor = {outer : rect; inner : rect; hires : bool}  (* unscaled *)

  let scale = ref (1, 1)

  let sx x = x * fst !scale
  let sy y = y * snd !scale
  let ux x = x / fst !scale
  let uy y = y / snd !scale
  let ufx x = x /. float (fst !scale)
  let ufy y = y /. float (snd !scale)
  let sxy (x, y) = (sx x, sy y)
  let uxy (x, y) = (ux x, uy y)
  let ufxy (x, y) = (ufx x, ufy y)

  let monitors = ref ([||] : monitor iarray)

  let init () =
    (* Probe screen geometries by opening a dummy window on each and maximise.
     * This is a hack that cannot react to dynamic changes, e.g., of resolution.
     * Unfortunately, Raylib provides no way to inquire monitor geometry
     * directly and by need. *)

    (* Open dummy window to initialize GLFW for monitor queries to work. *)
    Raylib.(set_config_flags ConfigFlags.[Window_undecorated; Window_hidden]);
    Raylib.init_window 0 0 "";
    let current = Raylib.get_current_monitor () in
    let monitor_poss = Iarray.init (Raylib.get_monitor_count ())
      (fun i -> point_of_vec2 (Raylib.get_monitor_position i)) in
    Raylib.close_window ();

    (* Probe all monitors. *)
    monitors := Iarray.mapi (fun i (x, y) ->
      Raylib.init_window 4000 4000 "";  (* on Mac, need to start big *)
      Raylib.set_window_position x y;
      Raylib.maximize_window ();
      let w, h = Raylib.get_monitor_width i, Raylib.get_monitor_height i in
      let min_x, min_y = point_of_vec2 (Raylib.get_window_position ()) in
      let max_w, max_h = Raylib.(get_screen_width (), get_screen_height ()) in
      let hires = Raylib.(Vector2.y (get_window_scale_dpi ())) > 1.0 in
      Raylib.close_window ();
      if !App.debug_layout then
      (
        Printf.eprintf
          "[screen %d] pos=%d,%d,%d,%d limit=%d,%d,%d,%d hires=%b\n%!"
          i x y w h min_x min_y max_w max_h hires
      );
      {outer = x, y, w, h; inner = min_x, min_y, max_w, max_h; hires}
    ) monitor_poss;

    (* Default scaling factor. *)
    let min_h = Iarray.fold_left
      (fun h {outer = _, _, _, h'; _} -> min h h') 0 !monitors in
    scale :=
      if min_h > 2880 (* 8K *) then (4, 4) else
      if min_h > 1440 (* 4K *) then (2, 2) else (1, 1);

    current

  let screen pos =
    Option.value ~default: 0
      (Iarray.find_index (fun mon -> inside pos mon.outer) !monitors)

  let mon scr = Iarray.get !monitors scr
  let pos scr = let {outer = x, y, _, _; _} = mon scr in uxy (x, y)
  let size scr = let {outer = _, _, w, h; _} = mon scr in uxy (w, h)
  let min_pos scr = let {inner = x, y, _, _; _} = mon scr in uxy (x, y)
  let max_size scr = let {inner = _, _, w, h; _} = mon scr in uxy (w, h)
  let is_hires scr = let {hires; _} = mon scr in hires || snd !scale > 1
end


(* Window *)

type window = unit
type icon = Raylib.Image.t

module Window =
struct
  open Screen

  let current_screen = ref (-1)
  let current_pos = ref (0, 0)   (* buffered during minimization *)
  let current_size = ref (0, 0)  (* target (virtual current) size for drawing *)
  let next_pos = ref None        (* defer window changes to frame end *)
  let next_size = ref None

  (* Updating the physical window size is delayed for a frame so that the new
   * contents can be drawn in the frame buffer first, avoiding ugly 1-frame
   * distortion or offsetting. To this end, the Draw module temporarily
   * creates a custom frame buffer that matches current_size (the target size)
   * while Raylib/GL's internal frame buffer always matches the physical size,
   * so cannot be used to prepare a larger frame. *)
  (* TODO: In Ui, only do Draw.begin after window resizing. *)
  (* TODO: We should only need a custom buffer if the size increases? *)
  (* TODO: Cache the custom buffer, only recreate when a larger is needed.*)
  (* TODO: Create with some oversize slack to make resizing smoother. *)
  (* TODO: Remove after_frame_start. *)
  let _ = before_frame_finish :=
    (fun () ->
      (* When a reposition request occured during this frame, then move the
       * window at the end of that frame. *)
      Option.iter (fun ((x, y) as pos) ->
Printf.printf "[move] %d,%d\n%!"(sx x)(sy y);
        Raylib.set_window_position (sx x) (sy y);
        current_pos := pos;
        next_pos := None;
      ) !next_pos;
      (* When the target size differs from the physical size at the end of a
       * frame, then it was a delayed request from last frame. The current
       * custom frame buffer has been updated for the new size and the physical
       * window size can finally be set, right before finalising the redraw. *)
      let size = Raylib.(get_screen_width (), get_screen_height ()) in
      let size' = Screen.sxy !current_size in
      if size <> size' then Raylib.set_window_size (fst size') (snd size');
    ) :: !before_frame_finish

  let _ = after_frame_finish :=
    (fun () ->
      (* When a resize request occured during this frame, make it the new size
       * targeted for drawing, but don't physically change it yet. *)
      Option.iter (fun size ->
        current_size := size;
        next_size := None;
      ) !next_size;
      Raylib.(set_exit_key Key.Null);  (* seems to be reset somehow? *)
    ) :: !after_frame_finish

  let init x y w h title =
    Raylib.(set_trace_log_level TraceLogLevel.Warning);
    Raylib.(set_exit_key Key.Null);
    Raylib.set_target_fps 60;

    current_screen := Screen.init ();
    current_pos := (x, y);
    current_size := (w, h);

    Raylib.(set_config_flags
      ConfigFlags.[Window_undecorated; Window_always_run;
        (*Window_transparent;*) Vsync_hint; Msaa_4x_hint]);
    Raylib.init_window (sx w) (sy h) title;
    Raylib.set_window_position (sx x) (sy y);
    Raylib.(clear_window_state ConfigFlags.[Window_hidden])

  let closed () = Raylib.window_should_close ()

  let pos () = !current_pos
  let size () = !current_size
  let screen () = !current_screen
  let set_pos () x y = next_pos := Some (x, y)  (* request reposition *)
  let set_size () w h = next_size := Some (w, h)  (* request resize *)
  let set_screen () scr = current_screen := scr
  let set_icon () img = if not is_mac then Raylib.set_window_icon img

  let minimize () = Raylib.minimize_window ()
  let restore () = Raylib.restore_window ()
  let is_minimized () = Raylib.is_window_minimized ()

  let hide () = Raylib.set_window_state [Raylib.ConfigFlags.Window_hidden]
  let reveal () = Raylib.clear_window_state [Raylib.ConfigFlags.Window_hidden]
  let is_hidden () = Raylib.is_window_hidden ()

  let rescale () dx dy =
    let x, y = !Screen.scale in
    Screen.scale := max 1 (x + dx), max 1 (y + dy)

  let scale () = !Screen.scale

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

type font = {font : Raylib.Font.t; shader : Raylib.Shader.t option}

module Font =
struct
  let vertex_glsl =
  {|
    #version 330

    in vec3 vertexPosition;
    in vec2 vertexTexCoord;
    in vec4 vertexColor;
    out vec2 fragTexCoord;
    out vec4 fragColor;

    uniform mat4 mvp;
    void main()
    {
      fragTexCoord = vertexTexCoord;
      fragColor = vertexColor;
      gl_Position = mvp*vec4(vertexPosition, 1.0);
    }
  |}

  let frag_glsl = Printf.sprintf
  {|
    #version 330

    // Input vertex attributes
    in vec2 fragTexCoord;
    in vec4 fragColor;

    // Input uniform values
    uniform sampler2D texture0;
    uniform vec4 colDiffuse;

    // Output fragment color
    out vec4 finalColor;

    void main()
    {
      float distOutline = texture(texture0, fragTexCoord).a - %.2f;
      float distChange = length(vec2(dFdx(distOutline), dFdy(distOutline)));
      float alpha = smoothstep(-distChange, distChange, distOutline);
      finalColor = vec4(fragColor.rgb, fragColor.a * alpha);
    }
  |}

  let shader dist =
    lazy (Raylib.load_shader_from_memory vertex_glsl (frag_glsl dist))
  let shader_s = shader 0.45
  let shader_l = shader 0.47

  let load () path min max size sdf =
    let size =
      Screen.sy size * int_of_float Raylib.(Vector2.y (get_window_scale_dpi ())) in
    if sdf then
    (
      (* https://github.com/raysan5/raylib/blob/master/examples/text/text_font_sdf.c *)
      let font = Ctypes.make Raylib.Font.t in
      Raylib.Font.set_base_size font size;
      let data = File.load `Bin path in
      let glyphs = Raylib.load_font_data data (String.length data) size
        Ctypes.(from_voidp int null) max Raylib.FontType.(to_int Sdf) in
      Raylib.Font.set_glyphs font (Ctypes.CArray.from_ptr glyphs (max - min));

      let recs' = Ctypes.allocate (Ctypes.ptr Raylib.Rectangle.t) (Raylib.Font.recs font) in
      let atlas = Raylib.gen_image_font_atlas glyphs recs' max size 0 1 in
      Raylib.Font.set_recs font Ctypes.(!@recs');
      Raylib.Font.set_texture font (Raylib.load_texture_from_image atlas);
      Raylib.unload_image atlas;

      let shader = if size <= 10 then shader_s else shader_l in
      Raylib.set_texture_filter (Raylib.Font.texture font)
        Raylib.TextureFilter.Bilinear;
      {font; shader = Some (Lazy.force shader)}
    )
    else
    (
      let glyphs = Ctypes.(CArray.make int (max - min)) in
      for i = min to max - 1 do Ctypes.CArray.set glyphs (i - min) i done;
      let font = Raylib.load_font_ex path size (Some glyphs) in
      (*Raylib.(set_texture_filter (Font.texture font) TextureFilter.Bilinear);*)
      {font; shader = None}
    )
end


(* Image *)

type image = Raylib.Texture.t

module Image =
struct
  type raw = Raylib.Image.t
  type prepared = image

  let load_raw' path = Raylib.load_image path

  let mime_prefix = "image/"
  let load_raw_from_memory' mime data =
    if not (String.starts_with ~prefix: mime_prefix mime) then
      failwith "Image.load_from_memory";
    let n = String.length mime_prefix in
    let ext = "." ^ String.sub mime n (String.length mime - n) in
    Raylib.load_image_from_memory ext data (String.length data)

  let extract img x y w h =
    Raylib.image_from_image img
      (Raylib.Rectangle.create (float x) (float y) (float w) (float h))

  let finalise raw =
    Gc.finalise Raylib.unload_image raw;
    raw

  let prepare () raw =
    let img = Raylib.load_texture_from_image raw in
    Raylib.set_texture_filter img Raylib.TextureFilter.Bilinear;
    Gc.finalise Raylib.unload_texture img;
    img

  let prepare' raw' =
    let img = prepare () raw' in
    Raylib.unload_image raw';
    img

  let load_raw path = finalise (load_raw' path)
  let load_raw_from_memory mime data = finalise (load_raw_from_memory' mime data)

  let load () path = prepare' (load_raw' path)
  let load_from_memory () mime data = prepare' (load_raw_from_memory' mime data)

  let size img = Raylib.Texture.(width img, height img)
end


(* Drawing *)

type buffer = {texture : Raylib.RenderTexture.t; scale : size}

module Buffer =
struct
  let needed_scale () =
    mul (Window.scale ()) (point_of_vec2 (Raylib.get_window_scale_dpi ()))

  let create () w h =
    let sx, sy = needed_scale () in
    let w, h = w * sx, h * sy in
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
    {texture = buf; scale = sx, sy}

  let dispose buf = Raylib.unload_render_texture buf.texture

  let size buf =
    let w, h = Image.size (Raylib.RenderTexture.texture buf.texture) in
    w / fst buf.scale, h / snd buf.scale

  let scale buf = buf.scale
end


module Draw =
struct
  let frame = ref 0
  let current_scale = ref (-1, -1)
  let current_clip = ref None
  let current_shader = ref None
  let current_buffer = ref None  (* custom frame buffer while resizing window *)

  let sx v = v * fst !current_scale
  let sy v = v * snd !current_scale
  let sfx v = v *. float (fst !current_scale)
  let sfy v = v *. float (snd !current_scale)
  let sxy x y = sx x, sy y
  let sxywh x y w h = sx x, sy y, sx w, sy h

  let unclipped f x =
    (* Ensure well-nesting *)
    Option.iter (fun _ -> Raylib.end_scissor_mode ()) !current_clip;
    f x;
    Option.iter (fun (x, y, w, h) -> Raylib.begin_scissor_mode x y w h)
      !current_clip

  let shader shader_opt =  (* minimize shader changes by doing them lazily *)
    (match !current_shader, shader_opt with
    | None, None -> ()
    | None, Some shader -> unclipped Raylib.begin_shader_mode shader
    | Some _, None -> unclipped Raylib.end_shader_mode ()
    | Some shader_old, Some shader_new ->
      if shader_old != shader_new then
        unclipped (fun _ ->
          Raylib.end_shader_mode (); Raylib.begin_shader_mode shader_new
        ) ()
    );
    current_shader := shader_opt

  let buffer () x y buf =
    let w, h = Buffer.size buf in
    let w', h' = w * fst buf.scale, h * snd buf.scale in
    let x, y, w, h = sxywh x y w h in
    let r' = Raylib.Rectangle.create 0.0 0.0 (float w') (-. float h') in
    let r = Raylib.Rectangle.create (float x) (float y) (float w) (float h) in
    let v = vec2_of_point (0, 0) in
    let img = Raylib.RenderTexture.texture buf.texture in
    shader None;
    Raylib.draw_texture_pro img r' r v 0.0 Raylib.Color.white

  let start () c =
    current_scale := Window.scale ();
    Raylib.begin_drawing ();
    (* If target size differs from physical size, create custom frame buffer. *)
    let size = Raylib.(get_screen_width (), get_screen_height ()) in
    let (w, h) as size' = !Window.current_size in
    if size <> size' then
    (
      Option.iter Buffer.dispose !current_buffer;
      current_buffer := Some (Buffer.create () w h);
    );
    (* If used, switch to custom frame buffer. *)
    Option.iter (fun buf ->
      Raylib.begin_texture_mode buf.texture;
      current_scale := Buffer.needed_scale ();
    ) !current_buffer;
    Raylib.clear_background (color c);
(* TODO: Raylib OCaml is missing set_blend_factors_separate
    Raylib.(begin_blend_mode BlendMode.Custom_separate);
    let rl_func_add = 0x8006 in
    let rl_max = 0x8008 in
    Raylib.Rlgl.set_blend_factors_separate 1 1 1 1 rl_func_add rl_max;
*)
    List.iter (fun f -> f ()) (List.rev !after_frame_start)

  let finish () =
    shader None;
    List.iter (fun f -> f ()) (List.rev !before_frame_finish);
    (* If a custom frame buffer was used, draw and dispose it. This is after
     * before_frame_finish has adjusted the physical window size and Raylib/GL's
     * internal frame buffer size. *)
    Option.iter (fun buf ->
      Raylib.end_texture_mode ();
      buffer () 0 0 buf;
      Buffer.dispose buf;
      current_buffer := None;
    ) !current_buffer;
    Raylib.end_drawing ();  (* polls input events *)
    List.iter (fun f -> f ()) (List.rev !after_frame_finish);
    incr frame

  let buffered () buf =
    shader None;
    if !current_buffer <> None then Raylib.end_texture_mode ();
    Raylib.begin_texture_mode buf.texture;
    (* Manual adjustment for High DPI scaling is needed *)
    current_scale := Screen.sxy (point_of_vec2 (Raylib.get_window_scale_dpi ()))

  let unbuffered () =
    shader None;
    Raylib.end_texture_mode ();
    match !current_buffer with
    | None -> current_scale := Window.scale ()
    | Some buf ->
      Raylib.begin_texture_mode buf.texture;
      current_scale := Buffer.needed_scale ()

  let clip () x y w h =
    assert (!current_clip = None);
    let (x, y, w, h) as r = sxywh x y w h in
    shader None;
    current_clip := Some r;
    Raylib.begin_scissor_mode x y w h

  let unclip () =
    assert (!current_clip <> None);
    shader None;
    current_clip := None;
    Raylib.end_scissor_mode ()

  let frame () = !frame

  let fill () x y w h c =
    let x, y, w, h = sxywh x y w h in
    Raylib.draw_rectangle x y w h (color c)

  let rect () x y w h c =
    let x, y, w, h = sxywh x y w h in
    Raylib.draw_rectangle_lines x y w h (color c)

  let gradient () x y w h c1 o c2 =
    let x, y, w, h = sxywh x y w h in
    (match o with
    | `Horizontal -> Raylib.draw_rectangle_gradient_h
    | `Vertical -> Raylib.draw_rectangle_gradient_v
    ) x y w h (color c1) (color c2)

  let fill_circ () x y w h c =
    let x, y, w, h = sxywh x y w h in
    Raylib.draw_ellipse (x + w/2) (y + h/2) (float w /. 2.0) (float h /. 2.0) (color c)

  let gradient_circ () x y w h c1 c2 =
    let x, y, w, h = sxywh x y w h in
    Raylib.draw_circle_gradient (x + w/2) (y + h/2) (float (w + h) /. 4.0) (color c1) (color c2)

  let circ () x y w h c =
    let x, y, w, h = sxywh x y w h in
    Raylib.draw_ellipse_lines (x + w/2) (y + h/2) (float w /. 2.0) (float h /. 2.0) (color c)

  let spline () ps w c =
    let n = Array.length ps / 2 in
    let array = Ctypes.CArray.make Raylib.Vector2.t n in
    for i = 0 to n - 1 do
      let vec = Raylib.Vector2.create (sfx ps.(2 * i)) (sfy ps.(2 * i + 1)) in
      Ctypes.CArray.unsafe_set array i vec;
    done;
    Raylib.draw_spline_linear (Ctypes.CArray.start array) n w (color c)

  let tri () x1 y1 x2 y2 x3 y3 c =
    let (x1, y1), (x2, y2), (x3, y3) = sxy x1 y1, sxy x2 y2, sxy x3 y3 in
    let vs = Array.map vec2_of_point [|x1, y1; x2, y2; x3, y3|] in
    Raylib.draw_triangle vs.(0) vs.(1) vs.(2) (color c)

  let arrow () x y w h c dir =
    let x, y, w, h = sxywh x y w h in
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
    let x, y, _, h = sxywh x y 1 h in
    shader f.shader;
    Raylib.draw_text_ex f.font s (vec2_of_point (x, y)) (float h) 1.0 (color c)

  let text_width () h f s : int =
    fst (point_of_vec2 (Raylib.measure_text_ex f.font s (float h) 1.0))

  let text_spacing () _h _f = 1

  let image () x y sc img =
    let x, y = sxy x y in
    let sc = sc *. float (fst !current_scale) in
    let v = vec2_of_point (x, y) in
    shader None;
    Raylib.draw_texture_ex img v 0.0 sc Raylib.Color.white

  let image_part () x y w h x' y' w' h' img =
    let x, y, w, h = sxywh x y w h in
    let r' = Raylib.Rectangle.create (float x') (float y') (float w') (float h') in
    let r = Raylib.Rectangle.create (float x) (float y) (float w) (float h) in
    let v = vec2_of_point (0, 0) in
    shader None;
    Raylib.draw_texture_pro img r' r v 0.0 Raylib.Color.white
end


(* Input devices *)

type modifier = [`Shift | `Command | `Alt]
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

type button = [`Left | `Right | `Middle]
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
  let last_abs_pos = ref (0, 0)
  let last_press_pos = ref (min_int, min_int)
  let last_press_left = ref 0.0
  let last_press_right = ref 0.0
  let last_press_middle = ref 0.0
  let multi_left = ref 0
  let multi_right = ref 0
  let multi_middle = ref 0
  let is_drag_left = ref false
  let is_drag_right = ref false
  let is_drag_middle = ref false
  let next_cursor = ref Raylib.MouseCursor.Default

  let pos () = !current_pos
  let abs_pos () = add (pos ()) (Window.pos ())
  let delta () = sub (abs_pos ()) !last_abs_pos
  let wheel () = Screen.ufxy (floats_of_vec2 (Raylib.get_mouse_wheel_move_v ()))

  let button = function
    | `Left -> Raylib.MouseButton.Left
    | `Right -> Raylib.MouseButton.Right
    | `Middle -> Raylib.MouseButton.Middle

  let is_down but = Raylib.is_mouse_button_down (button but)
  let is_pressed but = Raylib.is_mouse_button_pressed (button but)
  let is_released but = Raylib.is_mouse_button_released (button but)

  let is_double_click = function
    | `Left -> !multi_left = 2
    | `Right -> !multi_right = 2
    | `Middle -> !multi_middle = 2

  let is_triple_click = function
    | `Left -> !multi_left >= 3
    | `Right -> !multi_right >= 3
    | `Middle -> !multi_middle = 3

  let is_drag = function
    | `Left -> !is_drag_left
    | `Right -> !is_drag_right
    | `Middle -> !is_drag_middle

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
      current_pos := Screen.uxy (point_of_vec2 (Raylib.get_mouse_position ()));
      let win_pos = Screen.uxy (point_of_vec2 (Raylib.get_window_position ())) in
      let mouse_delta = Screen.uxy (point_of_vec2 (Raylib.get_mouse_delta ())) in
      let win_delta = sub win_pos !last_win_pos in
      if not is_mac || mouse_delta <> (0, 0) then
        last_win_pos := win_pos  (* true mouse location caught up *)
      else if is_mac && win_delta <> (0, 0) then
        current_pos := sub !current_pos win_delta;

      (* Detect multi clicks *)
      let left = is_pressed `Left in
      let right = is_pressed `Right in
      let middle = is_pressed `Middle in
      if left || right || middle then
      (
        let now = Unix.gettimeofday () in
        let mx, my = !last_press_pos in
        last_press_pos := pos ();
        let mx', my' = !last_press_pos in
        let unmoved = abs (mx' - mx) < 4 && abs (my' - my) < 4 in
        if left then
        (
          if unmoved && now -. !last_press_left < 0.5 then
            incr multi_left
          else
            multi_left := 1;
          last_press_left := now;
        );
        if right then
        (
          if unmoved && now -. !last_press_right < 0.5 then
            incr multi_right
          else
            multi_right := 1;
          last_press_right := now;
        );
        if middle then
        (
          if unmoved && now -. !last_press_middle < 0.5 then
            incr multi_middle
          else
            multi_middle := 1;
          last_press_middle := now;
        )
      );

      (* Detect dragging *)
      let moved = delta () <> (0, 0) in
      if is_down `Left then
        is_drag_left := !is_drag_left || moved
      else if not (is_released `Left) then
        is_drag_left := false;
      if is_down `Right then
        is_drag_right := !is_drag_right || moved
      else if not (is_released `Right) then
        is_drag_right := false;
      if is_down `Middle then
        is_drag_middle := !is_drag_middle || moved
      else if not (is_released `Middle) then
        is_drag_middle := false;

      (* Deferred update of mouse cursor *)
      Raylib.set_mouse_cursor !next_cursor;
      next_cursor := Raylib.MouseCursor.Default;
    ) :: !after_frame_start

  let _ = before_frame_finish :=
    (fun () ->
      last_pos := !current_pos;
      last_abs_pos := abs_pos ();
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

  (* Test string: "␣−←→↑↓⤒↟⤓↡⇤⇱⇥⇲⏎⌤⇆↹⎋⌫⌦⎀⁁⇪⇧⌥⎇⌘★" *)

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

type sound = {music : Raylib.Music.t; format : Format.t; temp : path option; (* for UTF-8 workaround *)}
type audio_processor = float array -> unit
type audio =
{
  mutex : Mutex.t;
  mutable sound : sound;
  mutable processors : (audio_processor * Raylib_callbacks.audio_callback) list;
}

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
    let audio = {mutex = Mutex.create (); sound = silence (); processors = []} in

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

  let load _ path =
    if not (File.exists path) then silence () else
    (* Raylib can't handle UTF-8 file paths, so copy those to temp file. *)
    (* (Raylib.load_music_stream_from_memory is broken and segfaults.) *)
    let path' =
      if not (Unicode.is_ascii path) then Storage.copy_to_temp path else path in
    let format = try Format.read path' with _ -> Format.unknown in
    let music = Raylib.load_music_stream path' in
    if Raylib.Music.ctx_type music = 0 then silence () else
    (
      (*Raylib.Music.set_looping music false;*)  (* TODO *)
      {music; format; temp = if path' = path then None else Some path'}
    )

  let free a sound =
    Mutex.protect a.mutex (fun () ->
      Raylib.stop_music_stream sound.music;
      Raylib.unload_music_stream sound.music;
      Option.iter Storage.delete_temp sound.temp;
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
      Raylib.set_master_volume x;
    )

  let protect a f = Mutex.protect a.mutex (fun () -> f a.sound.music)
  let is_playing a = protect a Raylib.is_music_stream_playing
  let length a = protect a Raylib.get_music_time_length
  let played a = protect a Raylib.get_music_time_played

  let channels _ sound = sound.format.channels
  let rate _ sound = sound.format.rate
  let depth _ sound = sound.format.depth
  let bitrate _ sound = sound.format.bitrate

  type processor = audio_processor

  let add_processor a f =
    Mutex.protect a.mutex (fun () ->
      let f' ptr n =
        let len = Unsigned.UInt.to_int n in
        let ptr' = Ctypes.from_voidp Ctypes.float ptr in
        f (Array.init len (fun i ->
          let l = Ctypes.(!@(ptr' +@ 2 * i)) in
          let r = Ctypes.(!@(ptr' +@ 2 * i +@ 1)) in
          (l +. r) /. 2.0
        ))
      in
      a.processors <- (f, f') :: a.processors;
      Raylib_callbacks.attach_audio_mixed_processor f';
    )

  let remove_processor a f =
    Mutex.protect a.mutex (fun () ->
      Option.iter (fun f' ->
        Raylib_callbacks.detach_audio_mixed_processor f';
        a.processors <- List.remove_assq f a.processors;
      ) (List.assq_opt f a.processors)
    )

  let remove_all_processors a =
    Mutex.protect a.mutex (fun () ->
      List.iter (fun (_, f') ->
        Raylib_callbacks.detach_audio_mixed_processor f'
      ) a.processors;
      a.processors <- [];
    )
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

  let _ = after_frame_start := (fun () -> paths := []) :: !after_frame_start
end


(* Clipboard *)

module Clipboard =
struct
  let read () = Raylib.get_clipboard_text ()
  let write () s = Raylib.set_clipboard_text s
end
