(* Immediate-style GUI widgets *)

open Api
open Audio_file


(* State *)

type drag = ..
type drag += No_drag

type image_load = [`Unloaded of string | `Loaded of image] ref

type t =
{
  win : window;
  mutable buffered : bool;
  mutable font_sdf : bool;
  mutable palette : int;
  mutable panes : rect array;
  mutable modal : bool;             (* whether a pop-up menu is shown *)
  mutable mouse_owned : bool;       (* whether mouse was owned by a widget *)
  mutable drag_origin : point;      (* starting position of mouse drag *)
  mutable drag_extra : drag;        (* associated data for drag operation *)
  mutable delayed : (unit -> unit) list;   (* draw at end of frame *)
  img_background : image_load;
  img_button : image_load;
  img_nocover : image_load;
  fonts : font option array;
}

let no_drag = (min_int, min_int)

let assets = File.(dir Sys.argv.(0) // "assets")

let make win =
  let icon = Image.load_raw File.(assets // "icon.png") in
  Window.set_icon win icon;
  { win;
    buffered = true;
    font_sdf = Window.is_hires win;
    palette = 0;
    panes = Array.make 10 (0, 0, 0, 0);
    modal = false;
    mouse_owned = false;
    drag_origin = no_drag;
    drag_extra = No_drag;
    delayed = [];
    img_background = ref (`Unloaded "bg.jpg");
    img_button = ref (`Unloaded "but.jpg");
    img_nocover = ref (`Unloaded "nocover.jpg");
    fonts = Array.make 64 None;
  }

let window ui = ui.win

let buffered ui b = ui.buffered <- b
let is_buffered ui = ui.buffered


(* Modal mode *)

let modal ui = ui.modal <- true
let nonmodal ui = ui.modal <- false
let is_modal ui = ui.modal


(* Panes *)

type pane = int

let pane ui i r =
  let x, y, w, h = r in
  let ww, wh = Window.size ui.win in
  let x = if x >= 0 then x else ww + x in
  let y = if y >= 0 then y else wh + y in
  let w = if w >= 0 then w else ww - x + w in
  let h = if h >= 0 then h else wh - y + h in
  if not (x >= 0 && y >= 0 && x + w <= ww && y + h <= wh) then
    Storage.log (Printf.sprintf
      "invalid element geometry: x=%d y=%d w=%d h=%d winw=%d winh=%d"
      x y w h ww wh
    );

  let n = Array.length ui.panes in
  if i >= n then
    ui.panes <-
      Array.init (2*i) (fun i -> if i < n then ui.panes.(i) else (0, 0, 0, 0));
  ui.panes.(i) <- (x, y, w, h)


(* Areas *)

type area = pane * int * int * int * int

let dim ui (i, x, y, w, h) =
  let px, py, pw, ph =
    if i >= 0 then ui.panes.(i) else
    let ww, wh = Window.size ui.win in 0, 0, ww, wh
  in
  let x' = x + (if x >= 0 then 0 else pw) in
  let y' = y + (if y >= 0 then 0 else ph) in
  let w' = w + (if w >= 0 then 0 else pw - x') in
  let h' = h + (if h >= 0 then 0 else ph - y') in
  px + x', py + y', w', h'

let mouse_inside ui area =
  inside (Mouse.pos ui.win) (dim ui area)


(* Geometry helpers *)

let snap_dist = 12

let is_shift_down () =
  Key.is_down (`Shift `Left) || Key.is_down (`Shift `Right)

let snap min max v =
  if is_shift_down () then v else
  if abs (v - min) < snap_dist then min else
  if abs (v - max) < snap_dist then max else
  v

let clamp min max v =
  if v < min then min else
  if v > max then max else
  v


(* Colors *)

type color = Api.color
type palette = {text : color; warn : color; error : color; hover : color}

let palettes =
[|
  {text = `Green; warn = `Yellow; error = `Red; hover = `Blue};
  {text = `RGB 0x92f2d6; warn = `RGB 0xc8bd4a; error = `RGB 0xec635b; hover = `RGB 0x5f7eb8};
  {text = `RGB 0x78cfeb; warn = `RGB 0xfef46d; error = `RGB 0xd35c6d; hover = `RGB 0x5186bb};
  {text = `RGB 0x51a6fb; warn = `RGB 0xfef46d; error = `RGB 0xd35c6d; hover = `RGB 0x78cfeb};
  {text = `RGB 0xddac4d; warn = `RGB 0xffff6d; error = `RGB 0xf14138; hover = `RGB 0xd5b482};
|]

let num_palette _ui = Array.length palettes
let get_palette ui = ui.palette
let set_palette ui i = ui.palette <- i


let unlit_alpha = 0x30
let semilit_alpha = 0x80
let unlit_color c = Color.darken unlit_alpha c (*`Trans (c, unlit_alpha)*)
let semilit_color c = Color.darken semilit_alpha c (*`Trans (c, semilit_alpha)*)
let text_color ui = palettes.(ui.palette).text
let warn_color ui = palettes.(ui.palette).warn
let error_color ui = palettes.(ui.palette).error
let hover_color ui = palettes.(ui.palette).hover
let active_color _ui = `RGB 0x40ff40
let inactive_color _ui = `Gray 0xc0

let mode c = function
  | true -> c
  | false -> unlit_color c

let fill ui b = mode (text_color ui) b

let border ui = function
  | `Hovered -> hover_color ui
(*
  | `Pressed -> `Orange
*)
  | _ -> `Black


(* Focus *)

let focus' ui x y w h c style =
  let c1 = `Trans (c, 0x60 (* 0x40 *)) in
  let c2 = `Trans (c, 0x00) in
  let b = 8 (* 6 *) in
  match style with
  | `Above ->
    Draw.gradient ui.win x y w b c1 `Vertical c2;
  | `Left ->
    Draw.gradient ui.win x y b h c1 `Horizontal c2;
  | `Inside ->
    Draw.gradient ui.win x y w b c1 `Vertical c2;
    Draw.gradient ui.win x (y + h - b) w b c2 `Vertical c1

let focus ui area =
  let x, y, w, h = dim ui area in
  focus' ui x y w h (text_color ui) `Inside


let mouse_focus' ui r =
  let mx, my = Mouse.pos ui.win in
  Draw.gradient_circ ui.win (mx - r) (my - r) (2 * r) (2 * r)
    (`Trans (`White, 0x20)) (`Trans (`White, 0x00))

let mouse_focus ui area r =
  let x, y, w, h = dim ui area in
  Draw.clip ui.win x y w h;
  mouse_focus' ui r;
  Draw.unclip ui.win


(* Fonts *)

let font_purge ui =
  Array.map_inplace (Fun.const None) ui.fonts

let font_is_sdf ui = ui.font_sdf

let font_sdf ui b =
  ui.font_sdf <- b;
  font_purge ui

let font' ui h file min max fonts =
  match fonts.(h) with
  | Some f -> f
  | None ->
    let f = Font.load ui.win file min max h ui.font_sdf in
    fonts.(h) <- Some f;
    f

let font ui h =
  let max = if h < 10 then 0x80 else 0x2800 in
  font' ui h File.(assets // "font.ttf") 0x0020 max ui.fonts


(* Images *)

let get_img ui rimg =
  match !rimg with
  | `Loaded img -> img
  | `Unloaded file ->
    let img = Image.load ui.win File.(assets // file) in
    rimg := `Loaded img;
    img

let nocover ui = get_img ui ui.img_nocover

let background ui x y w h =
  let bg = get_img ui ui.img_background in
  let iw, ih = Image.size bg in

  Draw.clip ui.win x y w h;

  for i = 0 to (w + iw - 1)/iw - 1 do
    let dx = if w < iw then - (iw - w)/2 else i*iw in
    for j = 0 to (h + ih - 1)/ih - 1 do
      let dy = if h < ih then - (ih - h)/2 else j*ih in
      Draw.image ui.win (x + dx) (y + dy) 1.0 bg
    done
  done;

  Draw.fill ui.win x y 1 (h - 2) (`Gray 0x50);
  Draw.fill ui.win x y w 1 (`Gray 0x70);
  Draw.fill ui.win (x + 1) (y + h - 2) (w - 1) 2 (`Gray 0x10);
  Draw.fill ui.win (x + w - 1) y 1 (h - 2) (`Gray 0x10);

  mouse_focus' ui 50;

  Draw.unclip ui.win


(* Window *)

type drag += Move of {target : point}
type drag += Resize of {overshoot : size}

let start ui =
  Draw.start ui.win (`Trans (`Black, 0x40));

  let ww, wh = Window.size ui.win in
  background ui 0 0 ww wh;

  Mouse.set_cursor ui.win `Default;
  if Mouse.is_down `Left then
  (
    if ui.drag_origin = no_drag then ui.drag_origin <- Mouse.pos ui.win
  )
  else if not (Mouse.is_released `Left) then
  (
    ui.drag_origin <- no_drag;
    ui.drag_extra <- No_drag;
  )


let finish ui margin (minw, minh) (maxw, maxh) =
  List.iter (fun f -> f ()) (List.rev ui.delayed);
  ui.delayed <- [];

  if ui.mouse_owned then
  (
     ui.mouse_owned <- false
  )
  else
  (
    let (wx, wy) as pos = Window.pos ui.win in
    let (ww, wh) as size = Window.size ui.win in
    let mouse = Mouse.pos ui.win in
    let origin = if Mouse.is_down `Left then ui.drag_origin else mouse in
    let varw = minw <> maxw in
    let varh = minh <> maxh in
    let left = inside origin (0, 0, margin, wh) in
    let right = inside origin (ww - margin, 0, margin, wh) in
    let upper = inside origin (0, 0, ww, margin) in
    let lower = inside origin (0, wh - margin, ww, margin) in
    let cursor =
      match varw && left, varw && right, varh && upper, varh && lower with
      | true, false, false, false
      | false, true, false, false -> `Resize `E_W
      | false, false, true, false
      | false, false, false, true -> `Resize `N_S
      | true, false, true, false
      | false, true, false, true -> `Resize `NW_SE
      | true, false, false, true
      | false, true, true, false -> `Resize `NE_SW
      | _ -> if Mouse.is_down `Left then `Point else `Default
    in
    Mouse.set_cursor ui.win cursor;

    if Mouse.is_down `Left && cursor <> `Default then
    (
      let sx, sy = Window.min_pos ui.win in
      let sw, sh = Window.max_size ui.win in
      match ui.drag_extra with
      | No_drag when cursor = `Point ->
        ui.drag_extra <- Move {target = pos};

      | No_drag ->
        ui.drag_extra <- Resize {overshoot = 0, 0};

      | Move {target} ->
        assert (cursor = `Point);
        let off = sub pos target in
        let (wx', wy') as pos' = sub (add pos (Mouse.screen_delta ui.win)) off in
        Window.set_pos ui.win (snap sx (sx + sw - ww) wx') (snap sy (sy + sh - wh) wy');
        ui.drag_extra <- Move {target = pos'};

      | Resize {overshoot = over} ->
        assert (cursor <> `Point);
        let signx = if left then -1 else if right then +1 else 0 in
        let signy = if upper then -1 else if lower then +1 else 0 in
        let delta = mul (signx, signy) (Mouse.screen_delta ui.win) in
        let ww', wh' = add (add size delta) over in
        let rx, ry = wx - sx, wy - sy in
        let maxw = if maxw >= 0 then maxw else if right then sw - rx else ww + rx in
        let maxh = if maxh >= 0 then maxh else if lower then sh - ry else wh + ry in
        let ww'', wh'' = clamp minw maxw ww', clamp minh maxh wh' in
        let dwx = if left then ww'' - ww else 0 in
        let dwy = if upper then wh'' - wh else 0 in
        let dmx = if right then ww'' - ww else 0 in
        let dmy = if lower then wh'' - wh else 0 in
        Window.set_size ui.win ww'' wh'';            (* deferred until end fo frame! *)
        Window.set_pos ui.win (wx - dwx) (wy - dwy); (* deferred until end fo frame! *)
        ui.drag_extra <- Resize {overshoot = ww' - ww'', wh' - wh''};
        ui.drag_origin <- add ui.drag_origin (dmx, dmy);  (* adjust for resize *)

      | _ -> ()
    )
  );

  Draw.finish ui.win


let rescale ui dx dy =
  if dx <> 0 || dy <> 0 then
  (
    Window.rescale ui.win dx dy;
    font_purge ui;
  )

let delay ui f =
  ui.delayed <- f :: ui.delayed


(* Input Status *)

let no_modkey = ([], `None)

let key_status' ui key =
  if ui.modal then `Untouched else
  (* Mouse click or drag masks keys *)
  if Mouse.is_down `Left then
    `Untouched
  else if Key.is_pressed key || Key.is_repeated key then
    `Pressed
  else if Key.is_released key then
    `Released
  else
    `Untouched

let key_status ui (modifiers, key) focus =
  if not (focus && Key.are_modifiers_down modifiers) then
    `Untouched
  else
    key_status' ui key

let mouse_status ui r = function
  | `Left ->
    if ui.modal then `Untouched else
    let side = `Left in
    if ui.drag_origin = no_drag && inside (Mouse.pos ui.win) r then
      (ui.mouse_owned <- true; `Hovered)
    else if not (inside ui.drag_origin r) then
      `Untouched
    else if Mouse.is_down side then
      (ui.mouse_owned <- true; `Pressed)
    else if Mouse.is_released side then
      `Released
    else
      `Untouched  (* is this reachable? *)
  | `Right ->
    if ui.modal then `Untouched else
    let side = `Right in
    if not (inside (Mouse.pos ui.win) r) then
      `Untouched
    else if Mouse.is_down side then
      `Pressed
    else if Mouse.is_released side then
      `Released
    else
      `Hovered


type motion = [`Unmoved | `Moving | `Moved]
type trajectory = [`Inside | `Outside | `Outward | `Inward]
type drag += Drag of {pos : point; moved : bool; inside : bool}

let drag_status ui r (stepx, stepy) =
  if ui.modal || not (inside ui.drag_origin r) then
    `None
  else if Mouse.is_released `Left then
    if Mouse.is_drag `Left then
      `Drop
    else
      `Click
  else
  let (mx, my) as m = Mouse.pos ui.win in
  ui.mouse_owned <- true;
  match ui.drag_extra with
  | No_drag ->
    ui.drag_extra <- Drag {pos = m; moved = false; inside = true};
    `Take
  | Drag {pos; moved; inside} ->
    let dx, dy = sub m pos in
    let dx' = if stepx = 0 then dx else dx / stepx in
    let dy' = if stepy = 0 then dy else dy / stepy in
    let pos = mx - dx mod max 1 stepx, my - dy mod max 1 stepy in
    let moved' = Mouse.is_drag `Left in
    let inside' = Api.inside m r in
    ui.drag_extra <- Drag {pos; moved = moved'; inside = inside'};
    let motion =
      match moved, moved' with
      | true, _ -> `Moved
      | false, true -> `Moving
      | false, false -> `Unmoved
    in
    let traj =
      match inside, inside' with
      | true, true -> `Inside
      | true, false -> `Outward
      | false, true -> `Inward
      | false, false -> `Outside
    in
    `Drag ((dx', dy'), motion, traj)
  | _ -> assert false

let wheel_status ui r =
  if not ui.modal && inside (Mouse.pos ui.win) r then
    Mouse.wheel ui.win
  else
    (0.0, 0.0)

let key ui modkey focus = (key_status ui modkey focus = `Released)
let mouse ui area side = (mouse_status ui (dim ui area) side = `Released)
let wheel ui area = wheel_status ui (dim ui area)
let drag ui area eps = drag_status ui (dim ui area) eps


(* Decorative Widgets *)

let indicator ui c area on =
  let x, y, w, h = dim ui area in
  Draw.fill_circ ui.win x y w h (if on then c else unlit_color c);
  Draw.fill_circ ui.win (x + w/4) (y + h/4) (min 2 (w/3)) (min 2 (h/3))
    (`Trans (`White, if on then 0xe0 else 0x30));
  Draw.circ ui.win x y w h (border ui `Untouched)

let colored_label ui c area align s =
  let x, y, w, h = dim ui area in
  let font = font ui h in
  let tw = Draw.text_width ui.win h font s in
  let dx =
    match align with
    | `Left -> 0
    | `Center -> (w - tw + 1) / 2
    | `Right -> w - tw
  in Draw.text ui.win (x + dx) y h c font s

let label ui area align s =
  colored_label ui `White area align s

let draw_lcd ui r c elem =
  let open Draw in
  let x, y, w, h = r in
  let m = h / 2 in
  match elem with
  | `N ->
    fill ui.win (x + 3) (y + 0) (w - 6) 2 c;
    tri ui.win (x + 1) (y + 0) (x + 3) (y + 2) (x + 3) (y + 0) c;
    tri ui.win (x + w - 1) (y + 0) (x + w - 3) (y + 0) (x + w - 3) (y + 2) c;
  | `S ->
    fill ui.win (x + 3) (y + h - 2) (w - 6) 2 c;
    tri ui.win (x + 1) (y + h) (x + 3) (y + h) (x + 3) (y + h - 2) c;
    tri ui.win (x + w - 1) (y + h) (x + w - 3) (y + h - 2) (x + w - 3) (y + h) c;
  | `C ->
    fill ui.win (x + 3) (y + m - 1) (w - 6) 2 c;
    tri ui.win (x + 1) (y + m - 1) (x + 3) (y + m + 1) (x + 3) (y + m - 1) c;
    tri ui.win (x + w - 1) (y + m - 1) (x + w - 3) (y + m - 1) (x + w - 3) (y + m + 1) c;
  | `NW ->
    fill ui.win (x + 0) (y + 3) 2 (m - 6) c;
    tri ui.win (x + 0) (y + 1) (x + 0) (y + 3) (x + 2) (y + 3) c;
    tri ui.win (x + 0) (y + m - 1) (x + 2) (y + m - 3) (x + 0) (y + m - 3) c;
  | `NE ->
    fill ui.win (x + w - 2) (y + 3) 2 (m - 6) c;
    tri ui.win (x + w) (y + 1) (x + w - 2) (y + 3) (x + w) (y + 3) c;
    tri ui.win (x + w) (y + m - 1) (x + w) (y + m - 3) (x + w - 2) (y + m - 3) c;
  | `SW ->
    fill ui.win (x + 0) (y + m + 3) 2 (h - m - 6) c;
    tri ui.win (x + 0) (y + m + 1) (x + 0) (y + m + 3) (x + 2) (y + m + 3) c;
    tri ui.win (x + 0) (y + h - 1) (x + 2) (y + h - 3) (x + 0) (y + h - 3) c;
  | `SE ->
    fill ui.win (x + w - 2) (y + m + 3) 2 (h - m - 6) c;
    tri ui.win (x + w) (y + m + 1) (x + w - 2) (y + m + 3) (x + w) (y + m + 3) c;
    tri ui.win (x + w) (y + h - 1) (x + w) (y + h - 3) (x + w - 2) (y + h - 3) c;
  | `Dots ->
    fill ui.win x (y + h / 4) 2 2 c;
    fill ui.win x (y + 3 * h / 4) 2 2 c

let lcd ui area d =
  let r = dim ui area in
  let c = text_color ui in
  if d = '-' || d = '+' then
    draw_lcd ui r c `C
  else if d = ':' then
    draw_lcd ui r c `Dots
  else
    List.iter (draw_lcd ui r c) [`N; `S; `C; `NW; `SW; `NE; `SE];
  List.iter (draw_lcd ui r (`Trans (`Black, 0x100 - unlit_alpha)))
    (match d with
    | ' ' -> [`N; `S; `C; `NW; `SW; `NE; `SE]
    | '+' -> [`C]
    | '0' -> [`C]
    | '1' -> [`N; `C; `S; `NW; `SW]
    | '2' -> [`NW; `SE]
    | '3' -> [`NW; `SW]
    | '4' -> [`N; `S; `SW]
    | '5' -> [`NE; `SW]
    | '6' -> [`NE]
    | '7' -> [`C; `S; `NW; `SW]
    | '8' -> []
    | '9' -> [`SW]
    | _ -> []
    )

type adjustment = [`Crop of orientation | `Shrink]

let image_size' ui area adjust img =
  let _, _, w, h = dim ui area in
  let iw, ih = Image.size img in
  let q = float w /. float h in
  let iq = float iw /. float ih in
  let iw', ih' =
    match adjust with
    | `Crop `Vertical -> iw, int_of_float (float ih *. iq /. q)
    | `Crop `Horizontal -> int_of_float (float iw /. iq *. q), ih
    | `Shrink -> iw, ih
  in
  let iq' = float iw' /. float ih' in
  if iq' > q then
    w, h - int_of_float (float h *. (1.0 -. q /. iq')), iw', ih'
  else
    w - int_of_float (float w *. (1.0 -. iq' /. q)), h, iw', ih'

let image ui area adjust img =
  let x, y, w, h = dim ui area in
  let w', h', iw', ih' = image_size' ui area adjust img in
  let dw, dh = w - w', h - h' in
  Draw.image_part ui.win (x + dw/2) (y + dh/2) w' h' 0 0 iw' ih' img

let image_size ui area adjust img =
  let w, h, _, _ = image_size' ui area adjust img in
  w, h


(* Passive Widgets *)

let widget ui area ?(focus = false) modkey =
  let r = dim ui area in
  r,
  match mouse_status ui r `Left, key_status ui modkey focus with
  | `Released, _ | _, `Released -> `Released
  | `Pressed, _ | _, `Pressed -> `Pressed
  | `Hovered, _ | _, `Hovered -> `Hovered
  | _, _ -> `Untouched


let box ui area c =
  let (x, y, w, h), _ = widget ui area no_modkey in
  Draw.fill ui.win x y w h c

let color_text ui area align c inv active s =
  let (x, y, w, h), _status = widget ui area no_modkey in
  let fg = mode c active in
  let bg = `Black in
  let fg, bg = if inv = `Inverted then bg, fg else fg, bg in
  Draw.fill ui.win x y w (h - 1) bg;  (* assume text has no descender *)
  let tw = Draw.text_width ui.win h (font ui h) s in
  let dx =
    match align with
    | `Left -> 0
    | `Center -> (w - tw) / 2
    | `Right -> w - tw
  in
  Draw.text ui.win (x + dx) y h fg (font ui h) s

let text ui area align =
  color_text ui area align (text_color ui)

let ticker ui area s =
  let (x, y, w, h), _status = widget ui area no_modkey in
  Draw.fill ui.win x y w h `Black;
  let tw = Draw.text_width ui.win h (font ui h) s in
  Draw.clip ui.win x y w h;
  let dx = if tw <= w then (w - tw)/2 else w - Draw.frame ui.win mod (w + tw) in
  Draw.text ui.win (x + dx) y h (fill ui true) (font ui h) s;
  Draw.unclip ui.win


(* Buttons *)

let invisible_button ui area mods modkey focus =
  let _, status = widget ui area no_modkey in
  focus && status = `Released && Key.are_modifiers_down mods ||
  key ui modkey focus

let button ui area ?(protrude = true) modkey focus active =
  let (x, y, w, h), status = widget ui area modkey ~focus in
  let img = get_img ui ui.img_button in
  let sx, sy = if status = `Pressed then 800, 400 else 0, 200 in
  Api.Draw.image_part ui.win x y w h sx sy w h img;
  if status <> `Pressed then
  (
    Draw.fill ui.win (x + 1) (y + 1) 1 (h - 2) (`Gray 0x50);
    if protrude then Draw.fill ui.win (x + 1) (y + 1) (w - 3) 1 (`Gray 0x50);
  );
  Draw.rect ui.win x y w h (border ui status);
  match active with
  | None -> false
  | Some active -> if status = `Released then not active else active

let labeled_button ui area ?(protrude = true) hsym c txt modkey focus active =
  let (x, y, w, h), status = widget ui area modkey ~focus in
  let result = button ui area ~protrude modkey focus active in
  let c =
    match active with
    | None -> Color.darken semilit_alpha (inactive_color ui)
    | Some false -> inactive_color ui
    | Some true -> c
  in
  let xsym = (x + (w - hsym + 1)/2) in
  let ysym = (y + (h - hsym + 1)/2) + Bool.to_int (status = `Pressed) in
  (match txt with
  | "" -> ()
  | "[]" ->
    Draw.fill ui.win xsym ysym hsym hsym c
  | "||" ->
    Draw.fill ui.win xsym ysym (hsym/3) hsym c;
    Draw.fill ui.win (xsym + hsym - hsym/3) ysym (hsym/3) hsym c;
  | ">" ->
    Draw.arrow ui.win xsym ysym hsym hsym c `Right
  | ">>" ->
    Draw.arrow ui.win xsym ysym (hsym/2 + 1) hsym c `Right;
    Draw.arrow ui.win (xsym + hsym/2) ysym (hsym/2 + 1) hsym c `Right;
  | "<<" ->
    Draw.arrow ui.win xsym ysym (hsym/2) hsym c `Left;
    Draw.arrow ui.win (xsym + hsym/2) ysym (hsym/2) hsym c `Left;
  | "^" ->
    Draw.arrow ui.win xsym ysym hsym (hsym/2) c `Up;
    Draw.fill ui.win xsym (ysym + hsym - hsym/3) hsym (hsym/3) c;
  | s ->
    let c =
      match active with
      | None -> Color.darken semilit_alpha `White
      | Some false -> `White
      | Some true -> c
    in
    let (i, x', y', w', _) = area in
    colored_label ui c (i, x' + 1, y' + (h - hsym)/2, w' - 1, hsym) `Center s
  );
  result


(* Bars *)

let progress_bar ui area v =
  let (x, y, w, h), status = widget ui area no_modkey in
  Draw.fill ui.win x y w h (fill ui false);
  Draw.fill ui.win x y (int_of_float (v *. float w)) h (fill ui true);
  for i = 0 to w / 2 - 1 do
    Draw.fill ui.win (x + 2*i + 1) y 1 h `Black
  done;
  Draw.rect ui.win x y w h (border ui status);
  if status <> `Pressed then v else
  let mx, _ = Mouse.pos ui.win in
  clamp 0.0 1.0 (float (mx - x) /. float w)


let volume_bar ui area v =
  let (x, y, w, h), status = widget ui area no_modkey in
  let h' = int_of_float ((1.0 -. v) *. float h) in
  Draw.fill ui.win (x + w - 2) y 2 h (fill ui true);
  Draw.tri ui.win (x + 2) y (x + w - 2) (y + h) (x + w - 2) y (fill ui true);
  Draw.fill ui.win x y w h' (`Trans (`Black, 0x100 - unlit_alpha));
  for j = 0 to h / 2 - 1 do
    Draw.fill ui.win x (y + 2*j + 1) w 1 `Black
  done;
  if status <> `Pressed then v else
  let _, my = Mouse.pos ui.win in
  clamp 0.0 1.0 (float (y + h - my) /. float h)


type drag += Scroll_bar_page of {last_repeat : time}
type drag += Scroll_bar_drag of {value : float; mx : int; my : int}

let scroll_bar ui area orient v len =
  assert (v +. len <= 1.0);
  let (x, y, w, h), status = widget ui area no_modkey in
  Draw.fill ui.win x y w h (fill ui false);
  let x', y', w', h' as r =
    match orient with
    | `Vertical ->
      let h' = int_of_float (Float.ceil (len *. float (h - 2))) in
      let h'' = max h' w in  (* minimum bar size *)
      x, y + int_of_float (v *. float (h - 2 - (h'' - h'))) + 1, w, h''
    | `Horizontal ->
      let w' = int_of_float (Float.ceil (len *. float (w - 2))) in
      let w'' = max w' h in  (* minimum bar size *)
      x + int_of_float (v *. float (w - 2 - (w'' - w'))) + 1, y, w'', h
  in
  if len < 1.0 then Draw.fill ui.win x' y' w' h' (fill ui true);
  (match orient with
  | `Vertical ->
    for j = 0 to h / 2 - 1 do
      Draw.fill ui.win x (y + 2*j) w 1 `Black
    done
  | `Horizontal ->
    for i = 0 to w / 2 - 1 do
      Draw.fill ui.win (x + 2*i) y 1 h `Black
    done
  );
  Draw.rect ui.win x y w h (border ui status);
  if status <> `Pressed then v else
  let (mx, my) as m = Mouse.pos ui.win in
  let v0, mx0, my0, last_repeat, dragging =
    match ui.drag_extra with
    | No_drag -> v, mx, my, 0.0, false
    | Scroll_bar_page {last_repeat} -> v, mx, my, last_repeat, false
    | Scroll_bar_drag {value; mx; my} -> value, mx, my, 0.0, true
    | _ -> assert false
  in
  let now = Unix.gettimeofday () in
  let v' =
    if dragging || inside m r then
    (
      ui.drag_extra <- Scroll_bar_drag {value = v0; mx = mx0; my = my0};
      match orient with
      | `Vertical -> v0 +. float (my - my0) /. float (h - 2)
      | `Horizontal -> v0 +. float (mx - mx0) /. float (w - 2)
    )
    else if now -. last_repeat > 0.3 (* TODO: use config *) then
    (
      ui.drag_extra <- Scroll_bar_page {last_repeat = now};
      match orient with
      | `Vertical ->
        if my < y' then v -. len else
        if my >= y' + h' then v +. len else
        v
      | `Horizontal ->
        if mx < x' then v -. len else
        if mx >= x' + w' then v +. len else
        v
    )
    else v
  in clamp 0.0 (1.0 -. len) v'


(* Dividers *)

type drag += Divide of {overshoot : size}

let divider ui area orient v minv maxv =
  let (x, y, w, h), status = widget ui area no_modkey in
  let proj = match orient with `Horizontal -> fst | `Vertical -> snd in
  let inj v = match orient with `Horizontal -> v, y | `Vertical -> x, v in
  let cursor = match orient with `Horizontal -> `E_W | `Vertical -> `N_S in
  if status <> `Untouched then Mouse.set_cursor ui.win (`Resize cursor);
  (*Draw.rect ui.win x y w h (border ui status);*)
  if status <> `Pressed then v else
  let over =
    match ui.drag_extra with
    | No_drag -> 0, 0
    | Divide {overshoot} -> overshoot
    | _ -> assert false
  in
  let vx, vy = inj v in
  let vx', vy' = add (add (vx, vy) (Mouse.delta ui.win)) over in
  let i, _, _, _, _ = area in
  let _, _, pw, ph = ui.panes.(i) in
  let minx, miny = inj minv in
  let maxx, maxy = inj maxv in
  let maxx = if maxx < 0 then pw else maxx in
  let maxy = if maxy < 0 then ph else maxy in
  let vx'', vy'' = clamp minx maxx vx', clamp miny maxy vy' in
  ui.drag_extra <- Divide {overshoot = vx' - vx'', vy' - vy''};
  (* HACK: Adjust owned drag_origin for size-relative position *)
  (* This assumes that the caller actually moves the divider! *)
  let dx = vx'' - vx in
  let dy = vy'' - vy in
  ui.drag_origin <- add ui.drag_origin (dx, dy);
  assert (inside ui.drag_origin (x + dx, y + dy, w, h));
  proj (vx'', vy'')


(* Text Input Field *)

let find_next_char s i =
  i + Uchar.utf_decode_length (String.get_utf_8_uchar s i)

let find_prev_char s i =
  let rec find j =
    if Char.code s.[j] land 0xc0 = 0x80 then find (j - 1) else j
  in find (i - 1)

let find_next_word s i =
  let rec find j =
    if j = String.length s || j > i && s.[j - 1] = ' ' && s.[j] <> ' ' then j else
    find (j + 1)
  in find i

let find_prev_word s i =
  let rec find j =
    if j = 0 || j < i && s.[j - 1] = ' ' && s.[j] <> ' ' then j else
    find (j - 1)
  in find i

let find_pos ui x h font s =
  let rec find i =
    if i = String.length s then i else
    let n = Uchar.utf_decode_length (String.get_utf_8_uchar s i) in
    let s' = String.sub s 0 (i + n) in
    let w = Draw.text_width ui.win h font s' in
    if w > x then i else find (i + n)
  in find 0


let edit_text ui area ph s scroll selection focus =
  let (x, y, w, h), status = widget ui area no_modkey in
  let len = String.length s in
  let font = font ui (max 0 (h - 2 * ph)) in
  let c = text_color ui in

  let focus' = focus || status = `Pressed in
  let selection' =
    if status <> `Pressed then selection else
    let mx, _ = Mouse.pos ui.win in
    let i = find_pos ui (mx - x + scroll) h font s in
    let lprim, rprim, _ = Option.value selection ~default: (i, i, i) in
    if Key.are_modifiers_down [] then
      if Mouse.is_pressed `Left then
        if Mouse.is_tripleclick `Left then
          Some (0, 0, len)
        else if Mouse.is_doubleclick `Left then
          let j = find_next_word s i in
          Some (find_prev_word s j, j, j)
        else
          Some (i, i, i)
      else if Mouse.is_drag `Left then
        if Mouse.is_doubleclick `Left then
          if i > rprim then
            Some (lprim, rprim, find_next_word s i)
          else if i < lprim then
            Some (lprim, rprim, find_prev_word s i)
          else
            Some (lprim, rprim, rprim)
        else
          Some (lprim, rprim, i)
      else selection
    else if Key.are_modifiers_down [`Shift] then
      if Mouse.is_tripleclick `Left then
        Some (0, 0, len)
      else if Mouse.is_doubleclick `Left then
        if i > rprim then
          Some (lprim, rprim, find_next_word s i)
        else if i < lprim then
          Some (lprim, rprim, find_prev_word s i)
        else
          Some (lprim, rprim, rprim)
      else
        Some (lprim, rprim, i)
    else
      selection
  in

  match selection' with
  | None ->
    Draw.clip ui.win x y w h;
    Draw.text ui.win (x - scroll) y h c font s;
    Draw.unclip ui.win;
    s, scroll, None, Uchar.of_int 0

  | Some (lprim, rprim, sec) ->
    let lprim, rprim, sec = min lprim len, min rprim len, min sec len in
    let l, r = min lprim sec, max rprim sec in
    let sl = String.sub s 0 l in
    let sm = String.sub s l (r - l) in
    let sr = String.sub s r (len - r) in
    let ws = Draw.text_spacing ui.win h font in
    let wl = Draw.text_width ui.win h font sl + ws in
    let wm = Draw.text_width ui.win h font sm + ws in
    let wt = if lprim >= sec then wl else wl + wm in
    let wc = 1 in
    let scroll' =
      if wt < scroll then wt else
      if wt + ws + wc > w + scroll then wt + ws + wc - w else scroll
    in

    Draw.clip ui.win x y w h;
    if not focus' then
    (
      Draw.text ui.win (x - scroll') y h c font s;
    )
    else if l = r then
    (
      Draw.text ui.win (x - scroll') y h c font s;
      Draw.fill ui.win (x - scroll' + wl) y 1 h c;
    )
    else
    (
      Draw.fill ui.win (x - scroll' + wl) y wm h c;
      Draw.text ui.win (x - scroll') y h c font sl;
      Draw.text ui.win (x - scroll' + wl) y h `Black font sm;
      Draw.text ui.win (x - scroll' + wl + wm) y h c font sr;
    );
    Draw.unclip ui.win;

    if not focus' then s, scroll', None, Uchar.of_int 0 else

    let ch = Key.char () in
    if ch >= Uchar.of_int 32 then
    (
      let open Stdlib in
      let buf = Buffer.create (len + 4) in
      Buffer.add_string buf sl;
      Buffer.add_utf_8_uchar buf ch;
      Buffer.add_string buf sr;
      let l' = l + Uchar.utf_8_byte_length ch in
      Buffer.contents buf, scroll', Some (l', l', l'), ch
    )
    else if Key.are_modifiers_down [] then
    (
      if Key.is_pressed `Return || Key.is_pressed `Enter then
        s, scroll', Some (sec, sec, sec), Uchar.of_char '\n'
      else if
        Key.is_pressed_or_repeated `Delete ||
        Key.is_pressed_or_repeated `Backspace
      then
      (
        if l <> r then
          sl ^ sr, scroll', Some (l, l, l), ch
        else if r < len && Key.is_pressed_or_repeated `Delete then
          let n = find_next_char sr 0 in
          sl ^ String.sub sr n (len - r - n), scroll', Some (l, l, l), ch
        else if l > 0 && Key.is_pressed_or_repeated `Backspace then
          let n = find_prev_char sl l in
          String.sub sl 0 n ^ sr, scroll', Some (n, n, n), ch
        else
          s, scroll', Some (l, l, r), ch
      )
      else if Key.is_pressed_or_repeated (`Arrow `Left) && l > 0 then
        let l' = find_prev_char s l in
        s, scroll', Some (l', l', l'), ch
      else if Key.is_pressed_or_repeated (`Arrow `Right) && r < len then
        let r' = find_next_char s r in
        s, scroll', Some (r', r', r'), ch
      else if Key.is_pressed_or_repeated (`End `Up) then
        s, scroll', Some (0, 0, 0), ch
      else if Key.is_pressed_or_repeated (`End `Down) then
        s, scroll', Some (len, len, len), ch
      else
        s, scroll', Some (lprim, rprim, sec), ch
    )
    else if Key.are_modifiers_down [`Shift] then
    (
      if Key.is_pressed_or_repeated (`Arrow `Left) && sec > 0 then
        let sec' = find_prev_char s sec in
        s, scroll', Some (lprim, rprim, sec'), ch
      else if Key.is_pressed_or_repeated (`Arrow `Right) && sec < len then
        let sec' = find_next_char s sec in
        s, scroll', Some (lprim, rprim, sec'), ch
      else if Key.is_pressed_or_repeated (`End `Up) then
        s, scroll', Some (lprim, rprim, 0), ch
      else if Key.is_pressed_or_repeated (`End `Down) then
        s, scroll', Some (lprim, rprim, len), ch
      else
        s, scroll', Some (lprim, rprim, sec), ch
    )
    else if Key.are_modifiers_down [`Command] then
    (
      if Key.is_pressed_or_repeated (`Arrow `Left) && sec > 0 then
        let l' = find_prev_word s sec in
        s, scroll', Some (l', l', l'), ch
      else if Key.is_pressed_or_repeated (`Arrow `Right) && sec < len then
        let l' = find_next_word s sec in
        s, scroll', Some (l', l', l'), ch
      else if Key.is_pressed_or_repeated (`Char 'A') then
        s, scroll', Some (0, 0, len), ch
      else if Key.is_pressed_or_repeated (`Char 'N') then
        s, scroll', Some (sec, sec, sec), ch
      else if Key.is_pressed_or_repeated (`Char 'X') && l <> r then
        let sm = String.sub s l (r - l) in
        Clipboard.write ui.win sm;
        sl ^ sr, scroll', Some (l, l, l), ch
      else if Key.is_pressed_or_repeated (`Char 'C') && l <> r then
        let sm = String.sub s l (r - l) in
        Clipboard.write ui.win sm;
        s, scroll', Some (lprim, rprim, sec), ch
      else if Key.is_pressed_or_repeated (`Char 'V') then
        match Clipboard.read ui.win with
        | None -> s, scroll', Some (lprim, rprim, sec), ch
        | Some sp ->
          let i = l + String.length sp in
          sl ^ sp ^ sr, scroll', Some (i, i, i), ch
      else
        s, scroll', Some (lprim, rprim, sec), ch
    )
    else if Key.are_modifiers_down [`Command; `Shift] then
    (
      if Key.is_pressed_or_repeated (`Arrow `Left) && sec > 0 then
        let sec' = find_prev_word s sec in
        s, scroll', Some (lprim, rprim, sec'), ch
      else if Key.is_pressed_or_repeated (`Arrow `Right) && sec < len then
        let sec' = find_next_word s sec in
        s, scroll', Some (lprim, rprim, sec'), ch
      else
        s, scroll', Some (lprim, rprim, sec), ch
    )
    else
      s, scroll', Some (lprim, rprim, sec), ch


let rich_edit_text ui area ph (edit : Edit.t) =
  let s', scroll', sel', ch =
    edit_text ui area ph edit.text edit.scroll edit.sel_range edit.focus in
  if edit.focus then focus ui area;
  if s' <> edit.text then
    Edit.update edit s';
  Edit.scroll edit scroll';
  if sel' <> None then
  (
    Edit.select edit sel';
    Edit.focus edit;
  );

  if edit.focus then
  (
    if Key.is_pressed_or_repeated (`Char 'Z') then
    (
      if Key.are_modifiers_down [`Command] then
        Edit.pop_undo edit
      else if Key.are_modifiers_down [`Command; `Shift] then
        Edit.pop_redo edit
    )
    else if Key.is_pressed_or_repeated (`Arrow `Up) then
    (
      Edit.prev_history edit
    )
    else if Key.is_pressed_or_repeated (`Arrow `Down) then
    (
      Edit.next_history edit
    )
  );

  ch


(* Tables *)

type align = [`Left | `Center | `Right]
type inversion = [`Regular | `Inverted]
type order = [`Asc | `Desc]
type sorting = (int * order) list
type column = int * align
type cell = [`Text of string | `Image of image]
type row = color * inversion * cell iarray
type heading = string iarray * sorting

let table_pad gw = (gw + 1)/2

let draw_table ui area gw ch ph cols rows hscroll =
  let x, y, w, h = dim ui area in
  Draw.fill ui.win x y w h `Black;
  let rh = ch + 2 * ph in
  let mw = table_pad gw in  (* inner width padding *)
  let flex = max 0
    (w - Iarray.fold_left (fun w (cw, _) -> w + cw + gw) (2 * mw - gw + 1) cols) in
  let font = font ui ch in
  (* Draw row background first since it must be unclipped. *)
  Iarray.iteri (fun j (fg, inv, _contents) ->
    let ry = y + j * rh in
    let bg = if j mod 2 = 0 then `Black else `Gray 0x20 in
    let bg = if inv = `Inverted then fg else bg in
    if bg <> `Black then Draw.fill ui.win x ry w rh bg
  ) rows;
  let cx = ref (x + mw - hscroll) in
  Iarray.iteri (fun i (cw, align) ->
    let cw = if cw < 0 then flex / (- cw) else cw in
    let cw' = min cw (x + w - mw - !cx) in
    let left = max !cx (x + mw) in
    Draw.clip ui.win left y (cw' - max 0 (left - !cx)) h;
    Iarray.iteri (fun j (fg, inv, contents) ->
      let cy = y + j * rh + ph in
      let bg = if j mod 2 = 0 then `Black else `Gray 0x20 in
      let fg, bg = if inv = `Inverted then bg, fg else fg, bg in
      (match Iarray.get contents i with
      | `Text text ->
        let tw = Draw.text_width ui.win ch font text in
        let dx =
          match align with
          | `Left -> 0
          | `Center -> (cw - tw) / 2
          | `Right ->
            (* Add extra padding if back to back with a left-aligned column *)
            cw - tw -
              ( if i + 1 < Iarray.length cols
                && snd (Iarray.get cols (i + 1)) = `Left then 4 else 0 )
        in
        Draw.text ui.win (!cx + max 0 dx) cy ch fg font text;
        if tw >= cw then
        (
          let rw = min cw 16 in
          Draw.gradient ui.win (!cx + cw - rw) cy rw ch
            (`Trans (bg, 0)) `Horizontal bg;
        )
      | `Image img ->
        let iw, ih = Api.Image.size img in
        let q = float cw /. float ch in
        let iq = float iw /. float ih in
        let ih' = int_of_float (float ih *. iq /. q) in
        Api.Draw.image_part ui.win !cx cy cw ch 0 0 iw ih' img;
      )
    ) rows;
    Draw.unclip ui.win;
    cx := !cx + cw + gw;
  ) cols

let find_column gw cols hscroll dx =
  let mw = table_pad gw in
  let rec find i cx =
    if i = Iarray.length cols then None else
    let cx' = cx + fst (Iarray.get cols i) in
    if dx >= cx && dx < cx' then Some i else
    if dx >= cx' then find (i + 1) (cx' + gw) else
    None
  in find 0 (mw - hscroll)

let find_gutter gw cols hscroll dx =
  let mw = table_pad gw in
  let gutter_tolerance = 5 in
  let rec find i cx =
    if i = Iarray.length cols then `None else
    let cx' = cx + fst (Iarray.get cols i) in
    if abs (cx' + gw/2 - dx) < gutter_tolerance then `Gutter i else
    if cx' + gw/2 < dx then find (i + 1) (cx' + gw) else
    `Header i
  in find 0 (mw - hscroll)

let table ui area gw ch ph cols rows hscroll =
  let (x, y, _, _), status = widget ui area no_modkey in
  draw_table ui area gw ch ph cols rows hscroll;
  if status = `Pressed || status = `Released then
    let mx, my = Mouse.pos ui.win in
    let rh = ch + 2 * ph in
    Some ((my - y) / rh), find_column gw cols hscroll (mx - x)
  else
    None, None


(* Table Headers *)

let symbols_asc = [|"▲" (* "▴" *); "▲'" (* "△", "▵", "▵" *); "▲''"; "▲'''"|]
let symbols_desc = [|"▼" (* "▾" *); "▼'" (* "▽", "▾", "▿" *); "▼''"; "▼'''"|]

type drag += Header_resize of {mouse_x : int; col : int}
type drag += Header_reorder of {mouse_x : int; col : int; moved : bool}

let header ui area ph gw cols (titles, sorting) hscroll =
  let (x, y, w, h) as r, status = widget ui area no_modkey in
  let texts = Iarray.map (fun s -> `Text s) titles in
  let th = h - 2 * ph in
  ignore (table ui area gw th ph cols
    [|text_color ui, `Inverted, texts|] hscroll);

  let mw = table_pad gw in
  Draw.clip ui.win x y w h;
  ignore (
    Iarray.fold_left (fun cx (cw, _) ->
      Draw.fill ui.win (cx + cw + gw/2 - hscroll) y 1 h `Black;
      cx + cw + gw;
    ) (x + mw) cols - x - mw
  );

  List.iteri (fun k (i, order) ->
    let rec find_header j cx =
      let cw = fst (Iarray.get cols j) in
      if j < i then find_header (j + 1) (cx + cw + gw) else
      cx, cw
    in
    let cx, cw = find_header 0 x in
    let syms = match order with `Asc -> symbols_asc | `Desc -> symbols_desc in
    if k < Array.length syms then
      let font = font ui th in
      let tw = Draw.text_width ui.win th font syms.(k) in
      if cw > tw then
        Draw.text ui.win (cx + cw - tw + 4 - hscroll) (y + ph) th `Black font syms.(k)
  ) sorting;
  Draw.unclip ui.win;

  let find_gutter cols mx = find_gutter gw cols hscroll (mx - x) in
  let find_column cols mx = find_column gw cols hscroll (mx - x) in

  let mx, my = Mouse.pos ui.win in
  match ui.drag_extra with
  | No_drag ->
    if not (inside (mx, my) r) then `None else
    (match find_gutter cols mx with
    | `None when status = `Released ->
      (match find_column cols mx with
      | None -> `None
      | Some i -> `Click i
      )
    | `None ->
      if not ui.modal && Mouse.is_pressed `Right then
        `Menu None
      else
        `None
    | `Gutter col ->
      Mouse.set_cursor ui.win (`Resize `E_W);
      if status = `Pressed then
        ui.drag_extra <- Header_resize {mouse_x = mx; col};
      `None
    | `Header col ->
      if not ui.modal && Mouse.is_pressed `Right then
        `Menu (Some col)
      else if status = `Pressed then
      (
        ui.drag_extra <- Header_reorder {mouse_x = mx; col; moved = false};
        `None
      )
      else `None
    )

  | Header_resize {mouse_x; col = i} when status = `Pressed ->
    Mouse.set_cursor ui.win (`Resize `E_W);
    let dx = mx - mouse_x in
    if dx = 0 then `None else
    let ws = Array.init (Iarray.length cols) (fun i -> fst (Iarray.get cols i)) in
    ws.(i) <- max 0 (ws.(i) + dx);
    if i + 1 < Array.length ws && Key.is_modifier_down `Shift then
      ws.(i + 1) <- max 0 (ws.(i + 1) - dx);
    ui.drag_extra <- Header_resize {mouse_x = mx; col = i};
    `Resize (Iarray.of_array ws)

  | Header_reorder {mouse_x; col = i; moved} when status = `Pressed ->
    if moved then Mouse.set_cursor ui.win `Point;
    let dx = mx - mouse_x in
    if dx = 0 then `None else
    let _ = ui.drag_extra <- Header_reorder {mouse_x; col = i; moved = true} in
    (match find_gutter cols mx with
    | `None | `Gutter _ -> `None
    | `Header j ->
      if i = j then `None else
      let perm =
        Iarray.init (Iarray.length cols) (fun k ->
          if k = j then i else
          if k >= min i j && k <= max i j then k + j - i else
          k
        )
      in
      let cols' =
        Iarray.mapi (fun i _ -> Iarray.get cols (Iarray.get perm i)) cols in
      (* Ignore change if new position is not stable. *)
      match find_gutter cols' mx with
      | `Header k when k = j ->
        ui.drag_extra <- Header_reorder {mouse_x = mx; col = j; moved = true};
        `Reorder perm
      | _ -> `None
    )

  | Header_reorder {col = i; moved = false; _} when status = `Released ->
    `Click i

  | _ -> `None


(* Rich Tables *)

type cached = buffer

type rich_table =
  { gutter_w : int;
    text_h : int;
    pad_h : int;
    scroll_w : int ;
    scroll_h : int;
    refl_r : int;
    has_heading : bool
  }

type table_action =
  [ `Click of int option * int option
  | `Select
  | `Scroll
  | `Move of int
  | `Drag of int * motion * trajectory
  | `Drop
  | `Menu of int option * int option
  | `None
  ]

type rich_table_action =
  [ table_action
  | `Sort of int
  | `Resize of int iarray   (* new sizes *)
  | `Reorder of int iarray  (* permutation *)
  | `HeadMenu of int option
  ]

let rich_table_inner_area _ui area geo =
  let p, ax, ay, aw, ah = area in
  let ty = if not geo.has_heading then ay else ay + geo.text_h + 2 * geo.pad_h + 2 in
  let th =
    ah -
    (if ah < 0 then 0 else ty - ay) -
    (if geo.scroll_h = 0 then 0 else geo.scroll_h + 1)
  in
  (p, ax, ty, aw - geo.scroll_w - 1, th)

let rich_table_mouse ui area geo cols (tab : _ Table.t) =
  let area' = rich_table_inner_area ui area geo in
  let (x, y, _, _) as r = dim ui area' in
  let (mx, my) as m = Mouse.pos ui.win in
  if inside m r then
    let row = (my - y) / (geo.text_h + 2 * geo.pad_h) + tab.vscroll in
    Some (
      (if row < Table.length tab then Some row else None),
      find_column geo.gutter_w cols tab.hscroll (mx - x)
    )
  else
    None

let rich_table_drag ui area geo style tab =
  match rich_table_mouse ui area geo [||] tab with
  | Some (i_opt, _) ->
    let area' = rich_table_inner_area ui area geo in
    let x, y, w, _ = dim ui area' in
    let rh = geo.text_h + 2 * geo.pad_h in
    let i' = Option.value i_opt ~default: (Table.length tab) - tab.vscroll in
    focus' ui x (y + i' * rh) w rh `White style
  | _ -> ()

let adjust_cache ui tab w h =
  Option.iter (fun buf ->
    if Buffer.size buf <> (w, h)
    || Buffer.scale buf <> Buffer.needed_scale ui.win then
    (
      Table.uncache tab;
      Buffer.dispose buf;
    )
  ) tab.cache;
  match tab.cache with
  | Some buf -> buf
  | None ->
    let buf = Buffer.create ui.win w h in
    Table.cache tab buf;
    buf

let rich_table ui area (geo : rich_table) cols header_opt (tab : _ Table.t) pp_row =
  assert (geo.has_heading = Option.is_some header_opt);
  let p, ax, ay, aw, ah = area in
  let rh = geo.text_h + 2 * geo.pad_h in
  let _, _, ty, tw, th = rich_table_inner_area ui area geo in
  let header_area = (p, ax, ay, tw, rh) in
  let table_area = (p, ax, ty, tw, th) in
  let vscroll_area = 
    (p, (if aw < 0 then tw else ax + aw + 1), ay, geo.scroll_w, ah) in
  let hscroll_area =
    (p, ax, (if ah < 0 then ah - geo.scroll_h else ty + th + 1), tw, geo.scroll_h) in
  let (x, y, w, h) as r = dim ui table_area in

  let shift = Key.are_modifiers_down [`Shift] in
  let command = Key.are_modifiers_down [`Command] in

  Mutex.protect tab.mutex (fun () ->
    let len = Table.length tab in
    let page = max 1 (int_of_float (Float.floor (float h /. float rh))) in
    let limit = min len (tab.vscroll + page) in
    (* Correct scrolling position for possible resize *)
    Table.adjust_vscroll tab tab.vscroll page;

    (* Body *)
    let buf = adjust_cache ui tab w h in
    if not ui.buffered || tab.dirty || Draw.frame ui.win mod 10 = 7 then
    (
      let rows =
        Iarray.init (min page len) (fun i ->
          let i = tab.vscroll + i in
          let c, cols = pp_row i in
          let inv = if Table.is_selected tab i then `Inverted else `Regular in
          c, inv, cols
        )
      in
      if ui.buffered then Draw.buffered ui.win buf;
      let area' = if ui.buffered then (-1, 0, 0, w, h) else table_area in
      draw_table ui area' geo.gutter_w geo.text_h geo.pad_h cols rows tab.hscroll;
      if ui.buffered then Draw.unbuffered ui.win;
      Table.clean tab;
    );
    if ui.buffered then Draw.buffer ui.win x y buf;

    let mx, my = Mouse.pos ui.win in
    let i = tab.vscroll + (my - y) / rh in
    let _, status = widget ui table_area no_modkey in
    (* Mirrors logic in table *)
    let left_mouse_used = (status = `Pressed || status = `Released) in

    let find_column cols mx =
      find_column geo.gutter_w cols tab.hscroll (mx - x) in

    let result =
      if not ui.modal && Mouse.is_pressed `Right then
      (
        if inside (mx, my) r then
        (
          let row = if i >= limit then None else Some i in
          if Table.has_selection tab
          && (row = None || not (Table.is_selected tab i)) then
          (
            Table.deselect_all tab;
            if row <> None then Table.select tab i i;
          );
          `Menu (row, find_column cols mx)
        )
        else
          `None
      )
      else if not left_mouse_used then
        `None
      else if not (shift || command) then
      (
        match drag_status ui r (max_int, rh) with
        | `None -> `None

        | `Take ->
          (* Click *)
          let col = find_column cols mx in
          if i >= limit then
          (
            (* Click on empty space *)
            Table.deselect_all tab;
            `Click (None, col)
          )
          else
          (
            (* Click on entry *)
            if not (Table.is_selected tab i) then
              Table.deselect_all tab;
            if not (Mouse.is_doubleclick `Left) then
              Table.select tab i i;
            `Click (Some i, col)
          )

        | `Click ->
          (* Click-release: deselect all except for clicked entry *)
          Table.deselect_all tab;
          let col = find_column cols mx in
          if i >= limit then
          (
            (* Click on empty space *)
            `Click (None, col)
          )
          else
          (
            (* Click on entry *)
            Table.select tab i i;
            `Click (Some i, col)
          )

        | `Drag ((_, dy), motion, traj) -> `Drag (dy, motion, traj)

        | `Drop -> `Drop
      )
      else if command && not ui.modal && Mouse.is_pressed `Left then
      (
        (* Cmd-click on entry: toggle selection of clicked entry *)
        let col = find_column cols mx in
        if i >= limit then
          `Click (None, col)
        else
        (
          if Table.is_selected tab i then
            Table.deselect tab i i
          else
            Table.select tab i i;
          `Click (Some i, col);
        )
      )
      else if shift && not ui.modal && Mouse.is_down `Left then
      (
        (* Shift-click/drag on playlist: adjust selection range *)
        let default = if i < len then (i, i) else (0, 0) in
        let pos1, pos2 = Option.value tab.sel_range ~default in
        let i' = max 0 (min i (len - 1)) in
        let old_selection = tab.selected in
        if tab.sel_range = None || Table.is_selected tab pos1 then
        (
          (* Entry was already selected: deselect old range, select new range *)
          Table.deselect tab pos2 i';
          Table.select tab pos1 i'
        )
        else
        (
          (* Track was not selected: select old range, deselect new range *)
          Table.select tab pos2 i';
          Table.deselect tab pos1 i'
        );
        if not ui.modal && Mouse.is_pressed `Left then
          `Click ((if i < len then Some i else None), find_column cols mx)
        else if Table.IntSet.equal tab.selected old_selection then
          `None
        else
          `Select
      )
      else `None
    in

    (* Header *)
    let result =
      match header_opt with
      | None -> result
      | Some heading ->
        match header ui header_area geo.pad_h geo.gutter_w cols heading tab.hscroll with
        | `Click i -> Table.dirty tab; `Sort i
        | `Resize ws -> Table.dirty tab; `Resize ws
        | `Reorder perm -> Table.dirty tab; `Reorder perm
        | `Menu i -> `HeadMenu i
        | `None -> result
    in

    (* Vertical scrollbar *)
    let wdx, wdy = wheel_status ui r in
    let wdx, wdy = if Float.abs wdx > Float.abs wdy then wdx, 0.0 else 0.0, wdy in
    let vwheel = not shift && len > page || wdy = 0.0 in
    let h' = page * rh in
    let ext = if len = 0 then 1.0 else min 1.0 (float h' /. float (len * rh)) in
    let pos = if len = 0 then 0.0 else float tab.vscroll /. float len in
    let coeff = max 1.0 (float page /. 4.0) /. float (len - page) in
    let wheel = if vwheel then coeff *. wdy else 0.0 in
    let pos' = scroll_bar ui vscroll_area `Vertical pos ext -. wheel in
    let result =
      if result <> `None || pos = pos' then result else
      (
        Table.set_vscroll tab
          (max 0 (int_of_float (Float.round (pos' *. float len)))) page;
        `Scroll
      )
    in

    (* Horizontal scrollbar *)
    let result =
      if geo.scroll_h = 0 then result else
      let vw = Iarray.fold_left (fun w (cw, _) -> w + cw + geo.gutter_w) 0 cols in
      let vw' = max vw (tab.hscroll + w) in
      let ext = if vw' = 0 then 1.0 else min 1.0 (float w /. float vw') in
      let pos = if vw' = 0 then 0.0 else float tab.hscroll /. float vw' in
      let wheel = if vwheel then wdx else wdy in
      let pos' = scroll_bar ui hscroll_area `Horizontal pos ext -. 0.05 *. wheel in
      if result <> `None || pos = pos' then result else
      (
        Table.set_hscroll tab
          (clamp 0 (max 0 (vw' - w)) (int_of_float (Float.round (pos' *. float vw'))));
        `Scroll
      )
    in

    (* Focus and mouse reflection *)
    if tab.focus && len > 0 then focus ui table_area;
    mouse_focus ui area geo.refl_r;

    (* Keys *)
    let result =
      if result <> `None || not tab.focus then result else
      (
        let d =
          if key_status' ui (`Arrow `Up) = `Pressed then -1 else
          if key_status' ui (`Arrow `Down) = `Pressed then +1 else
          if key_status' ui (`Page `Up) = `Pressed then -page else
          if key_status' ui (`Page `Down) = `Pressed then +page else
          if key_status' ui (`End `Up) = `Pressed then -len else
          if key_status' ui (`End `Down) = `Pressed then +len else
          0
        in
        if min len (abs d) > 0 then
        (
          (* Cursor movement *)
          let has_sel = tab.sel_range <> None in
          let default =
            0, if not shift then tab.vscroll else if d < 0 then len else -1 in
          let pos1, pos2 = Option.value tab.sel_range ~default in
          let i = if d < 0 then max 0 (pos2 + d) else min (len - 1) (pos2 + d) in

          if not (shift || command) then
          (
            (* Plain cursor movement: deselect all, reselect relative to range end *)
            if has_sel then
            (
              Table.deselect_all tab;
              Table.select tab i i;
              Table.adjust_vscroll tab i page;
              `Select
            )
            else
            (
              Table.set_vscroll tab i page;
              `Scroll
            )
          )
          else if shift then
          (
            (* Shift-cursor movement: adjust selection range *)
            if not has_sel then
            (
              (* No selection yet: range from end of playlist *)
              Table.select tab (len - 1) i;
            )
            else if Table.is_selected tab pos1 then
            (
              (* Range start was already selected: deselect old range, select new *)
              Table.deselect tab (max 0 pos2) i;
              Table.select tab pos1 i;
            )
            else
            (
              (* Range start was not selected: select old range, deselect new *)
              Table.select tab (max 0 pos2) i;
              Table.deselect tab pos1 i;
            );
            Table.adjust_vscroll tab i page;
            `Select
          )
          else if command && has_sel then
          (
            (* Cmd-cursor movement: move selection *)
            Table.adjust_vscroll tab i page;
            `Move d
          )
          else `None
        )
        else if command then
        (
          if key_status' ui (`Char 'A') = `Pressed then
          (
            (* Select-all key pressed: select all *)
            Table.select_all tab;
            `Select
          )
          else if key_status' ui (`Char 'N') = `Pressed then
          (
            (* Deselect-all key pressed: deselect all *)
            Table.deselect_all tab;
            `Select
          )
          else if key_status' ui (`Char 'I') = `Pressed then
          (
            (* Selection inversion key pressed: invert selection *)
            Table.select_invert tab;
            `Select
          )
          else `None
        )
        else
        (
          let ch = Key.char () in
          if ch >= Uchar.of_int 32 then
          (
            (* Plain character pressed: scroll to first entry *)
            let b = Bytes.make 8 '\000' in
            let s = Bytes.sub_string b 0 (Bytes.set_utf_8_uchar b 0 ch) in
            let col =
              match header_opt with
              | Some (_, (col, _)::_) -> col  (* primary sort key *)
              | _ -> 0
            in
            let rec find i =
              if i = len then i else
              let _, row = pp_row i in  (* TODO: only pp relevant column *)
              match Iarray.get row col with
              | `Text s' when Unicode.compare_utf_8 s s' <= 0 -> i
              | _ -> find (i + 1)
            in
            let i = find 0 in
            if i < len then
            (
              Table.set_vscroll tab i page;
              `Scroll
            )
            else `None
          )
          else `None
        )
      )
    in

    let result =
      if result <> `None || not tab.focus || geo.scroll_h = 0 then result else
      (
        let step = if shift then 10 else 50 in
        let dh =
          if key_status' ui (`Arrow `Left) = `Pressed then -step else
          if key_status' ui (`Arrow `Right) = `Pressed then +step else
          0
        in
        if abs dh > 0 && not command then
        (
          Table.set_hscroll tab (tab.hscroll + dh);
          `Scroll;
        )
        else `None
      )
    in

    result
  )


(* Browser *)

type browser_action =
  [ table_action
  | `Fold of int
  ]

let symbol_empty = " ○"
let symbol_folded = "►" (* "▸" *)
let symbol_unfolded = "▼" (* "▾" *)


let browser_pp_pre nest folded =
  let sym =
    match folded with
    | None -> symbol_empty
    | Some true -> symbol_folded
    | Some false -> symbol_unfolded
  in
  if nest = -1 then "" else String.make (3 * nest) ' ' ^ sym ^ " "

let browser_entry_text_area ui area geo (tab : _ Table.t) i nest folded =
  let p, x, y, w, _ = rich_table_inner_area ui area geo in
  let mw = (geo.gutter_w + 1) / 2 in  (* inner width padding *)
  let correction = if Api.is_mac then -2 else +1 in
  let dx = max 0
    (Draw.text_width ui.win geo.text_h (font ui geo.text_h)
      (browser_pp_pre nest folded) + mw - tab.hscroll + correction)
  and dy = (i - tab.vscroll) * (geo.text_h + 2 * geo.pad_h) in
  (p, x + dx, y + dy + geo.pad_h, (if w < 0 then w else w - dx), geo.text_h)

let browser ui area geo (tab : _ Table.t) pp_entry =
  let cols : _ iarray = [|-1, `Left|] in
  let pp_row i : _ * _ iarray =
    let nest, folded, c, name = pp_entry i in
    c, [|`Text (browser_pp_pre nest folded ^ name)|]
  in

  let selected = tab.selected in
  (match rich_table ui area geo cols None tab pp_row with
  | `None -> `None
  | `Scroll -> `Scroll
  | `Move i -> `Move i
  | `Drag (i, motion, traj) -> `Drag (i, motion, traj)
  | `Drop -> `Drop
  | `Menu (i, _) -> `Menu (i, None)
  | `Sort _ | `Resize _ | `Reorder _ | `HeadMenu _ -> assert false

  | `Select ->
    (* TODO: allow multiple selections *)
    if Table.num_selected tab <= 1 then `Select else
    (
      Table.reset_selected tab selected;  (* override *)
      `None
    )

  | `Click (None, _) -> `Click (None, None)
  | `Click (Some i, _) ->
    (* Click on entry *)
    let mx, _ = Mouse.pos ui.win in
    let x, _, _, _ = dim ui area in
    let nest, folded, _, _ = pp_entry i in
    let tw =
      Draw.text_width ui.win geo.text_h (font ui geo.text_h)
        (browser_pp_pre nest folded) in
    if mx + tab.hscroll < x + tw
    && not ui.modal && Mouse.(is_down `Left || is_released `Left) then
    (
      (* CLick on triangle *)
      Table.reset_selected tab selected;  (* override selection change*)
      if Mouse.is_released `Left then
        `Fold i
      else
        `None
    )
    else
    (
      (* Click on name *)
      (* TODO: allow multiple selections *)
      if Table.num_selected tab > 1 then
        Table.reset_selected tab selected;  (* override *)
      `Click (Some i, None)
    )
  )


(* Grids *)

let draw_grid ui area gw iw ch ph matrix =
  let x, y, w, h = dim ui area in
  Draw.fill ui.win x y w h `Black;
  let mw = (gw + 1)/2 in
  let font = font ui ch in
  let nrows = Iarray.length matrix in
  let ncols =
    if nrows = 0 then 0 else Iarray.length (Iarray.get matrix 0) in
  for i = 0 to ncols - 1 do
    let cx = x + mw + i * (iw + gw) in
    Draw.clip ui.win (cx - 1) y (iw + 2) h;
    for j = 0 to nrows - 1 do
      Option.iter (fun (img, c, inv, txt) ->
        let cy = y + mw + j * (iw + gw + ch + 2 * ph) in
        let fg, bg = if inv = `Inverted then `Black, c else c, `Black in
        if bg <> `Black then
          Draw.fill ui.win (cx - 1) (cy - 1) (iw + 2) (iw + ch + 2 * ph + 2) bg;
        let iw', ih' = Image.size img in
        let scale = float iw /. float (max iw' ih') in
        let dx = int_of_float ((float iw -. scale *. float iw') /. 2.0) in
        let dy = int_of_float ((float iw -. scale *. float ih') /. 2.0) in
        Api.Draw.image_part ui.win (cx + dx) (cy + dy) (iw - 2*dx) (iw - 2*dy) 0 0 iw' ih' img;
        let tw = Draw.text_width ui.win ch font txt in
        let dx = max 0 ((iw - tw - 2) / 2) in
        Draw.text ui.win (cx + dx + 1) (cy + iw + ph) ch fg font txt;
        if tw > iw - 2 then
        (
          let rw = min (iw - 2) 16 in
          Draw.gradient ui.win (cx + iw - rw + 1) (cy + iw) rw ch
            (`Trans (bg, 0)) `Horizontal bg;
        );
      ) (Iarray.get (Iarray.get matrix j) i)
    done;
    Draw.unclip ui.win
  done


let grid ui area gw iw ch ph matrix =
  let (x, y, _, _), status = widget ui area no_modkey in
  draw_grid ui area gw iw ch ph matrix;
  if status = `Pressed || status = `Released then
    let mx, my = Mouse.pos ui.win in
    Some ((mx - x) / (iw + gw), (my - y) / (iw + ch + gw))
  else
    None


type grid_table =
  { gutter_w : int;
    img_h : int;
    text_h : int;
    pad_h : int;
    scroll_w : int ;
    refl_r : int;
    has_heading : bool
  }

type grid_table_action = rich_table_action

let grid_table_inner_area _ui area geo =
  let p, ax, ay, aw, ah = area in
  let ty = if not geo.has_heading then ay else ay + geo.text_h + 2 in
  let tw = aw - geo.scroll_w - 1 in
  let th = ah - (if ah < 0 then 0 else ty - ay) in
  (p, ax, ty, tw, th)

let grid_table_mouse ui area geo (tab : _ Table.t) =
  let area' = grid_table_inner_area ui area geo in
  let (x, y, w, _) as r = dim ui area' in
  let iw = geo.gutter_w + geo.img_h in
  let ih = iw + geo.text_h in
  let line = max 1 Float.(to_int (floor (float w /. float iw))) in
  let (mx, my) as m = Mouse.pos ui.win in
  if inside m r then
    let row = (my - y) / ih * line + (mx - x) / iw + tab.vscroll in
    Some ((if row < Table.length tab then Some row else None), None)
  else
    None

let grid_table_drag ui area geo style tab =
  match grid_table_mouse ui area geo tab with
  | Some (i_opt, _) ->
    let area' = grid_table_inner_area ui area geo in
    let x, y, w, _ = dim ui area' in
    let iw = geo.gutter_w + geo.img_h in
    let ih = iw + geo.text_h in
    let line = max 1 Float.(to_int (floor (float w /. float iw))) in
    let i' = Option.value i_opt ~default: (Table.length tab) - tab.vscroll in
    focus' ui (x + i' mod line * iw) (y + i' / line * ih) iw ih `White style
  | _ -> ()

let grid_table ui area (geo : grid_table) header_opt (tab : _ Table.t) pp_cell =
  assert (geo.has_heading = Option.is_some header_opt);
  let p, ax, ay, aw, ah = area in
  let ch = geo.text_h in
  let _, _, ty, tw, th = grid_table_inner_area ui area geo in
  let header_area = (p, ax, ay, tw, ch) in
  let table_area = (p, ax, ty, tw, th) in
  let vscroll_area = 
    (p, (if aw < 0 then tw else ax + aw + 1), ay, geo.scroll_w, ah) in
  let (x, y, w, h) as r = dim ui table_area in

  let shift = Key.are_modifiers_down [`Shift] in
  let command = Key.are_modifiers_down [`Command] in

  Mutex.protect tab.mutex (fun () ->
    let len = Array.length tab.entries in
    let iw = geo.gutter_w + geo.img_h in
    let ih = iw + ch in
    let line = max 1 Float.(to_int (floor (float w /. float iw))) in
    let page =
      max 1 Float.(to_int (floor (float h /. float ih)) * line) in
    let page_ceil =
      max 1 Float.(to_int (ceil (float h /. float ih)) * line) in
    (* Correct scrolling position for possible resize *)
    Table.adjust_vscroll tab tab.vscroll page;

    (* Body *)
    let buf = adjust_cache ui tab w h in
    if not ui.buffered || tab.dirty then
    (
      let matrix =
        Iarray.init (page_ceil / line) (fun j ->
          Iarray.init line (fun i ->
            let k = tab.vscroll + j * line + i in
            if k >= len then None else
            let img, c, txt = pp_cell k in
            let inv = if Table.is_selected tab k then `Inverted else `Regular in
            Some (img, c, inv, txt)
          )
        )
      in
      if ui.buffered then Draw.buffered ui.win buf;
      let area' = if ui.buffered then (-1, 0, 0, w, h) else table_area in
      draw_grid ui area' geo.gutter_w geo.img_h geo.text_h geo.pad_h matrix;
      if ui.buffered then Draw.unbuffered ui.win;
      Table.clean tab;
    );
    if ui.buffered then Draw.buffer ui.win x y buf;

    let mx, my = Mouse.pos ui.win in
    let i, j = (mx - x) / iw, (my - y) / ih in
    let k = tab.vscroll + j * line + i in
    let on_bg = i >= line || k >= min len (tab.vscroll + page_ceil) in

    let _, status = widget ui table_area no_modkey in
    (* Mirrors logic in grid *)
    let left_mouse_used = (status = `Pressed || status = `Released) in

    let result =
      if not ui.modal && Mouse.is_pressed `Right then
      (
        if inside (mx, my) r then
        (
          let row = if on_bg then None else Some k in
          if Table.has_selection tab
          && (row = None || not (Table.is_selected tab k)) then
          (
            Table.deselect_all tab;
            if row <> None then Table.select tab k k;
          );
          `Menu (row, None)
        )
        else
          `None
      )
      else if not left_mouse_used then
        `None
      else if not (shift || command) then
      (
        match drag_status ui r (iw, ih) with
        | `None -> `None

        | `Take ->
          (* Click *)
          if on_bg then
          (
            (* Click on empty space *)
            Table.deselect_all tab;
            `Click (None, None)
          )
          else
          (
            (* Click on entry *)
            if not (Table.is_selected tab k) then
              Table.deselect_all tab;
            if not (Mouse.is_doubleclick `Left) then
              Table.select tab k k;
            `Click (Some k, None)
          )

        | `Click ->
          (* Click-release: deselect all except for clicked entry *)
          Table.deselect_all tab;
          if on_bg then
            `Click (None, None)
          else
          (
            Table.select tab k k;
            `Click (Some i, None)
          )

        | `Drag ((dx, dy), motion, traj) -> `Drag (dx + dy * line, motion, traj)

        | `Drop -> `Drop
      )
      else if command && not ui.modal && Mouse.is_pressed `Left then
      (
        (* Cmd-click on entry: toggle selection of clicked entry *)
        if on_bg then
          `Click (None, None)
        else
        (
          if Table.is_selected tab k then
            Table.deselect tab k k
          else
            Table.select tab k k;
          `Click (Some k, None);
        )
      )
      else if shift && not ui.modal && Mouse.is_down `Left then
      (
        (* Shift-click/drag on playlist: adjust selection range *)
        let default = if k < len then (k, k) else (0, 0) in
        let pos1, pos2 = Option.value tab.sel_range ~default in
        let k' = max 0 (min k (len - 1)) in
        let old_selection = tab.selected in
        if tab.sel_range = None || Table.is_selected tab pos1 then
        (
          (* Entry was already selected: deselect old range, select new range *)
          Table.deselect tab pos2 k';
          Table.select tab pos1 k'
        )
        else
        (
          (* Track was not selected: select old range, deselect new range *)
          Table.select tab pos2 k';
          Table.deselect tab pos1 k'
        );
        if not ui.modal && Mouse.is_pressed `Left then
          `Click ((if k < len then Some k else None), None)
        else if Table.IntSet.equal tab.selected old_selection then
          `None
        else
          `Select
      )
      else `None
    in

    (* Header *)
    let result =
      match header_opt with
      | None -> result
      | Some heading ->
        let cols = Iarray.map (Fun.const (40, `Left)) (fst heading) in
        match header ui header_area geo.pad_h geo.gutter_w cols heading tab.hscroll with
        | `Click i -> `Sort i
        | `Resize ws -> `Resize ws
        | `Reorder perm -> `Reorder perm
        | `Menu i -> `HeadMenu i
        | `None -> result
    in

    (* Vertical scrollbar *)
    let ext = if len = 0 then 1.0 else min 1.0 (float page /. float len) in
    let pos = if len = 0 then 0.0 else float tab.vscroll /. float len in
    let coeff = max 1.0 (float line) /. float (len - page) in
    let wheel = coeff *. snd (wheel_status ui r) in
    let pos' = scroll_bar ui vscroll_area `Vertical pos ext -. wheel in
    let result =
      if result <> `None || pos = pos' then result else
      (
        Table.set_vscroll tab
          (int_of_float (Float.round (pos' *. float len))) page;
        `Scroll
      )
    in

    (* Focus and mouse reflection *)
    if tab.focus && len > 0 then focus ui table_area;
    mouse_focus ui area geo.refl_r;

    (* Keys *)
    let result =
      if result <> `None || not tab.focus then result else
      (
        let d =
          if key_status' ui (`Arrow `Up) = `Pressed then -line else
          if key_status' ui (`Arrow `Down) = `Pressed then +line else
          if key_status' ui (`Page `Up) = `Pressed then -page else
          if key_status' ui (`Page `Down) = `Pressed then +page else
          if key_status' ui (`End `Up) = `Pressed then -len else
          if key_status' ui (`End `Down) = `Pressed then +len else
          0
        in
        if min len (abs d) > 0 then
        (
          (* Cursor movement *)
          let has_sel = tab.sel_range <> None in
          let default =
            0, if not shift then tab.vscroll else if d < 0 then len else -1 in
          let pos1, pos2 = Option.value tab.sel_range ~default in
          let i = if d < 0 then max 0 (pos2 + d) else min (len - 1) (pos2 + d) in

          if not (shift || command) then
          (
            (* Plain cursor movement: deselect all, reselect relative to range end *)
            if has_sel then
            (
              Table.deselect_all tab;
              Table.select tab i i;
              Table.adjust_vscroll tab i page;
              `Select
            )
            else
            (
              Table.set_vscroll tab i page;
              `Scroll
            )
          )
          else if shift then
          (
            (* Shift-cursor movement: adjust selection range *)
            if not has_sel then
            (
              (* No selection yet: range from end of playlist *)
              Table.select tab (len - 1) i;
            )
            else if Table.is_selected tab pos1 then
            (
              (* Range start was already selected: deselect old range, select new *)
              Table.deselect tab (max 0 pos2) i;
              Table.select tab pos1 i;
            )
            else
            (
              (* Range start was not selected: select old range, deselect new *)
              Table.select tab (max 0 pos2) i;
              Table.deselect tab pos1 i;
            );
            Table.adjust_vscroll tab i page;
            `Select
          )
          else if command && has_sel then
          (
            (* Cmd-cursor movement: move selection *)
            Table.adjust_vscroll tab i page;
            `Move d
          )
          else `None
        )
        else if command then
        (
          if key_status' ui (`Char 'A') = `Pressed then
          (
            (* Select-all key pressed: select all *)
            Table.select_all tab;
            `Select
          )
          else if key_status' ui (`Char 'N') = `Pressed then
          (
            (* Deselect-all key pressed: deselect all *)
            Table.deselect_all tab;
            `Select
          )
          else if key_status' ui (`Char 'I') = `Pressed then
          (
            (* Selection inversion key pressed: invert selection *)
            Table.select_invert tab;
            `Select
          )
          else `None
        )
        else
        (
          let ch = Key.char () in
          if ch >= Uchar.of_int 32 then
          (
            (* Plain character pressed: scroll to first entry *)
            let b = Bytes.make 8 '\000' in
            let s = Bytes.sub_string b 0 (Bytes.set_utf_8_uchar b 0 ch) in
            let rec find i =
              if i = Table.length tab then i else
              let _, _, txt = pp_cell i in  (* TODO: only pp relevant column *)
              if Unicode.compare_utf_8 s txt <= 0 then i else find (i + 1)
            in
            let i = find 0 in
            if i < len then
            (
              Table.adjust_vscroll tab ((i + line - 1) / line) page;
              `Scroll
            )
            else `None
          )
          else `None
        )
      )
    in
    result
  )


(* Pop-ups *)

let popup ui x y w h bw =
  assert ui.modal;
  let ww, wh = Window.size ui.win in
  let w' = w + 2 * bw in
  let h' = h + 2 * bw in
  let x' = max 0 (min x (ww - w')) in
  let y' = max 0 (min y (wh - h')) in
  background ui x' y' w' h';
  ui.mouse_owned <- true;
  (-1, x' + bw, y' + bw, w, h)


type menu_entry =
  [`Separator | `Entry of color * string * (modifier list * key) * bool]

let menu_separator = String.concat "" (List.init 80 (Fun.const "·"))

let menu ui x y bw gw ch ph items =
  assert ui.modal;

  let font = font ui ch in
  let keys =
    Iarray.map (function
      | `Separator -> ""
      | `Entry (_, _, (mods, key), _) ->
        String.concat "+" Api.Key.(List.map modifier_name mods @ [key_name key])
    ) items
  in
  let lw = 2 * gw +
    Iarray.fold_left (fun w -> function
      | `Separator -> w
      | `Entry (_, s, _, _) -> max w (Draw.text_width ui.win ch font s + 1)
    ) 0 items
  and rw =
    Iarray.fold_left (fun w s ->
      max w (Draw.text_width ui.win ch font s + 1)
    ) 0 keys
  in

  let mw = (gw + 1)/2 in  (* inner width padding *)
  let rh = ch + 2 * ph in
  let w = lw + gw + rw + 2 * mw in
  let h = rh * Iarray.length items in
  let area = popup ui x y w h bw in

  let _, my = Mouse.pos ui.win in
  let _, y', _, _ = dim ui area in
  let i = if mouse_inside ui area then (my - y')/rh else -1 in

  let cols : _ iarray = [|lw, `Left; rw, `Right|] in
  let c_sep = semilit_color (text_color ui) in
  let rows =
    Iarray.mapi (fun j entry ->
      (match entry with
      | `Separator -> c_sep, `Regular, [|`Text menu_separator; `Text ""|]
      | `Entry (c, txt, _, enabled) ->
        let c' = if enabled then c else semilit_color c in
        let inv = if enabled && i = j then `Inverted else `Regular in
        c', inv, [|`Text txt; `Text (Iarray.get keys j)|]
      : _ * _ * _ iarray)
    ) items
  in

  ui.modal <- false;
  let released = Mouse.is_released `Left || Mouse.is_pressed `Right in
  let enabled i =
    match Iarray.get items i with `Entry (_, _, _, b) -> b | _ -> false in
  match table ui area gw ch ph cols rows 0 with
  | Some i, _ when released && enabled i -> `Click i
  | None, _ when released -> `Close
  | _ ->
    let key_pressed = function
      | `Entry (_, _, modkey, _) -> key ui modkey true
      | `Separator -> false
    in
    match Iarray.find_index key_pressed items with
    | Some i -> `Click i
    | None -> ui.modal <- true; `None
