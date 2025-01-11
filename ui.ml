(* Immediate-style GUI abstractions *)

open Api


(* State *)

type drag = ..
type drag += No_drag

type image_load = [`Unloaded of string | `Loaded of image] ref

type t =
{
  win : window;
  mutable palette : int;
  mutable panes : rect array;
  mutable elems : rect list;        (* list of elements in last frame *)
  mutable drag_origin : point;      (* starting position of mouse drag *)
  mutable drag_extra : drag;        (* associated data for drag operation *)
  img_background : image_load;
  img_button : image_load;
  fonts : font option array;
}

let no_drag = (min_int, min_int)

let (//) = Filename.concat
let assets = Filename.dirname Sys.argv.(0) // "assets"

let make win =
  let icon = Image.load_raw (assets // "icon.png") in
  Window.set_icon win icon;
  { win;
    palette = 0;
    panes = Array.make 4 (0, 0, 0, 0);
    elems = [];
    drag_origin = no_drag;
    drag_extra = No_drag;
    img_background = ref (`Unloaded "bg.jpg");
    img_button = ref (`Unloaded "but.jpg");
    fonts = Array.make 64 None;
  }

let window ui = ui.win


(* Panes *)

type pane = int

let pane i ui r =
  let n = Array.length ui.panes in
  if i >= n then
    ui.panes <-
      Array.init (2*n) (fun i -> if i < n then ui.panes.(i) else (0, 0, 0, 0));
  ui.panes.(i) <- r


(* Areas *)

type area = pane * int * int * int * int

let dim ui (i, x, y, w, h) =
  let px, py, pw, ph = ui.panes.(i) in
  let x' = x + (if x >= 0 then 0 else pw) in
  let y' = y + (if y >= 0 then 0 else ph) in
  let w' = w + (if w >= 0 then 0 else pw - x') in
  let h' = h + (if h >= 0 then 0 else ph - y') in
  px + x', py + y', w', h'


(* Geometry helpers *)

let snap_dist = 12

let is_shift_down () =
  Key.is_down (`Shift `Left) || Key.is_down (`Shift `Right)

let snap min max v =
  if is_shift_down () then v else
  if abs (v - min) < snap_dist then 0 else
  if abs (v - max) < snap_dist then max else
  v

let clamp min max v =
  if v < min then min else
  if v > max then max else
  v


(* Colors *)

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
let semilit_alpha = 0x60
let unlit_color c = `Trans (c, unlit_alpha)
let semilit_color c = `Trans (c, semilit_alpha)
let text_color ui = palettes.(ui.palette).text
let warn_color ui = palettes.(ui.palette).warn
let error_color ui = palettes.(ui.palette).error
let hover_color ui = palettes.(ui.palette).hover

let modal c = function
  | true -> c
  | false -> unlit_color c

let fill ui b = modal (text_color ui) b

let border ui = function
  | `Hovered -> hover_color ui
(*
  | `Pressed -> `Orange
*)
  | _ -> `Black


(* Fonts *)

let font' ui h file min max fonts =
  match fonts.(h) with
  | Some f -> f
  | None ->
    let f = Api.Font.load ui.win file min max h in
    fonts.(h) <- Some f;
    f

let font ui h =
  font' ui h (assets // "tahoma.ttf") 0x0020 0x2800 ui.fonts


(* Images *)

let get_img ui rimg =
  match !rimg with
  | `Loaded img -> img
  | `Unloaded file ->
    let img = Image.load ui.win (assets // file) in
    rimg := `Loaded img;
    img


(* Window Background *)

type drag += Window of {target : point}

let background ui =
  let bg = get_img ui ui.img_background in
  let ww, wh = Window.size ui.win in
  let iw, ih = Image.size bg in
  for i = 0 to (ww + iw - 1)/iw - 1 do
    let x = if ww < iw then - (iw - ww)/2 else i*iw in
    for j = 0 to (wh + ih - 1)/ih - 1 do
      let y = if wh < ih then - (ih - wh)/2 else j*ih in
      Draw.image ui.win x y 1 bg
    done
  done;

  Draw.line ui.win 1 0 1 (wh - 2) (`Gray 0x40);
  Draw.line ui.win 0 0 ww 0 (`Gray 0x70);
  Draw.fill ui.win 1 (wh - 2) (ww - 1) 2 (`Gray 0x10);

  Mouse.set_cursor ui.win `Default;
  if Mouse.is_down `Left then
  (
    if ui.drag_origin = no_drag then ui.drag_origin <- Mouse.pos ui.win;
    if not (List.exists (inside ui.drag_origin) ui.elems) then
    (
      let off =
        match ui.drag_extra with
        | No_drag -> 0, 0
        | Window {target} -> sub (Window.pos ui.win) target
        | _ -> assert false
      in
      let (wx, wy) as w = sub (add (Window.pos ui.win) (Mouse.screen_delta ui.win)) off in
      let sx, sy = Window.min_pos ui.win in
      let sw, sh = sub (Window.max_size ui.win) (Window.size ui.win) in
      Window.set_pos ui.win (snap sx (sx + sw) wx) (snap sy (sy + sh) wy);
      ui.drag_extra <- Window {target = w};
    )
  )
  else
  (
    ui.drag_origin <- no_drag;
    ui.drag_extra <- No_drag;
  );
  ui.elems <- []


(* Input elements *)

let no_modkey = ([], `None)

let key_status _ui (modifiers, key) =
  if (Key.is_pressed key || Key.is_repeated key)
  && Api.Key.are_modifiers_down modifiers then
    `Pressed
  else if Key.is_released key && Api.Key.are_modifiers_down modifiers then
    `Released
  else
    `Untouched

let mouse_status ui r side =
  if Mouse.is_down side && inside ui.drag_origin r then
    `Pressed
  else if not (inside (Mouse.pos ui.win) r) then
    `Untouched
  else if Mouse.is_released side then
    `Released
  else if not (Mouse.is_down side) then
    `Hovered
  else
    `Untouched


type drag += Drag of {pos : point}

let drag_status ui r (stepx, stepy) =
  if not (inside ui.drag_origin r) then
    if inside (Mouse.pos ui.win) r && Mouse.(is_released `Left && not (is_drag `Left)) then
      `Click
    else
      `None
  else
  let (mx, my) as m = Mouse.pos ui.win in
  match ui.drag_extra with
  | No_drag ->
    ui.drag_extra <- Drag {pos = m};
    if stepx * stepy = 0 then `Drag (0, 0) else `None
  | Drag {pos; _} ->
    let dx, dy = sub m pos in
    let dx' = if stepx = 0 then dx else dx / stepx in
    let dy' = if stepy = 0 then dy else dy / stepy in
    ui.drag_extra <-
      Drag {pos = mx - dx mod max 1 stepx, my - dy mod max 1 stepy};
    if dx' = 0 && dy' = 0 && stepx * stepy <> 0 then `None else `Drag (dx', dy')
  | _ -> assert false

let wheel_status ui r =
  if inside (Mouse.pos ui.win) r then snd (Mouse.wheel ui.win) else 0.0

let key modkey ui = (key_status ui modkey = `Pressed)
let mouse r side ui = (mouse_status ui (dim ui r) side = `Released)
let wheel r ui = wheel_status ui (dim ui r)
let drag r ui eps = drag_status ui (dim ui r) eps


(* Auxiliary UI elements *)

let label' c r align s ui =
  let x, y, w, h = dim ui r in
  let font = font ui h in
  let tw = Api.Draw.text_width ui.win h font s in
  let dx =
    match align with
    | `Left -> 0
    | `Center -> (w - tw + 1) / 2
    | `Right -> w - tw
  in Api.Draw.text ui.win (x + dx) y h c font s

let label = label' `White

let indicator r ui on =
  let x, y, w, h = dim ui r in
  Draw.fill_circ ui.win x y w h (if on then `Green else unlit_color `Green);
  Draw.fill_circ ui.win (x + w/4) (y + h/4) (min 2 (w/3)) (min 2 (h/3))
    (`Trans (`White, if on then 0xe0 else 0x30));
  Draw.circ ui.win x y w h (border ui `Untouched)

let lcd' ui r' c elem =
  let open Api.Draw in
  let x, y, w, h = r' in
  let m = h / 2 in
  match elem with
  | `N ->
    line ui.win (x + 1) (y + 0) (x + w - 3) (y + 0) c;
    line ui.win (x + 2) (y + 1) (x + w - 4) (y + 1) c;
  | `S ->
    line ui.win (x + 1) (y + h - 1) (x + w - 3) (y + h - 1) c;
    line ui.win (x + 2) (y + h - 2) (x + w - 4) (y + h - 2) c;
  | `C ->
    line ui.win (x + 1) (y + m - 1) (x + w - 3) (y + m - 1) c;
    line ui.win (x + 2) (y + m) (x + w - 4) (y + m) c;
  | `NW ->
    line ui.win (x + 0) (y + 1) (x + 0) (y + m - 2) c;
    line ui.win (x + 1) (y + 2) (x + 1) (y + m - 3) c;
  | `NE ->
    line ui.win (x + w - 1) (y + 1) (x + w - 1) (y + m - 2) c;
    line ui.win (x + w - 2) (y + 2) (x + w - 2) (y + m - 3) c;
  | `SW ->
    line ui.win (x + 0) (y + m + 1) (x + 0) (y + h - 2) c;
    line ui.win (x + 1) (y + m + 2) (x + 1) (y + h - 3) c;
  | `SE ->
    line ui.win (x + w - 1) (y + m + 1) (x + w - 1) (y + h - 2) c;
    line ui.win (x + w - 2) (y + m + 2) (x + w - 2) (y + h - 3) c
  | `Dots ->
    rect ui.win x (y + h / 4) 2 2 c;
    rect ui.win x (y + 3 * h / 4) 2 2 c

let lcd r ui d =
  let c = text_color ui in
  if d = '-' || d = '+' then
    lcd' ui (dim ui r) c `C
  else if d = ':' then
    lcd' ui (dim ui r) c `Dots
  else
    List.iter (lcd' ui (dim ui r) c) [`N; `S; `C; `NW; `SW; `NE; `SE];
  List.iter (lcd' ui (dim ui r) (`Trans (`Black, 0x100 - unlit_alpha)))
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
(*
    | '0' -> [`N; `S; `NW; `SW; `NE; `SE]
    | '1' -> [`NE; `SE]
    | '2' -> [`N; `S; `C; `SW; `NE]
    | '3' -> [`N; `S; `C; `NE; `SE]
    | '4' -> [`C; `NW; `NE; `SE]
    | '5' -> [`N; `S; `C; `NW; `SE]
    | '6' -> [`N; `S; `C; `NW; `SW; `SE]
    | '7' -> [`N; `NE; `SE]
    | '8' -> [`N; `S; `C; `NW; `SW; `NE; `SE]
    | '9' -> [`N; `S; `C; `NW; `NE; `SE]
*)
    | _ -> []
    )


(* Passive UI Elements *)

let element r modkey ui =
  let r' = dim ui r in
  ui.elems <- r' :: ui.elems;
  r',
  match mouse_status ui r' `Left, key_status ui modkey with
  | `Released, _ | _, `Released -> `Released
  | `Pressed, _ | _, `Pressed -> `Pressed
  | `Hovered, _ | _, `Hovered -> `Hovered
  | _, _ -> `Untouched


let box r c ui =
  let (x, y, w, h), _ = element r no_modkey ui in
  Draw.fill ui.win x y w h c

let color_text r align ui c inv active s =
  let (x, y, w, h), _status = element r no_modkey ui in
  let fg = modal c active in
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

let text r align ui = color_text r align ui (text_color ui)

let ticker r ui s =
  let (x, y, w, h), _status = element r no_modkey ui in
  Draw.fill ui.win x y w h `Black;
  let tw = Draw.text_width ui.win h (font ui h) s in
  Draw.clip ui.win x y w h;
  let dx = if tw <= w then (w - tw)/2 else w - Draw.frame ui.win mod (w + tw) in
  Draw.text ui.win (x + dx) y h (fill ui true) (font ui h) s;
  Draw.unclip ui.win


(* Buttons *)

let button r ?(protrude=true) modkey ui active =
  let (x, y, w, h), status = element r modkey ui in
  let img = get_img ui ui.img_button in
  let sx, sy = if status = `Pressed then 800, 400 else 0, 200 in
  Draw.clip ui.win x y w h;
  Draw.image ui.win (x - sx) (y - sy) 1 img;
  Draw.unclip ui.win;
  if status <> `Pressed then
  (
    Draw.line ui.win (x + 2) (y + 1) (x + 2) (y + h - 2) (`Gray 0x50);
    if protrude then Draw.line ui.win (x + 1) (y + 1) (x + w - 2) (y + 1) (`Gray 0x50);
  );
  Draw.rect ui.win x y w h (border ui status);
  match active with
  | None -> false
  | Some active -> if status = `Released then not active else active


let labeled_button r ?(protrude=true) hsym txt modkey ui active =
  let (x, y, w, h), status = element r modkey ui in
  let result = button r ~protrude modkey ui active in
  let c =
    match active with
    | None -> `Gray 0x60
    | Some false -> `Gray 0xc0
    | Some true -> `RGB 0x40ff40
  in
  let xsym = (x + (w - hsym)/2) in
  let ysym = (y + (h - hsym)/2) + Bool.to_int (status = `Pressed) in
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
    let (i, x', y', w', _) = r in
    label' c (i, x' + 1, y' + (h - hsym)/2, w' - 1, hsym) `Center s ui
  );
  result


(* Bars *)

let progress_bar r ui v =
  let (x, y, w, h), status = element r no_modkey ui in
  Draw.fill ui.win x y w h (fill ui false);
  Draw.fill ui.win x y (int_of_float (v *. float w)) h (fill ui true);
  for i = 0 to w / 2 - 1 do
    Draw.fill ui.win (x + 2*i + 1) y 1 h `Black
  done;
  Draw.rect ui.win x y w h (border ui status);
  if status <> `Pressed then v else
  let mx, _ = Mouse.pos ui.win in
  clamp 0.0 1.0 (float (mx - x) /. float w)


let volume_bar r ui v =
  let (x, y, w, h), status = element r no_modkey ui in
  let h' = int_of_float ((1.0 -. v) *. float h) in
  Draw.fill ui.win (x + w - 2) y 2 h (fill ui true);
  Draw.tri ui.win x y (w - 2) h (fill ui true) `NE;
  Draw.fill ui.win x y w h' (`Trans (`Black, 0x100 - unlit_alpha));
  for j = 0 to h / 2 - 1 do
    Draw.fill ui.win x (y + 2*j + 1) w 1 `Black
  done;
  if status <> `Pressed then v else
  let _, my = Mouse.pos ui.win in
  clamp 0.0 1.0 (float (y + h - my) /. float h)
(*
  let (x, y, w, h), status = element r no_modkey ui in
  let h' = int_of_float (v *. float (h - 2)) in
  Draw.fill ui.win x y w h (fill ui false);
  Draw.fill ui.win x (y + h - h' - 1) w h' (fill true);
  Draw.rect ui.win x y w h (border ui status);
  if status <> `Pressed then v else
  let _, my = Mouse.pos ui.win in
  clamp 0.0 1.0 (float (y + h - my) /. float h)
*)


type drag += Scroll_bar_page of {last_repeat : time}
type drag += Scroll_bar_drag of {value : float; mouse : int}

let scroll_bar r ui v len =
  assert (v +. len <= 1.0);
  let (x, y, w, h), status = element r no_modkey ui in
  Draw.fill ui.win x y w h (fill ui false);
  let y' = y + int_of_float (v *. float (h - 2)) + 1 in
  let h' = int_of_float (Float.ceil (len *. float (h - 2))) in
  if len < 1.0 then Draw.fill ui.win x y' w h' (fill ui true);
  for j = 0 to h / 2 - 1 do
    Draw.fill ui.win x (y + 2*j) w 1 `Black
  done;
  Draw.rect ui.win x y w h (border ui status);
  if status <> `Pressed then v else
  let _, my = Mouse.pos ui.win in
  let v0, my0, t, dragging =
    match ui.drag_extra with
    | No_drag -> v, my, 0.0, false
    | Scroll_bar_page {last_repeat} -> v, my, last_repeat, false
    | Scroll_bar_drag {value; mouse} -> value, mouse, 0.0, true
    | _ -> assert false
  in
  let now = Unix.gettimeofday () in
  let v' =
    if dragging || y' <= my && my < y' + h' then
    (
      ui.drag_extra <- Scroll_bar_drag {value = v0; mouse = my0};
      v0 +. float (my - my0) /. float (h - 2)
    )
    else if now -. t > 0.5 then
    (
      ui.drag_extra <- Scroll_bar_page {last_repeat = now};
      if my < y' then v -. len else
      if my >= y' + h' then v +. len else
      v
    )
    else v
  in clamp 0.0 (1.0 -. len) v'


(* Tables *)

type align = [`Left | `Center | `Right]
type inversion = [`Regular | `Inverted]
type column = int * align
type row = color * inversion * string array

let table r gw ch ui cols rows =
  let (x, y, w, h), status = element r no_modkey ui in
  let font = font ui ch in
  Draw.fill ui.win x y w h `Black;
  Array.iteri (fun j (fg, inv, texts) ->
    let cy = y + j * ch in
    let bg = if j mod 2 = 0 then `Black else `Gray 0x10 in
    let fg, bg = if inv = `Inverted then bg, fg else fg, bg in
    if bg <> `Black then Draw.fill ui.win x cy w ch bg;
    let mw = (gw + 1)/2 in
    let cx = ref (x + mw) in
    Array.iteri (fun i (cw, align) ->
      let tw = Draw.text_width ui.win ch font texts.(i) in
      let dx =
        match align with
        | `Left -> 0
        | `Center -> (cw - tw) / 2
        | `Right -> cw - tw - mw  (* subtract another mw, since tw can be off *)
      in
      let cw' = min cw (x + w - mw - !cx) in
      if tw >= cw' then Draw.clip ui.win !cx y cw' h;
      Draw.text ui.win (!cx + max 0 dx) cy ch fg font texts.(i);
      if tw >= cw then
      (
        let rw = min cw 16 in
        Draw.gradient ui.win (!cx + cw - rw) cy rw ch
          (`Trans (bg, 0)) `Horizontal bg;
      );
      if tw >= cw' then Draw.unclip ui.win;
      cx := !cx + cw + gw;
    ) cols
  ) rows;
  if status = `Pressed || status = `Released then
    let _, my = Mouse.pos ui.win in
    Some ((my - y) / ch)
  else
    None


(* Dividers *)

type drag += Divide of {overshoot : size}

let divider r orient ui minv maxv =
  let (x, y, w, h), status = element r no_modkey ui in
  let proj = match orient with `Horizontal -> fst | `Vertical -> snd in
  let inj v = match orient with `Horizontal -> v, y | `Vertical -> x, v in
  let cursor = match orient with `Horizontal -> `E_W | `Vertical -> `N_S in
  if status <> `Untouched then Api.Mouse.set_cursor ui.win (`Resize cursor);
  Draw.rect ui.win x y w h (border ui status);
  if status <> `Pressed then 0 else
  let over =
    match ui.drag_extra with
    | No_drag -> 0, 0
    | Divide {overshoot} -> overshoot
    | _ -> assert false
  in
  let vx, vy = add (add (x, y) (Mouse.delta ui.win)) over in
  let i, _, _, _, _ = r in
  let px, _, pw, ph = ui.panes.(i) in
  let minx, miny = inj minv in
  let maxx, maxy = inj maxv in
  let minx = px + minx in
  let maxx = px + if maxx < 0 then pw else maxx in
  let maxy = if maxy < 0 then ph else maxy in
  let vx', vy' = clamp minx maxx vx, clamp miny maxy vy in
  ui.drag_extra <- Divide {overshoot = vx - vx', vy - vy'};
  (* HACK: Adjust owned drag_origin for size-relative position *)
  (* This assumes that the caller actually moves the divider! *)
  let dx = vx' - x in
  let dy = vy' - y in
  ui.drag_origin <- add ui.drag_origin (dx, dy);
  assert (inside ui.drag_origin (x + dx, y + dy, w, h));
  ui.elems <- [(x + dx, y + dy, w, h)];
  proj (dx, dy)


(* Resizers *)

type drag += Resize of {overshoot : size}

(* Assumes that the caller actually resizes the window according to response.
 * Dragging will misbehave otherwise.
 * Also assumes that positive x/y imply resizing to left/upward,
 * while negative x/y imply resizing to right/downward.
 * Negative max values are treated as screen size.
 *)
let resizer r cursor ui (minw, minh) (maxw, maxh) =
  let (x, y, w, h), status = element r no_modkey ui in
  if status <> `Untouched then Api.Mouse.set_cursor ui.win (`Resize cursor);
  Draw.fill ui.win x y w h (fill ui false);
  Draw.rect ui.win x y w h (border ui status);
  if status <> `Pressed then 0, 0 else
  let sz = Window.size ui.win in
  let _, x0, y0, _, _ = r in
  let sign = (if x0 >= 0 then -1 else +1), (if y0 >= 0 then -1 else +1) in
  let over =
    match ui.drag_extra with
    | No_drag -> 0, 0
    | Resize {overshoot} -> overshoot
    | _ -> assert false
  in
  let wx, wy = Window.pos ui.win in
  let ww, wh = add (add sz (mul sign (Mouse.screen_delta ui.win))) over in
  let sx, sy = Window.min_pos ui.win in
  let sw, sh = Window.max_size ui.win in
  let rx, ry = wx - sx, wy - sy in
  let maxw = if maxw >= 0 then maxw else if x0 < 0 then sw - rx else fst sz + rx in
  let maxh = if maxh >= 0 then maxh else if y0 < 0 then sh - ry else snd sz + ry in
  let ww', wh' = clamp minw maxw ww, clamp minh maxh wh in
  ui.drag_extra <- Resize {overshoot = ww - ww', wh - wh'};
  (* HACK: Adjust owned drag_origin for size-relative position *)
  (* This assumes that the caller actually resizes the window! *)
  let dx = if x0 >= 0 then 0 else ww' - fst sz in
  let dy = if y0 >= 0 then 0 else wh' - snd sz in
  ui.drag_origin <- add ui.drag_origin (dx, dy);
  assert (inside ui.drag_origin (x + dx, y + dy, w, h));
  ui.elems <- [(x + dx, y + dy, w, h)];
  sub (ww', wh') sz
