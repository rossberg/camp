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
  mutable mouse_owned : bool;       (* whether mouse was owned by an element *)
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
    mouse_owned = false;
    drag_origin = no_drag;
    drag_extra = No_drag;
    img_background = ref (`Unloaded "bg.jpg");
    img_button = ref (`Unloaded "but.jpg");
    fonts = Array.make 64 None;
  }

let window ui = ui.win


(* Panes *)

type pane = int

let pane ui i r =
  let x, y, w, h = r in
  let ww, wh = Api.Window.size ui.win in
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
      Array.init (2*n) (fun i -> if i < n then ui.panes.(i) else (0, 0, 0, 0));
  ui.panes.(i) <- (x, y, w, h)


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
let unlit_color c = Api.Color.darken unlit_alpha c (*`Trans (c, unlit_alpha)*)
let semilit_color c = Api.Color.darken semilit_alpha c (*`Trans (c, semilit_alpha)*)
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

type drag += Move of {target : point}
type drag += Resize of {overshoot : size}

let start ui =
  Api.Draw.start ui.win (`Trans (`Black, 0x40));

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

  Api.Mouse.set_cursor ui.win `Default;
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
  if ui.mouse_owned then ui.mouse_owned <- false else
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
    Api.Mouse.set_cursor ui.win cursor;

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

      | _ -> assert false
    )
  );

  Api.Draw.finish ui.win


(* Input elements *)

let no_modkey = ([], `None)

let key_status' _ui key =
  (* Mouse click or drag masks keys *)
  if Mouse.is_down `Left then
    `Untouched
  else if Key.is_pressed key || Key.is_repeated key then
    `Pressed
  else if Key.is_released key then
    `Released
  else
    `Untouched

let key_status ui (modifiers, key) =
  if not (Api.Key.are_modifiers_down modifiers) then
    `Untouched
  else
    key_status' ui key

let mouse_status ui r = function
  | `Left ->
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
    let side = `Right in
    if not (inside (Mouse.pos ui.win) r) then
      `Untouched
    else if Mouse.is_down side then
      `Pressed
    else if Mouse.is_released side then
      `Released
    else
      `Hovered


type drag += Drag of {pos : point}

let drag_status ui r (stepx, stepy) =
  if not (inside ui.drag_origin r) then
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
    ui.drag_extra <- Drag {pos = m};
    if stepx * stepy = 0 then `Drag (0, 0) else `None
  | Drag {pos; _} ->
    let dx, dy = sub m pos in
    let dx' = if stepx = 0 then dx else dx / stepx in
    let dy' = if stepy = 0 then dy else dy / stepy in
    ui.drag_extra <-
      Drag {pos = mx - dx mod max 1 stepx, my - dy mod max 1 stepy};
    `Drag (dx', dy')
  | _ -> assert false

let wheel_status ui r =
  if inside (Mouse.pos ui.win) r then snd (Mouse.wheel ui.win) else 0.0

let drop_status ui r =
  if Api.inside (Mouse.pos ui.win) r then Api.File.dropped ui.win else []

let key ui modkey = (key_status ui modkey = `Pressed)
let mouse ui r side = (mouse_status ui (dim ui r) side = `Released)
let wheel ui r = wheel_status ui (dim ui r)
let drag ui r eps = drag_status ui (dim ui r) eps
let drop ui r = drop_status ui (dim ui r)


(* Auxiliary UI elements *)

let colored_label ui c r align s =
  let x, y, w, h = dim ui r in
  let font = font ui h in
  let tw = Api.Draw.text_width ui.win h font s in
  let dx =
    match align with
    | `Left -> 0
    | `Center -> (w - tw + 1) / 2
    | `Right -> w - tw
  in Api.Draw.text ui.win (x + dx) y h c font s

let label ui r align s =
  colored_label ui `White r align s

let indicator ui r on =
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

let lcd ui r d =
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

let element ui r modkey =
  let r' = dim ui r in
  r',
  match mouse_status ui r' `Left, key_status ui modkey with
  | `Released, _ | _, `Released -> `Released
  | `Pressed, _ | _, `Pressed -> `Pressed
  | `Hovered, _ | _, `Hovered -> `Hovered
  | _, _ -> `Untouched


let box ui r c =
  let (x, y, w, h), _ = element ui r no_modkey in
  Draw.fill ui.win x y w h c

let color_text ui r align c inv active s =
  let (x, y, w, h), _status = element ui r no_modkey in
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

let text ui r align =
  color_text ui r align (text_color ui)

let ticker ui r s =
  let (x, y, w, h), _status = element ui r no_modkey in
  Draw.fill ui.win x y w h `Black;
  let tw = Draw.text_width ui.win h (font ui h) s in
  Draw.clip ui.win x y w h;
  let dx = if tw <= w then (w - tw)/2 else w - Draw.frame ui.win mod (w + tw) in
  Draw.text ui.win (x + dx) y h (fill ui true) (font ui h) s;
  Draw.unclip ui.win


(* Buttons *)

let button ui r ?(protrude=true) modkey active =
  let (x, y, w, h), status = element ui r modkey in
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


let labeled_button ui r ?(protrude=true) hsym txt modkey active =
  let (x, y, w, h), status = element ui r modkey in
  let result = button ui r ~protrude modkey active in
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
    colored_label ui c (i, x' + 1, y' + (h - hsym)/2, w' - 1, hsym) `Center s
  );
  result


(* Bars *)

let progress_bar ui r v =
  let (x, y, w, h), status = element ui r no_modkey in
  Draw.fill ui.win x y w h (fill ui false);
  Draw.fill ui.win x y (int_of_float (v *. float w)) h (fill ui true);
  for i = 0 to w / 2 - 1 do
    Draw.fill ui.win (x + 2*i + 1) y 1 h `Black
  done;
  Draw.rect ui.win x y w h (border ui status);
  if status <> `Pressed then v else
  let mx, _ = Mouse.pos ui.win in
  clamp 0.0 1.0 (float (mx - x) /. float w)


let volume_bar ui r v =
  let (x, y, w, h), status = element ui r no_modkey in
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
  let (x, y, w, h), status = element ui r no_modkey in
  let h' = int_of_float (v *. float (h - 2)) in
  Draw.fill ui.win x y w h (fill ui false);
  Draw.fill ui.win x (y + h - h' - 1) w h' (fill true);
  Draw.rect ui.win x y w h (border ui status);
  if status <> `Pressed then v else
  let _, my = Mouse.pos ui.win in
  clamp 0.0 1.0 (float (y + h - my) /. float h)
*)


type drag += Scroll_bar_page of {last_repeat : time}
type drag += Scroll_bar_drag of {value : float; mx : int; my : int}

let scroll_bar ui r orient v len =
  assert (v +. len <= 1.0);
  let (x, y, w, h), status = element ui r no_modkey in
  Draw.fill ui.win x y w h (fill ui false);
  let x', y', w', h' as r' =
    match orient with
    | `Vertical ->
      x, y + int_of_float (v *. float (h - 2)) + 1,
      w, int_of_float (Float.ceil (len *. float (h - 2)))
    | `Horizontal ->
      x + int_of_float (v *. float (w - 2)) + 1, y,
      int_of_float (Float.ceil (len *. float (w - 2))), h
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
  let v0, mx0, my0, t, dragging =
    match ui.drag_extra with
    | No_drag -> v, mx, my, 0.0, false
    | Scroll_bar_page {last_repeat} -> v, mx, my, last_repeat, false
    | Scroll_bar_drag {value; mx; my} -> value, mx, my, 0.0, true
    | _ -> assert false
  in
  let now = Unix.gettimeofday () in
  let v' =
    if dragging || inside m r' then
    (
      ui.drag_extra <- Scroll_bar_drag {value = v0; mx = mx0; my = my0};
      match orient with
      | `Vertical -> v0 +. float (my - my0) /. float (h - 2)
      | `Horizontal -> v0 +. float (mx - mx0) /. float (w - 2)
    )
    else if now -. t > 0.2 (* TODO: use config *) then
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

let divider ui r orient v minv maxv =
  let (x, y, w, h), status = element ui r no_modkey in
  let proj = match orient with `Horizontal -> fst | `Vertical -> snd in
  let inj v = match orient with `Horizontal -> v, y | `Vertical -> x, v in
  let cursor = match orient with `Horizontal -> `E_W | `Vertical -> `N_S in
  if status <> `Untouched then Api.Mouse.set_cursor ui.win (`Resize cursor);
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
  let i, _, _, _, _ = r in
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


(* Tables *)

type align = [`Left | `Center | `Right]
type inversion = [`Regular | `Inverted]
type column = int * align
type row = color * inversion * string array
type sorting = int * [`Asc | `Desc]

let table ui r gw ch cols rows hscroll =
  let (x, y, w, h), status = element ui r no_modkey in
  Draw.fill ui.win x y w h `Black;
  let mw = (gw + 1)/2 in
  let flex = max 0
    (w - Array.fold_left (fun w (cw, _) -> w + cw + gw) (2 * mw - gw + 1) cols) in
  let font = font ui ch in
  Array.iteri (fun j (fg, inv, texts) ->
    let cy = y + j * ch in
    let bg = if j mod 2 = 0 then `Black else `Gray 0x10 in
    let fg, bg = if inv = `Inverted then bg, fg else fg, bg in
    if bg <> `Black then Draw.fill ui.win x cy w ch bg;
    let cx = ref (x + mw - hscroll) in
    Array.iteri (fun i (cw, align) ->
      let tw = Draw.text_width ui.win ch font texts.(i) in
      let cw = if cw < 0 then flex / (- cw) else cw in
      let dx =
        match align with
        | `Left -> 0
        | `Center -> (cw - tw) / 2
        | `Right ->
          (* Add extra padding if back to back with a left-aligned column *)
          cw - tw -
            (if i + 1 < Array.length cols && snd cols.(i + 1) = `Left then 4 else 0)
      in
      let cw' = min cw (x + w - mw - !cx) in
      let left = max !cx (x + mw) in
      Draw.clip ui.win left y (cw' - max 0 (left - !cx)) h;
      Draw.text ui.win (!cx + max 0 dx) cy ch fg font texts.(i);
      if tw >= cw then
      (
        let rw = min cw 16 in
        Draw.gradient ui.win (!cx + cw - rw) cy rw ch
          (`Trans (bg, 0)) `Horizontal bg;
      );
      Draw.unclip ui.win;
      cx := !cx + cw + gw;
    ) cols
  ) rows;
  if status = `Pressed || status = `Released then
    let _, my = Mouse.pos ui.win in
    Some ((my - y) / ch)
  else
    None


(* Table Headers *)

let symbol_asc = "▲" (* "▴" *)
let symbol_desc = "▼" (* "▾" *)

let header ui area gw cols titles sort_opt hscroll =
  let (x, y, w, h) as r, status = element ui area no_modkey in
  ignore (table ui area gw h cols [|text_color ui, `Inverted, titles|] hscroll);

  let mw = (gw + 1)/2 in  (* match mw in table *)
  Draw.clip ui.win x y w h;
  ignore (
    Array.fold_left (fun cx (cw, _) ->
      Api.Draw.fill ui.win (cx + cw + gw/2 - hscroll) y 1 h `Black;
      cx + cw + gw;
    ) (x + mw) cols - x - mw
  );
  Draw.unclip ui.win;

  Option.iter (fun (i, order) ->
    let rec find_header j cx =
      let cw = fst cols.(j) in
      if j < i then find_header (j + 1) (cx + cw + gw) else
      cx, cw
    in
    let cx, cw = find_header 0 x in
    let sym = match order with `Asc -> symbol_asc | `Desc -> symbol_desc in
    if cw > 10 then
    Api.Draw.text ui.win (cx + cw - 8) y h `Black (font ui h) sym;
  ) sort_opt;

  let gutter_tolerance = 5 in
  let rec find_gutter' mx i cx =
    if i = Array.length cols then None else
    let cx' = cx + fst cols.(i) in
    if abs (cx' + gw/2 - mx) < gutter_tolerance then Some i else
    if cx' + gw/2 < mx then find_gutter' mx (i + 1) (cx' + gw) else
    None
  in
  let find_gutter mx = find_gutter' mx 0 (x + mw - hscroll) in

  let rec find_heading' mx i cx =
    if i = Array.length cols then `None else
    let cx' = cx + fst cols.(i) in
    if mx >= cx && mx < cx' then `Click i else
    if mx >= cx' then find_heading' mx (i + 1) (cx' + gw) else
    `None
  in
  let find_heading mx = find_heading' mx 0 (x + mw - hscroll) in

  let mx, _ = Mouse.pos ui.win in
  if status <> `Untouched && find_gutter mx <> None then
    Mouse.set_cursor ui.win (`Resize `E_W);

  match drag_status ui r (1, max_int) with
  | `None | `Drop -> `None
  | `Click -> find_heading mx
  | `Drag (dx, _) ->
    match find_gutter (mx - dx) with
    | None -> `None
    | Some i ->
      let add_fst d (x, y) = (max 0 (x + d), y) in
      cols.(i) <- add_fst dx cols.(i);
      if i + 1 < Array.length cols && Key.is_modifier_down `Shift then
        cols.(i + 1) <- add_fst (-dx) cols.(i + 1);
      `Arrange


(* Rich Tables *)

let rich_table ui area gw ch sw sh cols headings_opt (tab : _ Table.t) pp_row =
  let (p, ax, ay, aw, ah) = area in
  let ty = if headings_opt = None then ay else ay + ch + 2 in
  let th = ah - (if ah < 0 then 0 else ty - ay) - (if sh = 0 then 0 else sh + 1) in
  let header_area = (p, ax, ay, aw - sw - 1, ch) in
  let table_area = (p, ax, ty, aw - sw - 1, th) in
  let vscroll_area = (p, aw - sw - 1, ay, sw, ah) in
  let hscroll_area = (p, ax, (if ah < 0 then ah - sh else ty + th + 1), aw - sw - 1, sh) in
  let (_, _, w, h) as r = dim ui table_area in

  let shift = Key.are_modifiers_down [`Shift] in
  let command = Key.are_modifiers_down [`Command] in

  let len = Array.length tab.entries in
  let page = max 1 (int_of_float (Float.floor (float h /. float ch))) in
  (* Correct scrolling position for possible resize *)
  tab.vscroll <- clamp 0 (max 0 (len - page)) tab.vscroll;

  (* Body *)
  let rows =
    Array.init (min page len) (fun i ->
      let i = tab.vscroll + i in
      let c, cols = pp_row i in
      let inv = if Table.is_selected tab i then `Inverted else `Regular in
      c, inv, cols
    )
  in
  let result =
    match table ui table_area gw ch cols rows tab.hscroll with
    | None -> `None
    | Some i ->
      let i = tab.vscroll + i in
      let limit = min len (tab.vscroll + page) in
      if not (shift || command) then
      (
        match drag_status ui r (max_int, ch) with
        | `None ->
          (* Click *)
          if i >= limit then
          (
            (* Click on empty space *)
            Table.deselect_all tab;
            `Click None
          )
          else
          (
            (* Click on entry *)
            if not (Table.is_selected tab i) then
              Table.deselect_all tab;
            if not (Api.Mouse.is_doubleclick `Left) then
              Table.select tab i i;
            `Click (Some i)
          )

        | `Click ->
          (* Click-release: deselect all except for clicked entry *)
          Table.deselect_all tab;
          if i >= limit then
            `Click None
          else
          (
            Table.select tab i i;
            `Click (Some i)
          )

        | `Drag (_, dy) -> `Move dy

        | `Drop -> `Drop
      )
      else if command && Mouse.is_pressed `Left then
      (
        (* Cmd-click on entry: toggle selection of clicked entry *)
        if i >= limit then
          `Click None
        else
        (
          if Table.is_selected tab i then
            Table.deselect tab i i
          else
            Table.select tab i i;
          `Click (Some i);
        )
      )
      else if shift && Mouse.is_down `Left then
      (
        (* Shift-click/drag on playlist: adjust selection range *)
        let default = if i < len then (i, i) else (0, 0) in
        let pos1, pos2 = Option.value tab.sel_range ~default in
        let i' = max 0 (min i (len - 1)) in
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
        if Mouse.is_pressed `Left then
          `Click (if i < len then Some i else None)
        else
          `Select
      )
      else `None
  in
  if result <> `None then
    tab.focus <- true;

  (* Header *)
  let result =
    match headings_opt with
    | None -> result
    | Some (headings, sort) ->
      match header ui header_area gw cols headings (Some sort) tab.hscroll with
      | `Click i -> `Sort i
      | `Arrange -> `Arrange
      | `None -> result
  in

  (* Vertical scrollbar *)
  let h' = page * ch in
  let ext = if len = 0 then 1.0 else min 1.0 (float h' /. float (len * ch)) in
  let pos = if len = 0 then 0.0 else float tab.vscroll /. float len in
  let coeff = max 1.0 (float page /. 4.0) /. float (len - page) in
  let wheel = if not shift then coeff *. wheel_status ui r else 0.0 in
  let pos' = scroll_bar ui vscroll_area `Vertical pos ext -. wheel in
  let result =
    if result <> `None || pos = pos' then result else
    (
      tab.vscroll <- clamp 0 (max 0 (len - page))
        (int_of_float (Float.round (pos' *. float len)));
      `Scroll
    )
  in

  (* Horizontal scrollbar *)
  let result =
    if sh = 0 then result else
    let vw = Array.fold_left (fun w (cw, _) -> w + cw + gw) 0 cols in
    let vw' = max vw (tab.hscroll + w) in
    let ext = if vw' = 0 then 1.0 else min 1.0 (float w /. float vw') in
    let pos = if vw' = 0 then 0.0 else float tab.hscroll /. float vw' in
    let wheel = if shift then wheel_status ui r else 0.0 in
    let pos' = scroll_bar ui hscroll_area `Horizontal pos ext -. 0.05 *. wheel in
    if result <> `None || pos = pos' then result else
    (
      tab.hscroll <-
        clamp 0 (max 0 (vw' - w)) (int_of_float (Float.round (pos' *. float vw')));
      `Scroll
    )
  in

  (* Keys *)
  if result <> `None || not tab.focus then
    result
  else
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
      let default = 0, if d < 0 then len else -1 in
      let pos1, pos2 = Option.value tab.sel_range ~default in
      let i = if d < 0 then max 0 (pos2 + d) else min (len - 1) (pos2 + d) in

      if not (shift || command) then
      (
        (* Plain cursor movement: deselect all, reselect relative to range end *)
        Table.deselect_all tab;
        Table.select tab i i;
        Table.adjust_scroll tab (Some i) page;
        `Select
      )
      else if shift then
      (
        (* Shift-cursor movement: adjust selection range *)
        if tab.sel_range = None then
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
        Table.adjust_scroll tab (Some i) page;
        `Select
      )
      else if command then
      (
        (* Cmd-cursor movement: move selection *)
        Table.adjust_scroll tab (Some i) page;
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
    else `None
  )
