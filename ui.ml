(* Direct-style GUI abstractions *)

open Api


(* Keyboard *)

let is_shift_down () =
  Key.is_down (`Shift `Left) || Key.is_down (`Shift `Right)


(* Helpers *)

let dim win (x, y, w, h) =
  let ww, wh = Window.size win in
  (if x < 0 then ww + x else x), (if y < 0 then wh + y else y),
  (if w < 0 then ww - x + w else w), (if h < 0 then wh - y + h else h)

let snap_dist = 12

let snap min max v =
  if is_shift_down () then v else
  if abs (v - min) < snap_dist then 0 else
  if abs (v - max) < snap_dist then max else
  v

let clamp min max v =
  if v < min then min else
  if v > max then max else
  v


(* State *)

type drag_extra = ..
type drag_extra += No_drag

let no_drag = (min_int, min_int)

let inner = ref []            (* list of inner elements *)
let mouse_pos = ref (0, 0)    (* absolute mouse position *)
let mouse_delta = ref (0, 0)  (* absolute delta *)
let drag_pos = ref no_drag    (* starting point of mouse drag *)
let drag_extra = ref No_drag  (* associated data for drag operation *)
let drag_happened = ref false (* Dragging happened before last release *)
let win_pos = ref (0, 0)      (* logical position ignoring snap *)
let color_scheme = ref 0
let fonts = Array.make 64 None
let symfonts = Array.make 64 None

let background_img = ref (`Unloaded "bg.jpg")
let button_img = ref (`Unloaded "but.jpg")

let (//) = Filename.concat
let assets = Filename.dirname Sys.argv.(0) // "assets"

let get_img win rimg =
  match !rimg with
  | `Loaded img -> img
  | `Unloaded file ->
    let img = Image.load win (assets // file) in
    rimg := `Loaded img;
    img


(* Colors *)

type color_scheme = {text : color; warn : color; error : color; focus : color}

let color_schemes =
[|
  {text = `Green; warn = `Yellow; error = `Red; focus = `Blue};
  {text = `RGB 0x78cfeb; warn = `RGB 0xfef46d; error = `RGB 0xd35c6d; focus = `RGB 0x5186bb};
  {text = `RGB 0x51a6fb; warn = `RGB 0xfef46d; error = `RGB 0xd35c6d; focus = `RGB 0x78cfeb};
  {text = `RGB 0xddac4d; warn = `RGB 0xffff6d; error = `RGB 0xf14138; focus = `RGB 0xd5b482};
|]

let get_color_scheme () = !color_scheme
let set_color_scheme i = color_scheme := i


let unlit = 0x28

let text_color () = color_schemes.(!color_scheme).text
let focus_color () = color_schemes.(!color_scheme).focus

let fill = function
  | true -> text_color ()
  | false -> `Trans (text_color (), unlit)

let border = function
  | `Focused -> focus_color ()
(*
  | `Pressed -> `Orange
*)
  | _ -> `Black

let font' win h file min max fonts =
  match fonts.(h) with
  | Some f -> f
  | None ->
    let f = Api.Font.load win file min max h in
    fonts.(h) <- Some f;
    f


(* Window *)

let window win =
  let bg = get_img win background_img in
  let ww, wh = Window.size win in
  let iw, ih = Image.size bg in
  for i = 0 to (ww + iw - 1)/iw - 1 do
    let x = if ww < iw then - (iw - ww)/2 else i*iw in
    for j = 0 to (wh + ih - 1)/ih - 1 do
      let y = if wh < ih then - (ih - wh)/2 else j*ih in
      Draw.image win x y 1 bg
    done
  done;

  Draw.line win 1 0 1 wh (`Gray 0x40);
  Draw.line win 0 0 ww 0 (`Gray 0x70);

  Mouse.set_cursor win `Default;
  let m = Mouse.pos win in
  let mouse' = add m (Window.pos win) in
  mouse_delta := sub mouse' !mouse_pos;
  mouse_pos := mouse';
  if Mouse.is_down `Left then
  (
    if !drag_pos = no_drag then drag_pos := m;
    if not (List.exists (inside !drag_pos) !inner) then
    (
      let wx, wy = add !win_pos !mouse_delta in
      let mw, mh = sub (Window.screen_size win) (Window.size win) in
      Window.set_pos win (snap 0 mw wx) (snap 0 mh wy);
      win_pos := wx, wy
    )
  )
  else
  (
    drag_pos := no_drag;
    drag_extra := No_drag;
    win_pos := Window.pos win
  );
  inner := []


(* GUI elements *)

let font win h = font' win h "tahoma.ttf" 0x0020 0x0600 fonts
let _symfont win h = font' win h "webdings.ttf" 0x23c0 0x2400 symfonts


let no_modkey = ([], `None)

let key_status _win (modifiers, key) =
  if (Key.is_pressed key || Key.is_repeated key)
  && Api.Key.are_modifiers_down modifiers then
    `Pressed
  else if Key.is_released key && Api.Key.are_modifiers_down modifiers then
    `Released
  else
    `Untouched

let mouse_status win r side =
  if Mouse.is_down side && inside !drag_pos r then
    `Pressed
  else if not (inside (Mouse.pos win) r) then
    `Untouched
  else if Mouse.is_released side then
    `Released
  else
    `Focused

type drag_extra += Drag_gen of point

let drag_status win r (stepx, stepy) =
  if not (Mouse.is_down `Left) then
    let happened = !drag_happened in
    drag_happened := false;
    if happened then `None else `Click
  else
  let r' = dim win r in
  let (mx, my) as m = Mouse.pos win in
  if not (inside !drag_pos r') then `None else
  match !drag_extra with
  | No_drag -> drag_extra := Drag_gen m; `None
  | Drag_gen m' ->
    let dx, dy = sub m m' in
    let dx' = dx / stepx in
    let dy' = dy / stepy in
    let happened = dx' <> 0 || dy' <> 0 in
    drag_extra := Drag_gen (mx - dx mod stepx, my - dy mod stepy);
    drag_happened := !drag_happened || happened;
    if not happened then `None else `Drag (dx', dy')
  | _ -> assert false

let wheel_status win r =
  let r' = dim win r in
  if inside (Mouse.pos win) r' then snd (Mouse.wheel win) else 0.0

let key modkey win = (key_status win modkey = `Pressed)
let mouse r side win = (mouse_status win (dim win r) side = `Released)
let wheel r win = wheel_status win (dim win r)
let drag r win eps = drag_status win (dim win r) eps


let box r c win =
  let x, y, w, h = dim win r in
  Draw.fill win x y w h c

let label r align s win =
  let x, y, w, h = dim win r in
  let font = font win h in
  let tw = Api.Draw.text_width win h font s in
  let dx =
    match align with
    | `Left -> 0
    | `Center -> (w - tw) / 2
    | `Right -> w - tw
  in Api.Draw.text win (x + dx) y h `White font s

let indicator r win on =
  let x, y, w, h = dim win r in
  Draw.fill_circ win x y w h (fill on);
  Draw.circ win x y w h (border `Untouched)

let lcd' win r' c elem =
  let open Api.Draw in
  let x, y, w, h = r' in
  let m = h / 2 in
  match elem with
  | `N ->
    line win (x + 1) (y + 0) (x + w - 3) (y + 0) c;
    line win (x + 2) (y + 1) (x + w - 4) (y + 1) c;
  | `S ->
    line win (x + 1) (y + h - 1) (x + w - 3) (y + h - 1) c;
    line win (x + 2) (y + h - 2) (x + w - 4) (y + h - 2) c;
  | `C ->
    line win (x + 1) (y + m - 1) (x + w - 3) (y + m - 1) c;
    line win (x + 2) (y + m) (x + w - 4) (y + m) c;
  | `NW ->
    line win (x + 0) (y + 1) (x + 0) (y + m - 1) c;
    line win (x + 1) (y + 2) (x + 1) (y + m - 2) c;
  | `NE ->
    line win (x + w - 1) (y + 1) (x + w - 1) (y + m - 1) c;
    line win (x + w - 2) (y + 2) (x + w - 2) (y + m - 2) c;
  | `SW ->
    line win (x + 0) (y + m + 1) (x + 0) (y + h - 1) c;
    line win (x + 1) (y + m + 2) (x + 1) (y + h - 2) c;
  | `SE ->
    line win (x + w - 1) (y + m + 1) (x + w - 1) (y + h - 1) c;
    line win (x + w - 2) (y + m + 2) (x + w - 2) (y + h - 2) c
  | `Dots ->
    rect win x (y + h / 4) 2 2 c;
    rect win x (y + 3 * h / 4) 2 2 c

let lcd r win d =
  let c = text_color () in
  if d = '-' || d = '+' then
    lcd' win (dim win r) c `C
  else if d = ':' then
    lcd' win (dim win r) c `Dots
  else
    List.iter (lcd' win (dim win r) c) [`N; `S; `C; `NW; `SW; `NE; `SE];
  List.iter (lcd' win (dim win r) (`Trans (`Black, 0x100 - unlit)))
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


let element r modkey win =
  let r' = dim win r in
  inner := r' :: !inner;
  r',
  match mouse_status win r' `Left, key_status win modkey with
  | `Released, _ | _, `Released -> `Released
  | `Pressed, _ | _, `Pressed -> `Pressed
  | `Focused, _ | _, `Focused -> `Focused
  | _, _ -> `Untouched

let resizer r win (minw, minh) (maxw, maxh) =
  let (x, y, w, h), status = element r no_modkey win in
  if status <> `Untouched then Api.Mouse.set_cursor win (`Resize `N_S);
  Draw.fill win x y w h (fill false);
  Draw.rect win x y w h (border status);
  let sz = Window.size win in
  if status <> `Pressed then sz else
  (
    let ww, wh = add sz !mouse_delta in
    let sw, sh = Window.screen_size win in
    let maxw = if maxw < 0 then sw else maxw in
    let maxh = if maxh < 0 then sh else maxh in
    let ww', wh' = clamp minw maxw ww, clamp minh maxh wh in
    (*Window.set_size win ww' wh';*)
    (* Adjust owned drag_pos for size-relative position *)
    let x0, y0, _, _ = r in
    let dw = if x0 >= 0 then 0 else ww' - fst sz in
    let dh = if y0 >= 0 then 0 else wh' - snd sz in
    drag_pos := add !drag_pos (dw, dh);
    assert (inside !drag_pos (x + dw, y + dh, w, h));
    inner := (x + dw, y + dh, w, h) :: !inner;
    ww', wh'
  )

let button_text win x y w h c = function
  | "[]" ->
    Draw.fill win x y w w c
  | "||" ->
    Draw.fill win x y (w/3) w c;
    Draw.fill win (x + w - w/3) y (w/3) w c;
  | ">" ->
    Draw.arrow win x y w w c `Right
  | ">>" ->
    Draw.arrow win x y (w/2 + 1) w c `Right;
    Draw.arrow win (x + w/2) y (w/2 + 1) w c `Right;
  | "<<" ->
    Draw.arrow win x y (w/2) w c `Left;
    Draw.arrow win (x + w/2) y (w/2) w c `Left;
  | "^" ->
    Draw.arrow win x y w (w/2) c `Up;
    Draw.fill win x (y + w - w/3) w (w/3) c;
  | s ->
    label (x, y, w, h) `Center s win

let button r ?(protrude=true) txt modkey win active =
  let (x, y, w, h), status = element r modkey win in
  let img = get_img win button_img in
  let sx, sy, dy = if status = `Pressed then 800, 400, 1 else 0, 200, 0 in
  Draw.clip win x y w h;
  Draw.image win (x - sx) (y - sy) 1 img;
  Draw.unclip win;
  if status <> `Pressed then
  (
    Draw.line win (x + 2) (y + 1) (x + 2) (y + h - 2) (`Gray 0x50);
    if protrude then Draw.line win (x + 1) (y + 1) (x + w - 2) (y + 1) (`Gray 0x50);
  );
  Draw.rect win x y w h (border status);
  let wsym = h/3 in
  let c = if active then `RGB 0x40ff40 else `Gray 0xc0 in
  button_text win (x + (w - wsym)/2) (y + (h - wsym)/2 + dy) wsym (h/2) c txt;
(*
  Draw.fill win x y w h (fill active);
  Draw.rect win x y w h (border status);
  let hsym = h/2 in
  let font = font win hsym in  (* TODO *)
  let wsym = Draw.text_width win hsym font sym in
  let c = if active then `Green else `White in
  Draw.text win (x + (w - wsym)/2) (y + (h - hsym)/2) hsym c font sym;
*)
  if status = `Released then not active else active

let progress_bar r win v =
  let (x, y, w, h), status = element r no_modkey win in
  Draw.fill win x y w h (fill false);
  Draw.fill win (x + 1) y (int_of_float (v *. float (w - 2))) h (fill true);
  for i = 0 to w / 2 - 1 do
    Draw.line win (x + 2*i + 1) y (x + 2*i + 1) (y + h) `Black
  done;
  Draw.rect win x y w h (border status);
  if status <> `Pressed then v else
  let mx, _ = Mouse.pos win in
  clamp 0.0 1.0 (float (mx - x) /. float w)

let volume_bar r win v =
  let (x, y, w, h), status = element r no_modkey win in
  let h' = int_of_float ((1.0 -. v) *. float h) in
  Draw.fill win (x + w - 2) y 2 h (fill true);
  Draw.tri win x y (w - 2) h (fill true) `NE;
  Draw.fill win x y w h' (`Trans (`Black, 0x100 - unlit));
  for j = 0 to h / 2 - 1 do
    Draw.line win x (y + 2*j + 1) (x + w + 1) (y + 2*j + 1) `Black
  done;
  if status <> `Pressed then v else
  let _, my = Mouse.pos win in
  clamp 0.0 1.0 (float (y + h - my) /. float h)
(*
  let (x, y, w, h), status = element r no_modkey win in
  let h' = int_of_float (v *. float (h - 2)) in
  Draw.fill win x y w h (fill false);
  Draw.fill win x (y + h - h' - 1) w h' (fill true);
  Draw.rect win x y w h (border status);
  if status <> `Pressed then v else
  let _, my = Mouse.pos win in
  clamp 0.0 1.0 (float (y + h - my) /. float h)
*)

type drag_extra += Scroll_bar_page of time
type drag_extra += Scroll_bar_drag of float * int

let scroll_bar r win v len =
  assert (v +. len <= 1.0);
  let (x, y, w, h), status = element r no_modkey win in
  Draw.fill win x y w h (fill false);
  let y' = y + int_of_float (v *. float (h - 2)) + 1 in
  let h' = int_of_float (Float.ceil (len *. float (h - 2))) in
  Draw.fill win x y' w h' (fill true);
  for j = 0 to h / 2 - 1 do
    Draw.line win x (y + 2*j + 1) (x + w) (y + 2*j + 1) `Black
  done;
  Draw.rect win x y w h (border status);
  if status <> `Pressed then v else
  let _, my = Mouse.pos win in
  let v0, my0, t, dragging =
    match !drag_extra with
    | No_drag -> v, my, 0.0, false
    | Scroll_bar_page t -> v, my, t, false
    | Scroll_bar_drag (v0, my0) -> v0, my0, 0.0, true
    | _ -> assert false
  in
  let now = Unix.gettimeofday () in
  let v' =
    if dragging || y' <= my && my < y' + h' then
    (
      drag_extra := Scroll_bar_drag (v0, my0);
      v0 +. float (my - my0) /. float (h - 2)
    )
    else if now -. t > 0.5 then
    (
      drag_extra := Scroll_bar_page now;
      if my < y' then v -. len else
      if my >= y' + h' then v +. len else
      v
    )
    else v
  in clamp 0.0 (1.0 -. len) v'


let ticker r win s =
  let (x, y, w, h), _status = element r no_modkey win in
  Draw.fill win x y w h `Black;
  let tw = Draw.text_width win h (font win h) s in
  Draw.clip win x y w h;
  let dx = if tw <= w then (w - tw)/2 else w - Draw.frame win mod (w + tw) in
  Draw.text win (x + dx) y h (text_color ()) (font win h) s;
  Draw.unclip win


type align = [`Left | `Center | `Right]
type column = int * align
type row = color * color * string array

let table r ch win cols rows =
  let (x, y, w, h), status = element r no_modkey win in
  let font = font win ch in
  Draw.fill win x y w h `Black;
  Array.iteri (fun j (fg, bg, texts) ->
    let cy = y + j * ch in
    if bg <> `Black then Draw.fill win x cy w ch bg;
    let cx = ref x in
    Array.iteri (fun i (cw, align) ->
      let tw = Draw.text_width win ch font texts.(i) in
      let dx =
        match align with
        | `Left -> 0
        | `Center -> (cw - tw) / 2
        | `Right -> cw - tw
      in
      if tw >= cw then Draw.clip win !cx y (cw - 1) h;
      Draw.text win (!cx + max 0 dx) cy ch fg font texts.(i);
      if tw >= cw then
      (
        let gw = min cw 16 in
        Draw.gradient win (!cx + cw - gw) cy gw ch
          (`Trans (bg, 0)) `Horizontal bg;
        Draw.unclip win;
      );
      cx := !cx + cw;
    ) cols
  ) rows;
  if status = `Pressed || status = `Released then
    let _, my = Mouse.pos win in
    Some ((my - y) / ch)
  else
    None
