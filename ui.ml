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


(* Window *)

type drag_extra = ..
type drag_extra += No_drag

let no_drag = (min_int, min_int)

let inner = ref []           (* list of inner elements *)
let mouse_pos = ref (0, 0)   (* absolute mouse position *)
let mouse_delta = ref (0, 0) (* absolute delta *)
let drag_pos = ref no_drag   (* starting point of mouse drag *)
let drag_extra = ref No_drag (* associated data for drag operation *)
let win_pos = ref (0, 0)     (* logical position ignoring snap *)
let fonts = Array.make 64 None
let symfonts = Array.make 64 None

let window win =
  Api.Mouse.set_cursor win `Default;
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

let fill = function
  | true -> `Green
  | false -> `Gray 0x80

let border = function
  | `Pressed -> `Yellow
  | `Focused -> `Orange
  | _ -> `Blue

let font' win h file min max fonts =
  match fonts.(h) with
  | Some f -> f
  | None ->
    let f = Api.Font.load win file min max h in
    fonts.(h) <- Some f;
    f

let font win h = font' win h "bahn.ttf" 0x20 0x600 fonts
let _symfont win h = font' win h "webdings.ttf" 0x23c0 0x2400 symfonts


let no_modkey = ([], `None)

let key_status _win (modifiers, key) =
  if Key.is_down key && Api.Key.are_modifiers_down modifiers then `Pressed else
  if Key.is_released key && Api.Key.are_modifiers_down modifiers then `Released else
  `Untouched

let mouse_status win r side =
  if Mouse.is_down side && inside !drag_pos r then `Pressed else
  if not (inside (Mouse.pos win) r) then `Untouched else
  if Mouse.is_released `Left then `Released else
  `Focused

type drag_extra += Drag_gen of point

let drag_status win r (stepx, stepy) =
  let r' = dim win r in
  let (mx, my) as m = Mouse.pos win in
  if not (inside !drag_pos r') then None else
  match !drag_extra with
  | No_drag -> drag_extra := Drag_gen m; None
  | Drag_gen m' ->
    let dx, dy = sub m m' in
    let dx' = dx / stepx in
    let dy' = dy / stepy in
    drag_extra := Drag_gen (mx - dx mod stepx, my - dy mod stepy);
    if dx' = 0 && dy' = 0 then None else Some (dx', dy')
  | _ -> assert false

let wheel_status win r =
  let r' = dim win r in
  if inside (Mouse.pos win) r' then snd (Mouse.wheel win) else 0.0

let key modkey win = (key_status win modkey = `Released)
let mouse r side win = (mouse_status win r side = `Released)
let wheel r win = wheel_status win r
let drag r win eps = drag_status win r eps

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
  if status = `Pressed then
  (
    let sz = Window.size win in
    let ww, wh = add sz !mouse_delta in
    let sw, sh = Window.screen_size win in
    let maxw = if maxw < 0 then sw else maxw in
    let maxh = if maxh < 0 then sh else maxh in
    let ww', wh' = clamp minw maxw ww, clamp minh maxh wh in
    Window.set_size win ww' wh';
    (* Adjust owned drag_pos for size-relative position *)
    let x0, y0, _, _ = r in
    let dw = if x0 >= 0 then 0 else ww' - fst sz in
    let dh = if y0 >= 0 then 0 else wh' - snd sz in
    drag_pos := add !drag_pos (dw, dh);
    assert (inside !drag_pos (x + dw, y + dh, w, h));
    inner := (x + dw, y + dh, w, h) :: !inner
  )

let button r modkey win =
  let (x, y, w, h), status = element r modkey win in
  Draw.fill win x y w h (fill false);
  Draw.rect win x y w h (border status);
  status = `Released

let control_button r sym modkey win active =
  let (x, y, w, h), status = element r modkey win in
  Draw.fill win x y w h (fill active);
  Draw.rect win x y w h (border status);
  let hsym = h/2 in
  let font = font win hsym in  (* TODO *)
  let wsym = Api.Draw.text_width win hsym font sym in
  Api.Draw.text win (x + (w - wsym)/2) (y + (h - hsym)/2) hsym `White font sym;
  if status = `Released then not active else active

let progress_bar r win v =
  let (x, y, w, h), status = element r no_modkey win in
  Draw.fill win x y w h (fill false);
  Draw.fill win (x + 1) y (int_of_float (v *. float (w - 2))) h (fill true);
  Draw.rect win x y w h (border status);
  if status <> `Pressed then v else
  let mx, _ = Mouse.pos win in
  clamp 0.0 1.0 ((float mx -. float x) /. float w)

type drag_extra += Scroll_bar_page of time
type drag_extra += Scroll_bar_drag of float * int

let scroll_bar r win v len =
  assert (v +. len <= 1.0);
  let (x, y, w, h), status = element r no_modkey win in
  Draw.fill win x y w h (fill false);
  let y' = y + int_of_float (v *. float (h - 2)) + 1 in
  let h' = int_of_float (Float.ceil (len *. float (h - 2))) in
  Draw.fill win x y' w h' (fill true);
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


let scroller r win s =
  let (x, y, w, h), _status = element r no_modkey win in
  Draw.fill win x y w h `Black;
  let tw = Draw.text_width win h (font win h) s in
  Draw.clip win (x, y, w, h);
  let dx = if tw <= w then (w - tw)/2 else w - Draw.frame win mod (w + tw) in
  Draw.text win (x + dx) y h `Green (font win h) s;
  Draw.unclip win


type align = [`Left | `Center | `Right]
type column = int * align
type row = color * color * string array

let table r ch win cols rows =
  let (x, y, w, h), status = element r no_modkey win in
  let font = font win ch in
  Array.iteri (fun j (fg, bg, texts) ->
    let cy = y + j * ch in
    if bg <> `Black then Api.Draw.fill win x cy w ch bg;
    let cx = ref x in
    Array.iteri (fun i (cw, align) ->
      let tw = Api.Draw.text_width win ch font texts.(i) in
      let dx =
        match align with
        | `Left -> 0
        | `Center -> (cw - tw) / 2
        | `Right -> cw - tw
      in
      if tw >= cw then Api.Draw.clip win (!cx, y, cw - 1, h);
      Api.Draw.text win (!cx + max 0 dx) cy ch fg font texts.(i);
      if tw >= cw then
      (
        let gw = min cw 16 in
        Api.Draw.gradient win (!cx + cw - gw) cy gw ch
          (`Trans (bg, 0)) `Horizontal bg;
        Api.Draw.unclip win;
      );
      cx := !cx + cw;
    ) cols
  ) rows;
  if status = `Pressed || status = `Released then
    let _, my = Api.Mouse.pos win in
    Some ((my - y) / ch)
  else
    None
