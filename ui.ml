(* Direct-style GUI abstractions *)

open Api


(* Keyboard *)

let is_shift_down () =
  Key.is_down (`Shift `Left) || Key.is_down (`Shift `Right)


(* Helpers *)

let relative win x y =
  let w, h = Window.size win in
  (if x < 0 then w + x else x), (if y < 0 then h + y else y)

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

let inner = ref []           (* list of inner elements *)
let mouse_pos = ref (0, 0)   (* absolute mouse position *)
let mouse_delta = ref (0, 0) (* absolute delta *)
let drag_pos = ref (-1, -1)  (* starting point of mouse drag *)
let win_pos = ref (0, 0)     (* logical position ignoring snap *)
let fonts = Array.make 64 None

let window win =
  Api.Mouse.set_cursor win `Default;
  let m = Mouse.pos win in
  let mouse' = add m (Window.pos win) in
  mouse_delta := sub mouse' !mouse_pos;
  mouse_pos := mouse';
  if Mouse.is_down `Left then
  (
    if !drag_pos = (-1, -1) then drag_pos := m;
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
    drag_pos := -1, -1;
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

let font win h =
  match fonts.(h) with
  | Some f -> f
  | None ->
    let f = Api.Font.load win "bahn.ttf" 0x600 h in
    fonts.(h) <- Some f;
    f

let element (x0, y0, w, h) key win =
  let x, y = relative win x0 y0 in
  inner := (x, y, w, h) :: !inner;
  let m = Mouse.pos win in
  x, y,
  if Mouse.is_down `Left && inside !drag_pos (x, y, w, h) then `Pressed else
  if not (inside m (x, y, w, h)) then `Untouched else
  if Key.is_down key then `Pressed else
  if Mouse.is_released `Left || Key.is_released key then `Released else
  `Focused

let resizer (x0, y0, w, h) win (minw, minh) (maxw, maxh) =
  let x, y, status = element (x0, y0, w, h) `None win in
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
    let dw = if x0 >= 0 then 0 else ww' - fst sz in
    let dh = if y0 >= 0 then 0 else wh' - snd sz in
    drag_pos := add !drag_pos (dw, dh);
    assert (inside !drag_pos (x + dw, y + dh, w, h));
    inner := (x + dw, y + dh, w, h) :: !inner
  )

let button (x0, y0, w, h) key win =
  let x, y, status = element (x0, y0, w, h) key win in
  Draw.fill win x y w h (fill false);
  Draw.rect win x y w h (border status);
  status = `Released

let control_button (x0, y0, w, h) key win active =
  let x, y, status = element (x0, y0, w, h) key win in
  Draw.fill_circ win x y w h (fill active);
  Draw.circ win x y w h (border status);
  if status = `Released then not active else active

let progress_bar (x0, y0, w, h) win v =
  let x, y, status = element (x0, y0, w, h) `None win in
  Draw.fill win x y w h (fill false);
  Draw.fill win (x + 1) y (int_of_float (v *. float (w - 2))) h (fill true);
  Draw.rect win x y w h (border status);
  if status = `Pressed then
    let mx, _ = Mouse.pos win in
    Some (clamp 0.0 1.0 ((float mx -. float x) /. float w))
  else
    None

let scroller (x0, y0, w, h) win s =
  let x, y, _status = element (x0, y0, w, h) `None win in
  Draw.fill win x y w h `Black;
  let tw = Draw.text_width win h (font win h) s in
  Draw.clip win (x, y, w, h);
  let dx = if tw <= w then (w - tw)/2 else w - Draw.frame win mod (w + tw) in
  Draw.text win (x + dx) y h `Green (font win h) s;
  Draw.unclip win
