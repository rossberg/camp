(* Immediate-style GUI widgets *)

open Api
open Audio_file


(* State *)

type pane = int
type owner = string

type drag = ..
type drag += No_drag

type image_load = [`Unloaded of string | `Loaded of image] ref

module Map = Map.Make(String)

type t =
{
  win : window;
  mutable buffered : bool;
  mutable font_sdf : bool;
  mutable palette : int;
  mutable pane : pane;
  mutable pane_owners : pane Map.t;    (* map owners to panes *)
  mutable panes : (rect * rect) array; (* abstract and concrete rectangles *)
  mutable modal : bool;                (* whether a pop-up menu is shown *)
  mutable modal_resize : bool;         (* whether a resize happened this frame *)
  mutable modal_save : bool;           (* modal mode before the resize *)
  mutable modal_rect : rect option;    (* popup masking non-modal part *)
  mutable mouse_owner : owner option;  (* whether mouse was owned by a widget *)
  mutable drag  : drag;                (* associated data for drag operation *)
  mutable delayed : (unit -> unit) list; (* draw at end of frame *)
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
    font_sdf = false (*Screen.is_hires (Window.screen win)*);
    palette = 0;
    pane = 0;
    pane_owners = Map.empty;
    panes = Array.make 10 ((0, 0, 0, 0), (0, 0, 0, 0));
    modal = false;
    modal_resize = false;
    modal_save = false;
    modal_rect = None;
    mouse_owner = None;
    drag = No_drag;
    delayed = [];
    img_background = ref (`Unloaded "bg.jpg");
    img_button = ref (`Unloaded "but.jpg");
    img_nocover = ref (`Unloaded "nocover.jpg");
    fonts = Array.make 65 None;
  }

let window ui = ui.win

let buffered ui b = ui.buffered <- b
let is_buffered ui = ui.buffered


(* Mouse ownership *)

let has_mouse ui owner =
  Option.exists (String.starts_with ~prefix: owner) ui.mouse_owner

let grab_mouse ui owner =
  if not ui.modal && ui.mouse_owner = None && ui.drag = No_drag then
    ui.mouse_owner <- Some owner;
  has_mouse ui owner


(* Modal mode *)

let modal ui label =
  (* The flag modal_resize is set when we temporarily go into modal mode
   * for 1 frame because the window is being resized. This overrides the
   * existing modal mode, which is saved in modal_save and reinstated from
   * there for the next frame (see start function).
   * Hence, when setting modal mode, modal must either be false before,
   * or we are in resize mode and the save is non-modal.
   * Note that it cannot happen that modal_resize is true but modal isn't.
   *)
  if !App.debug_modality then
    Printf.eprintf "[modal %s] frame=%d\n%!" label (Api.Draw.frame ui.win);
  assert (ui.modal = ui.modal_resize);
  assert (not (ui.modal_resize && not ui.modal_save));
  if not ui.modal_resize then ui.modal <- true;
  ui.modal_save <- true

let nonmodal ui label =
  if !App.debug_modality then
    Printf.eprintf "[nonmodal %s] frame=%d\n%!" label (Api.Draw.frame ui.win);
  assert ui.modal;
  assert (not (ui.modal_resize && not ui.modal_save));
  if not ui.modal_resize then ui.modal <- false;
  ui.modal_save <- false

let is_modal ui =
  ui.modal_save

let except_modal ui label f =
  let save = is_modal ui in
  let save_rect = ui.modal_rect in
  if save then nonmodal ui label;
  ui.modal_rect <- None;
  let x = f () in
  assert (ui.modal_rect = None);
  ui.modal_rect <- save_rect;
  if save then modal ui label;
  x

let modal_rect ui label r =
  if !App.debug_modality then
    Printf.eprintf "[modal_rect %s] frame=%d r=%s\n%!" label
      (Api.Draw.frame ui.win)
      (match r with
      | None -> "-"
      | Some (x, y, w, h) -> Printf.sprintf "%d,%d,%d,%d" x y w h
      );
  assert (not (r = None && ui.modal_rect = None));
  ui.modal_rect <- r

let has_modal_rect ui =
  ui.modal_rect <> None


(* Panes *)

let rel b v =
  if v >= 0 then v else
  if v = -1 then b else max 0 (b + v)

let rel_rect (maxw, maxh) (x, y, w, h) =
  let x' = rel maxw x in
  let y' = rel maxh y in
  let w' = rel (maxw - x') w in
  let h' = rel (maxh - y') h in
  x', y', w', h'

let pane ui owner r =
  let ww, wh as wsize = Window.size ui.win in
  let x, y, w, h as r' = rel_rect wsize r in
(*
  if not (x >= 0 && y >= 0 && x + w <= ww && y + h <= wh) then
  (
    Storage.log (Printf.sprintf
      "invalid geometry for pane %s: x=%d y=%d w=%d h=%d winw=%d winh=%d"
      owner x y w h ww wh
    );
    if !App.debug_layout then assert false;
  );
*)

  let p =
    match Map.find_opt owner ui.pane_owners with
    | Some p -> p
    | None ->
      let p = ui.pane in
      ui.pane <- ui.pane + 1;
      ui.pane_owners <- Map.add owner p ui.pane_owners;
      p
  in

  let n = Array.length ui.panes in
  if p >= n then
  (
    let z = 0, 0, 0, 0 in
    ui.panes <-
      Array.init (2 * p) (fun i -> if p < n then ui.panes.(p) else z, z);
  );
  ui.panes.(p) <- r, r';

  p

let find_pane ui pos =
  match Array.find_opt (fun (_, r) -> inside pos r) ui.panes with
  | Some rr -> Some rr
  | None ->
    let w, h = Window.size ui.win in
    let all = (0, 0, w, h) in
    if inside pos all then
      Some (all, all)
    else
      None


(* Areas *)

type area = pane * int * int * int * int

let dim ui (p, x, y, w, h) =
  let px, py, pw, ph =
    if p >= 0 then snd ui.panes.(p) else
    let ww, wh = Window.size ui.win in 0, 0, ww, wh
  in
  let x', y', w', h' = rel_rect (pw, ph) (x, y, w, h) in
  px + x', py + y', w', h'

let mouse_inside ui area =
  inside (Mouse.pos ui.win) (dim ui area)


(* Geometry helpers *)

let snap_dist = 12

let is_shift_down () = Key.is_modifier_down `Shift || Mouse.is_down `Middle
let is_command_down () =
  Key.is_modifier_down `Command || Mouse.is_down `Middle && Mouse.is_down `Right

let snap min max v =
  if is_shift_down () then v else
  if abs (v - min) < snap_dist then min else
  if abs (v - max) < snap_dist then max else
  v

let clamp min max v =
  if v < min then min else
  if v > max then max else
  v

let quant_floor l v = v / l * l
let quant_ceil l v = (v + l - 1) / l * l


(* Colors *)

type color = Api.color
type palette =
  {name : string; text : color; warn : color; error : color; hover : color}

let palettes =
[|
  { name = "Teal";
    text = `RGB 0x78cfeb; warn = `RGB 0xfef46d;
    error = `RGB 0xd35c6d; hover = `RGB 0x5186bb;
  };
  { name = "Blue";
    text = `RGB 0x51a6fb; warn = `RGB 0xfef46d;
    error = `RGB 0xd35c6d; hover = `RGB 0x78cfeb;
  };
  { name = "Amber";
    text = `RGB 0xddac4d; warn = `RGB 0xffff6d;
    error = `RGB 0xf14138; hover = `RGB 0xd5b482;
  };
  { name = "White";
    text = `RGB 0xe0f0ff; warn = `RGB 0xffff8d;
    error = `RGB 0xf87148; hover = `RGB 0xffffff;
  };
  { name = "Green";
    text = `Green; warn = `Yellow;
    error = `Red; hover = `Blue;
  };
  { name = "Opal";
    text = `RGB 0x92f2d6; warn = `RGB 0xc8bd4a;
    error = `RGB 0xec635b; hover = `RGB 0x5f7eb8;
  };
|]

let num_palette _ui = Array.length palettes
let get_palette ui = ui.palette
let set_palette ui i = ui.palette <- i
let name_palette ui i = palettes.(i).name


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

let focus' ui x y w h b c style =
  let c1 = `Trans (c, 0x80 (* 0x40 *)) in
  let c2 = `Trans (c, 0x00) in
  (*let b = 8 (* 6 *) in*)
  match style with
  | `Above ->
    Draw.gradient ui.win x y w b c1 `Vertical c2;
  | `Left ->
    Draw.gradient ui.win x y b h c1 `Horizontal c2;
  | `Inside ->
    Draw.gradient ui.win x y w b c1 `Vertical c2;
    Draw.gradient ui.win x (y + h - b) w b c2 `Vertical c1

let focus ui area b =
  let x, y, w, h = dim ui area in
  focus' ui x y w h b (text_color ui) `Inside


let mouse_focus' ui r v offset =
  let mx, my = Mouse.pos ui.win in
  Draw.gradient_circ ui.win (mx + offset - r) (my + offset - r) (2 * r) (2 * r)
    (`Trans (`White, v)) (`Trans (`White, 0x00))

let mouse_focus ui area r v offset =
  let x, y, w, h = dim ui area in
  let rr = (x - r - offset, y - r - offset, w + 2*r, h + 2*r) in
  (* Don't use mouse_inside here, since values can get negative. *)
  if inside (Mouse.pos ui.win) rr then
  (
    Draw.clip ui.win x y w h;
    mouse_focus' ui r v offset;
    Draw.unclip ui.win;
  )


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
  let h = clamp 3 64 h in
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
      Draw.image ui.win (x + dx) (y + dy) 0.0 1.0 bg
    done
  done;

  Draw.fill_rect ui.win x y 1 (h - 2) (`Gray 0x50);
  Draw.fill_rect ui.win x y w 1 (`Gray 0x70);
  Draw.fill_rect ui.win (x + 1) (y + h - 2) (w - 1) 2 (`Gray 0x10);
  Draw.fill_rect ui.win (x + w - 1) y 1 (h - 2) (`Gray 0x10);

  mouse_focus' ui ((min w h) / 4) 0x20 0;

  Draw.unclip ui.win


(* Window *)

type drag += Move of {overshoot : size}
type drag += Resize of {offset : size; edge : bool * bool * bool * bool}
type drag += Abort

let delay ui f =
  ui.delayed <- f :: ui.delayed

let reset ui (wx, wy, ww, wh) =
  Window.set_pos ui.win wx wy;
  Window.set_size ui.win ww wh

let rescale ui (dx, dy) =
  if dx <> 0 || dy <> 0 then
  (
    Window.rescale ui.win dx dy;
    font_purge ui;
  )

let pin ui scr =
  Window.set_screen ui.win scr

let start ui (wx', wy', ww', wh' as wr') =
  let wx, wy = Window.pos ui.win in
  let ww, wh = Window.size ui.win in
  ui.modal <- ui.modal_save;
  ui.modal_resize <- wr' <> (wx, wy, ww, wh);
  if ui.modal_resize then
  (
    if !App.debug_layout then
    (
      Printf.eprintf "[win resize] %d,%d,%d,%d -> %d,%d,%d,%d\n%!"
        wx wy ww wh wx' wy' ww' wh'
    );
    (* Suppress input when window was just resized, mouse pos may be off *)
    ui.modal_save <- ui.modal;
    ui.modal <- true;
    Window.set_pos ui.win wx' wy';
    Window.set_size ui.win ww' wh';
  );

  Draw.start ui.win (`Trans (`Black, 0x40));
  background ui 0 0 ww wh


let cursor varw varh lft top rgt bot =
  match varw && lft, varw && rgt, varh && top, varh && bot with
  | true, false, false, false
  | false, true, false, false -> `Resize `E_W
  | false, false, true, false
  | false, false, false, true -> `Resize `N_S
  | true, false, true, false
  | false, true, false, true -> `Resize `NW_SE
  | true, false, false, true
  | false, true, true, false -> `Resize `NE_SW
  | _ -> `Point

let finish ui margin (varw, varh) =
  List.iter (fun f -> f ()) (List.rev ui.delayed);
  ui.delayed <- [];

  let screen_change = Draw.finish ui.win in
  Option.iter (pin ui) screen_change;

  let (wx, wy) as pos = Window.pos ui.win in
  let (ww, wh) as size = Window.size ui.win in
  let wr = (wx, wy, ww, wh) in

  let origin = Mouse.pos ui.win in
  let lft = inside origin (0, 0, margin, wh) in
  let rgt = inside origin (ww - margin, 0, margin, wh) in
  let top = inside origin (0, 0, ww, margin) in
  let bot = inside origin (0, wh - margin, ww, margin) in
  let no_edge = (false, false, false, false) in

  let owner = ui.mouse_owner in
  if
    not (
      Mouse.is_down `Left || Mouse.is_down `Right ||
      Mouse.is_released `Left || Mouse.is_released `Right
    )
  then
  (
    ui.drag <- No_drag;
    ui.mouse_owner <- None;
  );

  if owner <> None || ui.modal_save
  || Option.exists (inside (Mouse.pos ui.win)) ui.modal_rect then
  (
    wr, no_edge, screen_change
  )
  else if Mouse.is_down `Right || ui.drag = Abort then
  (
    Mouse.set_cursor ui.win `Default;
    ui.drag <- Abort;
    wr, no_edge, screen_change
  )
  else if not (Mouse.is_down `Left) then
  (
    let cursor = cursor varw varh lft top rgt bot in
    if cursor <> `Point then Mouse.set_cursor ui.win cursor;
    wr, no_edge, screen_change
  )
  else
  (
    match ui.drag with
    | No_drag ->
      let cursor = cursor varw varh lft top rgt bot in
      Mouse.set_cursor ui.win cursor;
      ui.drag <-
        if cursor = `Point then
          Move {overshoot = 0, 0}
        else
        (
          let mx, my = Mouse.abs_pos ui.win in
          let dx = if lft then mx - wx else mx - (wx + ww) in
          let dy = if top then my - wy else my - (wy + wh) in
          Resize {offset = dx, dy; edge = lft, top, rgt, bot}
        );
      wr, no_edge, screen_change

    | Move {overshoot} ->
      Mouse.set_cursor ui.win `Point;
      let mouse = Mouse.abs_pos ui.win in
      let delta = Api.add (Mouse.delta ui.win) overshoot in
      let wx', wy' = add pos delta in
      let scr = Screen.screen mouse in  (* snap relative to mouse's screen *)
      pin ui scr;
      let sx, sy = Screen.min_pos scr in
      let sw, sh = Screen.max_size scr in
      let wx'', wy'' = snap sx (sx + sw - ww) wx', snap sy (sy + sh - wh) wy' in
      ui.drag <- Move {overshoot = wx' - wx'', wy' - wy''};
      (wx'', wy'', ww, wh), no_edge, screen_change

    | Resize {offset; edge = lft, top, rgt, bot as edge} ->
      Mouse.set_cursor ui.win (cursor varw varh lft top rgt bot);
      let scr = Window.screen ui.win in  (* snap relative to window's screen *)
      let sx, sy = Screen.min_pos scr in
      let sw, sh = Screen.max_size scr in
      let mx, my = sub (Mouse.abs_pos ui.win) offset in
      let mx', my' = snap sx (sx + sw) mx, snap sy (sy + sh) my in
      let wx' = if lft then mx' else wx in
      let wy' = if top then my' else wy in
      let ww' = if rgt then mx' - wx else ww - (wx' - wx) in
      let wh' = if bot then my' - wy else wh - (wy' - wy) in
      (wx', wy', ww', wh'), edge, screen_change

    | _ ->
      wr, no_edge, screen_change
  )


let resize_repos ui (ox, oy) (dw, dh) =
  (* Figure out whether origin sticks to upper/left or lower/right.
   * If the former, respective window position coordinate remains unchanged.
   * If the latter, move window position along respective axis.
   * Assume that widgets in the lower/right half of a pane stick to that side.
   * (Breaking this assumption in a pane layout may lead to invariant violations
   * when dragging a window, since the mouse pointer can end up over a different
   * widget after resize!)
   * TODO: Declare pivot explicitly with pane geometry and check widgets?
   *)
  match find_pane ui (ox, oy) with
  | Some ((rx, ry, rw, rh), (x, y, w, h)) ->
    (if rx < 0 || rw < 0 && 2*(ox - x) > w then -dw else 0),
    (if ry < 0 || rh < 0 && 2*(oy - y) > h then -dh else 0)
  | None ->
    (if ox < 0 then 0 else -dw),
    (if oy < 0 then 0 else -dh)


(* Input Status *)

let no_modkey = ([], `None)

let key_status' ui key =
  if is_modal ui then `Untouched else
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

let mouse_status ui owner r (#side as side) =
  if ui.modal || Option.exists (inside (Mouse.pos ui.win)) ui.modal_rect
  || not (has_mouse ui owner || inside (Mouse.pos ui.win) r && (side = `Right || grab_mouse ui owner)) then
    `Untouched
  else if Mouse.is_down side && (side = `Left || not (Mouse.is_down `Middle)) then
    `Pressed
  else if Mouse.is_released side then
    `Released
  else
    `Hovered


type motion = [`Unmoved | `Moving | `Moved]
type trajectory = [`Inside | `Outside | `Outward | `Inward]
type drag += Drag of {pos : point; moved : bool; inside : bool}

let string_of_drag = ref (function
  | No_drag -> "No_drag"
  | Drag _ -> "Drag"
  | Move _ -> "Move"
  | Resize _ -> "Resize"
  | Abort -> "Abort"
  | _ -> assert false
  )

let unexpected_drag ui owner s =
  Storage.log (Printf.sprintf
    "Unexpected drag status `%s` owned by %s in %s by %s\n%!"
      (!string_of_drag ui.drag)
      (Option.value ui.mouse_owner ~default: "none")
      s owner
  )

let drag_status ui owner r (stepx, stepy) =
  if ui.modal || ui.drag = Abort
  || not (has_mouse ui owner || inside (Mouse.pos ui.win) r && grab_mouse ui owner) then
    `None
  else if Mouse.is_released `Left then
  (
    if Mouse.is_drag `Left then
      `Drop
    else
      `Click
  )
  else if Mouse.is_pressed `Right && Mouse.is_drag `Left then
  (
    ui.drag <- Abort;
    `Abort
  )
  else
  (
    let (mx, my) as m = Mouse.pos ui.win in
    match ui.drag with
    | No_drag when grab_mouse ui owner ->
      ui.drag <- Drag {pos = m; moved = false; inside = true};
      `Take
    | Drag {pos; moved; inside} when has_mouse ui owner ->
      let dx, dy = sub m pos in
      let dx' = if stepx = 0 then dx else dx / stepx in
      let dy' = if stepy = 0 then dy else dy / stepy in
      let pos = mx - dx mod max 1 stepx, my - dy mod max 1 stepy in
      let moved' = Mouse.is_drag `Left in
      let inside' = Api.inside m r in
      ui.drag <- Drag {pos; moved = moved'; inside = inside'};
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
    | Abort ->
      `None
    | _ ->
      (* Can happen after layout changes that invalidate origin *)
      unexpected_drag ui owner "drag_status"; `None
  )

let wheel_status ui r =
  if not ui.modal && inside (Mouse.pos ui.win) r then
    Mouse.wheel ui.win
  else
    (0.0, 0.0)

let key ui modkey focus = (key_status ui modkey focus = `Released)
let mouse ui owner area side =
  (mouse_status ui owner (dim ui area) side = `Released)
let drag ui owner area eps = drag_status ui owner (dim ui area) eps
let wheel ui area = wheel_status ui (dim ui area)


(* Decorative Widgets *)

let indicator ui c area on =
  let x, y, w, h = dim ui area in
  Draw.fill_circ ui.win x y w h (if on then c else unlit_color c);
  Draw.fill_circ ui.win (x + w/4) (y + h/4) (max 2 (w/4)) (max 2 (h/4))
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
  let m' = (h + 1) / 2 in
  let l = max 1 (min (if elem = `Dots then 4*w else w) h / 6) in
  let s = l * 3 / 2 in
  let d = s - l in
  match elem with
  | `N ->
    fill_rect ui.win (x + s) (y + 0) (w - 2*s) l c;
    fill_tri ui.win (x + d) (y + 0) (x + s) (y + l) (x + s) (y + 0) c;
    fill_tri ui.win (x + w - d) (y + 0) (x + w - s) (y + 0) (x + w - s) (y + l) c;
  | `S ->
    fill_rect ui.win (x + s) (y + h - l) (w - 2*s) l c;
    fill_tri ui.win (x + d) (y + h) (x + s) (y + h) (x + s) (y + h - l) c;
    fill_tri ui.win (x + w - d) (y + h) (x + w - s) (y + h - l) (x + w - s) (y + h) c;
  | `C ->
    fill_rect ui.win (x + s - l/2) (y + m - l/2) (w - 2*s + l/2*2) l c;
    let l2 = (l + 1)/2 in
    let s2 = d + l2 in
    fill_tri ui.win (x + d) (y + m) (x + s2) (y + m + l2) (x + s2) (y + m - l/2) c;
    fill_tri ui.win (x + w - s2) (y + m + l2) (x + w - d) (y + m) (x + w - s2) (y + m - l/2) c;
(*
    fill_tri ui.win (x + d) (y + m) (x + s2) (y + m) (x + s2) (y + m - l/2) c;
    fill_tri ui.win (x + d) (y + m) (x + s2) (y + m + l2) (x + s2) (y + m) c;
    fill_tri ui.win (x + w - d) (y + m) (x + w - s2) (y + m - l/2) (x + w - s2) (y + m) c;
    fill_tri ui.win (x + w - d) (y + m) (x + w - s2) (y + m) (x + w - s2) (y + m + l2) c;
*)
  | `NW ->
    fill_rect ui.win (x + 0) (y + s) l (m' - 2*s) c;
    fill_tri ui.win (x + 0) (y + d) (x + 0) (y + s) (x + l) (y + s) c;
    fill_tri ui.win (x + 0) (y + m' - d) (x + l) (y + m' - s) (x + 0) (y + m' - s) c;
  | `NE ->
    fill_rect ui.win (x + w - l) (y + s) l (m' - 2*s) c;
    fill_tri ui.win (x + w) (y + d) (x + w - l) (y + s) (x + w) (y + s) c;
    fill_tri ui.win (x + w) (y + m' - d) (x + w) (y + m' - s) (x + w - l) (y + m' - s) c;
  | `SW ->
    fill_rect ui.win (x + 0) (y + m + s) l (h - m - 2*s) c;
    fill_tri ui.win (x + 0) (y + m + d) (x + 0) (y + m + s) (x + l) (y + m + s) c;
    fill_tri ui.win (x + 0) (y + h - d) (x + l) (y + h - s) (x + 0) (y + h - s) c;
  | `SE ->
    fill_rect ui.win (x + w - l) (y + m + s) l (h - m - 2*s) c;
    fill_tri ui.win (x + w) (y + m + d) (x + w - l) (y + m + s) (x + w) (y + m + s) c;
    fill_tri ui.win (x + w) (y + h - d) (x + w) (y + h - s) (x + w - l) (y + h - s) c;
  | `Dots ->
    fill_rect ui.win (x + (w - l)/2) (y + h / 4) l l c;
    fill_rect ui.win (x + (w - l)/2) (y + 3 * h / 4) l l c

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

let image_size' ui (w, h) adjust img =
  let iw, ih = Image.size img in
  let q = float w /. float h in
  let iq = float iw /. float ih in
  let iw', ih' =
    match adjust with
    | `Crop `Vertical -> iw, int_of_float (float ih *. min 1.0 (iq /. q))
    | `Crop `Horizontal -> int_of_float (float iw /. min 1.0 (iq /. q)), ih
    | `Shrink -> iw, ih
  in
  let iq' = float iw' /. float ih' in
  if iq' > q then
    w, h - int_of_float (float h *. (1.0 -. q /. iq')), iw', ih'
  else
    w - int_of_float (float w *. (1.0 -. iq' /. q)), h, iw', ih'

let image ui area adjust img =
  let x, y, w, h = dim ui area in
  let w', h', iw', ih' = image_size' ui (w, h) adjust img in
  let x', y' =
    if adjust = `Shrink then
      x + (w - w')/2, y + (h - h')/2
    else x, y
  in
  Draw.image_part ui.win x' y' w' h' 0 0 iw' ih' 0.0 img

let image_size ui size adjust img =
  let w, h, _, _ = image_size' ui size adjust img in
  w, h


(* Passive Widgets *)

let widget ui owner_opt area ?(focus = false) modkey =
  let r = dim ui area in
  let mouse =
    match owner_opt with
    | None -> `Untouched
    | Some owner -> mouse_status ui owner r `Left
  in
  r,
  match mouse, key_status ui modkey focus with
  | `Released, _ | _, `Released -> `Released
  | `Pressed, _ | _, `Pressed -> `Pressed
  | `Hovered, _ | _, `Hovered -> `Hovered
  | _, _ -> `Untouched


let box ui area c =
  let (x, y, w, h), _ = widget ui None area no_modkey in
  Draw.fill_rect ui.win x y w h c

let color_text ui area align c inv active s =
  let (x, y, w, h), _status = widget ui None area no_modkey in
  let fg = mode c active in
  let bg = `Black in
  let fg, bg = if inv = `Inverted then bg, fg else fg, bg in
  Draw.fill_rect ui.win x y w h bg;
  let tw = Draw.text_width ui.win h (font ui h) s in
  let dx =
    match align with
    | `Left -> 0
    | `Center -> (w - min w tw + 1) / 2
    | `Right -> w - min w tw
  in
  if tw > w then Draw.clip ui.win x y w h;
  Draw.text ui.win (x + dx) y h fg (font ui h) s;
  if tw > w then Draw.unclip ui.win

let text ui area align =
  color_text ui area align (text_color ui)

let ticker ui area s =
  let (x, y, w, h), _status = widget ui None area no_modkey in
  Draw.fill_rect ui.win x y w h `Black;
  let tw = Draw.text_width ui.win h (font ui h) s in
  Draw.clip ui.win x y w h;
  let dx = if tw <= w then (w - tw)/2 else w - Draw.frame ui.win mod (w + tw) in
  Draw.text ui.win (x + dx) y h (fill ui true) (font ui h) s;
  Draw.unclip ui.win


(* Buttons *)

let invisible_button ui owner area mods modkey focus =
  let _, status = widget ui (Some owner) area no_modkey in
  focus && status = `Released && Key.are_modifiers_down mods ||
  key ui modkey focus

let button ui owner area ?(protrude = true) modkey focus active =
  let (x, y, w, h), status = widget ui (Some owner) area modkey ~focus in
  let img = get_img ui ui.img_button in
  let sx, sy, h' = if status = `Pressed then 800, 400, h + 1 else 0, 200, h in
  Api.Draw.image_part ui.win x y w h' sx sy w h' 0.0 img;
  let grey_left, shine_left = if status = `Pressed then 0x30, 0x20 else 0x50, 0x40 in
  Draw.fill_rect ui.win (x + 1) (y + 1) 1 (h - 2) (`Gray grey_left);
  mouse_focus ui (-1, x + 1, y + 1, 1, h - 2) (2 * w) shine_left 0;
  Draw.fill_rect ui.win (x + w - 1) (y + 1) 1 (h - 2) `Black;
  if protrude then
  (
    let grey_top, shine_top = if status = `Pressed then 0x00, 0x00 else 0x60, 0x40 in
    Draw.fill_rect ui.win (x + 1) (y + 1) (w - 3) 1 (`Gray grey_top);
    mouse_focus ui (-1, x + 1, y + 1, w - 3, 1) (2 * w) shine_top 0;
  )
  else
  (
    let grey_top, shine_top = if status = `Pressed then 0x00, 0x00 else 0x10, 0x20 in
    Draw.fill_rect ui.win (x + 1) (y + 1) (w - 3) 1 (`Gray grey_top);
    mouse_focus ui (-1, x + 1, y + 1, w - 3, 1) 20 shine_top 0;
  );
  (*Draw.rect ui.win x y (w - 1) h (border ui status);*)
  mouse_focus ui (-1, x, y, w - 1, h - 1) w 0x50 (-5);
  match active with
  | None -> false
  | Some active -> if status = `Released then not active else active

let labeled_button ui owner area ?(protrude = true) hsym c txt modkey focus active =
  let (x, y, w, h), status = widget ui (Some owner) area modkey ~focus in
  let result = button ui owner area ~protrude modkey focus active in
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
    Draw.fill_rect ui.win xsym ysym hsym hsym c
  | "||" ->
    Draw.fill_rect ui.win xsym ysym (hsym/3) hsym c;
    Draw.fill_rect ui.win (xsym + hsym - hsym/3) ysym (hsym/3) hsym c;
  | ">" ->
    Draw.fill_arrow ui.win xsym ysym hsym hsym c `Right
  | "<" ->
    Draw.fill_arrow ui.win xsym ysym hsym hsym c `Left
  | "/\\" ->
    Draw.fill_arrow ui.win xsym ysym hsym hsym c `Up
  | "\\/" ->
    Draw.fill_arrow ui.win xsym ysym hsym hsym c `Down
  | ">>" ->
    Draw.fill_arrow ui.win xsym ysym (hsym/2 + 1) hsym c `Right;
    Draw.fill_arrow ui.win (xsym + hsym/2) ysym (hsym/2 + 1) hsym c `Right;
  | "<<" ->
    Draw.fill_arrow ui.win xsym ysym (hsym/2) hsym c `Left;
    Draw.fill_arrow ui.win (xsym + hsym/2) ysym (hsym/2) hsym c `Left;
  | "^" ->
    Draw.fill_arrow ui.win xsym ysym hsym (hsym/2) c `Up;
    Draw.fill_rect ui.win xsym (ysym + hsym - hsym/3) hsym (hsym/3) c;
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

let progress_bar ui owner area l f_opt v =
  let (x, y, w, h), status = widget ui (Some owner) area no_modkey in
  let w = quant_floor l w in
  let w' = quant_ceil l (int_of_float (v *. float w)) in
  Draw.fill_rect ui.win x y w h (fill ui false);
  Draw.fill_rect ui.win x y w' h (fill ui true);
  for i = 0 to w / l / 2 - 1 do
    Draw.fill_rect ui.win (x + (2*i + 1)*l) y l h `Black
  done;
  (*Draw.rect ui.win x y w h (border ui status);*)
  if status <> `Untouched then
  (
    Option.iter (fun f ->
      let mx, _ = Mouse.pos ui.win in
      let s, th, color = f (float (mx - x) /. float w) in
      let font = font ui th in
      let tw = Draw.text_width ui.win th font s in
      let tx = clamp (x + 1) (x + w - tw - 1) (mx - tw/2) in
      let ty = y + (h - th)/2 in
      Draw.text ui.win tx ty th color font s
    ) f_opt
  );
  if status <> `Pressed then v else
  let mx, _ = Mouse.pos ui.win in
  clamp 0.0 1.0 (float (mx - x) /. float w)


let volume_bar ui owner area l v =
  let (x, y, w, h), status = widget ui (Some owner) area no_modkey in
  let h = quant_floor l h in
  let h' = quant_ceil l (int_of_float ((1.0 -. v) *. float h)) in
  Draw.fill_rect ui.win (x + w - 2) y 2 h (fill ui true);
  Draw.fill_tri ui.win (x + 2) y (x + w - 2) (y + h) (x + w - 2) y (fill ui true);
  Draw.fill_rect ui.win x y w h' (`Trans (`Black, 0x100 - unlit_alpha));
  for j = 0 to h / l / 2 - 1 do
    Draw.fill_rect ui.win x (y + (2*j + 1)*l) w l `Black
  done;
  if status <> `Pressed then v else
  let _, my = Mouse.pos ui.win in
  clamp 0.0 1.0 (float (y + h - my) /. float h)


type drag += Scroll_bar_page of {last_repeat : time}
type drag += Scroll_bar_drag of {value : float; mx : int; my : int}

let _ =
  let f' = !string_of_drag in
  string_of_drag := function
    | Scroll_bar_page _ -> "Scroll_bar_page"
    | Scroll_bar_drag _ -> "Scroll_bar_drag"
    | drag -> f' drag

let scroll_bar ui owner area l orient v len =
  assert (v +. len < 2.0); (* at most 1 line over 1.0, but line may be a page *)
  let (x, y, w, h), status = widget ui (Some owner) area no_modkey in
  let w, h =
    match orient with
    | `Vertical -> w, quant_floor l h
    | `Horizontal -> quant_floor l w, h
  in
  Draw.fill_rect ui.win x y w h (fill ui false);
  let x', y', w', h' as r =
    match orient with
    | `Vertical ->
      let h' = quant_floor l (int_of_float (Float.ceil (len *. float (h - 2)))) in
      let h'' = max h' w in  (* minimum bar size *)
      let dy = quant_ceil l (int_of_float (v *. float (h - 2 - (h'' - h')))) in
      x, y + dy, w, h''
    | `Horizontal ->
      let w' = quant_floor l (int_of_float (Float.ceil (len *. float (w - 2)))) in
      let w'' = max w' h in  (* minimum bar size *)
      let dx = quant_ceil l (int_of_float (v *. float (w - 2 - (w'' - w')))) in
      x + dx, y, w'', h
  in
  if len < 1.0 then Draw.fill_rect ui.win x' y' w' h' (fill ui true);
  (match orient with
  | `Vertical ->
    for j = 0 to h / l / 2 - 1 do
      Draw.fill_rect ui.win x (y + (2*j + 1)*l) w l `Black
    done
  | `Horizontal ->
    for i = 0 to w / l / 2 - 1 do
      Draw.fill_rect ui.win (x + (2*i + 1)*l) y l h `Black
    done
  );
  (*Draw.rect ui.win x y w h (border ui status);*)
  if status <> `Pressed then v else
  let (mx, my) as m = Mouse.pos ui.win in
  let v0, mx0, my0, last_repeat, dragging =
    match ui.drag with
    | No_drag when grab_mouse ui owner -> v, mx, my, 0.0, false
    | Scroll_bar_page {last_repeat} -> v, mx, my, last_repeat, false
    | Scroll_bar_drag {value; mx; my} -> value, mx, my, 0.0, true
    | _ -> unexpected_drag ui "scroll_bar" owner; v, mx, my, 0.0, false
  in
  if not (has_mouse ui owner) then v else
  let now = Unix.gettimeofday () in
  let v' =
    if dragging || inside m r then
    (
      ui.drag <- Scroll_bar_drag {value = v0; mx = mx0; my = my0};
      match orient with
      | `Vertical -> v0 +. float (my - my0) /. float (h - 2)
      | `Horizontal -> v0 +. float (mx - mx0) /. float (w - 2)
    )
    else if now -. last_repeat > 0.3 (* TODO: use config *) then
    (
      ui.drag <- Scroll_bar_page {last_repeat = now};
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

type drag += Divide of {offset : size}

let _ =
  let f' = !string_of_drag in
  string_of_drag := function
    | Divide _ -> "Divide"
    | drag -> f' drag

let divider2 ui owner area cursor (vx, vy) (minx, miny) (maxx, maxy) (snapx1, snapy1) (snapx2, snapy2) =
  let (x, y, w, h), status = widget ui (Some owner) area no_modkey in
  if not (has_mouse ui owner) then (vx, vy), false else
  let mouse = Mouse.pos ui.win in
  let vx', vy' =
    match ui.drag with
    | No_drag when status <> `Untouched ->
      ui.drag <- Divide {offset = sub mouse (x, y)}; vx, vy
    | Divide {offset} -> add (vx, vy) (sub (sub mouse offset) (x, y))
    | _ -> 0, 0
  in
  if status <> `Untouched then Mouse.set_cursor ui.win (`Resize cursor);
  (*Draw.rect ui.win x y w h (border ui status);*)
  if status <> `Pressed then (vx, vy), true else
  let p, _, _, _, _ = area in
  let _, _, pw, ph = dim ui (p, 0, 0, -1, -1) in
  let maxx = if maxx < 0 then pw else maxx in
  let maxy = if maxy < 0 then ph else maxy in
  let snapx1 = if snapx1 < 0 then min_int else snapx1 in
  let snapy1 = if snapy1 < 0 then min_int else snapy1 in
  let snapx2 = if snapx2 < 0 then max_int else snapx2 in
  let snapy2 = if snapy2 < 0 then max_int else snapy2 in
  ( snap snapx1 snapx2 (clamp minx maxx vx'),
    snap snapy1 snapy2 (clamp miny maxy vy')
  ), true

let divider ui owner area orient v minv maxv =
  let x, y, _, _ = dim ui area in
  let proj = match orient with `Horizontal -> fst | `Vertical -> snd in
  let inj v = match orient with `Horizontal -> v, y | `Vertical -> x, v in
  let cursor = match orient with `Horizontal -> `E_W | `Vertical -> `N_S in
  let vv, b =
    divider2 ui owner area cursor (inj v) (inj minv) (inj maxv) (-1, -1) (-1, -1) in
  proj vv, b


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


let edit_text ui owner area ph s scroll selection c focus =
  let (x, y, w, h), status = widget ui (Some owner) area no_modkey in
  let len = String.length s in
  let ch = max 1 (h - 2 * ph) in
  let font = font ui ch in

  let focus' = focus || status = `Pressed in
  let selection' =
    if status <> `Pressed then selection else
    let mx, _ = Mouse.pos ui.win in
    let i = find_pos ui (mx - x + scroll) h font s in
    let lprim, rprim, _ = Option.value selection ~default: (i, i, i) in
    if Key.are_modifiers_down [] then
      if Mouse.is_pressed `Left then
        if Mouse.is_triple_click `Left then
          Some (0, 0, len)
        else if Mouse.is_double_click `Left then
          let j = find_next_word s i in
          Some (find_prev_word s j, j, j)
        else
          Some (i, i, i)
      else if Mouse.is_drag `Left then
        if Mouse.is_double_click `Left then
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
      if Mouse.is_triple_click `Left then
        Some (0, 0, len)
      else if Mouse.is_double_click `Left then
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
    Draw.text ui.win (x - scroll) (y + ph) ch c font s;
    Draw.unclip ui.win;
    s, scroll, None, Uchar.of_int 0

  | Some (lprim, rprim, sec) ->
    let lprim, rprim, sec = min lprim len, min rprim len, min sec len in
    let l, r = min lprim sec, max rprim sec in
    let sl = String.sub s 0 l in
    let sm = String.sub s l (r - l) in
    let sr = String.sub s r (len - r) in
    let ws = Draw.text_spacing ui.win ch font in
    let wl = Draw.text_width ui.win ch font sl + ws in
    let wm = Draw.text_width ui.win ch font sm + ws in
    let wt = if lprim >= sec then wl else wl + wm in
    let wc = 1 in
    let scroll' =
      if wt < scroll then wt else
      if wt + ws + wc > w + scroll then wt + ws + wc - w else scroll
    in

    Draw.clip ui.win x y w h;
    if not focus' then
    (
      Draw.text ui.win (x - scroll') (y + ph) ch c font s;
    )
    else if l = r then
    (
      Draw.text ui.win (x - scroll') (y + ph) ch c font s;
      Draw.fill_rect ui.win (x - scroll' + wl) y 1 h c;
    )
    else
    (
      Draw.fill_rect ui.win (x - scroll' + wl) y wm h c;
      Draw.text ui.win (x - scroll') (y + ph) ch c font sl;
      Draw.text ui.win (x - scroll' + wl) (y + ph) ch `Black font sm;
      Draw.text ui.win (x - scroll' + wl + wm) (y + ph) ch c font sr;
    );
    Draw.unclip ui.win;

    if ui.modal || not focus' then s, scroll', None, Uchar.of_int 0 else

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


let rich_edit_text ui owner area ph highlight c (edit : Edit.t) =
  let _, _, _, h = dim ui area in
  let s', scroll', sel', ch =
    edit_text ui owner area ph edit.text edit.scroll edit.sel_range c edit.focus
  in
  if highlight && edit.focus then focus ui area (h / 2);
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

let flex_total w gw cols =
  let mw = table_pad gw in  (* inner width padding *)
  max 0 (w - Iarray.fold_left (fun w (cw, _) -> w + cw + gw) (2 * mw - gw + 1) cols)

let fix_w flex cw = if cw < 0 then flex / (- cw) else cw

let draw_table ui area gw ch ph cols rows hscroll =
  let x, y, w, h = dim ui area in
  Draw.fill_rect ui.win x y w h `Black;
  let rh = ch + 2 * ph in
  let mw = table_pad gw in  (* inner width padding *)
  let flex = flex_total w gw cols in
  let font = font ui ch in
  (* Draw row background first since it must be unclipped. *)
  Iarray.iteri (fun j (fg, inv, _contents) ->
    let ry = y + j * rh in
    let bg = if j mod 2 = 0 then `Black else `Gray 0x20 in
    let bg = if inv = `Inverted then fg else bg in
    if bg <> `Black then Draw.fill_rect ui.win x ry w rh bg
  ) rows;
  let cx = ref (x + mw - hscroll) in
  Iarray.iteri (fun i (cw, align) ->
    let cw = fix_w flex cw in
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
        Api.Draw.image_part ui.win !cx cy cw ch 0 0 iw ih' 0.0 img;
      )
    ) rows;
    Draw.unclip ui.win;
    cx := !cx + cw + gw;
  ) cols

let find_column w gw cols hscroll dx =
  let mw = table_pad gw in
  let flex = flex_total w gw cols in
  let rec find i cx =
    if i = Iarray.length cols then None else
    let cx' = cx + fix_w flex (fst (Iarray.get cols i)) in
    if dx >= cx && dx < cx' then Some i else
    if dx >= cx' then find (i + 1) (cx' + gw) else
    None
  in find 0 (mw - hscroll)

let find_gutter w gw cols hscroll dx =
  let mw = table_pad gw in
  let flex = flex_total w gw cols in
  let gutter_tolerance = 5 in
  let rec find i cx =
    if i = Iarray.length cols then `None else
    let cx' = cx + fix_w flex (fst (Iarray.get cols i)) in
    if abs (cx' + gw/2 - dx) < gutter_tolerance then `Gutter i else
    if cx' + gw/2 < dx then find (i + 1) (cx' + gw) else
    `Header i
  in find 0 (mw - hscroll)

let table ui owner area gw ch ph cols rows hscroll =
  let (x, y, w, _), status = widget ui (Some owner) area no_modkey in
  draw_table ui area gw ch ph cols rows hscroll;
  if status = `Pressed || status = `Released then
    let mx, my = Mouse.pos ui.win in
    let rh = ch + 2 * ph in
    Some ((my - y) / rh), find_column w gw cols hscroll (mx - x)
  else
    None, None


(* Table Headers *)

let symbols_asc = [|"▲" (* "▴" *); "▲'" (* "△", "▵", "▵" *); "▲''"; "▲'''"|]
let symbols_desc = [|"▼" (* "▾" *); "▼'" (* "▽", "▾", "▿" *); "▼''"; "▼'''"|]

type drag += Header_resize of {mouse_x : int; col : int}
type drag += Header_reorder of {mouse_x : int; col : int; moved : bool}

let _ =
  let f' = !string_of_drag in
  string_of_drag := function
    | Header_resize _ -> "Header_resize"
    | Header_reorder _ -> "Header_reorder"
    | drag -> f' drag

let header ui owner area ph gw cols (titles, sorting) hscroll =
  let (x, y, w, h) as r, status = widget ui (Some owner) area no_modkey in
  let texts = Iarray.map (fun s -> `Text s) titles in
  let th = h - 2 * ph in
  ignore (table ui owner area gw th ph cols
    [|text_color ui, `Inverted, texts|] hscroll);

  let mw = table_pad gw in
  let flex = max 0  (* mirrors draw_table *)
    (w - Iarray.fold_left (fun w (cw, _) -> w + cw + gw) (2 * mw - gw + 1) cols) in
  Draw.clip ui.win x y w h;
  ignore (
    Iarray.fold_left (fun cx (cw, _) ->
      let cw = if cw < 0 then flex / (- cw) else cw in
      Draw.fill_rect ui.win (cx + cw + gw/2 - hscroll) y 1 h `Black;
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

  let find_gutter cols mx = find_gutter w gw cols hscroll (mx - x) in
  let find_column cols mx = find_column w gw cols hscroll (mx - x) in

  let mx, my = Mouse.pos ui.win in
  if not (has_mouse ui owner || inside (mx, my) r && grab_mouse ui owner) then
    `None
  else
  match ui.drag with
  | No_drag ->
    (match find_gutter cols mx with
    | `None when status = `Released ->
      (match find_column cols mx with
      | None -> `None
      | Some i -> `Click i
      )
    | `None ->
      if not ui.modal && Mouse.is_pressed `Right && not (Mouse.is_down `Middle)
      && not (Option.exists (inside (Mouse.pos ui.win)) ui.modal_rect) then
        `Menu None
      else
        `None
    | `Gutter col ->
      Mouse.set_cursor ui.win (`Resize `E_W);
      if status = `Pressed then
        ui.drag <- Header_resize {mouse_x = mx; col};
      `None
    | `Header col ->
      if not ui.modal && Mouse.is_pressed `Right && not (Mouse.is_down `Middle)
      && not (Option.exists (inside (Mouse.pos ui.win)) ui.modal_rect) then
        `Menu (Some col)
      else if status = `Pressed then
      (
        ui.drag <- Header_reorder {mouse_x = mx; col; moved = false};
        `None
      )
      else `None
    )

  | Header_resize {mouse_x; col = i} when status = `Pressed ->
    Mouse.set_cursor ui.win (`Resize `E_W);
    let dx = mx - mouse_x in
    if dx = 0 then `None else
    let len = Iarray.length cols in
    let ws = Array.init len (fun j -> fix_w flex (fst (Iarray.get cols j))) in
    ws.(i) <- max 0 (ws.(i) + dx);
    if i + 1 < len && (fst (Iarray.get cols i) < 0 || is_shift_down ()) then
      ws.(i + 1) <- max 0 (ws.(i + 1) - dx);
    ui.drag <- Header_resize {mouse_x = mx; col = i};
    `Resize (Iarray.of_array ws)

  | Header_reorder {mouse_x; col = i; moved} when status = `Pressed ->
    if moved then Mouse.set_cursor ui.win `Point;
    let dx = mx - mouse_x in
    if dx = 0 then `None else
    let _ = ui.drag <- Header_reorder {mouse_x; col = i; moved = true} in
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
        ui.drag <- Header_reorder {mouse_x = mx; col = j; moved = true};
        `Reorder perm
      | _ -> `None
    )

  | Header_reorder {col = i; moved = false; _} when status = `Released ->
    `Click i

  | _ ->
    `None


(* Rich Tables *)

type cached = buffer

type rich_table_style =
  { gutter_w : int;
    text_h : int;
    pad_h : int;
    scroll_w : int;
    scroll_h : int;
    scroll_l : int;
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
  | `Abort
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

let rich_table_inner_area _ui area sty =
  let p, ax, ay, aw, ah = area in
  let ty = if not sty.has_heading then ay else ay + sty.text_h + 2 * sty.pad_h + 2 in
  let tw =
    aw - (if sty.scroll_w = 0 then 0 else sty.scroll_w + 1)
  in
  let th =
    ah -
    (if ah < 0 then 0 else ty - ay) -
    (if sty.scroll_h = 0 then 0 else sty.scroll_h + 1)
  in
  (p, ax, ty, tw, th)

let rich_table_mouse ui area sty cols (tab : _ Table.t) =
  let area' = rich_table_inner_area ui area sty in
  let (x, y, w, _) as r = dim ui area' in
  let (mx, my) as m = Mouse.pos ui.win in
  if inside m r then
    let row = (my - y) / (sty.text_h + 2 * sty.pad_h) + tab.vscroll in
    Some (
      (if row < Table.length tab then Some row else None),
      find_column w sty.gutter_w cols tab.hscroll (mx - x)
    )
  else
    None

let rich_table_drag ui area sty style tab =
  match rich_table_mouse ui area sty [||] tab with
  | Some (i_opt, _) ->
    let area' = rich_table_inner_area ui area sty in
    let x, y, w, _ = dim ui area' in
    let rh = sty.text_h + 2 * sty.pad_h in
    let i' = Option.value i_opt ~default: (Table.length tab) - tab.vscroll in
    focus' ui x (y + i' * rh) w rh (rh / 2) `White style
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

let rich_table ui owner area (sty : rich_table_style) cols header_opt
  (tab : _ Table.t) pp_row =
  assert (sty.has_heading = Option.is_some header_opt);
  let p, ax, ay, aw, ah = area in
  let rh = sty.text_h + 2 * sty.pad_h in
  let _, tx, ty, tw, th = rich_table_inner_area ui area sty in
  let header_area = (p, ax, ay, tw, rh) in
  let table_area = (p, ax, ty, tw, th) in
  let vscroll_area =
    (p, (if aw < 0 then tw else ax + tw) + 1, ay, sty.scroll_w, ah) in
  let hscroll_area =
    (p, ax, (if ah < 0 then ah - sty.scroll_h else ty + th + 1), tw, sty.scroll_h) in
  let (x, y, w, h) as r = dim ui table_area in

  let shift = is_shift_down () in
  let command = is_command_down () in

  Mutex.protect tab.mutex (fun () ->
    let len = Table.length tab in
    let page = max 1 (int_of_float (Float.trunc (float h /. float rh))) in
    let limit = min len (tab.vscroll + page) in
    (* Correct scrolling position for possible resize *)
    Table.adjust_vscroll tab tab.vscroll 1 page;

    (* Body *)
    let buf = adjust_cache ui tab w h in
    if not ui.buffered || tab.dirty || Draw.frame ui.win mod 10 = 7 then
    (
      let page' = max 1 (int_of_float (Float.round (float h /. float rh))) in
      let rows =
        Iarray.init (min page' (len - tab.vscroll)) (fun i ->
          let i = tab.vscroll + i in
          let c, cols = pp_row i in
          let inv = if Table.is_selected tab i then `Inverted else `Regular in
          c, inv, cols
        )
      in
      if ui.buffered then Draw.buffered ui.win buf;
      let area' = if ui.buffered then (-1, 0, 0, w, h) else table_area in
      draw_table ui area' sty.gutter_w sty.text_h sty.pad_h cols rows tab.hscroll;
      if ui.buffered then Draw.unbuffered ui.win;
      Table.clean tab;
    );
    if ui.buffered then Draw.buffer ui.win x y buf;

    let mx, my = Mouse.pos ui.win in
    let i = tab.vscroll + (my - y) / rh in
    let _, status = widget ui (Some (owner ^ ":body")) table_area no_modkey in
    (* Mirrors logic in table *)
    let left_mouse_used = (status = `Pressed || status = `Released) in

    let find_column cols mx =
      find_column w sty.gutter_w cols tab.hscroll (mx - x) in

    let result =
      if not ui.modal && ui.drag = No_drag
      && Mouse.is_pressed `Right && not (Mouse.is_down `Middle)
      && not (Option.exists (inside (Mouse.pos ui.win)) ui.modal_rect) then
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
        match drag_status ui (owner ^ ":body") r (max_int, rh) with
        | `None -> `None

        | `Take ->
          (* Click *)
          let col = find_column cols mx in
          if i >= limit then
          (
            (* Click on empty space *)
            if not Mouse.(is_double_click `Left) then
              Table.deselect_all tab;
            `Click (None, col)
          )
          else
          (
            (* Click on entry *)
            if not Mouse.(is_double_click `Left || is_triple_click `Left)
            && not (Table.is_selected tab i) then
            (
              Table.deselect_all tab;
              Table.select tab i i;
            );
            `Click (Some i, col)
          )

        | `Click ->
          (* Click-release: deselect all except for clicked entry *)
          if not Mouse.(is_double_click `Left || is_triple_click `Left) then
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
            if not Mouse.(is_double_click `Left || is_triple_click `Left) then
              Table.select tab i i;
            `Click (Some i, col)
          )

        | `Drag ((_, dy), motion, traj) -> `Drag (dy, motion, traj)

        | `Drop -> `Drop

        | `Abort -> `Abort
      )
      else if command && not ui.modal && Mouse.is_pressed `Left then
      (
        (* Cmd-click on entry: toggle selection of clicked entry *)
        let col = find_column cols mx in
        if i >= limit then
          `Click (None, col)
        else
        (
          if not Mouse.(is_triple_click `Left) then
          (
            if Table.is_selected tab i then
              Table.deselect tab i i
            else
              Table.select tab i i
          );
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
        match
          header ui (owner ^ ":header") header_area sty.pad_h sty.gutter_w
            cols heading tab.hscroll
        with
        | `Click i -> Table.dirty tab; `Sort i
        | `Resize ws -> Table.dirty tab; `Resize ws
        | `Reorder perm -> Table.dirty tab; `Reorder perm
        | `Menu i -> `HeadMenu i
        | `None -> result
    in

    (* Vertical scrollbar *)
    let wdx, wdy = wheel_status ui (dim ui area) in
    let wdx, wdy = if Float.abs wdx > Float.abs wdy then wdx, 0.0 else 0.0, wdy in
    let result, vwheel =
      if sty.scroll_w = 0 then result, true else
      let vwheel = not shift && len > page || wdy = 0.0 in
      let h' = page * rh in
      let ext = if len = 0 then 1.0 else min 1.0 (float h' /. float (len * rh)) in
      let pos = if len = 0 then 0.0 else float tab.vscroll /. float len in
      let coeff = max 1.0 (float page /. 4.0) /. float (max 1 len) in
      let wheel = if vwheel then coeff *. wdy else 0.0 in
      let pos' =
        scroll_bar ui (owner ^ ":vscroll") vscroll_area sty.scroll_l
          `Vertical pos ext -. wheel
      in
      if result <> `None || pos = pos' then result, vwheel else
      (
        Table.set_vscroll tab
          (max 0 (int_of_float (Float.round (pos' *. float len)))) 1 page;
        `Scroll, vwheel
      )
    in

    (* Horizontal scrollbar *)
    let result =
      if sty.scroll_h = 0 then result else
      let vw = Iarray.fold_left (fun w (cw, _) -> w + cw + sty.gutter_w) 2 cols in
      let vw' = max vw (tab.hscroll + w) in
      let ext = if vw' = 0 then 1.0 else min 1.0 (float w /. float vw') in
      let pos = if vw' = 0 then 0.0 else float tab.hscroll /. float vw' in
      let wheel = if vwheel then wdx else wdy in
      let pos' =
        scroll_bar ui (owner ^ ":hscroll") hscroll_area sty.scroll_l
          `Horizontal pos ext -. 0.05 *. wheel
      in
      if result <> `None || pos = pos' then result else
      (
        Table.set_hscroll tab
          (clamp 0 (max 0 (vw' - w)) (int_of_float (Float.round (pos' *. float vw'))));
        `Scroll
      )
    in

    (* Focus and mouse reflection *)
    if tab.focus then focus ui table_area (rh / 2);
    mouse_focus ui area sty.refl_r 0x20 0;

    (* Keys *)
    let result =
      if result <> `None || ui.modal || not tab.focus then result else
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
              Table.adjust_vscroll tab i 1 page;
              `Select
            )
            else
            (
              Table.set_vscroll tab i 1 page;
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
            Table.adjust_vscroll tab i 1 page;
            `Select
          )
          else if command && has_sel then
          (
            (* Cmd-cursor movement: move selection *)
            Table.adjust_vscroll tab i 1 page;
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
              Table.set_vscroll tab i 1 page;
              `Scroll
            )
            else `None
          )
          else `None
        )
      )
    in

    let result =
      if result <> `None || not tab.focus || sty.scroll_h = 0 then result else
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

let browser_entry_text_area ui area sty (tab : _ Table.t) i nest folded =
  let p, x, y, w, _ = rich_table_inner_area ui area sty in
  let mw = (sty.gutter_w + 1) / 2 in  (* inner width padding *)
  let correction = if Api.is_mac then -2 else +1 in
  let dx = max 0
    (Draw.text_width ui.win sty.text_h (font ui sty.text_h)
      (browser_pp_pre nest folded) + mw - tab.hscroll + correction)
  and dy = (i - tab.vscroll) * (sty.text_h + 2 * sty.pad_h) in
  (p, x + dx, y + dy + sty.pad_h, (if w < 0 then w else w - dx), sty.text_h)

let browser ui owner area sty (tab : _ Table.t) pp_entry =
  let cols : _ iarray = [|-1, `Left|] in
  let pp_row i : _ * _ iarray =
    let nest, folded, c, name = pp_entry i in
    c, [|`Text (browser_pp_pre nest folded ^ name)|]
  in

  let selected = tab.selected in
  (match rich_table ui owner area sty cols None tab pp_row with
  | `None -> `None
  | `Scroll -> `Scroll
  | `Move i -> `Move i
  | `Drag (i, motion, traj) -> `Drag (i, motion, traj)
  | `Drop -> `Drop
  | `Abort -> `Abort
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
      Draw.text_width ui.win sty.text_h (font ui sty.text_h)
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
  Draw.fill_rect ui.win x y w h `Black;
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
          Draw.fill_rect ui.win (cx - 1) (cy - 1) (iw + 2) (iw + ch + 2 * ph + 2) bg;
        let iw', ih' = Image.size img in
        let scale = float iw /. float (max iw' ih') in
        let dx = int_of_float ((float iw -. scale *. float iw') /. 2.0) in
        let dy = int_of_float ((float iw -. scale *. float ih') /. 2.0) in
        Api.Draw.image_part ui.win (cx + dx) (cy + dy) (iw - 2*dx) (iw - 2*dy) 0 0 iw' ih' 0.0 img;
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


let grid ui owner area gw iw ch ph matrix =
  let (x, y, _, _), status = widget ui (Some owner) area no_modkey in
  draw_grid ui area gw iw ch ph matrix;
  if status = `Pressed || status = `Released then
    let mx, my = Mouse.pos ui.win in
    Some ((mx - x) / (iw + gw), (my - y) / (iw + ch + gw))
  else
    None


type grid_table_style =
  { gutter_w : int;
    img_h : int;
    text_h : int;
    pad_h : int;
    scroll_w : int;
    scroll_l : int;
    refl_r : int;
    has_heading : bool
  }

type grid_table_action = rich_table_action

let grid_table_inner_area _ui area sty =
  let p, ax, ay, aw, ah = area in
  let ty = if not sty.has_heading then ay else ay + sty.text_h + 2 in
  let tw = aw - (if sty.scroll_w = 0 then 0 else sty.scroll_w + 1) in
  let th = ah - (if ah < 0 then 0 else ty - ay) in
  (p, ax, ty, tw, th)

let grid_table_mouse ui area sty (tab : _ Table.t) =
  let area' = grid_table_inner_area ui area sty in
  let (x, y, w, _) as r = dim ui area' in
  let iw = sty.gutter_w + sty.img_h in
  let ih = iw + sty.text_h in
  let line = max 1 Float.(to_int (floor (float w /. float iw))) in
  let vscroll = tab.vscroll / line * line in
  let (mx, my) as m = Mouse.pos ui.win in
  if inside m r then
    let row = (my - y) / ih * line + (mx - x) / iw + vscroll in
    Some ((if row < Table.length tab then Some row else None), None)
  else
    None

let grid_table_drag ui area sty style tab =
  match grid_table_mouse ui area sty tab with
  | Some (i_opt, _) ->
    let area' = grid_table_inner_area ui area sty in
    let x, y, w, _ = dim ui area' in
    let iw = sty.gutter_w + sty.img_h in
    let ih = iw + sty.text_h in
    let line = max 1 Float.(to_int (floor (float w /. float iw))) in
    let vscroll = tab.vscroll / line * line in
    let i' = Option.value i_opt ~default: (Table.length tab) - vscroll in
    focus' ui (x + i' mod line * iw) (y + i' / line * ih) iw ih (sty.text_h / 2) `White style
  | _ -> ()

let grid_table ui owner area (sty : grid_table_style) header_opt
  (tab : _ Table.t) pp_cell =
  assert (sty.has_heading = Option.is_some header_opt);
  let p, ax, ay, aw, ah = area in
  let ch = sty.text_h + 2 * sty.pad_h in
  let _, _, ty, tw, th = grid_table_inner_area ui area sty in
  let header_area = (p, ax, ay, tw, ch) in
  let table_area = (p, ax, ty, tw, th) in
  let vscroll_area =
    (p, (if aw < 0 then tw else ax + aw + 1), ay, sty.scroll_w, ah) in
  let (x, y, w, h) as r = dim ui table_area in

  let shift = is_shift_down () in
  let command = is_command_down () in

  Mutex.protect tab.mutex (fun () ->
    let len = Array.length tab.entries in
    let iw = sty.gutter_w + sty.img_h in
    let ih = iw + ch in
    let line = max 1 Float.(to_int (floor (float w /. float iw))) in
    let page =
      max line Float.(to_int (floor (float h /. float ih)) * line) in
    let page_ceil =
      max line Float.(to_int (ceil (float h /. float ih)) * line) in
    (* Correct scrolling position for possible resize *)
    Table.adjust_vscroll tab tab.vscroll line page;

    (* Body *)
    let vscroll = tab.vscroll / line * line in
    let buf = adjust_cache ui tab w h in
    if not ui.buffered || tab.dirty then
    (
      let matrix =
        Iarray.init (page_ceil / line) (fun j ->
          Iarray.init line (fun i ->
            let k = vscroll + j * line + i in
            if k >= len then None else
            let img, c, txt = pp_cell k in
            let inv = if Table.is_selected tab k then `Inverted else `Regular in
            Some (img, c, inv, txt)
          )
        )
      in
      if ui.buffered then Draw.buffered ui.win buf;
      let area' = if ui.buffered then (-1, 0, 0, w, h) else table_area in
      draw_grid ui area' sty.gutter_w sty.img_h sty.text_h sty.pad_h matrix;
      if ui.buffered then Draw.unbuffered ui.win;
      Table.clean tab;
    );
    if ui.buffered then Draw.buffer ui.win x y buf;

    let mx, my = Mouse.pos ui.win in
    let i, j = (mx - x) / iw, (my - y) / ih in
    let k = vscroll + j * line + i in
    let on_bg = i >= line || k >= min len (vscroll + page_ceil) in

    let _, status = widget ui (Some (owner ^ ":body")) table_area no_modkey in
    (* Mirrors logic in grid *)
    let left_mouse_used = (status = `Pressed || status = `Released) in

    let result =
      if not ui.modal && Mouse.is_pressed `Right && not (Mouse.is_down `Middle)
      && not (Option.exists (inside (Mouse.pos ui.win)) ui.modal_rect) then
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
        match drag_status ui (owner ^ ":body") r (iw, ih) with
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
            if not (Mouse.is_double_click `Left) then
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

        | `Abort -> `Abort
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
      | Some ((titles, _) as heading) ->
        let cw = max 5 (w / Iarray.length titles - sty.gutter_w) in
        let cols = Iarray.map (Fun.const (cw, `Left)) titles in
        match
          header ui (owner ^ ":header") header_area sty.pad_h sty.gutter_w
            cols heading tab.hscroll
        with
        | `Click i -> `Sort i
        | `Resize ws -> `None
        | `Reorder perm -> `Reorder perm
        | `Menu i -> `HeadMenu i
        | `None -> result
    in

    (* Vertical scrollbar *)
    let result =
      if sty.scroll_w = 0 then result else
      let len' = (len + line - 1)/line * line in (* round to multiple of line *)
      let ext = if len = 0 then 1.0 else min 1.0 (float page /. float len') in
      let pos = if len = 0 then 0.0 else float tab.vscroll /. float len' in
      let coeff = max 1.0 (float line) /. float (len' - page) in
      let (hx, hy, hw, hh) = dim ui header_area in
      let wheel = coeff *. snd (wheel_status ui (hx, hy, hw, hh + h)) in
      let pos' =
        scroll_bar ui (owner ^ ":scroll") vscroll_area sty.scroll_l
          `Vertical pos ext -. wheel
      in
      if result <> `None || pos = pos' then result else
      (
        Table.set_vscroll tab
          (int_of_float (Float.round (pos' *. float len'))) line page;
        `Scroll
      )
    in

    (* Focus and mouse reflection *)
    if tab.focus then focus ui table_area (sty.text_h / 2);
    mouse_focus ui area sty.refl_r 0x20 0;

    (* Keys *)
    let result =
      if result <> `None || ui.modal || not tab.focus then result else
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
            0, if not shift then vscroll else if d < 0 then len else -1 in
          let pos1, pos2 = Option.value tab.sel_range ~default in
          let i = if d < 0 then max 0 (pos2 + d) else min (len - 1) (pos2 + d) in

          if not (shift || command) then
          (
            (* Plain cursor movement: deselect all, reselect relative to range end *)
            if has_sel then
            (
              Table.deselect_all tab;
              Table.select tab i i;
              Table.adjust_vscroll tab i line page;
              `Select
            )
            else
            (
              Table.set_vscroll tab i line page;
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
            Table.adjust_vscroll tab i line page;
            `Select
          )
          else if command && has_sel then
          (
            (* Cmd-cursor movement: move selection *)
            Table.adjust_vscroll tab i line page;
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
              Table.adjust_vscroll tab ((i + line - 1) / line) line page;
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


(* Settings *)

type setting = string * setting_item
and setting_item =
  [ `Flag of bool * (bool -> unit)
  | `Choice of (string * bool * (string -> unit)) list
  | `Text of Edit.t * color * (Edit.t -> unit) * (string -> unit)
  | `Number of string * Edit.t * int * int * int * (Edit.t -> unit) * (int -> unit)
  | `Button of string * (unit -> unit)
  | `Section of setting list
  ]

type settings_style =
  { margin : int;
    item_h : int;
    label_h : int;
    pad_w : int;
    pad_h : int;
    sep_h : int;
    sep_w : int;
    indent_w : int;
    scroll_w : int;
    scroll_l : int;
  }

let rec settings_w ui sty = function
  | [] -> 0, 0
  | setting :: settings ->
    let lw1, rw1 = setting_w ui sty setting in
    let lw', rw' = settings_w ui sty settings in
    max lw1 lw', max rw1 rw'

and setting_w ui sty (name, item) =
  let lw = Draw.text_width ui.win sty.item_h (font ui sty.item_h) name in
  let lw', rw = item_w ui sty item in
  max lw lw', max rw (lw - lw' - sty.sep_w)

and item_w ui sty = function
  | `Flag _ | `Text _ | `Number _ | `Button _ -> 0, 0  (* always flat *)
  | `Choice choices -> 0, choices_w ui sty choices
  | `Section settings ->
    let lw, _ = settings_w ui sty settings in
    lw + sty.indent_w, max_int  (* never flat *)

and choices_w ui sty = function
  | [] -> 0
  | (label, _, _) :: choices ->
    let lw = Draw.text_width ui.win sty.label_h (font ui sty.label_h) label in
    let w1 = sty.item_h + sty.pad_w + lw in
    let w' = choices_w ui sty choices in
    if w' = 0 then w1 else w1 + sty.sep_w + w'


let rec settings_h ui sty xr xmax = function
  | [] -> 0
  | setting :: settings ->
    setting_h ui sty xr xmax setting + sty.pad_h +
    settings_h ui sty xr xmax settings

and setting_h ui sty xr xmax (_name, item) =
  let _, wr = item_w ui sty item in
  if xr + wr >= 0 && xr + wr <= xmax then
    sty.item_h + sty.sep_h  (* flat *)
  else
    item_h ui sty xr xmax item + sty.sep_h

and item_h ui sty xr xmax = function
  | `Flag _ | `Text _ | `Number _ | `Button _ -> sty.item_h
  | `Choice choices ->
    sty.item_h + (List.length choices - 1) * (sty.item_h + 2 * sty.pad_h)
  | `Section settings ->
    sty.item_h + 3 * sty.sep_h + settings_h ui sty xr xmax settings


let rec settings_focus_dy ui sty xr xmax = function
  | [] -> None
  | setting :: settings ->
    match setting_focus_dy ui sty xr xmax setting with
    | Some _ as some -> some
    | None ->
      Option.map ((+) (setting_h ui sty xr xmax setting + sty.pad_h))
        (settings_focus_dy ui sty xr xmax settings)

and setting_focus_dy ui sty xr xmax (_name, item) =
  item_focus_dy ui sty xr xmax item

and item_focus_dy ui sty xr xmax = function
  | `Text (ed, _, _, _) | `Number (_, ed, _, _, _, _, _) when ed.Edit.focus ->
    Some 0
  | `Flag _ | `Choice _ | `Button _ | `Text _ | `Number _ -> None
  | `Section settings ->
    Option.map ((+) (sty.item_h + 2 * sty.sep_h))
      (settings_focus_dy ui sty xr xmax settings)


let scrolled_area x y w h ymin ymax vscroll f =
  let y' = y - vscroll in
  if y' >= ymin && y' + h <= ymax then f (-1, x, y', w, h)

let rec draw_settings ui sty owner xl xr y xmax ymin ymax vscroll = function
  | [] -> y
  | setting :: settings ->
    let y' = draw_setting ui sty owner xl xr y xmax ymin ymax vscroll setting in
    draw_settings ui sty owner xl xr (y' + sty.pad_h) xmax ymin ymax vscroll settings

and draw_setting ui sty owner xl xr y xmax ymin ymax vscroll (name, item) =
  let owner' = owner ^ ":" ^ name in
  scrolled_area xl y (xr - xl) sty.item_h ymin ymax vscroll (fun area ->
    label ui area `Left name
  );
  let _, wr = item_w ui sty item in
  if xr + wr >= 0 && xr + wr <= xmax then
    draw_item_flat ui sty owner' xl xr y xmax ymin ymax vscroll item
      + sty.sep_h
  else
  (
    (match item with
    | `Section _ ->
      let nw = Draw.text_width ui.win sty.item_h (font ui sty.item_h) name in
      let x' = xl + nw + 2 * sty.pad_w in
      let y' = y + sty.item_h * 4 / 5 in
      scrolled_area x' y' (xmax - x') 1 ymin ymax vscroll (fun area ->
        box ui area `White
      );
    | _ -> ()
    );
    draw_item ui sty owner' (xl + sty.indent_w) xr y xmax ymin ymax vscroll item
      + sty.sep_h
  )

and draw_item_flat ui sty owner xl xr y xmax ymin ymax vscroll = function
  | `Choice choices ->
    draw_choices_flat ui sty owner xr y xmax ymin ymax vscroll choices
  | `Section _ ->
    assert false
  | item ->
    draw_item ui sty owner xl xr y xmax ymin ymax vscroll item

and draw_item ui sty owner xl xr y xmax ymin ymax vscroll = function
  | `Flag (b, f) ->
    let y' = y + (sty.item_h - sty.label_h) / 2 in
    scrolled_area xr y' sty.label_h sty.label_h ymin ymax
      vscroll (fun area ->
        indicator ui `Green area b;
        if invisible_button ui owner area [] no_modkey true then
          f (not b)
    );
    y + sty.item_h

  | `Choice choices ->
    draw_choices ui sty owner xr y xmax ymin ymax vscroll choices

  | `Text (ed, color, f, g) ->
    scrolled_area xr y (xmax - xr) sty.item_h ymin ymax vscroll (fun area ->
      let s = ed.Edit.text in
      box ui area `Black;
      ignore (rich_edit_text ui owner area 0 false color ed);
      if ed.focus then f ed;
      if s <> ed.text then g ed.text;
    );
    y + sty.item_h

  | `Number (name, ed, n, nmin, nmax, f, g) ->
    let owner' = owner ^ ":" ^ name in
    let valid () =
      let n' = Option.value (int_of_string_opt ed.Edit.text) ~default: (-1) in
      n' >= nmin && n' <= nmax
    in
    let zeros = String.make (int_of_float (Float.log10 (float nmax)) + 2) '0' in
    let w1 = Draw.text_width ui.win sty.item_h (font ui sty.item_h) zeros in
    let color = if valid () then text_color ui else error_color ui in
    scrolled_area xr y w1 sty.item_h ymin ymax vscroll (fun area ->
      box ui area `Black;
      let s = ed.text in
      let n = Option.value (int_of_string_opt s) ~default: (-1) in
      let prev = if n >= nmax then [] else [string_of_int (n + 1)] in
      let next = if n <= nmin then [] else [string_of_int (n - 1)] in
      Edit.set_history ed prev next;
      ignore (rich_edit_text ui owner' area 0 false color ed);
      if ed.focus then f ed;
      if s <> ed.text && valid () then
        g (int_of_string ed.text)
    );
    let xr' = xr + w1 + sty.pad_w in
    let sh = sty.item_h/2 in
    scrolled_area xr' y sty.item_h sty.item_h ymin ymax vscroll (fun area ->
      if labeled_button ui (owner' ^ ":dn") area sh `White "\\/" no_modkey false (Some false)
      && n > nmin then
      (
        f ed;
        Edit.set ed (string_of_int (n - 1));
        g (n - 1)
      )
    );
    let xr'' = xr' + sty.item_h in
    scrolled_area xr'' y sty.item_h sty.item_h ymin ymax vscroll (fun area ->
      if labeled_button ui (owner' ^ ":up") area sh `White "/\\" no_modkey false (Some false)
      && n < nmax then
      (
        f ed;
        Edit.set ed (string_of_int (n + 1));
        g (n + 1)
      )
    );
    let xr''' = xr'' + sty.item_h + sty.pad_w in
    let y' = y + (sty.item_h - sty.label_h) / 2 in
    scrolled_area xr''' y' (xmax - xr''') sty.label_h ymin ymax vscroll (fun area ->
      label ui area `Left name
    );
    y + sty.item_h

  | `Button (name, f) ->
    let tw = Draw.text_width ui.win sty.item_h (font ui sty.item_h) name in
    scrolled_area xr y (tw + 2 * sty.margin) sty.item_h ymin ymax vscroll (fun area ->
      if
        labeled_button ui (owner ^ ":" ^ name) area sty.label_h `White name
          no_modkey false (Some false)
      then f ()
    );
    y + sty.item_h

  | `Section settings ->
    let y' = y + sty.item_h + 2 * sty.sep_h in
    draw_settings ui sty owner xl xr y' xmax ymin ymax vscroll settings + sty.sep_h

and draw_choices_flat ui sty owner x y xmax ymin ymax vscroll = function
  | [] -> y + sty.item_h
  | choice :: choices ->
    let x' = draw_choice ui sty owner x y xmax ymin ymax vscroll choice in
    draw_choices_flat ui sty owner
      (x' + sty.sep_w) y xmax ymin ymax vscroll choices

and draw_choices ui sty owner x y xmax ymin ymax vscroll = function
  | [] -> y
  | choice :: choices ->
    let _ = draw_choice ui sty owner x y xmax ymin ymax vscroll choice in
    let y' = y + sty.item_h + if choices = [] then 0 else 2 * sty.pad_h in
    draw_choices ui sty owner x y' xmax ymin ymax vscroll choices

and draw_choice ui sty owner x y xmax ymin ymax vscroll (name, b, f) =
    let y' = y + (sty.item_h - sty.label_h) / 2 in
    scrolled_area x y' sty.label_h sty.label_h ymin ymax vscroll (fun area ->
      indicator ui `Green area b
    );
    let x' = x + sty.item_h + sty.pad_w in
    let lw = Draw.text_width ui.win sty.label_h (font ui sty.label_h) name in
    scrolled_area x' y' lw sty.label_h ymin ymax vscroll (fun area ->
      label ui area `Left name
    );
    scrolled_area x (y - sty.pad_h/2) (x' - x + lw) (sty.item_h + sty.pad_h)
      ymin ymax vscroll (fun area ->
      if invisible_button ui (owner ^ ":" ^ name) area [] no_modkey true then
        f name
    );
    x' + lw


let settings ui owner area sty vscroll adjust_vscroll settings =
  let x, y, w, h as r = dim ui area in
  let p, ax, ay, _, _ = area in

  Draw.fill_rect ui.win x y 2 (h - 2) (`Gray 0x00);
  Draw.fill_rect ui.win x y w 2 (`Gray 0x00);
  Draw.fill_rect ui.win (x + 1) (y + h - 2) (w - 1) 2 (`Gray 0x50);
  Draw.fill_rect ui.win (x + w - 1) y 1 (h - 2) (`Gray 0x70);

  let wl, _ = settings_w ui sty settings in
  let xl = x + sty.margin in
  let xr = xl + wl + sty.sep_w in
  let xmax = x + w - 2 * sty.margin - sty.scroll_w in
  let ymin, ymax = y + sty.margin, y + h - sty.margin in
  let hh = settings_h ui sty xr xmax settings in

  let vscroll' =
    if not adjust_vscroll then min hh vscroll else
    match settings_focus_dy ui sty xr xmax settings with
    | None -> min hh vscroll
    | Some dy ->
      if dy - sty.item_h >= vscroll && dy + 2 * sty.item_h <= vscroll + h then
        vscroll
      else
        dy - (h - sty.item_h - 2 * sty.margin)/2
  in

  let y' =
    draw_settings ui sty owner xl xr (y + sty.margin)
      xmax ymin ymax vscroll' settings
  in

  (* Vertical scrollbar *)
  let page_h = h in
  let set_h = y' - y in
  let coeff = float (max 1 page_h) /. float (max 1 set_h) /. 4.0 in
  let _, wdy = wheel_status ui r in
  let ext = if set_h = 0 then 1.0 else min 1.0 (float page_h /. float set_h) in
  let pos = if set_h = 0 then 0.0 else float vscroll' /. float set_h in
(*Printf.printf "scroll=%d pos=%.2f/ext=%.2f page_h=%d/set_h=%d\n%!" set.vscroll pos ext page_h set_h;*)
  let scroll_area = (p, ax + w - sty.scroll_w - 1, ay + 2, sty.scroll_w, h - 3) in
  let pos' =
    scroll_bar ui (owner ^ ":vscroll") scroll_area sty.scroll_l `Vertical
      pos ext -. coeff *. wdy
  in
  clamp 0 (max 0 (set_h - page_h)) (int_of_float (Float.round (pos' *. float set_h)))


(* Pop-ups *)

let popup_rect ui (x, y, w, h) bw =
  x - bw, y - bw, w + 2*bw, h + 2* bw

let popup ui owner r bw (varw, varh, mov) greyout =
  assert (is_modal ui || mov);
  let x, y, w, h = r in
  let ww, wh = Window.size ui.win in
  let w' = w + 2 * bw in
  let h' = h + 2 * bw in
  let x' = clamp 0 (ww - w') x in
  let y' = clamp 0 (wh - h') y in

  if greyout then
    Draw.fill_rect ui.win 0 0 ww wh (`Trans (`Black, 0x40));

  if mov then
    modal_rect ui "ui.popup" (Some (x', y', w', h'));

  background ui x' y' w' h';
  let sw = bw / 3 in
  Draw.fill_rect ui.win (x' + w') (y' + sw) sw h' `Black;
  Draw.fill_rect ui.win (x' + sw) (y' + h') w' sw `Black;

  let origin = Mouse.pos ui.win in
  let lft = inside origin (x', y', bw, h') in
  let rgt = inside origin (x' + w' - bw, y', bw, h') in
  let top = inside origin (x', y', w', bw) in
  let bot = inside origin (x', y' + h' - bw, w', bw) in
  let on = inside origin (x', y', w', h') in

  let cursor' = cursor varw varh lft top rgt bot in
  let r' =
    except_modal ui owner (fun () ->
      if Mouse.is_down `Right || ui.drag = Abort then
      (
        if has_mouse ui owner then
        (
          Mouse.set_cursor ui.win `Default;
          ui.drag <- Abort;
        );
        None
      )
      else if not (Mouse.is_down `Left) then
      (
        if on && cursor' <> `Point && grab_mouse ui owner then
          Mouse.set_cursor ui.win cursor';
        match ui.drag with
        | Move _ | Resize _ when Mouse.is_released `Left && grab_mouse ui owner ->
          Some (r, (lft, top, rgt, bot))
        | _ -> None
      )
      else
      (
        match ui.drag with
        | No_drag when on && (mov || cursor' <> `Point) && grab_mouse ui owner ->
          Mouse.set_cursor ui.win cursor';
          ui.drag <-
            if cursor' = `Point then
              Move {overshoot = 0, 0}
            else
            (
              let mx, my = Mouse.abs_pos ui.win in
              let dx = if lft then mx - x' else mx - (x' + w') in
              let dy = if top then my - y' else my - (y' + h') in
              Resize {offset = dx, dy; edge = lft, top, rgt, bot}
            );
          Some (r, (lft, top, rgt, bot))

        | Move {overshoot} when grab_mouse ui owner ->
          Mouse.set_cursor ui.win `Point;
          let delta = Api.add (Mouse.delta ui.win) overshoot in
          let x'', y'' = add (x', y') delta in
          let x''', y''' = clamp 0 (ww - w') x'', clamp 0 (wh - h') y'' in
          ui.drag <- Move {overshoot = x'' - x''', y'' - y'''};
          Some ((x''', y''', w, h), (false, false, false, false))

        | Resize {offset; edge = lft, top, rgt, bot} when grab_mouse ui owner ->
          Mouse.set_cursor ui.win (cursor varw varh lft top rgt bot);
          let mx, my = sub (Mouse.abs_pos ui.win) offset in
          let mx', my' = clamp 0 ww mx, clamp 0 wh my in
          let x'' = if lft then mx' else x' in
          let y'' = if top then my' else y' in
          let w'' = if rgt then mx' - x' else w' - (x'' - x') in
          let h'' = if bot then my' - y' else h' - (y'' - y') in
          let r' = x'', y'', w + w'' - w', h + h'' - h' in
          Some (r', (lft, top, rgt, bot))

        | _ ->
          None
      )
    )
  in

  pane ui owner (x' + bw, y' + bw, w, h), r'


(* Menus *)

type menu_style =
  { margin : int;
    gutter_w : int;
    text_h : int;
    pad_h : int;
    scroll_w : int;
    scroll_h : int;
    scroll_l : int;
    refl_r : int;
  }

type menu_entry =
  [`Separator | `Entry of color * string * (modifier list * key) * bool]

let menu_separator = String.concat "" (List.init 80 (Fun.const "·"))

let menu ui x y sty hscroll vscroll items =
  assert (is_modal ui);

  let font = font ui sty.text_h in
  let keys =
    Iarray.map (function
      | `Separator -> ""
      | `Entry (_, _, (mods, key), _) ->
        String.concat "+" Api.Key.(List.map modifier_name mods @ [name key])
    ) items
  in
  let lw = 2 * sty.gutter_w +
    Iarray.fold_left (fun w -> function
      | `Separator -> w
      | `Entry (_, s, _, _) ->
        max w (Draw.text_width ui.win sty.text_h font s + 1)
    ) 0 items
  and rw =
    Iarray.fold_left (fun w s ->
      max w (Draw.text_width ui.win sty.text_h font s + 1)
    ) 0 keys
  in

  let enabled i =
    match Iarray.get items i with `Entry (_, _, _, b) -> b | _ -> false in

  let ww, wh = Window.size ui.win in

  let maxw, maxh = ww - 2 * sty.margin, wh - 2 * sty.margin in
  let mw = (sty.gutter_w + 1)/2 in  (* inner width padding *)
  let rh = sty.text_h + 2 * sty.pad_h in
  let w = lw + sty.gutter_w + rw + 2 * mw in
  let h = rh * Iarray.length items in
  let scroll_w = if h <= maxh then 0 else sty.scroll_w in
  let scroll_h = if w <= maxw then 0 else sty.scroll_h in
  let w' = if scroll_w = 0 then w else w + scroll_w + 1 in
  let h' = if scroll_h = 0 then h else h + scroll_h + 1 in
  let w'' = min w' maxw in
  let h'' = min h' maxh in
  let p, _ =
    popup ui "(menu)" (x, y, w'', h'') sty.margin (false, false, false) true in
  let area = (p, 0, 0, -1, -1) in
  let page = (if scroll_h = 0 then h'' else h'' - scroll_h - 1) / rh in

  let sty' : rich_table_style =
    { gutter_w = sty.gutter_w;
      text_h = sty.text_h;
      pad_h = sty.pad_h;
      scroll_w;
      scroll_h;
      scroll_l = sty.scroll_l;
      refl_r = sty.refl_r;
      has_heading = false;
    }
  in

  let _, my = Mouse.pos ui.win in
  let inner = rich_table_inner_area ui area sty' in
  let _, iy, _, _ = dim ui inner in
  let i = if mouse_inside ui inner then (my - iy)/rh + vscroll else -1 in

  let cols : _ iarray = [|lw, `Left; rw, `Right|] in
  let c_sep = semilit_color (text_color ui) in

  let tab = Table.make 0 in
  Table.set tab (Iarray.to_array items);
  Table.set_hscroll tab hscroll;
  Table.set_vscroll tab vscroll 1 page;
  Table.focus tab;
  if i >= 0 && i < Iarray.length items && enabled i then Table.select tab i i;

  let pp_row j : _ * _ iarray =
    match Iarray.get items j with
    | `Separator -> c_sep, [|`Text menu_separator; `Text ""|]
    | `Entry (c, txt, _, enabled) ->
      let c' = if enabled then c else semilit_color c in
      c', [|`Text txt; `Text (Iarray.get keys j)|]
  in

  nonmodal ui "ui.menu";
  let owner = "(menu)" in
  match rich_table ui owner area sty' cols None tab pp_row with
  | `Click (Some i, _) when enabled i -> `Click i
  | `Click (Some _, _) -> modal ui "ui.menu"; `None
  | `Click (None, _) -> `Close
  | `Scroll -> modal ui "ui.menu"; `Scroll (tab.hscroll, tab.vscroll)
  | `Sort _ | `Resize _ | `Reorder _ | `HeadMenu _ -> assert false
  | `None | `Move _ | `Drag _ | `Drop | `Abort | `Menu _ | `Select ->
    if (Mouse.is_released `Left || Mouse.is_pressed `Right)
    && not (has_mouse ui owner) then
      `Close
    else
      let key_pressed = function
        | `Entry (_, _, modkey, _) -> key ui modkey true
        | `Separator -> false
      in
      match Iarray.find_index key_pressed items with
      | Some i -> `Click i
      | None when key ui ([], `Escape) true -> `Close
      | None -> modal ui "ui.menu"; `None
