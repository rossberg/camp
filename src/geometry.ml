(* UI Geometry *)

type t =
{
  ui : Ui.t;
  mutable scaling : int * int;
  mutable margin : int;
  mutable text : int;
  mutable pad_x : int;
  mutable pad_y : int;
  mutable label : int;
  mutable button_label : int;
  mutable gutter : int;
  mutable scrollbar : int;
  mutable reflection : int;
  mutable control_width : int;
  mutable control_height : int;
  mutable extension_width : int;
  mutable extension_height : int;
  mutable extension_side : Api.side;
  mutable playlist_shown : bool;
  mutable playlist_headers : bool;
  mutable library_shown : bool;
  mutable window : float * float * float * float;
  mutable repair_log_columns : int iarray;
  mutable filesel_shown : bool;
  mutable menu_shown : bool;
  mutable popup_shown : (int * int) option;
  mutable popup_size : int;
  mutable browser_width : int;
  mutable directories_width : int;
  mutable left_width : int;
  mutable right_shown : bool;
  mutable upper_height : int;
  mutable lower_shown : bool;
  mutable album_grid : int;
  mutable track_grid : int;
}


(* Constructor *)

let control_min_w = 360
let control_min_h = 160

let make ui =
  {
    ui;
    scaling = 0, 0;
    margin = 10;
    text = 14;
    pad_x = 0;
    pad_y = 1;
    label = 9;
    button_label = 9;
    gutter = 7;
    scrollbar = 11;
    reflection = 200;
    control_width = control_min_w * 5 / 4;
    control_height = control_min_h * 5 / 4;
    extension_width = 600;
    extension_height = 200;
    extension_side = `Right;
    playlist_shown = false;
    playlist_headers = false;
    library_shown = false;
    window = 1.0, 1.0, 1.0, 1.0;
    repair_log_columns = [|200; 300; 300|];
    filesel_shown = false;
    menu_shown = false;
    popup_shown = None;
    popup_size = 500;
    browser_width = 160;
    directories_width = 150;
    left_width = 200;
    right_shown = false;
    upper_height = 200;
    lower_shown = false;
    album_grid = 100;
    track_grid = 100;
  }


(* Constants and derived parameters *)

let min_text_size = 8
let max_text_size = 64
let min_pad_size = 0
let max_pad_size = 8
let min_grid_size = 32
let max_grid_size = 1024
let min_popup_size = 100
let max_popup_size = 1000

let sx g x = x * g.control_width / control_min_w
let sy g y = y * g.control_height / control_min_h
let smin g v = min (sx g v) (sy g v)
let smax g v = max (sx g v) (sy g v)

let margin g = smin g g.margin
let popup_margin g = margin g / 2
let divider_w g = margin g

let text_h g = min max_text_size (smin g g.text)
let pad_w g = smin g g.pad_x
let pad_h g = smin g g.pad_y
let line_h g = text_h g + 2 * pad_h g
let label_h g = min max_text_size (smin g g.label)
let button_label_h g = min max_text_size (smin g g.button_label)
let gutter_w g = sx g g.gutter
let scrollbar_w g = smin g g.scrollbar
let indicator_w g = smin g 7

let bottom_h g = line_h g + margin g
let footer_y g = - line_h g - (bottom_h g - line_h g)/2

let extension_shown_w g = g.library_shown || g.filesel_shown
let extension_shown_h g = g.playlist_shown
let extension_left g = g.extension_side = `Left

let control_w g = g.control_width
let control_h g = g.control_height
let control_x g = if extension_shown_w g && extension_left g then -control_w g else 0
let control_y _g = 0

let extension_x g = if extension_left g then 0 else control_w g - margin g
let extension_y g = control_y g + control_h g
let extension_w g = g.extension_width
let extension_h g = g.extension_height

let win_w g = control_w g + if extension_shown_w g then extension_w g else 0
let win_h g = control_h g + if extension_shown_h g then extension_h g else 0

let playlist_x g = control_x g
let playlist_y g = extension_y g
let playlist_w g = control_w g
let playlist_h g = if g.playlist_shown then extension_h g else 0

let library_x g = extension_x g
let library_y g = control_y g
let library_w g = if g.library_shown then extension_w g else 0
let library_h g = control_h g + extension_h g

let filesel_x g = extension_x g
let filesel_y g = control_y g
let filesel_w g = if g.library_shown then extension_w g else 0
let filesel_h g = control_h g + extension_h g


(* Resizing limits *)

let live_win_w g =
  let win = Ui.window g.ui in
  min (fst (Api.Window.size win)) (fst (Api.Window.next_size win))
let live_win_h g =
  let win = Ui.window g.ui in
  min (snd (Api.Window.size win)) (snd (Api.Window.next_size win))

let browser_min_w g = sx g 160
let left_min_w g = sx g 40
let views_min_w g = 2 * left_min_w g
let library_min_w g = browser_min_w g + views_min_w g

let directories_min_w g = browser_min_w g
let files_min_w g = left_min_w g
let filesel_min_w g = directories_min_w g + files_min_w g

let upper_min_h g = margin g + 3 * line_h g + scrollbar_w g + 3
let lower_min_h g = divider_w g + 3 * line_h g + scrollbar_w g + 3

let browser_max_w g = extension_w g - views_min_w g
let directories_max_w g = extension_w g - files_min_w g
let left_max_w g = extension_w g - g.browser_width - left_min_w g
let upper_max_h g = library_h g - bottom_h g - lower_min_h g

let playlist_min_h g = bottom_h g + max (margin g + 2 * line_h g) (upper_min_h g + lower_min_h g - control_h g)

let extension_min_w g = max (library_min_w g) (filesel_min_w g)
let extension_max_w g = live_win_w g - control_min_w
let extension_min_h g = playlist_min_h g
let extension_max_h g = live_win_h g - control_min_h

let control_max_w g = live_win_w g - extension_min_w g
let control_max_h g = live_win_h g - extension_min_h g

let win_min_w flex_ctl flex_ext g =
  (if flex_ctl then control_min_w else control_w g) +
  (if not (extension_shown_w g) then 0 else
   if flex_ext then extension_min_w g else extension_w g)
let win_min_h flex_ctl flex_ext g =
  (if flex_ctl then control_min_h else control_h g) +
  (if not (extension_shown_h g) then 0 else
   if flex_ext then extension_min_h g else extension_h g)
let win_max_w flex_ctl flex_ext g =
  if flex_ctl && not flex_ext then
    fst Api.(Screen.max_size (Window.screen (Ui.window g.ui)))
  else if flex_ctl || flex_ext then Int.max_int else win_w g
let win_max_h flex_ctl flex_ext g =
  if flex_ctl && not flex_ext then
    snd Api.(Screen.max_size (Window.screen (Ui.window g.ui)))
  else if flex_ctl || flex_ext then Int.max_int else win_h g


(* Validation *)

let check msg b = if b then [] else [msg]

let ok geo =
  check "window width in range"
    (live_win_w geo >= win_min_w true true geo) @
  check "window height in range"
    (live_win_h geo >= win_min_h true true geo) @
  check "text size in range"
    (geo.text >= min_text_size && geo.text <= max_text_size) @
  check "label size in range"
    (geo.label >= min_text_size && geo.label <= max_text_size) @
  check "button label size in range"
    (geo.button_label >= min_text_size && geo.button_label <= max_text_size) @
  check "album grid size in range"
    (geo.album_grid >= min_grid_size && geo.album_grid <= max_grid_size) @
  check "track grid size in range"
    (geo.track_grid >= min_grid_size && geo.track_grid <= max_grid_size) @
  check "extension width minimum positive"
   (extension_min_w geo >= 0) @
  check "extension height minimum positive"
   (extension_min_h geo >= 0) @
  check "extension width in range"
    (geo.extension_width >= extension_min_w geo) @
  check "extension height in range"
    (geo.extension_height >= extension_min_h geo) @
  check "browser width minimum positive"
    (browser_min_w geo >= 0) @
  check "browser width maximum larger than minimum"
    (browser_max_w geo >= browser_min_w geo) @
  check "browser width in range"
    ( geo.browser_width >= browser_min_w geo &&
      geo.browser_width <= browser_max_w geo ) @
  check "left view width in range"
    ( geo.left_width >= left_min_w geo &&
      geo.left_width <= left_max_w geo ) @
  check "upper view height in range"
    ( geo.upper_height >= upper_min_h geo &&
      geo.upper_height <= upper_max_h geo ) @
  check "directories width in range"
    ( geo.directories_width >= directories_min_w geo &&
      geo.directories_width <= directories_max_w geo ) @
  []


(* Resolution-independent Window Geometry *)

let clamp min max v =
  if v > max then max else
  if v < min then min else
  v

let concrete_geo geo : float * float * float * float =
  let x, y = Api.Window.pos (Ui.window geo.ui) in
  float x, float y, float geo.extension_width, float geo.extension_height

let abstract_geo' geo (wx, wy, ww, wh) : float * float * float * float =
  let win = Ui.window geo.ui in
  let scr = Api.Window.screen win in
  let sx, sy = Api.Screen.min_pos scr in
  let sw, sh = Api.Screen.max_size scr in
  let sw', sh' = Api.sub (sw, sh) (control_w geo, control_h geo) in
  let cx = let cx = control_x geo in if cx >= 0 then cx else ww + cx in
  let cy = let cy = control_y geo in if cy >= 0 then cy else wh + cy in
  assert (cx >= 0 && cy >= 0);  (* relative position of control pane *)
  let lw, ph = geo.extension_width, geo.extension_height in
  let tweak q = if not (Float.is_finite q) then 1.0 else q in
  let ax = tweak (float (wx + cx - sx) /. float sw') in
  let ay = tweak (float (wy + cy - sy) /. float sh') in
  let aw = clamp 0.0 1.0 (tweak (float lw /. float sw')) in
  let ah = clamp 0.0 1.0 (tweak (float ph /. float sh')) in
  (ax, ay, aw, ah)

let abstract_geo geo : float * float * float * float =
  let win = Ui.window geo.ui in
  let wx, wy = Api.Window.pos win in
  let ww, wh = Api.Window.size win in
  abstract_geo' geo (wx, wy, ww, wh)

let clamp_geo' geo (ww, wh) =
  assert (ww > 1 && wh > 1);
  if extension_shown_w geo then
  (
    let cw, ew = geo.control_width, geo.extension_width in
    (* Changing control pane size may change minima gradually *)
    while extension_w geo < extension_min_w geo do
      geo.control_width <- geo.control_width - 1;
      geo.extension_width <- geo.extension_width + 1;
    done;
(*
    geo.control_width <- clamp control_min_w (ww - extension_min_w geo) cw;
    geo.extension_width <- clamp (extension_min_w geo) (ww - control_w geo) ew;
*)
    if !App.debug_layout
    && (cw, ew) <> (geo.control_width, geo.extension_width) then
      Printf.printf "[layout refit w] win=%d ctl=%d->%d ext=%d->%d\n%!"
        ww cw geo.control_width ew geo.extension_width;
  )
  else if extension_w geo < extension_min_w geo then
  (
    geo.extension_width <- extension_min_w geo;
  );
  if extension_shown_h geo then
  (
    let ch, eh = geo.control_height, geo.extension_height in
    (* Changing control pane size may change minima gradually *)
    while extension_h geo < extension_min_h geo do
      geo.control_height <- geo.control_height - 1;
      geo.extension_height <- geo.extension_height + 1;
    done;
(*
    geo.control_height <- clamp control_min_h (wh - extension_min_h geo) ch;
    geo.extension_height <- clamp (extension_min_h geo) (wh - control_h geo) eh;
*)
    if !App.debug_layout
    && (ch, eh) <> (geo.control_height, geo.extension_height) then
      Printf.printf "[layout refit h] win=%d ctl=%d->%d ext=%d->%d\n%!"
        wh ch geo.control_height eh geo.extension_height;
  )
  else if extension_h geo < extension_min_h geo then
  (
    geo.extension_height <- extension_min_h geo;
  );

  let browser_width = geo.browser_width in
  let left_width = geo.left_width in
  let upper_height = geo.upper_height in
  let directories_width = geo.directories_width in
  geo.browser_width <-
    clamp (browser_min_w geo) (browser_max_w geo) geo.browser_width;
  geo.left_width <-
    clamp (left_min_w geo) (left_max_w geo) geo.left_width;
  geo.upper_height <-
    clamp (upper_min_h geo) (upper_max_h geo) geo.upper_height;
  geo.directories_width <-
    clamp (directories_min_w geo) (directories_max_w geo) geo.directories_width;

  if !App.debug_layout && (
    browser_width <> geo.browser_width ||
    left_width <> geo.left_width ||
    upper_height <> geo.upper_height ||
    directories_width <> geo.directories_width
  ) then
  (
    Printf.eprintf
      "[layout clamp] ext=%d,%d bw=%d->%d lw=%d->%d uh=%d->%d dw=%d->%d\n%!"
      geo.extension_width geo.extension_height
      browser_width geo.browser_width
      left_width geo.left_width
      upper_height geo.upper_height
      directories_width geo.directories_width
  )

let clamp_geo geo =
  let win = Ui.window geo.ui in
  let ww, wh = Api.Window.next_size win in
  clamp_geo' geo (ww, wh)

let update_geo' geo (wx, wy, ww, wh) =
  clamp_geo' geo (ww, wh);
  geo.window <- abstract_geo' geo (wx, wy, ww, wh)

let update_geo geo =
  clamp_geo geo;
  geo.window <- abstract_geo geo

let apply_geo geo (ax, ay, aw, ah) : int * int * int * int =
  let win = Ui.window geo.ui in
  let scr = Api.Window.screen win in
  let sx, sy = Api.Screen.min_pos scr in
  let sw, sh = Api.Screen.max_size scr in
  let sw', sh' = Api.sub (sw, sh) (control_w geo, control_h geo) in
  let ew = clamp (extension_min_w geo) sw' (int_of_float (aw *. float sw')) in
  let eh = clamp (extension_min_h geo) sh' (int_of_float (ah *. float sh')) in
  geo.extension_width <- ew;
  geo.extension_height <- eh;
  let w, h = Api.add (ew, eh) (control_w geo, control_h geo) in
  let cx = let cx = control_x geo in if cx >= 0 then cx else w + cx in
  let cy = let cy = control_y geo in if cy >= 0 then cy else h + cy in
  assert (cx >= 0 && cy >= 0);  (* relative position of control pane *)
  let margin = margin geo in
  let clamp_x = clamp (- w + margin) (sw + control_w geo - margin) in
  let clamp_y = clamp (- h + margin) (sh + control_h geo - margin) in
  let x = clamp_x (int_of_float (ax *. float sw') - cx) + sx in
  let y = clamp_y (int_of_float (ay *. float sh') - cy) + sy in

  if !App.debug_layout then
  (
    let sw, sh = Api.Screen.max_size scr in
    Printf.eprintf
      "[layout apply] abs=%.2f,%.2f,%.2f,%.2f concr=%d,%d,%d+%d,%d+%d scr=%d,%d,%d,%d\n%!"
      ax ay aw ah x y ew (control_w geo) eh (control_h geo) sx sy sw sh;
  );

  let w, h = win_w geo, win_h geo in
  clamp_geo' geo (w, h);
  x, y, w, h


let abstract_view_geo geo : int * int =
  let w = geo.left_width in
  let h = geo.upper_height in
  let aw = if w - left_min_w geo < left_max_w geo - w then w else w - left_max_w geo in
  let ah = if h - upper_min_h geo < upper_max_h geo - h then h else h - upper_max_h geo in
  aw, ah

let apply_view_geo geo (aw, ah) =
  let w = if aw >= 0 then aw else left_max_w geo + aw in
  let h = if ah >= 0 then ah else upper_max_h geo + ah in
  geo.left_width <- clamp (left_min_w geo) (left_max_w geo) w;
  geo.upper_height <- clamp (upper_min_h geo) (upper_max_h geo) h


(* Persistence *)

let side_enum = ["left", `Left; "right", `Right]

let print_state geo =
  let open Text.Print in
  let ax, ay, aw, ah = abstract_geo geo in
  record (fun geo -> [
    "win_pos", pair float float (ax, ay);
    "scaling", pair int int geo.scaling;
    "buffered", bool (Ui.is_buffered geo.ui);
    "color_palette", nat (Ui.get_palette geo.ui);
    "text_size", nat geo.text;
    "text_pad_x", nat geo.pad_x;
    "text_pad_y", nat geo.pad_y;
    "text_sdf", bool (Ui.font_is_sdf geo.ui);
    "ctl_width", nat geo.control_width;
    "ctl_height", nat geo.control_height;
    "play_open", bool geo.playlist_shown;
    "play_height", interval 0.0 1.0 ah;
    "play_headers", bool geo.playlist_headers;
    "lib_open", bool geo.library_shown;
    "lib_side", enum side_enum geo.extension_side;
    "lib_width", interval 0.0 1.0 aw;
    "browser_width", nat geo.browser_width;
    "upper_height", nat geo.upper_height;
    "left_width", nat geo.left_width;
    "directories_width", nat geo.directories_width;
    "album_grid", nat geo.album_grid;
    "track_grid", nat geo.track_grid;
    "repair_cols", iarray nat geo.repair_log_columns;
    "popup_size", nat geo.popup_size;
  ]) geo

let print_intern geo =
  let open Text.Print in
  let x, y = Api.Window.pos (Ui.window geo.ui) in
  let w, h = Api.Window.size (Ui.window geo.ui) in
  print_state geo @@@
  record (fun geo -> [
    "win_pos", pair int int (x, y);
    "win_size", pair int int (w, h);
    "ext_width", nat geo.extension_width;
    "ext_height", nat geo.extension_height;
  ]) geo

let parse_state geo =  (* assumes playlist and library loaded *)
  let open Text.Parse in
  let sw, sh = Api.Screen.max_size (Api.Window.screen (Ui.window geo.ui)) in
  let ww, wh = control_w geo, control_h geo in
  let wx, wy = (sw - ww)/2, (sh - wh)/2 in  (* default to mid-screen *)
  let ax, ay, aw, ah = abstract_geo' geo (wx, wy, ww, wh) in
  let rax, ray, raw, rah = ref ax, ref ay, ref aw, ref ah in
  record (fun r ->
    apply (r $? "scaling") (pair (num (-1) 8) (num (-1) 8))
      (fun delta -> geo.scaling <- delta);
    apply (r $? "win_pos") (pair float float)
      (fun (x, y) -> rax := x; ray := y);
    apply (r $? "buffered") bool
      (fun b -> Ui.buffered geo.ui b);
    apply (r $? "text_sdf") bool
      (fun b -> Ui.font_sdf geo.ui b);
    apply (r $? "color_palette") (num 0 (Ui.num_palette geo.ui - 1))
      (fun i -> Ui.set_palette geo.ui i);
    apply (r $? "text_size") (num min_text_size max_text_size)
      (fun h -> geo.text <- h);
    apply (r $? "text_pad_x") (num min_pad_size max_pad_size)
      (fun w -> geo.pad_x <- w);
    apply (r $? "text_pad_y") (num min_pad_size max_pad_size)
      (fun h -> geo.pad_y <- h);
    apply (r $? "ctl_width") (num control_min_w sw)
      (fun w -> geo.control_width <- w);
    apply (r $? "ctl_height") (num control_min_h sh)
      (fun h -> geo.control_height <- h);
    apply (r $? "play_open") bool
      (fun b -> geo.playlist_shown <- b);
    apply (r $? "play_height") (interval 0.0 1.0)
      (fun h -> rah := h);
    apply (r $? "play_headers") bool
      (fun b -> geo.playlist_headers <- b);
    apply (r $? "lib_open") bool
      (fun b -> geo.library_shown <- b);
    apply (r $? "lib_side") (enum side_enum)
      (fun s -> geo.extension_side <- s);
    apply (r $? "lib_width") (interval 0.0 1.0)
      (fun w -> raw := w);
    apply (r $? "browser_width") (num (browser_min_w geo) (browser_max_w geo))
      (fun w -> geo.browser_width <- w);
    apply (r $? "left_width") (num (left_min_w geo) (left_max_w geo))
      (fun w -> geo.left_width <- w);
    apply (r $? "upper_height") (num (upper_min_h geo) (upper_max_h geo))
      (fun h -> geo.upper_height <- h);
    apply (r $? "directories_width") (num (directories_min_w geo) (directories_max_w geo))
      (fun w -> geo.directories_width <- w);
    apply (r $? "album_grid") (num min_grid_size max_grid_size)
      (fun w -> geo.album_grid <- w);
    apply (r $? "track_grid") (num min_grid_size max_grid_size)
      (fun w -> geo.track_grid <- w);
    apply (r $? "repair_cols") (iarray (num 10 1000))
      (fun ws -> if Iarray.length ws = 3 then geo.repair_log_columns <- ws);
    apply (r $? "popup_size") (num min_popup_size max_popup_size)
      (fun w -> geo.popup_size <- w);

    geo.window <- (!rax, !ray, !raw, !rah);
    Ui.rescale geo.ui geo.scaling;
    let r = apply_geo geo geo.window in
    if !App.debug_layout then
    (
      let _, _, ww, wh = r in
      Printf.eprintf "[layout load] win=%d,%d\n%!" ww wh;
    );
    Ui.reset geo.ui r;
  )
