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
  mutable playlist_shown : bool;
  mutable playlist_height : int;
  mutable library_shown : bool;
  mutable library_width : int;
  mutable library_side : Api.side;
  mutable window : (float * float * float * float) option;
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
    playlist_shown = false;
    playlist_height = 200;
    library_shown = false;
    library_width = 600;
    library_side = `Right;
    window = None;
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

let margin g = g.margin
let popup_margin g = g.margin / 2
let divider_w g = g.margin

let text_h g = g.text
let pad_w g = g.pad_x
let pad_h g = g.pad_y
let line_h g = g.text + 2 * g.pad_y
let label_h g = g.label
let button_label_h g = g.button_label
let gutter_w g = g.gutter
let scrollbar_w g = g.scrollbar
let indicator_w _g = 7

let bottom_h g = line_h g + g.margin
let footer_y g = - line_h g - (bottom_h g - line_h g)/2

let overlay_shown g = g.library_shown || g.filesel_shown
let overlay_left g = g.library_side = `Left

let control_min_w = 360
let control_min_h = 160

let control_w _g = control_min_w
let control_h _g = control_min_h
let control_x g = if overlay_shown g && overlay_left g then -control_w g else 0
let control_y _g = 0

let playlist_x g = control_x g
let playlist_y g = control_y g + control_h g
let playlist_w g = control_w g
let playlist_h g = if g.playlist_shown then g.playlist_height else 0

let library_x g = if overlay_left g then 0 else control_w g - margin g
let library_y _g = 0
let library_w g = if overlay_shown g then g.library_width else 0
let library_h g = control_h g + playlist_h g

let win_w g = control_w g + library_w g
let win_h g = control_h g + playlist_h g


(* Resizing limits *)

let min_text_size = 8
let max_text_size = 64
let min_pad_size = 0
let max_pad_size = 8
let min_grid_size = 32
let max_grid_size = 1024
let min_popup_size = 100
let max_popup_size = 1000

let browser_min _g = 160
let directories_min _g = 150
let left_min _g = 40
let right_min _g = 40
let views_min g = left_min g + right_min g
let files_min _g = 40
let library_min g = max (browser_min g + views_min g) (directories_min g + files_min g)

let upper_min g = margin g + 3 * (line_h g) + scrollbar_w g + 3
let lower_min g = divider_w g + 3 * (line_h g) + scrollbar_w g + 3

let browser_max g = g.library_width - views_min g
let directories_max g = g.library_width - files_min g
let left_max g = g.library_width - g.browser_width - right_min g
let upper_max g = control_h g + g.playlist_height - bottom_h g - lower_min g

let playlist_min g = bottom_h g + max (margin g + 2 * (line_h g)) (upper_min g + lower_min g - control_h g)

let win_min_w g = control_w g + if overlay_shown g then library_min g else 0
let win_min_h g = control_h g + if g.playlist_shown then playlist_min g else 0
let win_max_w g = if overlay_shown g then -1 else control_w g
let win_max_h g = if g.playlist_shown then -1 else control_h g


(* Validation *)

let check msg b = if b then [] else [msg]

let ok geo =
  check "text size in range"
    (geo.text >= min_text_size && geo.text <= max_text_size) @
  check "album grid size in range"
    (geo.album_grid >= min_grid_size && geo.album_grid <= max_grid_size) @
  check "track grid size in range"
    (geo.track_grid >= min_grid_size && geo.track_grid <= max_grid_size) @
  check "playlist height minimum positive"
   (playlist_min geo >= 0) @
  check "playlist height in range"
    (geo.playlist_height >= playlist_min geo) @
  check "library width minimum positive"
    (library_min geo >= 0) @
  check "library width in range"
    (geo.library_width >= library_min geo) @
  check "browser width minimum positive"
    (browser_min geo >= 0) @
  check "browser width maximum larger than minimum"
    (browser_max geo >= browser_min geo) @
  check "browser width in range"
    ( geo.browser_width >= browser_min geo &&
      geo.browser_width <= browser_max geo ) @
  check "left view width in range"
    ( geo.left_width >= left_min geo &&
      geo.left_width <= left_max geo ) @
  check "upper view height in range"
    ( geo.upper_height >= upper_min geo &&
      geo.upper_height <= upper_max geo ) @
  check "directories width in range"
    ( geo.directories_width >= directories_min geo &&
      geo.directories_width <= directories_max geo ) @
  []


(* Resolution-independent Window Geometry *)

let clamp min max v =
  if v > max then max else
  if v < min then min else
  v

let concrete_geo geo : float * float * float * float =
  let x, y = Api.Window.pos (Ui.window geo.ui) in
  float x, float y, float geo.library_width, float geo.playlist_height

let abstract_geo geo : float * float * float * float =
  let win = Ui.window geo.ui in
  let scr = Api.Window.screen win in
  let sx, sy = Api.Screen.min_pos scr in
  let sw, sh = Api.(sub (Screen.max_size scr) (control_min_w, control_min_h)) in
  let cw, ch = Api.Window.size win in
  let cx = let cx = control_x geo in if cx >= 0 then cx else cw + cx in
  let cy = let cy = control_y geo in if cy >= 0 then cy else ch + cy in
  assert (cx >= 0 && cy >= 0);  (* relative position of control pane *)
  let x, y = Api.Window.pos win in
  let w, h = geo.library_width, geo.playlist_height in
  let ax = float (x + cx - sx) /. float sw in
  let ay = float (y + cy - sy) /. float sh in
  let aw = float w /. float sw in
  let ah = float h /. float sh in
  (ax, ay, aw, ah)

let apply_geo geo (ax, ay, aw, ah) : int * int =
  let win = Ui.window geo.ui in
  let scr = Api.Window.screen win in
  let sx, sy = Api.Screen.min_pos scr in
  let sw, sh = Api.(sub (Screen.max_size scr) (control_min_w, control_min_h)) in
  let w = clamp (library_min geo) sw (int_of_float (aw *. float sw)) in
  let h = clamp (playlist_min geo) sh (int_of_float (ah *. float sh)) in
  geo.library_width <- w;
  geo.playlist_height <- h;
  let cw, ch = Api.add (w, h) (control_min_w, control_min_h) in
  let cx = let cx = control_x geo in if cx >= 0 then cx else cw + cx in
  let cy = let cy = control_y geo in if cy >= 0 then cy else ch + cy in
  assert (cx >= 0 && cy >= 0);  (* relative position of control pane *)
  let margin = margin geo in
  let clamp_x = clamp (- cw + margin) (sw - margin) in
  let clamp_y = clamp (- ch + margin) (sh - margin) in
  let x = clamp_x (int_of_float (ax *. float sw) - cx) + sx in
  let y = clamp_y (int_of_float (ay *. float sh) - cy) + sy in

(*
  geo.browser_width <-
    clamp (browser_min geo) (browser_max geo) geo.browser_width;
  geo.left_width <- clamp (left_min geo) (left_max geo) geo.left_width;
  geo.upper_height <- clamp (upper_min geo) (upper_max geo) geo.upper_height;
  geo.directories_width <-
    clamp (directories_min geo) (directories_max geo) geo.directories_width;
*)

  (x, y)


let abstract_view_geo geo : int * int =
  let w = geo.left_width in
  let h = geo.upper_height in
  let aw = if w - left_min geo < left_max geo - w then w else w - left_max geo in
  let ah = if h - upper_min geo < upper_max geo - h then h else h - upper_max geo in
  aw, ah

let apply_view_geo geo (aw, ah) =
  let w = if aw >= 0 then aw else left_max geo + aw in
  let h = if ah >= 0 then ah else upper_max geo + ah in
  geo.left_width <- clamp (left_min geo) (left_max geo) w;
  geo.upper_height <- clamp (upper_min geo) (upper_max geo) h


(* Persistence *)

let side_enum = ["left", `Left; "right", `Right]

let print_state' geo (ax, ay, aw, ah) =
  let open Text.Print in
  record (fun geo -> [
    "win_pos", pair float float (ax, ay);
    "scaling", pair int int geo.scaling;
    "buffered", bool (Ui.is_buffered geo.ui);
    "color_palette", nat (Ui.get_palette geo.ui);
    "text_size", nat geo.text;
    "text_pad_x", nat geo.pad_x;
    "text_pad_y", nat geo.pad_y;
    "text_sdf", bool (Ui.font_is_sdf geo.ui);
    "play_open", bool geo.playlist_shown;
    "play_height", float ah;
    "lib_open", bool geo.library_shown;
    "lib_side", enum side_enum geo.library_side;
    "lib_width", float aw;
    "browser_width", nat geo.browser_width;
    "upper_height", nat geo.upper_height;
    "left_width", nat geo.left_width;
    "directories_width", nat geo.directories_width;
    "album_grid", nat geo.album_grid;
    "track_grid", nat geo.track_grid;
    "repair_cols", iarray nat geo.repair_log_columns;
    "popup_size", nat geo.popup_size;
  ]) geo

let print_state geo = print_state' geo (abstract_geo geo)
let print_intern geo = print_state' geo (concrete_geo geo)

let parse_state geo pos =  (* assumes playlist and library loaded *)
  let open Text.Parse in
  let float' max t =
    (* Backwards compatibility with old integer coordinates *)
    let x = float t in
    if x > 1.0 then x /. float_of_int max else
    if x <= -1.0 then 1.0 +. x /. float_of_int max else
    x
  in
  let sw, sh = Api.Screen.max_size (Api.Window.screen (Ui.window geo.ui)) in
  record (fun r ->
    let ax, ay, aw, ah = ref 0.0, ref 0.0, ref 0.0, ref 0.0 in
    apply (r $? "scaling") (pair (num (-1) 8) (num (-1) 8))
      (fun delta -> Ui.rescale geo.ui delta; geo.scaling <- delta);
    apply (r $? "win_pos") (pair (float' sw) (float' sh))
      (fun (x, y) -> ax := x; ay := y);
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
    apply (r $? "play_open") bool
      (fun b -> geo.playlist_shown <- b);
    apply (r $? "play_height") (float' (sw - control_min_w))
      (fun h -> ah := h);
    apply (r $? "lib_open") bool
      (fun b -> geo.library_shown <- b);
    apply (r $? "lib_side") (enum side_enum)
      (fun s -> geo.library_side <- s);
    apply (r $? "lib_width") (float' (sh - control_min_h))
      (fun w -> aw := w);
    apply (r $? "browser_width") (num (browser_min geo) (browser_max geo))
      (fun w -> geo.browser_width <- w);
    apply (r $? "left_width") (num (left_min geo) (left_max geo))
      (fun w -> geo.left_width <- w);
    apply (r $? "upper_height") (num (upper_min geo) (upper_max geo))
      (fun h -> geo.upper_height <- h);
    apply (r $? "directories_width") (num (directories_min geo) (directories_max geo))
      (fun w -> geo.directories_width <- w);
    apply (r $? "album_grid") (num min_grid_size max_grid_size)
      (fun w -> geo.album_grid <- w);
    apply (r $? "track_grid") (num min_grid_size max_grid_size)
      (fun w -> geo.track_grid <- w);
    apply (r $? "repair_cols") (iarray (num 10 1000))
      (fun ws -> if Iarray.length ws = 3 then geo.repair_log_columns <- ws);
    apply (r $? "popup_size") (num min_popup_size max_popup_size)
      (fun w -> geo.popup_size <- w);
    pos := apply_geo geo (!ax, !ay, !aw, !ah)
  )
