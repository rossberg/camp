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
    library_side = `Left;
    repair_log_columns = [|200; 300; 300|];
    filesel_shown = false;
    menu_shown = false;
    popup_shown = None;
    popup_size = 500;
    browser_width = 100;
    directories_width = 120;
    left_width = 200;
    right_shown = false;
    upper_height = 200;
    lower_shown = false;
    album_grid = 100;
    track_grid = 100;
  }


(* Constants and derived parameters *)

let control_min_w = 360
let control_min_h = 160

let control_w _g = control_min_w
let control_h _g = control_min_h

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

let playlist_x g = if (g.library_shown || g.filesel_shown) && g.library_side = `Left then g.library_width else 0
let library_x g = if g.library_side = `Left then 0 else control_w g - margin g
let library_w g = g.library_width


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
let left_min _g = 20
let right_min _g = 20
let views_min g = left_min g + right_min g
let files_min _g = 20
let library_min g = max (browser_min g + views_min g) (directories_min g + files_min g)

let upper_min g = margin g + 3 * (line_h g) + scrollbar_w g + 3
let lower_min g = divider_w g + 3 * (line_h g) + scrollbar_w g + 3

let browser_max g = g.library_width - views_min g
let directories_max g = g.library_width - files_min g
let left_max g = g.library_width - g.browser_width - right_min g
let upper_max g = control_h g + g.playlist_height - bottom_h g - lower_min g

let playlist_min g = bottom_h g + max (margin g + 2 * (line_h g)) (upper_min g + lower_min g - control_h g)


(* Validation *)

let check msg b = if b then [] else [msg]

let ok geo =
  check "text size in range"
    (geo.text >= min_text_size && geo.text <= max_text_size) @
  check "album grid size in range"
    (geo.album_grid >= min_grid_size && geo.album_grid <= max_grid_size) @
  check "track grid size in range"
    (geo.track_grid >= min_grid_size && geo.track_grid <= max_grid_size) @
  check "playlist height in range"
    (geo.playlist_height >= playlist_min geo) @
  check "library width in range"
    (geo.library_width >= library_min geo) @
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

let concrete_geo geo =
  let x, y = Api.Window.pos (Ui.window geo.ui) in
  x, y, geo.library_width, geo.playlist_height

let abstract_geo geo =
  let win = Ui.window geo.ui in
  let sx, sy = Api.Window.min_pos win in
  let sw, sh = Api.Window.max_size win in
  let x', y' = Api.Window.pos win in
  let w', h' = Api.Window.size win in
  let x, y = max 0 (x' - sx), max 0 (y' - sy) in
  let w, h = min w' sw, min h' sh in
  let x =
    if geo.library_shown || not geo.filesel_shown then x
    else x + geo.library_width
  in
  let dx = if x = 0 || x + w < sw then 0 else -sw in
  let dy = if y = 0 || y + h < sh then 0 else -sh in
  let dw = if geo.library_shown || geo.filesel_shown then 0 else geo.library_width in
  let dh = if geo.playlist_shown then 0 else geo.playlist_height in
  let left = geo.library_side = `Left in

  let ax = x + dx in
  let ay = y + dy in
  let ah = if y + h + dh <> sh then geo.playlist_height else -1 in
  let aw = if x + w + dw <> sw || left then geo.library_width else -1 in
  (ax, ay, aw, ah)


let apply_geo geo (ax, ay, aw, ah) : int * int =
  let win = Ui.window geo.ui in
  let ww, wh = control_min_w, control_min_h in
  let sx, sy = Api.Window.min_pos win in
  let sw, sh = Api.Window.max_size win in

  let ax, ay = clamp (-sw) sw ax, clamp (-sh) sh ay in
  let dx = if ax >= 0 then 0 else +sw in
  let dy = if ay >= 0 then 0 else +sh in
  let x, y = ax + sx + dx, ay + sy + dy in

  geo.playlist_height <- clamp (playlist_min geo) (sh - wh)
    (if ah = -1 then sh - y + sy - wh else ah);
  geo.library_width <- clamp (library_min geo) (sw - ww)
    (if aw = -1 then sw - x + sx - ww else aw);

  geo.browser_width <-
    clamp (browser_min geo) (browser_max geo) geo.browser_width;
  geo.left_width <- clamp (left_min geo) (left_max geo) geo.left_width;
  geo.upper_height <- clamp (upper_min geo) (upper_max geo) geo.upper_height;
  geo.directories_width <-
    clamp (directories_min geo) (directories_max geo) geo.directories_width;

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

let print_state' geo (x, y, w, h) =
  let open Text.Print in
  record (fun geo -> [
    "win_pos", pair int int (x, y);
    "scaling", pair int int geo.scaling;
    "buffered", bool (Ui.is_buffered geo.ui);
    "color_palette", nat (Ui.get_palette geo.ui);
    "text_size", nat geo.text;
    "text_pad_x", nat geo.pad_x;
    "text_pad_y", nat geo.pad_y;
    "text_sdf", bool (Ui.font_is_sdf geo.ui);
    "play_open", bool geo.playlist_shown;
    "play_height", int h;
    "lib_open", bool geo.library_shown;
    "lib_side", enum side_enum geo.library_side;
    "lib_width", int w;
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
  record (fun r ->
    let cx, cy, cw, ch = ref (-1), ref (-1), ref (-1), ref (-1) in
    apply (r $? "scaling") (pair (num (-1) 8) (num (-1) 8))
      (fun (dx, dy) -> Ui.rescale geo.ui dx dy; geo.scaling <- dx, dy);
    apply (r $? "win_pos") (pair int int)
      (fun (x, y) -> cx := x; cy := y);
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
    apply (r $? "play_height") int
      (fun h -> ch := h);
    apply (r $? "lib_open") bool
      (fun b -> geo.library_shown <- b);
    apply (r $? "lib_side") (enum side_enum)
      (fun s -> geo.library_side <- s);
    apply (r $? "lib_width") int
      (fun w -> cw := w);
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
    pos := apply_geo geo (!cx, !cy, !cw, !ch)
  )
