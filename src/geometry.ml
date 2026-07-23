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
  mutable control_ratio : float option;
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
    control_ratio = None;
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

let win_min_w g flex_ctl flex_ext =
  (if flex_ctl then control_min_w else control_w g) +
  (if not (extension_shown_w g) then 0 else
   if flex_ext then extension_min_w g else extension_w g)
let win_min_h g flex_ctl flex_ext =
  (if flex_ctl then control_min_h else control_h g) +
  (if not (extension_shown_h g) then 0 else
   if flex_ext then extension_min_h g else extension_h g)
let win_max_w g flex_ctl flex_ext =
  if flex_ctl || flex_ext then
    fst (Api.Window.max_size (Ui.window g.ui))
  else win_w g
let win_max_h g flex_ctl flex_ext =
  if flex_ctl || flex_ext then
    snd (Api.Window.max_size (Ui.window g.ui))
  else win_h g

let win_min_x g = fst (Api.Window.min_pos (Ui.window g.ui))
let win_min_y g = snd (Api.Window.min_pos (Ui.window g.ui))
let win_max_x g = win_min_x g + fst (Api.Window.max_size (Ui.window g.ui))
let win_max_y g = win_min_y g + snd (Api.Window.max_size (Ui.window g.ui))


(* Validation *)

let control_ratio_accurate geo =
  (* May round either way *)
  Option.for_all (fun ratio ->
    geo.control_width = int_of_float (float geo.control_height *. ratio) ||
    geo.control_height = int_of_float (float geo.control_width /. ratio)
  ) geo.control_ratio

let check msg b = if b then [] else [msg]

let ok geo =
  check "window width in range"
    (live_win_w geo >= win_min_w geo true true) @
  check "window height in range"
    (live_win_h geo >= win_min_h geo true true) @
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
  check "control width in range"
    (geo.control_width >= control_min_w) @
  check "control height in range"
    (geo.control_height >= control_min_h) @
(*
  check "control ratio accurate"
    (control_ratio_accurate geo) @
*)
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


(* Divider Modifications *)

let clamp min max v =
  (*assert (min <= max);*)
  if v > max then max else
  if v < min then min else
  v

let check_geo geo (ww, wh) =
  let cw, ch = geo.control_width, geo.control_height in
  let ew, eh = geo.extension_width, geo.extension_height in
  let ww' = if extension_shown_w geo then cw + ew else cw in
  let wh' = if extension_shown_h geo then ch + eh else ch in
  if (ww, wh) <> (ww', wh') then
  (
    Printf.eprintf
      "[layout failure] win=%d,%d<>%d,%d ctl=%d,%d ext=%d,%d shown=%b,%b\n%!"
      ww wh ww' wh' cw ch ew eh (extension_shown_w geo) (extension_shown_h geo);
assert false;
  )


(*
(* Set control/extension w/h to (clamped) value, adjust the inverse if open. *)
let set_control_width geo w =
  let w' = max w control_min_w in
  let dw = w' - geo.control_width in
let ctl_w, ext_w = geo.control_width, geo.extension_width in
  geo.control_width <- w';
  if extension_shown_w geo then
    geo.extension_width <- geo.extension_width - dw
;if !App.debug_layout then
Printf.eprintf "  [set_control_width %d] w=%d~%d ctl=%d->%d ext=%d->%d\n%!"
w w w' ctl_w geo.control_width ext_w geo.extension_width

let set_control_height geo h =
  let h' = max h control_min_h in
  let dh = h' - geo.control_height in
let ctl_h, ext_h = geo.control_height, geo.extension_height in
  geo.control_height <- h';
  if extension_shown_h geo then
    geo.extension_height <- geo.extension_height - dh
;if !App.debug_layout then
Printf.eprintf "  [set_control_height %d] h=%d~%d ctl=%d->%d ext=%d->%d\n%!"
h h h' ctl_h geo.control_height ext_h geo.extension_height

let set_extension_width geo w =
  let w' = max w (extension_min_w geo) in
  let dw = w' - geo.extension_width in
let ctl_w, ext_w = geo.control_width, geo.extension_width in
  geo.extension_width <- w';
  geo.control_width <- geo.control_width - dw
;if !App.debug_layout then
Printf.eprintf "  [set_extension_width %d] w=%d~%d ctl=%d->%d ext=%d->%d\n%!"
w w w' ctl_w geo.control_width ext_w geo.extension_width

let set_extension_height geo h =
  let h' = max h (extension_min_h geo) in
  let dh = h' - geo.extension_height in
let ctl_h, ext_h = geo.control_height, geo.extension_height in
  geo.extension_height <- h';
  geo.control_height <- geo.control_height - dh
;if !App.debug_layout then
Printf.eprintf "  [set_extension_height %d] h=%d~%d ctl=%d->%d ext=%d->%d\n%!"
h h h' ctl_h geo.control_height ext_h geo.extension_height

(* Add/sub control/extension w/h to (clamped) delta without adjusting inverse. *)
let change_control_width geo dw =
  set_control_width geo (geo.control_width + dw);
  if extension_shown_w geo then
    geo.extension_width <- geo.extension_width + dw  (* reverse and tweak adapt *)

let change_control_height geo dh =
  set_control_height geo (geo.control_height + dh);
  if extension_shown_h geo then
    geo.extension_height <- geo.extension_height + dh  (* reverse and tweak adapt *)

let change_extension_width geo dw =
  let ext_w = geo.extension_width in
  set_extension_width geo (ext_w + dw);
  let dw' = geo.extension_width - ext_w in
  geo.control_width <- geo.control_width + dw'  (* reverse adapt *)

let change_extension_height geo dh =
  let ext_h = geo.extension_height in
  set_extension_height geo (ext_h + dh);
  let dh' = geo.extension_height - ext_h in
  geo.control_height <- geo.control_height + dh'  (* reverse adapt *)

(* Adapt control w/h to ratio by setting it, clamp if necessary. *)
let rec adapt_control_width geo =
  Option.iter (fun ratio ->
    let w = int_of_float (float geo.control_height *. ratio) in
    set_control_width geo w
(* TODO: this may lead to an infinite loop
    if geo.control_width <> w then  (* may have been clamped *)
      adapt_control_width geo
*)
  ) geo.control_ratio

and adapt_control_height geo =
  Option.iter (fun ratio ->
    let h = int_of_float (float geo.control_width /. ratio) in
    set_control_height geo h
(* TODO: this may lead to an infinite loop
    if geo.control_height <> h then  (* may have been clamped *)
      adapt_control_width geo
*)
  ) geo.control_ratio
*)


let change_geo geo dx dy dw dh dcw dch focusw focush flexcw flexch =
  let win = Ui.window geo.ui in
  let x, y = Api.Window.pos win in
  let w, h = Api.Window.size win in
  let cw, ch = geo.control_width, geo.control_height in
  let ew, eh = geo.extension_width, geo.extension_height in
  let shownw, shownh = extension_shown_w geo, extension_shown_h geo in
(*
  let leftw = shownw || focusw <> `Lft && geo.extension_side = `Right in
  let toph = shownh || focush <> `Top in
*)
  let rightw = not shownw && (focusw = `Lft || geo.extension_side = `Right) in
  let both = not shownh in
  let dew, deh = dw - dcw, dh - dch in

  if !App.debug_layout then
  (
    Printf.eprintf
      "  [change geo] focus=%s%s flex_ctl=%b,%b\n%!"
      (match focush with `Top -> "T" | `Bot -> "R" | `Hor -> "H" | `None -> "")
      (match focusw with `Lft -> "L" | `Rgt -> "R" | `Ver -> "V" | `None -> "")
      flexcw flexch;
    Printf.eprintf
      "    win=%d%+d,%d%+d,%d%+d,%d%+d ctl=%d%+d,%d%+d ext=%d%+d,%d%+d\n%!"
      x dx y dy w dw h dh cw dcw ch dch ew dew eh deh;
  );

  (* Correct ratio *)
  let cw', ch' =
    match geo.control_ratio with
    | None -> cw + dcw, ch + dch
    | Some ratio ->
      (*  TL  T  TV T  TR
       *   +-----+-----+
       *  L|     V     |R
       * LH+--H--M--H--+RH
       *  L|     V     |R
       *   +-----+-----+
       *  BL  B  BV B  BR
       *
       *            -XW-XH -XW+XH +XW-XH +XW+XH
       *  -            r!     h      w      r
       *  L R          h!     h      h!     h
       *  T B          w!     w!     w      w
       *  TL TR BL BR  r!     r!     r!     r*
       *  V            -      -      h!     h
       *  TV BV        -      -      r!     r
       *  H            -      w!     -      w
       *  LH RH        -      r!     -      r
       *  M            -      -      -      r
       *
       * (! will affect window size)
       *)
      let cw', ch' = cw + dcw, ch + dch in
      let adapt_w () = int_of_float (float ch' *. ratio), ch' in
      let adapt_h () = cw', int_of_float (float cw' /. ratio) in
      match focusw, focush with
      | (`Lft | `Rgt | `Ver), `None -> adapt_h ()
      | `None, (`Top | `Bot | `Hor) -> adapt_w ()
      | `None, `None when shownw <> shownh ->
        if shownw then adapt_w () else adapt_h ()
      | (`Lft | `Rgt), (`Top | `Bot) when shownw && shownh && cw' * ch' < cw * ch ->
        let ratio' = float cw' /. float ch' in
        if ratio < ratio' then adapt_w () else adapt_h ()
      | _, _ ->
        let ratio' = float cw' /. float ch' in
        if ratio' < ratio then adapt_w () else adapt_h ()
  in

  if !App.debug_layout && (cw' <> cw + dcw || ch' <> ch + dch) then
  (
    Printf.eprintf
      "    ratio=%.4f ctl=%d,%d(%.4f)->%d,%d(%.4f)~%d,%d(%.4f)\n%!"
      (Option.get geo.control_ratio)
      cw ch (float cw /. float ch)
      (cw + dcw) (ch + dch) (float (cw + dcw) /. float (ch + dch))
      cw' ch' (float cw' /. float ch');
  );

  let dcw', dch' = cw' - cw, ch' - ch in
  let corrw, corrh = dcw' - dcw, dch' - dch in
  let dw' = dw + if shownw then 0 else corrw in
  let dh' = dh + if shownh then 0 else corrh in
  let dx' = dx - if rightw then 0 else corrw in
  let dy' = dy - if both then 0 else corrh in

  let x', y' = x + dx', y + dy' in
  let w', h' = w + dw', h + dh' in

Printf.printf "dw'=%d dcw'=%d dh=%d dh'=%d dch'=%d h=%d h'=%d \n%!" dw' dcw' dh dh' dch' h h';
  assert (shownw || dw' = dcw');
  assert (shownh || dh' = dch');

(*
  assert (w = cw + if shownw then ew else 0);
  assert (h = ch + if shownh then eh else 0);
  assert (w' = cw' + if shownw then ew' else 0);
  assert (h' = ch' + if shownh then eh' else 0);
*)

  (* Clamp window *)
  let edge = (focusw = `Lft || focusw = `Rgt) && (focush = `Top || focush = `Bot) in
  let flexl = edge && focusw <> `Rgt in
  let flext = edge && focush <> `Bot in
  let flexr = edge && focusw <> `Lft in
  let flexb = edge && focush <> `Top in

  let minw = win_min_w geo flexcw true in
  let minh = win_min_h geo flexch true in
  let maxw = win_max_w geo flexcw true in
  let maxh = win_max_h geo flexch true in
  let minx, miny = -maxw, win_min_y geo in  (* cannot move to negative y coords *)
  let maxx, maxy = win_max_x geo, win_max_y geo in

  let minw' = if flexl || flexr then minw else max minw w' in
  let minh' = if flext || flexb then minh else max minh h' in
  let maxw' = if flexl || flexr then maxw else min maxw (max minw w') in
  let maxh' = if flext || flexb then maxh else min maxh (max minh h') in
  let minx' = if flexl || flext || flexb then minx else max minx x' in
  let miny' = if flext || flext || flexb then miny else max miny y' in
  let maxx' = if flexl then maxx - minw else min (maxx - minw) (max minx x') in
  let maxy' = if flext then maxy - minh else min (maxy - minh) (max miny y') in
Printf.eprintf "    x=%d~%d minx=%d~%d maxx=%d~%d\n    w=%d~%d minw=%d~%d maxw=%d~%d\n%!"
x x' minx minx' maxx maxx' w w' minw minw' maxw maxw';
Printf.eprintf "    y=%d~%d miny=%d~%d maxy=%d~%d\n    h=%d~%d minh=%d~%d maxh=%d~%d\n%!"
y y' miny miny' maxy maxy' h h' minh minh' maxh maxh';

(*
  assert (minx' <= maxx');
  assert (miny' <= maxy');
*)
  assert (minw' <= maxw');
  assert (minh' <= maxh');

  let w'', h'' = clamp minw' maxw' w', clamp minh' maxh' h' in
(*
  let x'', y'' = clamp minx' maxx' x', clamp miny' maxy' y' in
*)
  let x'' = x' - if rightw then 0 else w'' - w' in
  let y'' = y' - if both then 0 else h'' - h' in

  let dx'', dy'' = x'' - x, y'' - y in
  let dw'', dh'' = w'' - w, h'' - h in
  let dcw'' = dcw' + dw'' - dw' in
  let dch'' = dch' + dh'' - dh' in
  let dew'' = dw'' - dcw'' in
  let deh'' = dh'' - dch'' in

Printf.printf "dw''=%d dcw''=%d dh=%d dh'=%d dh''=%d dch''=%d h=%d h'=%d h''=%d\n%!" dw'' dcw'' dh dh' dh'' dch'' h h' h'';
  assert (shownw || dw'' = dcw'');
  assert (shownh || dh'' = dch'');

  let cw'', ch'' = cw + dcw'', ch + dch'' in
  let ew'', eh'' = ew + dew'', eh + deh'' in

  (* Clamp dividers *)
  let mincw = control_min_w in
  let minch = control_min_h in
  let minew = extension_min_w geo in
  let mineh = extension_min_h geo in
  let maxcw = w'' - (if shownw then minew else 0) in
  let maxch = h'' - (if shownh then mineh else 0) in
  let maxew = (if shownw then w'' else maxw) - mincw in
  let maxeh = (if shownh then h'' else maxh) - minch in
Printf.printf "    cw=%d~%d~%d mincw=%d maxcw=%d\n    ew=%d~%d minew=%d maxew=%d\n%!"
cw cw' cw'' mincw maxcw ew ew'' minew maxew;
Printf.printf "    ch=%d~%d~%d minch=%d maxch=%d\n    eh=%d~%d mineh=%d maxeh=%d\n%!"
ch ch' ch'' minch maxch eh eh'' mineh maxeh;
assert (mincw <= maxcw);
assert (minch <= maxch);
assert (minew <= maxew);
assert (mineh <= maxeh);

  let cw''', ch''' = clamp mincw maxcw cw'', clamp minch maxch ch'' in
  let ew''', eh''' = clamp minew maxew ew'', clamp mineh maxeh eh'' in

  let dcw''', dch''' = cw''' - cw, ch''' - ch in
  let dew''', deh''' = ew''' - ew, eh''' - eh in

  if !App.debug_layout then
  (
    Printf.eprintf
      "  [changed geo] focus=%s%s flex_ctl=%b,%b\n%!"
      (match focush with `Top -> "T" | `Bot -> "R" | `Hor -> "H" | `None -> "")
      (match focusw with `Lft -> "L" | `Rgt -> "R" | `Ver -> "V" | `None -> "")
      flexcw flexch;
    Printf.eprintf
      "    win = %d%+d~%+d(%d,%d), %d%+d~%+d(%d,%d), %d%+d~%+d(%d,%d), %d%+d~%+d(%d,%d)\n%!"
      x dx dx'' minx' maxx' y dy dy'' miny' maxy' w dw dw'' minw' maxw' h dh dh'' minh' maxh';
    Printf.eprintf
      "    ctl = %d%+d~%+d(%d,%d), %d%+d~%+d(%d,%d)\n%!"
      cw dcw dcw''' mincw maxcw ch dch dch''' minch maxch;
    Printf.eprintf
      "    ext = %d%+d~%+d(%d,%d), %d%+d~%+d(%d,%d)\n%!"
      ew dew dew''' minew maxew eh deh deh''' mineh maxeh;
  );

  geo.control_width <- geo.control_width + dcw''';
  geo.control_height <- geo.control_height + dch''';
  geo.extension_width <- geo.extension_width + dew''';
  geo.extension_height <- geo.extension_height + deh''';

  (dx'', dy'', dw'', dh'')


(* Resolution-independent Window Geometry *)

let concrete_geo geo : float * float * float * float =
  let x, y = Api.Window.pos (Ui.window geo.ui) in
  float x, float y, float geo.extension_width, float geo.extension_height

let abstract_geo' geo (wx, wy, ww, wh) : float * float * float * float =
  let win = Ui.window geo.ui in
  let sx, sy = Api.Window.min_pos win in
  let sw, sh = Api.Window.max_size win in
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
check_geo geo (ww, wh);
  let cw, ch = geo.control_width, geo.control_height in
  let ew, eh = geo.extension_width, geo.extension_height in

  if extension_shown_w geo then
  (
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
    (
      Printf.printf "[layout refit w] win=%d ctl=%d->%d ext=%d->%d\n%!"
        ww cw geo.control_width ew geo.extension_width
    )
  )
  else if extension_w geo < extension_min_w geo then
  (
    geo.extension_width <- extension_min_w geo;
  );
  if extension_shown_h geo then
  (
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
    (
      Printf.eprintf "[layout refit h] win=%d ctl=%d->%d ext=%d->%d\n%!"
        wh ch geo.control_height eh geo.extension_height
    )
  )
  else if extension_h geo < extension_min_h geo then
  (
    geo.extension_height <- extension_min_h geo;
  );

check_geo geo (ww, wh);

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
;check_geo geo (ww, wh)

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
  let sx, sy = Api.Window.min_pos win in
  let sw, sh = Api.Window.max_size win in
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
  let sw, sh = Api.Window.max_size (Ui.window geo.ui) in
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
