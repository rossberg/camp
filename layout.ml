(* UI Layout *)


(* Global Geometry *)

type t =
{
  ui : Ui.t;
  mutable margin : int;
  mutable text : int;
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
  mutable filesel_shown : bool;
  mutable browser_width : int;
  mutable directories_width : int;
  mutable left_width : int;
  mutable right_shown : bool;
  mutable upper_height : int;
  mutable lower_shown : bool;
  mutable albums_grid : int;
  mutable tracks_grid : int;
}

let make ui =
  {
    ui;
    margin = 10;
    text = 13;
    label = 8;
    button_label = 9;
    gutter = 7;
    scrollbar = 11;
    reflection = 200;
    playlist_shown = false;
    playlist_height = 200;
    library_shown = false;
    library_width = 600;
    library_side = `Left;
    filesel_shown = false;
    browser_width = 100;
    directories_width = 120;
    left_width = 200;
    right_shown = false;
    upper_height = 200;
    lower_shown = false;
    albums_grid = 100;
    tracks_grid = 100;
  }


let margin g = g.margin
let divider_w g = g.margin

let text_h g = g.text
let label_h g = g.label
let button_label_h g = g.button_label
let gutter_w g = g.gutter
let scrollbar_w g = g.scrollbar
let indicator_w _g = 7

let bottom_h g = g.text + g.margin
let footer_y g = - g.text - (bottom_h g - g.text)/2


let control_min_w = 360
let control_min_h = 160
let control_w _g = control_min_w
let control_h _g = control_min_h

let playlist_x g = if (g.library_shown || g.filesel_shown) && g.library_side = `Left then g.library_width else 0
let library_x g = if g.library_side = `Left then 0 else control_w g - margin g
let library_w g = g.library_width


(* Control Pane *)

let cp = 0
let control_pane g = Ui.pane g.ui cp (playlist_x g, 0, control_w g, control_h g)

(* Power and pane activation buttons *)
let shown_w = 35
let shown_h = 12
let shown_x g = - margin g - shown_w
let shown_indicator_x g = shown_x g + (shown_w - indicator_w g)/2 + 1

let power_h = 22
let power_y g = margin g
let power_button g = Ui.button g.ui (cp, shown_x g, power_y g, shown_w, power_h) ([`Command], `Char 'Q') true
let power_label g = Ui.label g.ui (cp, shown_x g, power_y g + power_h + 1, shown_w, label_h g) `Center "POWER"
let minimize_button g = Ui.mouse g.ui (cp, shown_x g, power_y g, shown_w, power_h) `Right

let shown_indicator y g = Ui.indicator g.ui `Green (cp, shown_indicator_x g, y - indicator_w g - 1, indicator_w g, indicator_w g)
let shown_button y ch g = Ui.button g.ui (cp, shown_x g, y, shown_w, shown_h) ([`Command], `Char ch) true
let shown_label y txt g = Ui.label g.ui (cp, shown_x g, y + shown_h + 1, shown_w, label_h g) `Center txt

let play_y = 56
let playlist_indicator = shown_indicator play_y
let playlist_button = shown_button play_y 'P'
let playlist_label = shown_label play_y "PLAYLIST"

let lib_y = 93
let library_indicator = shown_indicator lib_y
let library_button = shown_button lib_y 'L'
let library_label = shown_label lib_y "LIBRARY"
let library_mouse g = Ui.mouse g.ui (cp, shown_x g, lib_y, shown_w, shown_h) `Right
let library_key g = Ui.key g.ui ([`Alt], `Char 'L') true

(* Display box *)
let info_w _g = -55
let info_h _g = -52
let info_margin _g = 4
let info_area g = (cp, margin g, margin g, info_w g, info_h g)
let info_box g = Ui.box g.ui (info_area g) `Black
let info_refl g = Ui.mouse_reflection g.ui (info_area g) (info_h g)

(* Volume *)
let volume_w = 27
let volume_h = 50
let volume_x g = info_w g - info_margin g - volume_w
let volume_y g = margin g + info_margin g

let mute_w = 22
let mute_h = 15
let mute_x g = volume_x g - 4
let mute_y g = volume_y g + volume_h - 8
let mute_area g = (cp, mute_x g, mute_y g, mute_w, mute_h)

let volume_bar g = Ui.volume_bar g.ui (cp, volume_x g, volume_y g, volume_w, volume_h)
let volume_wheel g = Ui.wheel g.ui (cp, 0, 0, control_w g, control_h g)

let mute_text g = Ui.color_text g.ui (cp, mute_x g, mute_y g, mute_w, 8) `Center
let mute_button g = Ui.mouse g.ui (mute_area g) `Left
let mute_drag g = Ui.drag g.ui (mute_area g)

let mute_key g = Ui.key g.ui ([], `Char '0')
let volup_key g = Ui.key g.ui ([], `Char '+')
let voldown_key g = Ui.key g.ui ([], `Char '-')

(* Time *)
let lcd_space = 3
let colon_w = 4
let lcd_w = 14
let lcd_h = 20
let lcd_x g i = margin g + info_margin g + i*(lcd_w + lcd_space)
let lcd_y g = margin g + info_margin g + 10
let lcd_minus g = Ui.lcd g.ui (cp, lcd_x g 0, lcd_y g, lcd_w, lcd_h)
let lcd1 g = Ui.lcd g.ui (cp, lcd_x g 1, lcd_y g, lcd_w, lcd_h)
let lcd2 g = Ui.lcd g.ui (cp, lcd_x g 2, lcd_y g, lcd_w, lcd_h)
let lcd_colon g = Ui.lcd g.ui (cp, lcd_x g 3, lcd_y g, colon_w, lcd_h)
let lcd3 g = Ui.lcd g.ui (cp, lcd_x g 3 + colon_w + lcd_space, lcd_y g, lcd_w, lcd_h)
let lcd4 g = Ui.lcd g.ui (cp, lcd_x g 4 + colon_w + lcd_space, lcd_y g, lcd_w, lcd_h)
let lcd_button g = Ui.mouse g.ui (cp, lcd_x g 0, lcd_y g, colon_w + lcd_x g 4, lcd_h) `Left

(* Cover *)
let cover_x g = lcd_x g 5 + 30
let cover_y g = margin g + info_margin g
let cover_w = 80
let cover_h = 40
let cover_area g = (cp, cover_x g, cover_y g, cover_w, cover_h)
let cover_key g = Ui.key g.ui ([`Command], `Char 'Y') true

(* Info *)
let seek_h _g = 14
let seek_y g = info_h g - info_margin g/2 - seek_h g
let ticker_h _g = 16
let ticker_y g = seek_y g - ticker_h g - 3
let prop_h _g = 12
let prop_y g = ticker_y g - prop_h g - 2

let prop_text g = Ui.text g.ui (cp, margin g + info_margin g, prop_y g, mute_x g, prop_h g) `Left
let title_ticker g = Ui.ticker g.ui (cp, margin g + info_margin g, ticker_y g, info_w g - info_margin g, ticker_h g)
let seek_bar g = Ui.progress_bar g.ui (cp, margin g + info_margin g / 2, seek_y g, info_w g - info_margin g, seek_h g)

(* Hidden mode buttons *)
let color_y g = lcd_y g + lcd_h
let color_button side g = Ui.mouse g.ui (cp, margin g, color_y g, mute_x g, ticker_y g) side
let color_button_fwd = color_button `Left
let color_button_bwd = color_button `Right

let fps_text g = Ui.text g.ui (cp, cover_x g + cover_w + 20, margin g + info_margin g, 40, 12) `Left
let fps_key g = Ui.key g.ui ([`Command], `Char 'U') true

(* Control buttons *)
let ctl_w = 40
let ctl_h = 30
let control_button i sym ch g = Ui.labeled_button g.ui (cp, margin g + i * ctl_w, - 8 - ctl_h, ctl_w, ctl_h) ~protrude: false 10 (Ui.active_color g.ui) sym ([], `Char ch)

let bwd_button = control_button 0 "<<" 'Z'
let play_button = control_button 1 ">" 'X'
let pause_button = control_button 2 "||" 'C'
let stop_button = control_button 3 "[]" 'V'
let fwd_button = control_button 4 ">>" 'B'
let eject_button = control_button 5 "^" 'N'

let start_stop_key g = Ui.key g.ui ([], `Char ' ')
let rw_key g = Ui.key g.ui ([], `Arrow `Left)
let ff_key g = Ui.key g.ui ([], `Arrow `Right)

(* Play mode buttons *)
let mode_w = 25
let mode_h = 12
let mode_x g i = - margin g - mode_w - i * (mode_w + 8)
let mode_y _g = -30
let mode_indicator_x g x = function
  | `Center -> x + (mode_w - indicator_w g)/2 + 1
  | `Left -> x + 4
  | `Right -> x + mode_w - indicator_w g - 4

let mode_indicator i al g = Ui.indicator g.ui `Green (cp, mode_indicator_x g (mode_x g i) al, mode_y g - indicator_w g - 1, indicator_w g, indicator_w g)
let mode_button i ch g = Ui.button g.ui (cp, mode_x g i, mode_y g, mode_w, mode_h) ([`Command], `Char ch) true
let mode_label i label g = Ui.label g.ui (cp, mode_x g i, mode_y g + mode_h + 1, mode_w, label_h g) `Center label

let shuffle_indicator = mode_indicator 2 `Center
let shuffle_button = mode_button 2 'T'
let shuffle_label = mode_label 2 "SHUFFLE"

let repeat_indicator1 = mode_indicator 1 `Left
let repeat_indicator2 = mode_indicator 1 `Right
let repeat_button = mode_button 1 'R'
let repeat_label = mode_label 1 "REPEAT"

let loop_indicator1 = mode_indicator 0 `Left
let loop_indicator2 = mode_indicator 0 `Right
let loop_button = mode_button 0 'J'
let loop_label = mode_label 0 "LOOP"


(* Playlist Pane *)

let pp = cp + 1
let playlist_pane g = Ui.pane g.ui pp (playlist_x g, control_h g, control_w g, - bottom_h g)

(* Playlist *)
let playlist_area g = (pp, margin g, margin g, - margin g, - 1)
let playlist_table g = Ui.rich_table g.ui (playlist_area g) (gutter_w g) (text_h g) (scrollbar_w g) 0 g.reflection
let playlist_mouse g = Ui.rich_table_mouse g.ui (playlist_area g) (gutter_w g) (text_h g) (scrollbar_w g) 0 false

(* Total text field *)
let total_w g = - margin g - scrollbar_w g
let total_x g = total_w g - 100
let total_y g = g.playlist_height + footer_y g  (* Hack: this is outside the pane *)
let playlist_total_box g = Ui.box g.ui (pp, total_x g, total_y g, total_w g, text_h g) `Black
let playlist_total_text g = Ui.text g.ui (pp, total_x g, total_y g, total_w g - (gutter_w g + 1)/2, text_h g) `Right

let enlarge_key g = Ui.key g.ui ([`Command], `Char '+') true
let reduce_key g = Ui.key g.ui ([`Command], `Char '-') true

let enlarge_grid_key g = Ui.key g.ui ([`Command; `Shift], `Char '+') true
let reduce_grid_key g = Ui.key g.ui ([`Command; `Shift], `Char '-') true


(* Edit Pane *)

let ep = pp + 1
let edit_pane g = Ui.pane g.ui ep (playlist_x g, - bottom_h g, control_w g, bottom_h g)

(* Buttons *)
let edit_w = 25
let edit_h = 20
let edit_button i j label key g = Ui.labeled_button g.ui (ep, margin g + i*5 + j*edit_w, - edit_h, edit_w, edit_h) (button_label_h g) (Ui.inactive_color g.ui) label key true

let sep_button = edit_button 0 0 "SEP" ([], `Insert)
let del_button = edit_button 1 1 "DEL" ([], `Delete)
let crop_button = edit_button 1 2 "CROP" ([`Shift], `Delete)
let clean_button = edit_button 1 3 "CLEAN" ([`Command], `Delete)
let del_button_alt g = Ui.key g.ui ([], `Backspace) true
let crop_button_alt g = Ui.key g.ui ([`Shift], `Backspace) true
let clean_button_alt g = Ui.key g.ui ([`Command], `Backspace) true
let undo_button = edit_button 2 4 "UNDO" ([`Command], `Char 'Z')
let redo_button = edit_button 2 5 "REDO" ([`Shift; `Command], `Char 'Z')
let tag_button = edit_button 3 6 "TAG" ([`Command], `Char 'T')
let save_button = edit_button 4 7 "SAVE" ([`Command], `Char 'S')
let load_button = edit_button 4 8 "LOAD" ([`Command], `Char 'O')

let cut_key g = Ui.key g.ui ([`Command], `Char 'X') true
let copy_key g = Ui.key g.ui ([`Command], `Char 'C') true
let paste_key g = Ui.key g.ui ([`Command], `Char 'V') true

let focus_next_key g = Ui.key g.ui ([], `Tab) true
let focus_prev_key g = Ui.key g.ui ([`Shift], `Tab) true


(* Browser Pane *)

let bp = ep + 1
let browser_pane g = Ui.pane g.ui bp (library_x g, 0, g.browser_width, -1)

(* Divider *)
let browser_divider g = Ui.divider g.ui (bp, - divider_w g, margin g, divider_w g, - bottom_h g) `Horizontal

(* Scan and view buttons *)
let scan_w = 32
let scan_y g = margin g
let scan_indicator_w g = indicator_w g + 5
let scan_indicator_area g = (bp, margin g + (scan_w - scan_indicator_w g)/2, scan_y g + label_h g + 2, scan_indicator_w g, scan_indicator_w g)
let scan_indicator g = Ui.indicator g.ui `Yellow (scan_indicator_area g)
let scan_button g = Ui.mouse g.ui (scan_indicator_area g) `Left
let scan_label g = Ui.label g.ui (bp, margin g, scan_y g, scan_w, label_h g) `Center "SCANNING"

let view_w = 25
let view_h = 12
let view_x g i = - margin g - view_w - i*(view_w + 8) - 2
let view_y g = margin g + indicator_w g + 1
let view_indicator_x g x = function
  | `Center -> x + (view_w - indicator_w g)/2 + 1
  | `Left -> x + 4
  | `Right -> x + view_w - indicator_w g - 4

let view_indicator i al g = Ui.indicator g.ui `Green (bp, view_indicator_x g (view_x g i) al, view_y g - indicator_w g - 1, indicator_w g, indicator_w g)
let view_button i g = Ui.button g.ui (bp, view_x g i, view_y g, view_w, view_h) ([], `None) false
let view_label i label g = Ui.label g.ui (bp, view_x g i - 4, view_y g + view_h + 1, view_w + 8, label_h g) `Center label

let artists_indicator = view_indicator 2 `Center
let artists_button = view_button 2
let artists_label = view_label 2 "ARTISTS"

let albums_indicator1 = view_indicator 1 `Left
let albums_indicator2 = view_indicator 1 `Right
let albums_button = view_button 1
let albums_label = view_label 1 "ALBUMS"

let tracks_indicator1 = view_indicator 0 `Left
let tracks_indicator2 = view_indicator 0 `Right
let tracks_button = view_button 0
let tracks_label = view_label 0 "TRACKS"

(* Search *)
let search_label_w _g = 27
let search_x g = margin g + search_label_w g + 3
let search_y g = 2 * margin g + indicator_w g + view_h + label_h g
let search_label g = Ui.label g.ui (bp, margin g, search_y g + (text_h g - label_h g + 1)/2, search_label_w g, label_h g) `Left "SEARCH"
let search_button g = Ui.mouse g.ui (bp, margin g, search_y g, search_label_w g, text_h g) `Left
let search_box g = Ui.box g.ui (bp, search_x g, search_y g, - divider_w g, text_h g) `Black
let search_text g = Ui.rich_edit_text g.ui (bp, search_x g + 2, search_y g, - divider_w g - 2, text_h g)

(* Browser *)
let browser_y g = search_y g + text_h g + margin g
let browser_area g = (bp, margin g, browser_y g, - divider_w g, - bottom_h g)
let browser_table g = Ui.browser g.ui (browser_area g) (text_h g) (scrollbar_w g) 0 g.reflection
let browser_mouse g = Ui.rich_table_mouse g.ui (browser_area g) (gutter_w g) (text_h g) (scrollbar_w g) 0 false
let browser_error_box g = Ui.box g.ui (browser_area g) (Ui.error_color g.ui)

let del_key g = Ui.key g.ui ([`Command], `Delete) true
let backspace_key g = Ui.key g.ui ([`Command], `Backspace) true


(* View Panes *)

let left_x g = library_x g + g.browser_width
let left_w g = if g.right_shown then g.left_width else library_w g - g.browser_width
let right_w g = if g.right_shown then library_w g - g.browser_width - g.left_width else 0
let upper_h g = if g.lower_shown then g.upper_height else - bottom_h g
let lower_h g = - bottom_h g

(* Upper left view *)
let lp = bp + 1
let left_pane g = Ui.pane g.ui lp (left_x g, 0, left_w g, upper_h g)

let left_area g = (lp, 0, margin g, -1, -1)
let left_table g = Ui.rich_table g.ui (left_area g) (gutter_w g) (text_h g) (scrollbar_w g) (scrollbar_w g) g.reflection
let left_grid g iw = Ui.grid_table g.ui (left_area g) (gutter_w g) iw (text_h g) (scrollbar_w g) g.reflection
let left_mouse g = Ui.rich_table_mouse g.ui (left_area g) (gutter_w g) (text_h g) (scrollbar_w g) (scrollbar_w g) true
let left_spin g = Ui.text g.ui (lp, 4, margin g + text_h g + 4, -scrollbar_w g - gutter_w g, text_h g) `Left `Regular true

let left_view = left_pane, left_area, left_table, left_grid, left_spin

(* Upper right view (optional) *)
let rp = lp + 1
let right_pane g = Ui.pane g.ui rp (left_x g + g.left_width, 0, right_w g, upper_h g)

let right_divider g = Ui.divider g.ui (rp, 0, 0, divider_w g, -1) `Horizontal

let right_area g = (rp, divider_w g, margin g, -1, -1)
let right_table g = Ui.rich_table g.ui (right_area g) (gutter_w g) (text_h g) (scrollbar_w g) (scrollbar_w g) g.reflection
let right_grid g iw = Ui.grid_table g.ui (right_area g) (gutter_w g) iw (text_h g) (scrollbar_w g) g.reflection
let right_mouse g = Ui.rich_table_mouse g.ui (right_area g) (gutter_w g) (text_h g) (scrollbar_w g) (scrollbar_w g) true
let right_spin g = Ui.text g.ui (rp, divider_w g + 4, margin g + text_h g + 4, -scrollbar_w g - gutter_w g, text_h g) `Left `Regular true

let right_view = right_pane, right_area, right_table, right_grid, right_spin

(* Lower view (optional) *)
let lp = rp + 1
let lower_pane g = Ui.pane g.ui lp (left_x g, g.upper_height, library_w g - g.browser_width, lower_h g)

let lower_divider g = Ui.divider g.ui (lp, 0, 0, -1, divider_w g) `Vertical

let lower_area g = (lp, 0, divider_w g, -1, -1)
let lower_table g = Ui.rich_table g.ui (lower_area g) (gutter_w g) (text_h g) (scrollbar_w g) (scrollbar_w g) g.reflection
let lower_grid g iw = Ui.grid_table g.ui (lower_area g) (gutter_w g) iw (text_h g) (scrollbar_w g) g.reflection
let lower_mouse g = Ui.rich_table_mouse g.ui (lower_area g) (gutter_w g) (text_h g) (scrollbar_w g) (scrollbar_w g) true
let lower_spin g = Ui.text g.ui (lp, 4, divider_w g + text_h g + 4, -scrollbar_w g - gutter_w g, text_h g) `Left `Regular true

let lower_view = lower_pane, lower_area, lower_table, lower_grid, lower_spin

(* Keys *)
let lib_cover_key g = Ui.key g.ui ([`Command; `Shift], `Char 'Y') true


(* Message Pane *)

let mp = lp + 1
let info_pane g = Ui.pane g.ui mp (library_x g + g.browser_width, - bottom_h g, library_w g - g.browser_width, bottom_h g)

let msg_x = 0
let msg_w g = - margin g
let msg_box g = Ui.box g.ui (mp, msg_x, footer_y g, msg_w g, text_h g) `Black
let msg_text g = Ui.color_text g.ui (mp, msg_x, footer_y g, msg_w g - 2, text_h g) `Left


(* Directories Pane *)

let dp = mp + 1
let directories_pane g = Ui.pane g.ui dp (library_x g, 0, g.directories_width, -1)

(* Divider *)
let directories_divider g = Ui.divider g.ui (dp, - divider_w g, margin g, divider_w g, - bottom_h g) `Horizontal

(* Directories Browser *)
let directories_area g = (dp, margin g, margin g, - divider_w g, - bottom_h g)
let directories_table g = Ui.browser g.ui (directories_area g) (text_h g) (scrollbar_w g) (scrollbar_w g) g.reflection
let directories_mouse g = Ui.rich_table_mouse g.ui (directories_area g) (gutter_w g) (text_h g) (scrollbar_w g) (scrollbar_w g) false

(* Buttons *)
let select_w g = (g.directories_width - margin g - divider_w g) / 2
let select_h = edit_h
let select_button i c label key g = Ui.labeled_button g.ui (dp, margin g + i * select_w g, - select_h, select_w g, select_h) (button_label_h g + 2) (c g.ui) label key true

let ok_button = select_button 0 Ui.active_color "OK" ([], `Return)
let overwrite_button = select_button 0 Ui.error_color "OVERWRITE" ([], `None)
let cancel_button = select_button 1 Ui.inactive_color "CANCEL" ([], `Escape)

let return_key g = Ui.key g.ui ([], `Return) true


(* Files Pane *)

let fp = dp + 1
let files_pane g = Ui.pane g.ui fp (library_x g + g.directories_width, 0, library_w g - g.directories_width, -1)

(* Table *)
let files_area g = (fp, 0, margin g, -1, -bottom_h g)
let files_table g = Ui.rich_table g.ui (files_area g) (gutter_w g) (text_h g) (scrollbar_w g) (scrollbar_w g) g.reflection
let files_mouse g = Ui.rich_table_mouse g.ui (files_area g) (gutter_w g) (text_h g) (scrollbar_w g) (scrollbar_w g) true

(* Input field *)
let file_label_w _g = 20
let file_label g = Ui.label g.ui (fp, 0, footer_y g + (text_h g - label_h g + 1)/2, file_label_w g, label_h g) `Left "FILE"
let file_button g = Ui.mouse g.ui (fp, margin g, footer_y g, file_label_w g, text_h g) `Left
let file_box g = Ui.box g.ui (fp, file_label_w g, footer_y g, - divider_w g, text_h g) `Black
let file_text g = Ui.rich_edit_text g.ui (fp, file_label_w g + 2, footer_y g, - divider_w g - 2, text_h g)


(* Resizing limits *)

let browser_min _g = 150
let directories_min _g = 150
let left_min _g = 100
let right_min _g = 100
let views_min g = left_min g + right_min g
let files_min _g = 100
let library_min g = max (browser_min g + views_min g) (directories_min g + files_min g)

let upper_min g = margin g + 3 * (text_h g) + scrollbar_w g + 3
let lower_min g = divider_w g + 3 * (text_h g) + scrollbar_w g + 3

let browser_max g = g.library_width - views_min g
let directories_max g = g.library_width - files_min g
let left_max g = g.library_width - g.browser_width - right_min g
let upper_max g = control_h g + g.playlist_height - bottom_h g - lower_min g

let playlist_min g = bottom_h g + max (margin g + 2 * (text_h g)) (upper_min g + lower_min g - control_h g)
