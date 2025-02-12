(* UI Layout *)


(* Global Geometry *)

type t =
{
  ui : Ui.t;
  mutable margin : int;
  mutable text : int;
  mutable label : int;
  mutable gutter : int;
  mutable scrollbar : int;
  mutable playlist_shown : bool;
  mutable playlist_height : int;
  mutable library_shown : bool;
  mutable library_width : int;
  mutable library_side : Api.side;
  mutable browser_width : int;
  mutable left_width : int;
  mutable right_shown : bool;
  mutable upper_height : int;
  mutable lower_shown : bool;
}

let make ui =
  {
    ui;
    margin = 10;
    text = 13;
    label = 8;
    gutter = 7;
    scrollbar = 11;
    playlist_shown = false;
    playlist_height = 200;
    library_shown = false;
    library_width = 600;
    library_side = `Left;
    browser_width = 100;
    left_width = 200;
    right_shown = false;
    upper_height = 200;
    lower_shown = false;
  }


let margin g = g.margin
let divider_w g = g.margin

let text_h g = g.text
let label_h g = g.label
let gutter_w g = g.gutter
let scrollbar_w g = g.scrollbar
let indicator_w _g = 7

let bottom_h g = g.text + g.margin
let footer_y g = - g.text - (bottom_h g - g.text)/2


let control_min_w = 360
let control_min_h = 160
let control_w _g = control_min_w
let control_h _g = control_min_h

let playlist_x g = if g.library_shown && g.library_side = `Left then g.library_width else 0
let library_x g = if g.library_side = `Left then 0 else control_w g - margin g
let library_w g = g.library_width


(* Control Pane *)

let control_pane g = Ui.pane g.ui 0 (playlist_x g, 0, control_w g, control_h g)

(* Power and pane activation buttons *)
let shown_w = 35
let shown_h = 12
let shown_x g = - margin g - shown_w
let shown_indicator_x g = shown_x g + (shown_w - indicator_w g)/2 + 1

let power_h = 22
let power_y g = margin g
let power_button g = Ui.button g.ui (0, shown_x g, power_y g, shown_w, power_h) ([`Command], `Char 'Q')
let power_label g = Ui.label g.ui (0, shown_x g, power_y g + power_h + 1, shown_w, label_h g) `Center "POWER"
let minimize_button g = Ui.mouse g.ui (0, shown_x g, power_y g, shown_w, power_h) `Right

let shown_indicator y g = Ui.indicator g.ui (0, shown_indicator_x g, y - indicator_w g - 1, indicator_w g, indicator_w g)
let shown_button y ch g = Ui.button g.ui (0, shown_x g, y, shown_w, shown_h) ([], `Char ch)
let shown_label y txt g = Ui.label g.ui (0, shown_x g, y + shown_h + 1, shown_w, label_h g) `Center txt

let play_y = 56
let playlist_indicator = shown_indicator play_y
let playlist_button = shown_button play_y 'P'
let playlist_label = shown_label play_y "PLAYLIST"

let lib_y = 93
let library_indicator = shown_indicator lib_y
let library_button = shown_button lib_y 'L'
let library_label = shown_label lib_y "LIBRARY"
let library_mouse g = Ui.mouse g.ui (0, shown_x g, lib_y, shown_w, shown_h) `Right
let library_key g = Ui.key g.ui ([`Shift], `Char 'L')

(* Display box *)
let info_w _g = -55
let info_h _g = -52
let info_margin _g = 4
let info_box g = Ui.box g.ui (0, margin g, margin g, info_w g, info_h g) `Black

(* Volume *)
let volume_w = 27
let volume_h = 50
let volume_x g = info_w g - info_margin g - volume_w
let volume_y g = margin g + info_margin g

let mute_w = 22
let mute_h = 15
let mute_x g = volume_x g - 4
let mute_y g = volume_y g + volume_h - 8
let mute_area g = (0, mute_x g, mute_y g, mute_w, mute_h)

let volume_bar g = Ui.volume_bar g.ui (0, volume_x g, volume_y g, volume_w, volume_h)
let volume_wheel g = Ui.wheel g.ui (0, 0, 0, control_w g, control_h g)

let mute_text g = Ui.color_text g.ui (0, mute_x g, mute_y g, mute_w, 8) `Center
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
let lcd_minus g = Ui.lcd g.ui (0, lcd_x g 0, lcd_y g, lcd_w, lcd_h)
let lcd1 g = Ui.lcd g.ui (0, lcd_x g 1, lcd_y g, lcd_w, lcd_h)
let lcd2 g = Ui.lcd g.ui (0, lcd_x g 2, lcd_y g, lcd_w, lcd_h)
let lcd_colon g = Ui.lcd g.ui (0, lcd_x g 3, lcd_y g, colon_w, lcd_h)
let lcd3 g = Ui.lcd g.ui (0, lcd_x g 3 + colon_w + lcd_space, lcd_y g, lcd_w, lcd_h)
let lcd4 g = Ui.lcd g.ui (0, lcd_x g 4 + colon_w + lcd_space, lcd_y g, lcd_w, lcd_h)
let lcd_button g = Ui.mouse g.ui (0, lcd_x g 0, lcd_y g, colon_w + lcd_x g 4, lcd_h) `Left

(* Info *)
let seek_h _g = 14
let seek_y g = info_h g - info_margin g/2 - seek_h g
let ticker_h _g = 16
let ticker_y g = seek_y g - ticker_h g - 4
let prop_h _g = 12
let prop_y g = ticker_y g - prop_h g - 4

let prop_text g = Ui.text g.ui (0, margin g + info_margin g, prop_y g, mute_x g, prop_h g) `Left
let title_ticker g = Ui.ticker g.ui (0, margin g + info_margin g, ticker_y g, info_w g - info_margin g, ticker_h g)
let seek_bar g = Ui.progress_bar g.ui (0, margin g + info_margin g / 2, seek_y g, info_w g - info_margin g, seek_h g)

(* Hidden mode buttons *)
let color_y g = lcd_y g + lcd_h
let color_button side g = Ui.mouse g.ui (0, margin g, color_y g, mute_x g, ticker_y g) side
let color_button_fwd = color_button `Left
let color_button_bwd = color_button `Right

let fps_text g = Ui.text g.ui (0, 130, margin g + info_margin g, 40, 12) `Left
let fps_key g = Ui.key g.ui ([`Command], `Char 'U')

(* Control buttons *)
let ctl_w = 40
let ctl_h = 30
let control_button i sym ch g = Ui.labeled_button g.ui (0, margin g + i * ctl_w, - 8 - ctl_h, ctl_w, ctl_h) ~protrude: false 10 sym ([], `Char ch)

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

let mode_indicator i al g = Ui.indicator g.ui (0, mode_indicator_x g (mode_x g i) al, mode_y g - indicator_w g - 1, indicator_w g, indicator_w g)
let mode_button i ch g = Ui.button g.ui (0, mode_x g i, mode_y g, mode_w, mode_h) ([], `Char ch)
let mode_label i label g = Ui.label g.ui (0, mode_x g i, mode_y g + mode_h + 1, mode_w, label_h g) `Center label

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

let playlist_pane g = Ui.pane g.ui 1 (playlist_x g, control_h g, control_w g, g.playlist_height)

(* Playlist *)
let playlist_area g = (1, margin g, margin g, - margin g, - bottom_h g)
let playlist_table g = Ui.rich_table g.ui (playlist_area g) (gutter_w g) (text_h g) (scrollbar_w g) 0
let playlist_drop g = Ui.drop g.ui (playlist_area g)

(* Total text field *)
let total_w g = - margin g - scrollbar_w g
let total_x g = total_w g - 100
let playlist_total_box g = Ui.box g.ui (1, total_x g, footer_y g, total_w g, text_h g) `Black
let playlist_total_text g = Ui.text g.ui (1, total_x g, footer_y g, total_w g - (gutter_w g + 1)/2, text_h g) `Right

let enlarge_key g = Ui.key g.ui ([`Command], `Char '+')
let reduce_key g = Ui.key g.ui ([`Command], `Char '-')

(* Buttons *)
let pl_w = 25
let pl_h = 20
let pl_button i j label key g = Ui.labeled_button g.ui (1, margin g + i*5 + j*pl_w, -pl_h, pl_w, pl_h) (label_h g) label key

let sep_button = pl_button 0 0 "SEP" ([`Command], `Char ' ')
let del_button = pl_button 1 1 "DEL" ([], `Delete)
let crop_button = pl_button 1 2 "CROP" ([`Shift], `Delete)
let clean_button = pl_button 1 3 "CLEAN" ([`Command], `Delete)
let undo_button = pl_button 2 4 "UNDO" ([`Command], `Char 'Z')
let redo_button = pl_button 2 5 "REDO" ([`Shift; `Command], `Char 'Z')
let tag_button = pl_button 3 6 "TAG" ([`Command], `Char 'T')
let save_button = pl_button 4 7 "SAVE" ([`Command], `Char 'S')

let cut_key g = Ui.key g.ui ([`Command], `Char 'X')
let copy_key g = Ui.key g.ui ([`Command], `Char 'C')
let paste_key g = Ui.key g.ui ([`Command], `Char 'V')


(* Browser Pane *)

let browser_pane g = Ui.pane g.ui 2 (library_x g, 0, g.browser_width, -1)

(* Divider *)
let browser_divider g = Ui.divider g.ui (2, - divider_w g, margin g, divider_w g, - bottom_h g) `Horizontal

(* Scan and view buttons *)
let scan_w = 32
let scan_y g = margin g
let scan_indicator_w g = indicator_w g + 5
let scan_indicator_area g = (2, margin g + (scan_w - scan_indicator_w g)/2, scan_y g + label_h g + 2, scan_indicator_w g, scan_indicator_w g)
let scan_indicator g = Ui.indicator g.ui (scan_indicator_area g)
let scan_button g = Ui.mouse g.ui (scan_indicator_area g) `Left
let scan_label g = Ui.label g.ui (2, margin g, scan_y g, scan_w, label_h g) `Center "SCANNING"

let view_w = 25
let view_h = 12
let view_x g i = - margin g - view_w - i*(view_w + 8) - 2
let view_y g = margin g + indicator_w g + 1
let view_indicator_x g x = x + (view_w - indicator_w g)/2 + 1
let view_indicator i g = Ui.indicator g.ui (2, view_indicator_x g (view_x g i), view_y g - indicator_w g - 1, indicator_w g, indicator_w g)
let view_button i g = Ui.button g.ui (2, view_x g i, view_y g, view_w, view_h) ([], `None)
let view_label i label g = Ui.label g.ui (2, view_x g i - 4, view_y g + view_h + 1, view_w + 8, label_h g) `Center label

let artists_indicator = view_indicator 2
let artists_button = view_button 2
let artists_label = view_label 2 "ARTISTS"

let albums_indicator = view_indicator 1
let albums_button = view_button 1
let albums_label = view_label 1 "ALBUMS"

let tracks_indicator = view_indicator 0
let tracks_button = view_button 0
let tracks_label = view_label 0 "TRACKS"

(* Browser *)
let browser_y g = margin g + indicator_w g + view_h + label_h g + 10
let browser_area g = (2, margin g, browser_y g, - divider_w g, - bottom_h g)
let browser_table g = Ui.rich_table g.ui (browser_area g) 0 (text_h g) (scrollbar_w g) 0
let browser_drop g = Ui.drop g.ui (browser_area g)
let browser_error_box g = Ui.box g.ui (browser_area g) (Ui.error_color g.ui)

let del_key g = Ui.key g.ui ([`Command], `Delete)


(* View Panes *)

let left_x g = library_x g + g.browser_width
let left_w g = if g.right_shown then g.left_width else library_w g - g.browser_width
let right_w g = if g.right_shown then library_w g - g.browser_width - g.left_width else 0
let upper_h g = if g.lower_shown then g.upper_height else - bottom_h g
let lower_h g = - bottom_h g

(* Upper left view *)
let left_pane g = Ui.pane g.ui 3 (left_x g, 0, left_w g, upper_h g)

let left_area g = (3, 0, margin g, -1, -1)
let left_table g = Ui.rich_table g.ui (left_area g) (gutter_w g) (text_h g) (scrollbar_w g) (scrollbar_w g)

let left_view = left_pane, left_area, left_table

(* Upper right view (optional) *)
let right_pane g = Ui.pane g.ui 4 (left_x g + g.left_width, 0, right_w g, upper_h g)

let right_divider g = Ui.divider g.ui (4, 0, 0, divider_w g, -1) `Horizontal

let right_area g = (4, divider_w g, margin g, -1, -1)
let right_table g = Ui.rich_table g.ui (right_area g) (gutter_w g) (text_h g) (scrollbar_w g) (scrollbar_w g)

let right_view = right_pane, right_area, right_table

(* Lower view (optional) *)
let lower_pane g = Ui.pane g.ui 5 (left_x g, g.upper_height, library_w g - g.browser_width, lower_h g)

let lower_divider g = Ui.divider g.ui (5, 0, 0, -1, divider_w g) `Vertical

let lower_area g = (5, 0, divider_w g, -1, -1)
let lower_table g = Ui.rich_table g.ui (lower_area g) (gutter_w g) (text_h g) (scrollbar_w g) (scrollbar_w g)

let lower_view = lower_pane, lower_area, lower_table


(* Info Pane *)

let info_pane g = Ui.pane g.ui 6 (library_x g + g.browser_width, - bottom_h g, library_w g - g.browser_width, bottom_h g)

let error_x = 0
let error_w g = - margin g
let error_box g = Ui.box g.ui (6, error_x, footer_y g, error_w g, text_h g) `Black
let error_text g = Ui.color_text g.ui (6, error_x, footer_y g, error_w g - 2, text_h g) `Left


(* Resizing limits *)

let browser_min _g = 150
let left_min _g = 100
let right_min _g = 100
let views_min g = left_min g + right_min g
let library_min g = browser_min g + views_min g

let upper_min g = margin g + 3 * (text_h g) + scrollbar_w g + 3
let lower_min g = divider_w g + 3 * (text_h g) + scrollbar_w g + 3

let browser_max g = g.library_width - views_min g
let left_max g = g.library_width - g.browser_width - right_min g
let upper_max g = control_h g + g.playlist_height - bottom_h g - lower_min g

let playlist_min g = bottom_h g + max (margin g + 2 * (text_h g)) (upper_min g + lower_min g - control_h g)
