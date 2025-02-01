(* UI Layout *)


(* Global Configuration *)

let margin = 10
let divider_w = margin

let bottom_h = 24
let footer_h = row_h
let footer_y = -footer_h-(bottom_h-footer_h)/2

let row_h = 13
let gutter_w = 7

let scrollbar_w = 10

let indicator_w = 7
let label_h = 8


(* Control Pane *)

let control_pane = Ui.pane 0

let control_w = 360
let control_h = 160

(* Power and pane activation buttons *)
let shown_w = 35
let shown_h = 12
let shown_x = -margin-shown_w
let shown_indicator_x = shown_x+(shown_w-indicator_w)/2+1

let power_h = 22
let power_y = margin
let power_button = Ui.button (0, shown_x, power_y, shown_w, power_h) ([`Command], `Char 'Q')
let power_label = Ui.label (0, shown_x, power_y+power_h+1, shown_w, label_h) `Center "POWER"
let minimize_button = Ui.mouse (0, shown_x, power_y, shown_w, power_h) `Right

let shown_indicator y = Ui.indicator (0, shown_indicator_x, y-indicator_w-1, indicator_w, indicator_w)
let shown_button y ch = Ui.button (0, shown_x, y, shown_w, shown_h) ([], `Char ch)
let shown_label y txt = Ui.label (0, shown_x, y+shown_h+1, shown_w, label_h) `Center txt

let play_y = 56
let playlist_indicator = shown_indicator play_y
let playlist_button = shown_button play_y 'P'
let playlist_label = shown_label play_y "PLAYLIST"

let lib_y = 93
let library_indicator = shown_indicator lib_y
let library_button = shown_button lib_y 'L'
let library_label = shown_label lib_y "LIBRARY"
let library_mouse = Ui.mouse (0, shown_x, lib_y, shown_w, shown_h) `Right
let library_key = Ui.key ([`Shift], `Char 'L')

(* Display box *)
let info_w = -55
let info_h = 98
let info_margin = 4
let info_box = Ui.box (0, margin, margin, info_w, info_h) `Black

let lcd_space = 3
let lcd_w = 14
let lcd_h = 20
let lcd_x i = margin+info_margin+i*(lcd_w+lcd_space)
let lcd_y = margin+info_margin+10
let lcd_minus = Ui.lcd (0, lcd_x 0, lcd_y, lcd_w, lcd_h)
let lcd1 = Ui.lcd (0, lcd_x 1, lcd_y, lcd_w, lcd_h)
let lcd2 = Ui.lcd (0, lcd_x 2, lcd_y, lcd_w, lcd_h)
let lcd_colon = Ui.lcd (0, lcd_x 3, lcd_y, 4, lcd_h)
let lcd3 = Ui.lcd (0, 4+lcd_space+lcd_x 3, lcd_y, lcd_w, lcd_h)
let lcd4 = Ui.lcd (0, 4+lcd_space+lcd_x 4, lcd_y, lcd_w, lcd_h)
let lcd_button = Ui.mouse (0, lcd_x 0, lcd_y, 4+lcd_x 4, lcd_h) `Left

let color_y = lcd_y+lcd_h
let color_button = Ui.mouse (0, margin, color_y, mute_x, title_y-color_y)
let color_button_fwd = color_button `Left
let color_button_bwd = color_button `Right

let fps_text = Ui.text (0, 130, margin+info_margin, 40, 12) `Left
let fps_key = Ui.key ([`Command], `Char 'U')

let seek_h = 14
let seek_y = margin+info_h-info_margin/2-seek_h
let title_h = 16
let title_y = seek_y-title_h-4

let prop_text = Ui.text (0, margin+info_margin, lcd_y+lcd_h+10, mute_x, 12) `Left
let title_ticker = Ui.ticker (0, margin+info_margin, title_y, info_w-info_margin, title_h)
let seek_bar = Ui.progress_bar (0, margin+info_margin/2, seek_y, info_w-info_margin, seek_h)

let rw_key = Ui.key ([], `Arrow `Left)
let ff_key = Ui.key ([], `Arrow `Right)

(* Volume *)
let volume_w = 27
let volume_h = 50
let volume_x = info_w-info_margin-volume_w
let volume_y = margin+info_margin

let mute_w = 22
let mute_h = 15
let mute_x = volume_x-4
let mute_y = volume_y+volume_h-8
let mute_area = (0, mute_x, mute_y, mute_w, mute_h)

let volume_bar = Ui.volume_bar (0, volume_x, volume_y, volume_w, volume_h)
let volume_wheel = Ui.wheel (0, 0, 0, control_w, control_h)

let mute_text = Ui.color_text (0, mute_x, mute_y, mute_w, 8) `Center
let mute_button = Ui.mouse mute_area `Left
let mute_drag = Ui.drag mute_area

let mute_key = Ui.key ([], `Char '0')
let volup_key = Ui.key ([], `Char '+')
let voldown_key = Ui.key ([], `Char '-')

(* Control buttons *)
let ctl_w = 40
let ctl_h = 30
let control_button i sym ch =
  Ui.labeled_button (0, margin+i*ctl_w, -8-ctl_h, ctl_w, ctl_h)
    ~protrude: false 10 sym ([], `Char ch)

let bwd_button = control_button 0 "<<" 'Z'
let play_button = control_button 1 ">" 'X'
let pause_button = control_button 2 "||" 'C'
let stop_button = control_button 3 "[]" 'V'
let fwd_button = control_button 4 ">>" 'B'
let eject_button = control_button 5 "^" 'N'
let start_stop_key = Ui.key ([], `Char ' ')

(* Play mode buttons *)
let mode_w = 25
let mode_h = 12
let mode_x i = -margin-mode_w-i*(mode_w+8)
let mode_y = 130
let mode_indicator_x x = function
  | `Center -> x + (mode_w - indicator_w)/2 + 1
  | `Left -> x + 4
  | `Right -> x + mode_w - indicator_w - 4

let mode_indicator i al = Ui.indicator (0, mode_indicator_x (mode_x i) al, mode_y-indicator_w-1, indicator_w, indicator_w)
let mode_button i ch = Ui.button (0, mode_x i, mode_y, mode_w, mode_h) ([], `Char ch)
let mode_label i label = Ui.label (0, mode_x i, mode_y+mode_h+1, mode_w, label_h) `Center label

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

let playlist_pane = Ui.pane 1
let playlist_min = 31 + 4 * row_h

(* Playlist *)
let playlist_area = (1, margin, margin, -margin, -bottom_h)
let playlist_table rh = Ui.rich_table playlist_area gutter_w rh scrollbar_w 0
let playlist_drop = Ui.drop playlist_area

(* Total text field *)
let total_w = -margin-scrollbar_w
let total_x = total_w-100
let playlist_total_box = Ui.box (1, total_x, footer_y, total_w, footer_h) `Black
let playlist_total_text = Ui.text (1, total_x, footer_y, total_w-(gutter_w+1)/2, footer_h) `Right

let enlarge_key = Ui.key ([`Command], `Char '+')
let reduce_key = Ui.key ([`Command], `Char '-')

(* Buttons *)
let pl_w = 25
let pl_h = bottom_h-4
let pl_button i j label key =
  Ui.labeled_button (1, margin+i*5+j*pl_w, -pl_h, pl_w, pl_h) label_h label key
let save_button = pl_button 0 0 "SAVE" ([`Command], `Char 'S')
let tag_button = pl_button 1 1 "TAG" ([`Command], `Char 'T')
let del_button = pl_button 2 2 "DEL" ([], `Delete)
let crop_button = pl_button 2 3 "CROP" ([`Shift], `Delete)
let clean_button = pl_button 2 4 "CLEAN" ([`Command], `Delete)
let undo_button = pl_button 3 5 "UNDO" ([`Command], `Char 'Z')
let redo_button = pl_button 3 6 "REDO" ([`Shift; `Command], `Char 'Z')
let sep_button = pl_button 4 7 "SEP" ([`Command], `Char ' ')

let cut_key = Ui.key ([`Command], `Char 'X')
let copy_key = Ui.key ([`Command], `Char 'C')
let paste_key = Ui.key ([`Command], `Char 'V')


(* Browser Pane *)

let browser_pane = Ui.pane 2
let browser_min = 150
let views_min = 200
let library_min = browser_min + views_min
let browser_max pw = pw-views_min

(* Divider *)
let browser_divider = Ui.divider (2, -divider_w, margin, divider_w, -bottom_h) `Horizontal

(* Scan and view buttons *)
let scan_w = 32
let scan_y = margin
let scan_indicator_w = indicator_w+5
let scan_indicator = Ui.indicator (2, margin+(scan_w-scan_indicator_w)/2, scan_y+label_h+2, scan_indicator_w, scan_indicator_w)
let scan_label = Ui.label (2, margin, scan_y, scan_w, label_h) `Center "SCANNING"

let view_w = 25
let view_h = 12
let view_x i = -margin-view_w-i*(view_w+8)-2
let view_y = margin+indicator_w+1
let view_indicator_x x = x + (view_w - indicator_w)/2 + 1
let view_indicator i = Ui.indicator (2, view_indicator_x (view_x i), view_y-indicator_w-1, indicator_w, indicator_w)
let view_button i = Ui.button (2, view_x i, view_y, view_w, view_h) ([], `None)
let view_label i label = Ui.label (2, view_x i-4, view_y+view_h+1, view_w+8, label_h) `Center label

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
let browser_y = margin+indicator_w+view_h+label_h+10
let browser_area = (2, margin, browser_y, -divider_w, -bottom_h)
let browser_table rh = Ui.rich_table browser_area 0 rh scrollbar_w 0
let browser_drop = Ui.drop browser_area
let browser_error_box = Ui.box browser_area

let del_key = Ui.key ([`Command], `Delete)


(* View Panes *)

(* Upper left view *)
let left_pane = Ui.pane 3

let left_area = (3, 0, margin, -1, -1)
let left_table rh = Ui.rich_table left_area gutter_w rh scrollbar_w scrollbar_w

(* Upper right view (optional) *)
let right_pane = Ui.pane 4

let right_divider = Ui.divider (4, 0, 0, divider_w, -1) `Horizontal

let right_area = (4, divider_w, margin, -1, -1)
let right_table rh = Ui.rich_table right_area gutter_w rh scrollbar_w scrollbar_w

(* Lower view (optional) *)
let lower_pane = Ui.pane 5

let lower_divider = Ui.divider (5, 0, 0, -1, divider_w) `Vertical

let lower_area = (5, 0, divider_w, -1, -1)
let lower_table rh = Ui.rich_table lower_area gutter_w rh scrollbar_w scrollbar_w


(* Info Pane *)

let info_pane = Ui.pane 6

let error_x = 0
let error_w = -margin
let error_box = Ui.box (6, error_x, footer_y, error_w, footer_h) `Black
let error_text = Ui.color_text (6, error_x, footer_y, error_w-2, footer_h) `Left
