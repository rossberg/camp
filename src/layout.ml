(* UI Layout *)

(* Basic parameters *)

open Geometry

let rich_table g sh has_heading : Ui.rich_table =
  { gutter_w = gutter_w g;
    text_h = text_h g;
    pad_h = pad_h g;
    scroll_w = scrollbar_w g;
    scroll_h = sh * scrollbar_w g;
    refl_r = g.reflection;
    has_heading
  }

let grid_table g img_h has_heading : Ui.grid_table =
  { gutter_w = gutter_w g;
    img_h;
    text_h = text_h g;
    pad_h = pad_h g;
    scroll_w = scrollbar_w g;
    refl_r = g.reflection;
    has_heading
  }


(* Keys *)

let nokey = ([], `None)
let plain ch = ([], `Char ch)
let cmd ch = ([`Command], `Char ch)
let shiftcmd ch = ([`Shift; `Command], `Char ch)

let key_bwd = plain 'Z'
let key_play = plain 'X'
let key_pause = plain 'C'
let key_stop = plain 'V'
let key_fwd = plain 'B'
let key_eject = plain 'N'
let key_startstop = plain ' '

let key_loop = plain 'L'
let key_repeat = plain 'R'
let key_shuffle = plain 'S'

let key_mute = plain '0'
let key_volup = plain '+'
let key_voldn = plain '-'

let key_next = ([], `Tab)
let key_prev = ([`Shift], `Tab)
let key_del = ([], `Delete)
let key_del2 = ([], `Backspace)
let key_sep = ([], `Insert)
let key_rw = ([], `Arrow `Left)
let key_ff = ([], `Arrow `Right)

let key_ok = ([], `Return)
let key_overwrite = ([`Shift], `Return)
let key_cancel = ([], `Escape)

let key_all = cmd 'A'
let key_rev = cmd 'B'
let key_copy = cmd 'C'
let key_search = cmd 'F'
let key_invert = cmd 'I'
let key_crop = cmd 'K'
let key_lib = cmd 'L'
let key_side = shiftcmd 'L'
let key_queue = cmd 'M'
let key_none = cmd 'N'
let key_load = cmd 'O'
let key_pl = cmd 'P'
let key_quit = cmd 'Q'
let key_min = shiftcmd 'Q'
let key_rescan = cmd 'R'
let key_rescan2 = shiftcmd 'R'
let key_save = cmd 'S'
let key_save2 = shiftcmd 'S'
let key_tag = cmd 'T'
let key_tag2 = shiftcmd 'T'
let key_fps = cmd 'U'
let key_sdf = shiftcmd 'U'
let key_paste = cmd 'V'
let key_wipe = cmd 'W'
let key_dedupe = shiftcmd 'W'
let key_cut = cmd 'X'
let key_visual = cmd 'Y'
let key_libcover = shiftcmd 'Y'
let key_undo = cmd 'Z'
let key_redo = shiftcmd 'Z'
let key_textup = cmd '+'
let key_textdn = cmd '-'
let key_padup = nokey
let key_paddn = nokey
let key_gridup = shiftcmd '+'
let key_griddn = shiftcmd '-'
let key_popupup = cmd ']'
let key_popupdn = cmd '['
let key_scaleup = shiftcmd ']'
let key_scaledn = shiftcmd '['

let key_color = nokey

let key_reorder = nokey
let key_export = nokey

let key_clear_search = nokey
let key_clear_history = nokey

let key_artists = nokey
let key_albums = nokey
let key_tracks = nokey

let key_folddir = plain ' '
let key_namedir = ([], `Return)
let key_namedir2 = ([], `Enter)
let key_adddir = nokey
let key_deldir = nokey
let key_newdir = nokey
let key_revdir = nokey
let key_viewdir = nokey
let key_scandir = nokey


(* Menu *)

let menu g x y = Ui.menu g.ui x y (popup_margin g) (gutter_w g) (text_h g) (pad_h g)


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
let power_area g = (cp, shown_x g, power_y g, shown_w, power_h)
let power_button g = Ui.button g.ui (power_area g) key_quit true
let power_label g = Ui.label g.ui (cp, shown_x g, power_y g + power_h + 3, shown_w, label_h g) `Center "POWER"
let power_shadow g = Ui.box g.ui (cp, shown_x g, power_y g, shown_w + 1, power_h + 2) `Black
let minimize_button g = Ui.invisible_button g.ui (power_area g) [`Shift] key_min true

let shown_indicator y g = Ui.indicator g.ui `Green (cp, shown_indicator_x g, y - indicator_w g - 1, indicator_w g, indicator_w g)
let shown_button y key g = Ui.button g.ui (cp, shown_x g, y, shown_w, shown_h) key true
let shown_label y txt g = Ui.label g.ui (cp, shown_x g, y + shown_h + 2, shown_w, label_h g) `Center txt
let shown_shadow y g = Ui.box g.ui (cp, shown_x g, y, shown_w, shown_h + 1) `Black

let play_y = 57
let playlist_indicator = shown_indicator play_y
let playlist_button = shown_button play_y key_pl
let playlist_label = shown_label play_y "PLAYLIST"
let playlist_shadow = shown_shadow play_y

let lib_y = 93
let library_indicator = shown_indicator lib_y
let library_button = shown_button lib_y key_lib
let library_label = shown_label lib_y "LIBRARY"
let library_shadow = shown_shadow lib_y
let library_side_key g = Ui.key g.ui key_side true

(* Display box *)
let info_w _g = -55
let info_h _g = -52
let info_margin _g = 4
let info_area g = (cp, margin g, margin g, info_w g, info_h g)
let info_box g = Ui.box g.ui (info_area g) `Black
let info_refl g = Ui.mouse_focus g.ui (info_area g) (control_h g + info_h g) 0x30 0

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

let mute_text g = Ui.color_text g.ui (cp, mute_x g, mute_y g, mute_w, label_h g) `Center
let mute_button g = Ui.invisible_button g.ui (mute_area g) [] key_mute true
let mute_drag g = Ui.drag g.ui (mute_area g)

let volup_key g = Ui.key g.ui key_volup
let voldown_key g = Ui.key g.ui key_voldn

(* Time *)
let lcd_space = 3
let colon_w = 4
let lcd_w = 14
let lcd_h = 20
let lcd_x g i = margin g + info_margin g + i*(lcd_w + lcd_space)
let lcd_y g = margin g + info_margin g + 10
let lcd_sign g = Ui.lcd g.ui (cp, lcd_x g 0, lcd_y g, lcd_w, lcd_h)
let lcd_min1 g = Ui.lcd g.ui (cp, lcd_x g 1, lcd_y g, lcd_w, lcd_h)
let lcd_min2 g = Ui.lcd g.ui (cp, lcd_x g 2, lcd_y g, lcd_w, lcd_h)
let lcd_colon g = Ui.lcd g.ui (cp, lcd_x g 3, lcd_y g, colon_w, lcd_h)
let lcd_sec1 g = Ui.lcd g.ui (cp, lcd_x g 3 + colon_w + lcd_space, lcd_y g, lcd_w, lcd_h)
let lcd_sec2 g = Ui.lcd g.ui (cp, lcd_x g 4 + colon_w + lcd_space, lcd_y g, lcd_w, lcd_h)
let lcd_button g = Ui.mouse g.ui (cp, lcd_x g 0, lcd_y g, colon_w + lcd_x g 4, lcd_h) `Left

(* Visuals *)
let visual_x g = lcd_x g 5 + 28
let visual_y g = margin g + info_margin g
let visual_w g = volume_x g - 16
let visual_h _g = 40
let visual_indicator g i = Ui.box g.ui (cp, visual_x g - 8, visual_y g + 8 * i, 4, 4) (Ui.text_color g.ui)
let visual_button g = Ui.mouse g.ui (cp, visual_x g - 20, visual_y g, 20, visual_h g) `Left
let visual_key g = Ui.key g.ui key_visual true

let cover_x g = visual_x g + 10
let cover_y g = visual_y g
let cover_w g = visual_h g * 2
let cover_h g = visual_h g
let cover_area g = (cp, cover_x g, cover_y g, cover_w g, cover_h g)
let cover g = Ui.image g.ui (cover_area g) (`Crop `Vertical)

let graph_x g = visual_x g
let graph_y g = visual_y g
let graph_w g = visual_w g
let graph_h g = visual_h g
let graph_area g = (cp, graph_x g, graph_y g, graph_w g, graph_h g)
let graph_drag g = Ui.drag g.ui (graph_area g)

let novisual_button g = Ui.mouse g.ui (graph_area g) `Left

(* Info *)
let seek_h _g = 14
let seek_y g = info_h g - info_margin g/2 - seek_h g
let ticker_h _g = 16
let ticker_y g = seek_y g - ticker_h g - 3
let prop_h _g = (*12*) 13
let prop_y g = ticker_y g - prop_h g - 2

let prop_text g = Ui.text g.ui (cp, margin g + info_margin g, prop_y g, mute_x g, prop_h g) `Left
let title_ticker g = Ui.ticker g.ui (cp, margin g + info_margin g, ticker_y g, info_w g - info_margin g, ticker_h g)
let seek_bar g = Ui.progress_bar g.ui (cp, margin g + info_margin g / 2, seek_y g, info_w g - info_margin g, seek_h g)

(* Hidden mode buttons *)
let color_y g = visual_y g + visual_h g
let color_button g = Ui.mouse g.ui (cp, margin g, color_y g, mute_x g, ticker_y g) `Left

let fps_w = 40
let fps_text g = Ui.text g.ui (cp, visual_w g - fps_w, margin g + info_margin g, fps_w, 12) `Left
let fps_key g = Ui.key g.ui key_fps true

let sdf_key g = Ui.key g.ui key_sdf true

(* Control buttons *)
let ctl_w = 39
let ctl_h = 30
let ctl_y = - 8 - ctl_h
let control_button i sym key g =
  Ui.labeled_button g.ui (cp, margin g + i * ctl_w, ctl_y, ctl_w, ctl_h)
    ~protrude: false 10 (Ui.active_color g.ui) sym key

let bwd_button = control_button 0 "<<" key_bwd
let play_button = control_button 1 ">" key_play
let pause_button = control_button 2 "||" key_pause
let stop_button = control_button 3 "[]" key_stop
let fwd_button = control_button 4 ">>" key_fwd
let eject_button = control_button 5 "^" key_eject

let button_shadow g = Ui.box g.ui (cp, margin g - 1, ctl_y, 6 * ctl_w + 3, ctl_h + 5) `Black

let start_stop_key g = Ui.key g.ui key_startstop
let rw_key g = Ui.key g.ui key_rw
let ff_key g = Ui.key g.ui key_ff

(* Play mode buttons *)
let mode_w = 25
let mode_h = 12
let mode_x g i = - margin g - mode_w - i * (mode_w + 10)
let mode_y _g = -30
let mode_indicator_x g x = function
  | `Center -> x + (mode_w - indicator_w g)/2 + 1
  | `Left -> x + 4
  | `Right -> x + mode_w - indicator_w g - 4

let mode_indicator i al g = Ui.indicator g.ui `Green (cp, mode_indicator_x g (mode_x g i) al, mode_y g - indicator_w g - 1, indicator_w g, indicator_w g)
let mode_button i key g = Ui.button g.ui (cp, mode_x g i, mode_y g, mode_w, mode_h) key
let mode_label i label g = Ui.label g.ui (cp, mode_x g i, mode_y g + mode_h + 2, mode_w, label_h g) `Center label
let mode_shadow i g = Ui.box g.ui (cp, mode_x g i, mode_y g, mode_w, mode_h + 1) `Black

let shuffle_indicator = mode_indicator 2 `Center
let shuffle_button = mode_button 2 key_shuffle
let shuffle_label = mode_label 2 "SHUFFLE"
let shuffle_shadow = mode_shadow 2

let repeat_indicator1 = mode_indicator 1 `Left
let repeat_indicator2 = mode_indicator 1 `Right
let repeat_button = mode_button 1 key_repeat
let repeat_label = mode_label 1 "REPEAT"
let repeat_shadow = mode_shadow 1

let loop_indicator1 = mode_indicator 0 `Left
let loop_indicator2 = mode_indicator 0 `Right
let loop_button = mode_button 0 key_loop
let loop_label = mode_label 0 "LOOP"
let loop_shadow = mode_shadow 0

(* Pop-ups *)
let info_context g = Ui.mouse g.ui (cp, margin g, margin g, margin g + info_w g - volume_w, info_h g - seek_h g) `Right
let seek_context g = Ui.mouse g.ui (cp, margin g, seek_y g, info_w g - margin g, seek_h g) `Right
let volume_context g = Ui.mouse g.ui (cp, volume_x g, volume_y g, volume_w, volume_h) `Right
let shown_context g = Ui.mouse g.ui (cp, margin g + info_w g, margin g, - margin g, info_h g) `Right
let control_context g = Ui.mouse g.ui (cp, margin g, ctl_y, - margin g, -1) `Right

let cover_popup_open g = Ui.mouse g.ui (cover_area g) `Left

let cover_popup_w g = g.popup_size |>
  min (control_w g + Bool.to_int g.library_shown * g.library_width - 2 * popup_margin g) |>
  min (control_h g + Bool.to_int g.playlist_shown * g.playlist_height - line_h g - 2 * popup_margin g)
let cover_popup_image_size g = Ui.image_size g.ui (-1, 0, 0, cover_popup_w g, cover_popup_w g) `Shrink
let cover_popup g x y iw ih = Ui.popup g.ui x y iw (ih + line_h g) (popup_margin g)
let cover_popup_image g (p, x, y, w, h) = Ui.image g.ui (p, x, y, w, h - line_h g) `Shrink
let cover_popup_text g (p, x, y, w, _) ih = Ui.ticker g.ui (p, x, y + ih + pad_h g, w, text_h g)


(* Playlist Pane *)

let pp = cp + 1
let playlist_pane g = Ui.pane g.ui pp (playlist_x g, control_h g, control_w g, - bottom_h g)

(* Playlist *)
let playlist_area g = (pp, margin g, margin g, - margin g, - 1)
let playlist_table g = Ui.rich_table g.ui (playlist_area g) (rich_table g 0 false)
let playlist_mouse g = Ui.rich_table_mouse g.ui (playlist_area g) (rich_table g 0 false)
let playlist_drag g = Ui.rich_table_drag g.ui (playlist_area g) (rich_table g 0 false) `Above

(* Total text field *)
let total_w g = - margin g - scrollbar_w g
let total_x g = total_w g - 90
let total_y g = g.playlist_height + footer_y g  (* Hack: this is outside the pane *)
let playlist_total_box g = Ui.box g.ui (pp, total_x g, total_y g, total_w g, line_h g) `Black
let playlist_total_text g = Ui.text g.ui (pp, total_x g, total_y g + pad_h g, total_w g - (gutter_w g + 1)/2, text_h g) `Right

let enlarge_text_key g = Ui.key g.ui key_textup true
let reduce_text_key g = Ui.key g.ui key_textdn true

let enlarge_grid_key g = Ui.key g.ui key_gridup true
let reduce_grid_key g = Ui.key g.ui key_griddn true

let enlarge_scale_key g = Ui.key g.ui key_scaleup true
let reduce_scale_key g = Ui.key g.ui key_scaledn true

let enlarge_popup_key g = Ui.key g.ui key_popupup true
let reduce_popup_key g = Ui.key g.ui key_popupdn true


(* Edit Pane *)

let ep = pp + 1
let edit_pane g = Ui.pane g.ui ep (playlist_x g, - bottom_h g, control_w g, bottom_h g)

(* Buttons *)
let edit_w = 27
let edit_h g = line_h g + 7
let edit_area i j g = (ep, margin g + i*5 + j*edit_w, - edit_h g, edit_w, edit_h g)
let edit_button i j label key g = Ui.labeled_button g.ui (edit_area i j g) (button_label_h g) (Ui.inactive_color g.ui) label key true

let tag_button = edit_button 0 0 "TAG" key_tag
let tag_add_button g = Ui.invisible_button g.ui (edit_area 0 0 g) [`Shift] key_tag2 true
let sep_button = edit_button 1 1 "SEP" key_sep
let del_button = edit_button 2 2 "DEL" key_del
let crop_button = edit_button 2 3 "CROP" key_crop
let wipe_button = edit_button 2 4 "WIPE" key_wipe
let dedupe_button g = Ui.invisible_button g.ui (edit_area 2 4 g) [`Shift] key_dedupe true
let del_button_alt g = Ui.key g.ui key_del2 true
let undo_button = edit_button 3 5 "UNDO" key_undo
let redo_button g = Ui.invisible_button g.ui (edit_area 3 5 g) [`Shift] key_redo true
let save_button = edit_button 4 6 "SAVE" key_save
let save_view_button g = Ui.invisible_button g.ui (edit_area 4 6 g) [`Shift] key_save2 true
let load_button = edit_button 4 7 "LOAD" key_load

let cut_key g = Ui.key g.ui key_cut true
let copy_key g = Ui.key g.ui key_copy true
let paste_key g = Ui.key g.ui key_paste true

let focus_next_key g = Ui.key g.ui key_next true
let focus_prev_key g = Ui.key g.ui key_prev true


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
let view_x g i = - margin g - view_w - i*(view_w + 12) - 2
let view_y g = margin g + indicator_w g + 1
let view_indicator_x g x = function
  | `Center -> x + (view_w - indicator_w g)/2 + 1
  | `Left -> x + 4
  | `Right -> x + view_w - indicator_w g - 4

let view_indicator i al g = Ui.indicator g.ui `Green (bp, view_indicator_x g (view_x g i) al, view_y g - indicator_w g - 1, indicator_w g, indicator_w g)
let view_button i key g = Ui.button g.ui (bp, view_x g i, view_y g, view_w, view_h) key false
let view_label i label g = Ui.label g.ui (bp, view_x g i - 4, view_y g + view_h + 1, view_w + 8, label_h g) `Center label

let artists_indicator = view_indicator 2 `Center
let artists_button = view_button 2 key_artists
let artists_label = view_label 2 "ARTISTS"

let albums_indicator1 = view_indicator 1 `Left
let albums_indicator2 = view_indicator 1 `Right
let albums_button = view_button 1 key_albums
let albums_label = view_label 1 "ALBUMS"

let tracks_indicator1 = view_indicator 0 `Left
let tracks_indicator2 = view_indicator 0 `Right
let tracks_button = view_button 0 key_tracks
let tracks_label = view_label 0 "TRACKS"

(* Search *)
let search_label_w _g = 30
let search_x g = margin g + search_label_w g + 3
let search_y g = 2 * margin g + indicator_w g + view_h + label_h g
let search_label g = Ui.label g.ui (bp, margin g, search_y g + (line_h g - label_h g + 1)/2, search_label_w g, label_h g) `Left "SEARCH"
let search_button g = Ui.mouse g.ui (bp, margin g, search_y g, search_label_w g, line_h g) `Left
let search_key g = Ui.key g.ui key_search true
let search_box g = Ui.box g.ui (bp, search_x g, search_y g, - divider_w g, line_h g) `Black
let search_edit g = Ui.rich_edit_text g.ui (bp, search_x g + 2, search_y g, - divider_w g - 2, line_h g) (pad_h g)
let search_context g = Ui.mouse g.ui (bp, search_x g, search_y g, - divider_w g, line_h g) `Right

(* Browser *)
let browser_y g = search_y g + line_h g + margin g
let browser_area g = (bp, margin g, browser_y g, - divider_w g, - bottom_h g)
let browser_table g = Ui.browser g.ui (browser_area g) (rich_table g 0 false)
let browser_mouse g = Ui.rich_table_mouse g.ui (browser_area g) (rich_table g 0 false)
let browser_drag g = Ui.rich_table_drag g.ui (browser_area g) (rich_table g 0 false)
let browser_error_box g = Ui.box g.ui (browser_area g) (Ui.error_color g.ui)

let rename_area g = Ui.browser_entry_text_area g.ui (browser_area g) (rich_table g 0 false)
let rename_box g area = Ui.box g.ui area `Black
let rename_edit g = Ui.rich_edit_text g.ui

let fold_key g = Ui.key g.ui key_folddir
let rename_key g b =
  b && (Ui.key g.ui key_namedir b || Ui.key g.ui key_namedir2 b)

(* Buttons *)
let ledit_w = 25
let ledit_h g = line_h g + 7
let ledit_button i j label key g = Ui.labeled_button g.ui (bp, margin g + i*5 + j*ledit_w, - ledit_h g, ledit_w, ledit_h g) (button_label_h g) (Ui.inactive_color g.ui) label key true

let insert_button = ledit_button 0 0 "ADD" key_adddir
let remove_button = ledit_button 0 1 "DEL" key_deldir
let create_button = ledit_button 0 2 "NEW" key_newdir
let view_button = ledit_button 0 3 "VIEW" key_viewdir
let rescan_button = ledit_button 1 4 "SCAN" key_scandir


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
let left_table g = Ui.rich_table g.ui (left_area g) (rich_table g 1 true)
let left_grid g iw = Ui.grid_table g.ui (left_area g) (grid_table g iw true)
let left_mouse g = Ui.rich_table_mouse g.ui (left_area g) (rich_table g 1 true)
let left_drag g = Ui.rich_table_drag g.ui (left_area g) (rich_table g 1 true) `Above
let left_grid_mouse g iw = Ui.grid_table_mouse g.ui (left_area g) (grid_table g iw true)
let left_grid_drag g iw = Ui.grid_table_drag g.ui (left_area g) (grid_table g iw true) `Left
let left_spin g = Ui.text g.ui (lp, 4 + pad_w g, margin g + line_h g + 4 + pad_h g, -scrollbar_w g - gutter_w g, text_h g) `Left `Regular true

let left_view =
  left_pane, left_area, left_table, left_grid, left_mouse, left_grid_mouse, left_spin

(* Upper right view (optional) *)
let rp = lp + 1
let right_pane g = Ui.pane g.ui rp (left_x g + g.left_width, 0, right_w g, upper_h g)

let right_divider g = Ui.divider g.ui (rp, 0, 0, divider_w g, -1) `Horizontal

let right_area g = (rp, divider_w g, margin g, -1, -1)
let right_table g = Ui.rich_table g.ui (right_area g) (rich_table g 1 true)
let right_grid g iw = Ui.grid_table g.ui (right_area g) (grid_table g iw true)
let right_mouse g = Ui.rich_table_mouse g.ui (right_area g) (rich_table g 1 true)
let right_drag g = Ui.rich_table_drag g.ui (right_area g) (rich_table g 1 true) `Above
let right_grid_mouse g iw = Ui.grid_table_mouse g.ui (right_area g) (grid_table g iw true)
let right_grid_drag g iw = Ui.grid_table_drag g.ui (right_area g) (grid_table g iw true) `Left
let right_spin g = Ui.text g.ui (rp, divider_w g + 4 + pad_w g, margin g + line_h g + 4 + pad_h g, -scrollbar_w g - gutter_w g, text_h g) `Left `Regular true

let right_view =
  right_pane, right_area, right_table, right_grid, right_mouse, right_grid_mouse, right_spin

(* Lower view (optional) *)
let lp = rp + 1
let lower_pane g = Ui.pane g.ui lp (left_x g, g.upper_height, library_w g - g.browser_width, lower_h g)

let lower_divider g = Ui.divider g.ui (lp, 0, 0, -1, divider_w g) `Vertical

let lower_area g = (lp, 0, divider_w g, -1, -1)
let lower_table g = Ui.rich_table g.ui (lower_area g) (rich_table g 1 true)
let lower_grid g iw = Ui.grid_table g.ui (lower_area g) (grid_table g iw true)
let lower_mouse g = Ui.rich_table_mouse g.ui (lower_area g) (rich_table g 1 true)
let lower_drag g = Ui.rich_table_drag g.ui (lower_area g) (rich_table g 1 true) `Above
let lower_grid_mouse g iw = Ui.grid_table_mouse g.ui (lower_area g) (grid_table g iw true)
let lower_grid_drag g iw = Ui.grid_table_drag g.ui (lower_area g) (grid_table g iw true) `Left
let lower_spin g = Ui.text g.ui (lp, 4 + pad_w g, divider_w g + line_h g + 4 + pad_h g, -scrollbar_w g - gutter_w g, text_h g) `Left `Regular true

let lower_view =
  lower_pane, lower_area, lower_table, lower_grid, lower_mouse, lower_grid_mouse, lower_spin

(* Keys *)
let queue_key g = Ui.key g.ui key_queue true
let lib_cover_key g = Ui.key g.ui key_libcover true


(* Log Pane *)

let log_x g = library_x g + g.browser_width
let log_w g = library_w g - g.browser_width
let log_pane g = Ui.pane g.ui lp (log_x g, 0, log_w g, - bottom_h g)

let log_area g = (lp, 0, margin g, -1, -1)
let log_table g = Ui.rich_table g.ui (log_area g) (rich_table g 1 true)

let log_button_w g = (g.browser_width - margin g - divider_w g) / 2
let log_button_h g = edit_h g
let log_button i c label key g = Ui.labeled_button g.ui (bp, margin g + i * log_button_w g, - log_button_h g, log_button_w g, log_button_h g) (button_label_h g) (c g.ui) label key true

let log_ok_button = log_button 0 Ui.active_color "OK" key_ok
let log_cancel_button = log_button 1 Ui.inactive_color "CANCEL" key_cancel


(* Message Pane *)

let mp = lp + 1
let info_pane g = Ui.pane g.ui mp (library_x g + g.browser_width, - bottom_h g, library_w g - g.browser_width, bottom_h g)

let msg_x = 0
let msg_w _g = -1
let msg_box g = Ui.box g.ui (mp, msg_x, footer_y g, msg_w g, line_h g) `Black
let msg_text g = Ui.color_text g.ui (mp, msg_x + pad_w g, footer_y g + pad_h g, msg_w g - 2 - 2 * pad_w g, text_h g) `Left


(* Directories Pane *)

let dp = mp + 1
let directories_pane g = Ui.pane g.ui dp (library_x g, 0, g.directories_width, -1)

(* Divider *)
let directories_divider g = Ui.divider g.ui (dp, - divider_w g, margin g, divider_w g, - bottom_h g) `Horizontal

(* Directories Browser *)
let directories_area g = (dp, margin g, margin g, - divider_w g, - bottom_h g)
let directories_table g = Ui.browser g.ui (directories_area g) (rich_table g 1 false)
let directories_mouse g = Ui.rich_table_mouse g.ui (directories_area g) (rich_table g 1 false)

(* Buttons *)
let select_button_w g = (g.directories_width - margin g - divider_w g) / 2
let select_button_h g = edit_h g
let select_button i c label key g = Ui.labeled_button g.ui (dp, margin g + i * select_button_w g, - select_button_h g, select_button_w g, select_button_h g) (button_label_h g) (c g.ui) label key true

let select_ok_button = select_button 0 Ui.active_color "OK" key_ok
let select_overwrite_button = select_button 0 Ui.error_color "OVERWRITE" key_overwrite
let select_cancel_button = select_button 1 Ui.inactive_color "CANCEL" key_cancel

let return_key g = Ui.key g.ui key_ok true


(* Files Pane *)

let fp = dp + 1
let files_pane g = Ui.pane g.ui fp (library_x g + g.directories_width, 0, library_w g - g.directories_width, -1)

(* Table *)
let files_area g = (fp, 0, margin g, -1, -bottom_h g)
let files_table g = Ui.rich_table g.ui (files_area g) (rich_table g 1 true)
let files_mouse g = Ui.rich_table_mouse g.ui (files_area g) (rich_table g 1 true)

(* Input field *)
let file_label_w _g = 20
let file_label g = Ui.label g.ui (fp, 0, footer_y g + (line_h g - label_h g + 1)/2, file_label_w g, label_h g) `Left "FILE"
let file_button g = Ui.mouse g.ui (fp, margin g, footer_y g, file_label_w g, line_h g) `Left
let file_box g = Ui.box g.ui (fp, file_label_w g, footer_y g, - divider_w g, line_h g) `Black
let file_edit g = Ui.rich_edit_text g.ui (fp, file_label_w g + 2, footer_y g, - divider_w g - 2, line_h g) (pad_h g)
