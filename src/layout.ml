(* UI Layout *)

open Geometry


(* Key Binndings *)

module KeyBind =
struct
  type t = Api.(modifier list * key)

  let na = ([], `None)
  let plain ch = ([], `Char ch)
  let shift ch = ([`Shift], `Char ch)
  let cmd ch = ([`Command], `Local ch)
  let shiftcmd ch = ([`Shift; `Command], `Local ch)

  let bwd = plain 'Z'
  let play = plain 'X'
  let pause = plain 'C'
  let stop = plain 'V'
  let fwd = plain 'B'
  let eject = plain 'N'
  let start_stop = plain ' '

  let loop = plain 'L'
  let repeat = plain 'R'
  let shuffle = plain 'S'

  let mute = plain '0'
  let vol_up = plain '+'
  let vol_dn = plain '-'

  let next = ([], `Tab)
  let prev = ([`Shift], `Tab)
  let del = ([], `Delete)
  let del2 = ([], `Backspace)
  let sep = ([], `Insert)
  let rw = ([], `Arrow `Left)
  let ff = ([], `Arrow `Right)

  let ok = ([], `Return)
  let overwrite = ([`Shift], `Return)
  let cancel = ([], `Escape)

  let all = cmd 'A'
  let rev = cmd 'B'
  let copy = cmd 'C'
  let search = cmd 'F'
  let invert = cmd 'I'
  let crop = cmd 'K'
  let lib = cmd 'L'
  let side = shiftcmd 'L'
  let queue = cmd 'M'
  let none = cmd 'N'
  let load = cmd 'O'
  let pl = cmd 'P'
  let quit = cmd 'Q'
  let min = shiftcmd 'Q'
  let rescan = cmd 'R'
  let rescan2 = shiftcmd 'R'
  let save = cmd 'S'
  let save2 = shiftcmd 'S'
  let tag = cmd 'T'
  let tag2 = shiftcmd 'T'
  let fps = cmd 'U'
  let sdf = shiftcmd 'U'
  let paste = cmd 'V'
  let wipe = cmd 'W'
  let dedupe = shiftcmd 'W'
  let cut = cmd 'X'
  let visual = cmd 'Y'
  let covers = shiftcmd 'Y'
  let undo = cmd 'Z'
  let redo = shiftcmd 'Z'
  let append_left = plain '<'
  let append_right = plain '>'
  let replace_left = shift '<'
  let replace_right = shift '>'
  let text_up = cmd '+'
  let text_dn = cmd '-'
  let pad_up = na
  let pad_dn = na
  let grid_up = shiftcmd '+'
  let grid_dn = shiftcmd '-'
  let zoom_up = cmd ']'
  let zoom_dn = cmd '['
  let scale_up = shiftcmd ']'
  let scale_dn = shiftcmd '['
  let settings = cmd ','
  let dev_settings = shiftcmd ','

  let color = na

  let reorder = na
  let export = na

  let clear_search = na
  let clear_history = na

  let artists = na
  let albums = na
  let tracks = na

  let fold_dir = plain ' '
  let name_dir = ([], `Return)
  let name_dir2 = ([], `Enter)
  let add_dir = na
  let del_dir = na
  let new_dir = na
  let rev_dir = na
  let view_dir = na
  let scan_dir = na
  let repair_dir = na
end


(* Main Window *)

module Window (G : sig val it : t end) =
struct
  let g = G.it

  (* Basic Parameters *)

  let subowner owner sub = owner ^ ":" ^ sub
  let idxowner owner i = owner ^ "_" ^ string_of_int i

  let rich_table_style sh has_heading : Ui.rich_table_style =
    { gutter_w = gutter_w g;
      text_h = text_h g;
      pad_h = pad_h g;
      scroll_w = scrollbar_w g;
      scroll_h = sh * scrollbar_w g;
      scroll_l = scrollbar_l g;
      refl_r = g.reflection;
      has_heading
    }

  let grid_table_style has_heading : Ui.grid_table_style =
    { gutter_w = gutter_w g;
      img_h = g.grid;
      text_h = text_h g;
      pad_h = pad_h g;
      scroll_w = scrollbar_w g;
      scroll_l = scrollbar_l g;
      refl_r = g.reflection;
      has_heading
    }

  let menu_style : Ui.menu_style =
    { margin = g.margin;
      gutter_w = gutter_w g;
      text_h = text_h g;
      pad_h = pad_h g;
      scroll_w = scrollbar_w g;
      scroll_h = scrollbar_w g;
      scroll_l = scrollbar_l g;
      refl_r = g.reflection;
    }

  let settings_style : Ui.settings_style =
    { margin = g.margin;
      item_h = label_h g * 4 / 3;
      label_h = label_h g;
      pad_w = label_h g / 3;
      pad_h = label_h g / 4;
      sep_h = label_h g * 2 / 3;
      sep_w = 2 * margin g;
      indent_w = margin g;
      scroll_w = scrollbar_w g;
      scroll_l = scrollbar_l g;
    }


  let sx, sy, smin = sx g, sy g, smin g
  let margin = margin g
  let footer = footer_y g
  let bottom = bottom_h g
  let sep = sep g
  let div = divider_w g
  let line = line_h g
  let text = text_h g
  let gutter = gutter_w g
  let padw, padh = pad_w g, pad_h g

  let focus_key k b = Ui.key g.ui k b
  let key k () = focus_key k true


  (* Global keys *)

  module Key =
  struct
    let next_focus = key KeyBind.next
    let prev_focus = key KeyBind.prev

    let enlarge_text = key KeyBind.text_up
    let reduce_text = key KeyBind.text_dn

    let enlarge_grid = key KeyBind.grid_up
    let reduce_grid = key KeyBind.grid_dn

    let enlarge_scale = key KeyBind.scale_up
    let reduce_scale = key KeyBind.scale_dn

    let enlarge_zoom = key KeyBind.zoom_up
    let reduce_zoom = key KeyBind.zoom_dn

    let settings = key KeyBind.settings
    let dev_settings = key KeyBind.dev_settings

    let covers = key KeyBind.covers
  end


  (* Control Pane *)

  module Control =
  struct
    let x, y, w, h as r = (control_x g, control_y g, control_w g, control_h g)
    let p = Ui.pane g.ui "ctl" r

    let iw = indicator_w g
    let lh = label_h g

    (* Power and pane activation buttons *)
    module Power =
    struct
      let w = sx 35
      let h = sy 22
      let x = - margin - w
      let y = margin
      let total_h = h + sy 3 + lh

      let key = key KeyBind.quit
      let button b =
        Ui.button g.ui "power_but" (p, x, y, w, h) KeyBind.quit b (Some false)
      let label () =
        Ui.label g.ui (p, x, y + h + sy 3, w, lh) `Center "POWER"
      let shadow () = Ui.box g.ui (p, x, y, w + sx 1, h + sy 2) `Black
      let minimize () =
        Ui.invisible_button g.ui "power_but" (p, x, y, w, h) [`Shift]
          KeyBind.min true
    end

    module Shown =
    struct
      let x = Power.x
      let w = Power.w
      let h = sy 12
      let total_h = iw + 1 + h + sy 2 + lh
      let ix = x + (w - iw)/2 + sy 1

      module Mk (X : sig val i : int val label : string val key : KeyBind.t end) =
      struct
        open X

        let y = Power.y + Power.total_h + i*total_h + (i + 1) * sy 5 + iw + sy 1

        let indicator = Ui.indicator g.ui `Green (p, ix, y - iw - sy 1, iw, iw)
        let button =
          Ui.button g.ui (idxowner "shown_but" i) (p, x, y, w, h) key true
        let label () = Ui.label g.ui (p, x, y + h + sy 2, w, lh) `Center label
        let shadow () = Ui.box g.ui (p, x, y, w, h + sy 1) `Black
      end

      module Playlist =
        Mk (struct let i = 0 let label = "PLAYLIST" let key = KeyBind.pl end)
      module Library =
        Mk (struct let i = 1 let label = "LIBRARY" let key = KeyBind.lib end)

      module Key =
      struct
        let side = key KeyBind.side
      end
    end

    (* Navigation buttons *)
    module Nav =
    struct
      let w = sx 39
      let h = sy 30
      let y = - sy 8 - h

      let mk_button i sym key =
        Ui.labeled_button g.ui (idxowner "ctl_but" i)
          (p, margin + i * w, y, w, h) ~protrude: false (smin 10)
          (Ui.active_color g.ui) sym key

      let bwd = mk_button 0 "<<" KeyBind.bwd
      let play = mk_button 1 ">" KeyBind.play
      let pause = mk_button 2 "||" KeyBind.pause
      let stop = mk_button 3 "[]" KeyBind.stop
      let fwd = mk_button 4 ">>" KeyBind.fwd
      let eject = mk_button 5 "^" KeyBind.eject

      let shadow () =
        Ui.box g.ui (p, margin - smin 1, y, 6 * w + smin 3, h + smin 5) `Black

      module Key =
      struct
        let start_stop = focus_key KeyBind.start_stop
        let rw = focus_key KeyBind.rw
        let ff = focus_key KeyBind.ff
      end
    end

    (* Play mode buttons *)
    module Mode =
    struct
      let w = sx 25
      let h = sy 12
      let y = Nav.y + iw + (Nav.h - iw - h - lh) / 2

      module Mk (X : sig val i : int val label : string val key : KeyBind.t end) =
      struct
        open X

        let x = - margin - w - i * (w + sx 10)

        let ix = function
          | `Center -> x + (w - iw)/2 + sx 1
          | `Left -> x + (w - 2 * iw - sep) / 2
          | `Right -> x + w - (w - 2 * iw - sep) / 2 - iw

        let mk_indicator al =
          Ui.indicator g.ui `Green (p, ix al, y - iw - sx 1, iw, iw)

        let indicator = mk_indicator `Center
        let indicator1 = mk_indicator `Left
        let indicator2 = mk_indicator `Right

        let button = Ui.button g.ui (idxowner "mode_but" i) (p, x, y, w, h) key
        let label () = Ui.label g.ui (p, x, y + h + sy 2, w, lh) `Center label
        let shadow () = Ui.box g.ui (p, x, y, w, h + sy 1) `Black
      end

      module Shuffle =
        Mk (struct let i = 2 let label = "SHUFFLE" let key = KeyBind.shuffle end)
      module Repeat =
        Mk (struct let i = 1 let label = "REPEAT" let key = KeyBind.repeat end)
      module Loop =
        Mk (struct let i = 0 let label = "LOOP" let key = KeyBind.loop end)
    end

    (* Display box *)
    module Info =
    struct
      let x = margin
      let y = margin
      let w = - Power.w - 2 * margin
      let h = Nav.y - margin
      let margin' = smin 4

      let area = (p, x, y, w, h)
      let box () = Ui.box g.ui area `Black
      let refl () = Ui.mouse_focus g.ui area (control_h g + h) 0x30 0

      (* Volume *)
      module Volume =
      struct
        let info_w, info_y = w, y
        let w = sx 27
        let h = sy 50
        let x = info_w - margin' - w
        let y = info_y + margin'

        let bar = Ui.volume_bar g.ui "vol_bar" (p, x, y, w, h) (smin 1)
        let wheel () = Ui.wheel g.ui (p, 0, 0, control_w g, control_h g)

        module Key =
        struct
          let up = focus_key KeyBind.vol_up
          let down = focus_key KeyBind.vol_dn
        end
      end

      (* Mute *)
      module Mute =
      struct
        let w = sx 22
        let h = lh
        let x = Volume.x - sx 4
        let y = Volume.y + Volume.h - h
        let area = (p, x, y, w, h)

        let text b =
          Ui.color_text g.ui (p, x, y, w, h) `Center `Red `Inverted b "MUTE"
        let button () = Ui.invisible_button g.ui "mute_but" area [] KeyBind.mute true
        let drag () = Ui.drag g.ui "mute_drag" area (0, 0)
      end

      (* Seek bar *)
      module Seek =
      struct
        let info_x, info_w, info_h = x, w, h
        let margin = margin' / 2
        let w = info_w - margin
        let h = smin 14
        let x = info_x + margin
        let y = info_h - margin - h

        let bar v = Ui.progress_bar g.ui "seek_bar" (p, x, y, w, h) (smin 1) v
      end

      (* Ticker *)
      module Ticker =
      struct
        let info_x, info_w = x, w
        let w = info_w - margin'
        let h = min 64 (sy 16)
        let x = info_x + margin'
        let y = Seek.y - h - sy 3

        let title = Ui.ticker g.ui (p, x, y, w, h)
      end

      (* Property line *)
      module Prop =
      struct
        let info_x = x
        let w = Mute.x
        let h = min 64 (smin (*12*) 13)
        let x = info_x + margin'
        let y = Ticker.y - h - sy 2

        let text = Ui.text g.ui (p, x, y, w, h) `Left `Regular true
      end

      (* Time *)
      module Lcd =
      struct
        let space = sx 3
        let col = sx 4
        let w = sx 14
        let h = sy 20
        let x i = margin + margin' + i * (w + space)
        let y = margin + margin' + sy 10

        let sign = Ui.lcd g.ui (p, x 0, y, w, h)
        let min1 = Ui.lcd g.ui (p, x 1, y, w, h)
        let min2 = Ui.lcd g.ui (p, x 2, y, w, h)
        let colon = Ui.lcd g.ui (p, x 3, y, col, h)
        let sec1 = Ui.lcd g.ui (p, x 3 + col + space, y, w, h)
        let sec2 = Ui.lcd g.ui (p, x 4 + col + space, y, w, h)

        let button () = Ui.mouse g.ui "lcd_but" (p, x 0, y, col + x 4, h) `Left
      end

      (* Visuals *)
      module Visual =
      struct
        let x = Lcd.x 5 + sx 28
        let y = y + margin'
        let w = Volume.x - sx 16
        let h = Prop.y - margin'
        let area = (p, x, y, w, h)

        let button () =
          Ui.mouse g.ui "vis_but" (p, x - sx 20, y, sx 20, h) `Left
        let key = key KeyBind.visual

        let cw = w - sx 20
        let ch = h
        let cx = x + (w - cw) / 2
        let cy = y
        let cover_area = (p, cx, cy, cw, ch)

        let fw = smin 40
        let fps =
          Ui.text g.ui (p, w - fw, margin + margin', fw, smin 12)
            `Left `Regular true
      end

      (* Keys & Hidden mode buttons *)
      module Button =
      struct
        let fps = key KeyBind.fps
        let sdf = key KeyBind.sdf

        let zoom () = Ui.mouse g.ui "zoom_but" Visual.area `Left
        let color () =
          Ui.mouse g.ui "color_but"
            (p, margin, Prop.y, Mute.x, Ticker.y + Ticker.h) `Left
      end
    end

    (* Context menus *)
    module Context =
    struct
      open Info
      let mouse area owner = Ui.mouse g.ui area owner `Right

      let info = mouse "info_ctx" (p, margin, margin, w - Volume.w, h - Seek.h)
      let seek = mouse "seek_ctx" (p, margin, Seek.y, w - margin, Seek.h)
      let volume = mouse "vol_ctx" (p, Volume.x, Volume.y, Volume.w, Volume.h)
      let shown = mouse "shown_ctx" (p, margin + Info.w, margin, -margin, Info.h)
      let nav = mouse "nav_ctx" (p, margin, Nav.y, -margin, -1)
    end
  end


  (* Divider Panes *)

  module Divider =
  struct
    module Y () =
    struct
      let px, py, pw = playlist_x g, playlist_y g, playlist_w g
      let p = Ui.pane g.ui "div_y" (px, py, pw, div)

      let mid =
        Ui.divider g.ui "ext_div_h" (p, margin, 0, -margin, -1) `Vertical
      let left =
        Ui.divider2 g.ui "ext_div_wh_l" (p, 0, 0, margin, -1) `NE_SW
      let right =
        Ui.divider2 g.ui "ext_div_wh_r" (p, -margin, 0, -1, -1) `NW_SE
    end

    module X () =
    struct
      let cx = control_x g
      let py = playlist_y g
      let lx, ly = library_x g, library_y g

      let left = extension_left g
      let p = Ui.pane g.ui "div_x" ((if left then cx else lx), ly, div, -1)

      let upper =
        Ui.divider g.ui "ext_div_w_u"
          (p, 0, margin, -1, py - ly - margin) `Horizontal
      let lower =
        Ui.divider g.ui "ext_div_w_l"
          (p, 0, py - ly + div, -1, - margin) `Horizontal
      let mid =
        Ui.divider2 g.ui "ext_div_wh_m"
          (p, 0, py - ly, -1, div) (if left then `NE_SW else `NW_SE)
      let top =
        Ui.divider2 g.ui "ext_div_wh_t"
          (p, 0, 0, -1, margin) (if left then `NW_SE else `NE_SW)
      let bot =
        Ui.divider2 g.ui "ext_div_wh_b"
          (p, 0, - margin, -1, -1) (if left then `NE_SW else `NW_SE)
    end
  end


  (* Playlist *)

  module Playlist =
  struct
    let sep = edit_sep g

    let p = Ui.pane g.ui "pl" (playlist_x g, playlist_y g, playlist_w g, -1)

    let style = rich_table_style 0 g.playlist_headers
    let area = (p, margin, margin, -margin, -bottom)

    let table args = Ui.rich_table g.ui "pl" area style args
    let mouse args = Ui.rich_table_mouse g.ui area style args
    let drag args = Ui.rich_table_drag g.ui area style `Above args

    module Total =
    struct
      let tw = -margin - scrollbar_w g
      let tx = margin + 4 * sep + 7 * edit_w g  (* = Edit.x 4 7 *)
      let ty = footer

      let box () = Ui.box g.ui (p, tx, ty, tw, line) `Black
      let text =
        Ui.text g.ui (p, tx, ty + padh, tw - (gutter + 1)/2, text)
          `Right `Regular true
    end

    (* Edit Pane *)

    module Edit =
    struct
      let p = Ui.pane g.ui "edit" (playlist_x g, -bottom, playlist_w g, bottom)

      let w, h, sep = edit_w g, edit_h g, edit_sep g

      (* Buttons *)
      module Button =
      struct
        let x i j = margin + i * sep + j * w
        let y = if extension_shown_h g then -h else control_h g (* effectively hidden *)
        let area i j = (p, x i j, y, w, h)

        let button i j label key =
          Ui.labeled_button g.ui (idxowner "edit_but" j) (area i j)
            (button_label_h g) (Ui.inactive_color g.ui) label key true
        let shift_button i j key =
          Ui.invisible_button g.ui (idxowner "edit_but" j) (area i j)
            [`Shift] key

        (*let tag = button 0 0 "TAG" KeyBind.tag*)
        (*let tag_add = shift_button 0 0 KeyBind.tag2*)
        let sep = button 0 0 "SEP" KeyBind.sep
        let del = button 1 1 "DEL" KeyBind.del
        let crop = button 1 2 "CROP" KeyBind.crop
        let wipe = button 1 3 "WIPE" KeyBind.wipe
        let dedupe = shift_button 2 4 KeyBind.dedupe
        let undo = button 2 4 "UNDO" KeyBind.undo
        let redo = shift_button 3 5 KeyBind.redo
        let save = button 3 5 "SAVE" KeyBind.save
        let view = shift_button 4 6 KeyBind.save2
        let load = button 3 6 "LOAD" KeyBind.load
      end

      module Key =
      struct
        let del = focus_key KeyBind.del
        let del_alt = focus_key KeyBind.del2
        let cut = focus_key KeyBind.cut
        let copy = focus_key KeyBind.copy
        let paste = focus_key KeyBind.paste
        let rev = focus_key KeyBind.rev
      end

      (* Total text field *)
      module Total =
      struct
        let w = - margin - scrollbar_w g
        let x = Button.x 4 7
        let y = footer

        let box = Ui.box g.ui (p, x, y, w, line) `Black
        let text =
          Ui.text g.ui (p, x, y + padh, w - (gutter + 1)/2, text) `Right
      end
    end
  end


  (* Settings Pane *)

  module Settings () =
  struct
    let p = Ui.pane g.ui "set" (settings_x g, settings_y g, settings_w g, -1)

  (*
    let done_w = Mode.w
    let done_h = Mode.h
    let done_x = - margin - done_w
    let done_y = margin
    let done_but =
      Ui.button g.ui (subowner "settings" "done")
        (p, done_x, done_y, -margin, done_h) nokey false (Some false)
    let done_label =
      Ui.label g.ui
        (p, done_x - smin g 40, done_y + (done_h - label_h g)/2, smin g 36, label_h g)
        `Right "DONE"
  *)
    let done_w = sx 30
    let done_h = label_h g * 3 / 2
    let done_ () =
      Ui.labeled_button g.ui (subowner "settings" "done")
        (p, -margin - done_w, margin, -margin, done_h) (label_h g)
        `White "DONE" KeyBind.na false (Some false)

    let settings args =
      Ui.settings g.ui "settings"
        (p, margin, margin + done_h + sy 4, -margin, -margin) settings_style
        args
  end


  (* Library *)

  module Library =
  struct
    (* Browser Pane *)

    let p = Ui.pane g.ui "brow" (library_x g, library_y g, g.browser_width, -1)

    let lh = label_h g
    let iw = indicator_w g

    (* Scan button *)
    module Scan =
    struct
      let w = sx 32
      let y = margin
      let iw' = iw * 12 / 7

      let area = (p, margin + (w - iw')/2, y + label_h g + 2, iw', iw')
      let indicator = Ui.indicator g.ui `Yellow area
      let button () = Ui.mouse g.ui "scan_but" area `Left
      let label () = Ui.label g.ui (p, margin, y, w, lh) `Center "SCANNING"
    end

    (* View button *)
    module Toggle =
    struct
      let w = sx 25
      let h = sy 12
      let y = margin + iw + 1

      module Mk (X : sig val i : int val label : string val key : KeyBind.t end) =
      struct
        open X

        let x = - margin - w - i * (w + sx 12) - sx 2
        let ix = function
          | `Center -> x + (w - iw)/2 + 1
          | `Left -> x + (w - 2 * iw - sep) / 2
          | `Right -> x + w - (w - 2 * iw - sep) / 2 - iw

        let mk_indicator al =
          Ui.indicator g.ui `Green (p, ix al, y - iw - sx 1, iw, iw)

        let indicator = mk_indicator `Center
        let indicator1 = mk_indicator `Left
        let indicator2 = mk_indicator `Right

        let button =
          Ui.button g.ui (idxowner "view_but" i) (p, x, y, w, h) key false
        let label () =
          Ui.label g.ui (p, x - sx 4, y + h + sx 1, w + sx 8, lh) `Center label
      end

      module Artists =
        Mk (struct let i = 2 let label = "ARTISTS" let key = KeyBind.artists end)
      module Albums =
        Mk (struct let i = 1 let label = "ALBUMS" let key = KeyBind.albums end)
      module Tracks =
        Mk (struct let i = 0 let label = "TRACKS" let key = KeyBind.tracks end)
    end

    (* Search *)
    module Search =
    struct
      let lw = smin 30
      let x = margin + lw + sx 3
      let y = 2 * margin + iw + Toggle.h + lh

      let box () = Ui.box g.ui (p, x, y, - div, line) `Black
      let label () =
        Ui.label g.ui (p, margin, y + (line - lh + sy 1)/2, lw, lh) `Left "SEARCH"
      let button () = Ui.mouse g.ui "search_but" (p, margin, y, lw, line) `Left
      let edit =
        Ui.rich_edit_text g.ui "search_edit"
          (p, x + sx 2, y, - div - sx 2, line) padh true

      let key = key KeyBind.search
      let context () = Ui.mouse g.ui "search_ctx" (p, x, y, -div, line) `Right
    end

    (* Browser *)
    module Browser =
    struct
      let y = Search.y + line + margin

      let style = rich_table_style 0 false
      let area = (p, margin, y, -div, -bottom)

      let table args = Ui.browser g.ui "br" area style args
      let mouse args = Ui.rich_table_mouse g.ui area style args
      let drag args = Ui.rich_table_drag g.ui area style args
      let error_box () = Ui.box g.ui area (Ui.error_color g.ui)

      module Key =
      struct
        let fold = focus_key KeyBind.fold_dir
        let rename b =
          focus_key KeyBind.name_dir b || focus_key KeyBind.name_dir2 b
      end

      module Rename =
      struct
        let area args = Ui.browser_entry_text_area g.ui area style args
        let box area = Ui.box g.ui area `Black
        let edit area pad = Ui.rich_edit_text g.ui "name_edit" area pad true
      end

      (* Divider *)
      let divider =
        Ui.divider g.ui "br_div" (p, -div, margin, div, -bottom) `Horizontal
    end

    (* Buttons *)
    module Edit =
    struct
      let w = edit_w g
      let h = edit_h g
      let button i j label key =
        Ui.labeled_button g.ui (idxowner "ledit_but" j)
          (p, margin + i * sx 5 + j * w, -h, w, h) (button_label_h g)
          (Ui.inactive_color g.ui) label key true

      let insert = button 0 0 "ADD" KeyBind.add_dir
      (*let remove = button 0 1 "DEL" KeyBind.deldir*)
      let create = button 0 1 "NEW" KeyBind.new_dir
      let view = button 0 2 "VIEW" KeyBind.view_dir
      let tag = button 1 3 "TAG" KeyBind.tag
      let rescan = button 1 4 "SCAN" KeyBind.scan_dir

      let del = focus_key KeyBind.del
      let del_alt = focus_key KeyBind.del2
      let copy = focus_key KeyBind.copy
    end


    (* View Panes *)

    module View =
    struct
      let libw = extension_w g  (* never is 0 *)

      let style = rich_table_style 1 true
      let grid_style = grid_table_style true

      let make_mouse area =
        let mouse = Ui.rich_table_mouse g.ui area style in
        let grid_mouse = Ui.grid_table_mouse g.ui area grid_style in
        mouse, grid_mouse

      let make_drag area =
        let drag = Ui.rich_table_drag g.ui area style `Above in
        let grid_drag = Ui.grid_table_drag g.ui area grid_style `Left in
        drag, grid_drag

      let make_view owner area (spin_off_x, spin_off_y) =
        let table = Ui.rich_table g.ui (subowner owner "tbl") area style in
        let grid = Ui.grid_table g.ui (subowner owner "grid") area grid_style in
        let mouse, grid_mouse = make_mouse area in
        let p, _, _, _, _ = area in
        let spin_x = spin_off_x + sx 4 + padw in
        let spin_y = margin + line + spin_off_y + sy 4 + padh in
        let spin_w = - scrollbar_w g - gutter in
        let spin =
          Ui.text g.ui (p, spin_x, spin_y, spin_w, text) `Left `Regular true in
        area, table, grid, mouse, grid_mouse, spin

      (* Upper left view *)
      module Left =
      struct
        let x = library_x g + g.browser_width
        let y = library_y g
        let w = if g.right_shown then g.left_width else libw - g.browser_width
        let h = if g.lower_shown then g.upper_height else -bottom
        let p = Ui.pane g.ui "lft" (x, y, w, h)
        let area = (p, 0, margin, -1, -1)

        let view () = make_view "lft" area (0, 0)
        let mouse () = make_mouse area
        let drag () = make_drag area
      end

      (* Upper right view (optional) *)
      module Right =
      struct
        let x = Left.x + g.left_width
        let y = Left.y
        let w = libw - g.browser_width - g.left_width
        let h = Left.h
        let p = Ui.pane g.ui "rgt" (x, y, w, h)
        let area = (p, div, margin, -1, -1)

        let view () = make_view "rgt" area (div, 0)
        let mouse () = make_mouse area
        let drag () = make_drag area
      end

      (* Lower view (optional) *)
      module Lower =
      struct
        let x = Left.x
        let y = Left.y + g.upper_height
        let w = libw - g.browser_width
        let h = -bottom
        let p = Ui.pane g.ui "low" (x, y, w, h)
        let area = (p, 0, div, -1, -1)

        let view () = make_view "low" area (0, div)
        let mouse () = make_mouse area
        let drag () = make_drag area
      end

      (* Dividers *)
      module Divider =
      struct
        let right = Ui.divider g.ui "rgt_div" (Right.p, 0, 0, div, -1) `Horizontal
        let lower = Ui.divider g.ui "low_div" (Lower.p, 0, 0, -1, div) `Vertical
      end
    end


    (* Log Pane *)

    module Log =
    struct
      let p' = p
      let x = library_x g + g.browser_width
      let w = extension_w g - g.browser_width
      let p = Ui.pane g.ui "log" (x, library_y g, w, -bottom)

      let area = (p, 0, margin, -1, -1)
      let table args =
        Ui.rich_table g.ui "log_tbl" area (rich_table_style 1 true) args

      module Button =
      struct
        let bw = (g.browser_width - margin - div) / 2
        let bh = edit_h g
        let bx i = margin + i * bw
        let by = -bh
        let button i color label key =
          Ui.labeled_button g.ui (idxowner "log_but" i) (p', bx i, by, bw, bh)
            (button_label_h g) (color g.ui) label key true

        let ok = button 0 Ui.active_color "OK" KeyBind.ok
        let cancel = button 1 Ui.inactive_color "CANCEL" KeyBind.cancel
      end
    end


    (* Message Pane *)

    module Message =
    struct
      let x = library_x g + g.browser_width
      let y = -bottom
      let w = extension_w g - g.browser_width
      let h = bottom
      let p = Ui.pane g.ui "msg" (x, y, w, h)

      let mw = -2 * edit_w g (* = Copy.x 0 0 *) - sx 4
      let box () = Ui.box g.ui (p, 0, footer, mw, line) `Black
      let text =
        Ui.color_text g.ui (p, sx 2, footer + padh, mw - sx 4, text) `Left
    end


    (* Copy Buttons *)

    module Copy =
    struct
      let p = Message.p

      module Button =
      struct
        let w = edit_w g
        let h = edit_h g
        let x i j = - i * sx 5 - (2 - j) * w
        let area i j = (p, x i j, -h, w, h)

        let button i j label key =
          Ui.labeled_button g.ui (idxowner "copy_but" j) (area i j)
            (button_label_h g) (Ui.inactive_color g.ui) label key true
        let shift_button i j key =
          Ui.invisible_button g.ui (idxowner "copy_but" j) (area i j)
            [`Shift] key

        let append_left = button 0 0 " < " KeyBind.append_left
        let append_right = button 0 1 " > " KeyBind.append_right
        let replace_left = shift_button 0 0 KeyBind.replace_left
        let replace_right = shift_button 0 1 KeyBind.replace_right

        let append_lib, replace_lib, append_pl, replace_pl =
          if g.extension_side = `Left then
            append_left, replace_left, append_right, replace_right
          else
            append_right, replace_right, append_left, replace_left
      end

      module KeyBind =
      struct
        let append_lib, replace_lib, append_pl, replace_pl =
          if g.extension_side = `Left then
            KeyBind.(append_left, replace_left, append_right, replace_right)
          else
            KeyBind.(append_right, replace_right, append_left, replace_left)
      end
    end
  end


  (* File Selector *)

  module Filesel () =
  struct
    (* Browser *)
    module Dirs =
    struct
      let p = Ui.pane g.ui "dirs"
        (library_x g, library_y g, g.directories_width, -1)

      let style = rich_table_style 1 false
      let area = (p, margin, margin, -div, -bottom)
      let table args = Ui.browser g.ui "dir_tbl" area style args
      let mouse args = Ui.rich_table_mouse g.ui area style args
    end

    (* Files *)
    module Files =
    struct
      let p = Ui.pane g.ui "files"
        (library_x g + g.directories_width, library_y g, library_w g - g.directories_width, library_h g)

      let style = rich_table_style 1 true
      let area = (p, 0, margin, -1, -bottom)
      let table args = Ui.rich_table g.ui "file_tbl" area style args
      let mouse args = Ui.rich_table_mouse g.ui area style args
    end

    (* Input field *)
    module Input =
    struct
      let lw = smin 20
      let lh = label_h g
      let p = Files.p

      let label () =
        Ui.label g.ui (p, 0, footer + (line - lh + sy 1)/2, lw, lh) `Left "FILE"
      let button () =
        Ui.mouse g.ui "file_but" (p, margin, footer, lw, line) `Left
      let box () =
        Ui.box g.ui (p, lw, footer, -div, line) `Black
      let edit =
        Ui.rich_edit_text g.ui "file_edit"
          (p, lw + 2, footer, - div - sx 2, line) padh true
    end

    (* Buttons *)
    module Button =
    struct
      let bw = (g.directories_width - margin - div) / 2
      let bh = edit_h g
      let button i color label key =
        Ui.labeled_button g.ui (idxowner "sel_but" i)
          (Dirs.p, margin + i * bw, -bh, bw, bh)
          (button_label_h g) (color g.ui) label key true

      let ok = button 0 Ui.active_color "OK" KeyBind.ok
      let overwrite = button 0 Ui.error_color "OVERWRITE" KeyBind.overwrite
      let cancel = button 1 Ui.inactive_color "CANCEL" KeyBind.cancel
    end

    (* Keys *)
    module Key =
    struct
      let return = key KeyBind.ok
    end

    (* Divider *)
    let divider =
      Ui.divider g.ui "dir_div" (Dirs.p, -div, margin, div, -bottom)
        `Horizontal
  end


  (* Context Menu *)

  module Menu () =
  struct
    let menu x y = Ui.menu g.ui x y menu_style
  end


  (* Zoom Pop-up *)

  module Zoom (Z : sig val size : int -> int * int val var : bool end) =
  struct
    let w0, h0 = Z.size g.zoom_size
    let ratio = float w0 /. float h0
    let w1, h1 =
      let wmax = win_w g - 2 * zoom_margin g in
      if w0 <= wmax then w0, h0 else wmax, int_of_float (float wmax /. ratio)
    let w2, h2 =
      let hmax = win_h g - line - 2 * zoom_margin g in
      if h1 <= hmax then w1, h1 else int_of_float (float hmax *. ratio), hmax
    let w, h = w2, h2 + line
    let x, y = Option.get g.popup_shown
    let p, r'' =
      Ui.popup g.ui "zoom" (x, y, w, h) (zoom_margin g)
        (Z.var, Z.var, Z.var) false

    let resize =
      Option.map (fun ((x', y', w', h'), edge) ->
        (x', y', w', h' - line), edge
      ) r''

    let image_area = (p, 0, 0, -1, -line)
    let text = Ui.ticker g.ui (p, 0, -text, -1, -1)
    let refl () = Ui.mouse_focus g.ui (p, 0, 0, -1, -1) (control_h g) 0x30 0
  end


  (* Custom Attribute pop-up *)

  module Custom () =
  struct
    let w = smin 200
    let h = 2 * line + 2  (* cf Ui.rich_table *)
    let x, y = Option.get (g.popup_shown)
    let p, _ =
      Ui.popup g.ui "custom" (x, y, w, h) (zoom_margin g)
        (false, false, false) true

    module Name =
    struct
      let box () = Ui.box g.ui (p, 0, 0, -1, line) (Ui.text_color g.ui)
      let edit =
        Ui.rich_edit_text g.ui (subowner "custom" "name_edit")
          (p, gutter / 2, padh, - gutter / 2, text) padh true `Black
    end

    module Text =
    struct
      let box () = Ui.box g.ui (p, 0, line, -1, line) `Black
      let edit =
        Ui.rich_edit_text g.ui (subowner "custom" "text_edit")
          (p, gutter / 2, padh + line + 2, - gutter / 2, text) padh false
    end

    module Key =
    struct
      let ok = key KeyBind.ok
      let cancel = key KeyBind.cancel
    end
  end
end

module type Window =
  module type of Window
    (struct let it = Geometry.make (Ui.make (Api.Window.init 0 0 0 0 "")) end)
