(* Main Entry Point *)


(* Layout *)

let margin = 10
let row_h = 13
let gutter_w = 7
let bottom_h = 24
let footer_h = row_h
let footer_y = -footer_h-(bottom_h-footer_h)/2
let scrollbar_w = 10
let divider_w = margin
let resizer_w = 16
let indicator_w = 7

let control_pane = Ui.pane 0

let control_w = 360
let control_h = 160
let label_h = 8

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

let fps_text = Ui.text (0, 130, margin+info_margin, 40, 12) `Left
let fps_key = Ui.key ([`Command], `Char 'U')

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
let mute_text = Ui.text (0, mute_x, mute_y, mute_w, 8) `Center
let mute_button = Ui.mouse mute_area `Left
let mute_drag = Ui.drag mute_area
let mute_key = Ui.key ([], `Char '0')
let volup_key = Ui.key ([], `Char '+')
let voldown_key = Ui.key ([], `Char '-')

let seek_h = 14
let seek_y = margin+info_h-info_margin/2-seek_h
let title_h = 16
let title_y = seek_y-title_h-4
let prop_text = Ui.text (0, margin+info_margin, lcd_y+lcd_h+10, mute_x, 12) `Left
let title_ticker = Ui.ticker (0, margin+info_margin, title_y, info_w-info_margin, title_h)
let seek_bar = Ui.progress_bar (0, margin+info_margin/2, seek_y, info_w-info_margin, seek_h)
let rw_key = Ui.key ([], `Arrow `Left)
let ff_key = Ui.key ([], `Arrow `Right)

let color_y = lcd_y+lcd_h
let color_button = Ui.mouse (0, margin, color_y, mute_x, title_y-color_y)
let color_button_fwd = color_button `Left
let color_button_bwd = color_button `Right

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


let playlist_pane = Ui.pane 1
let playlist_min = 31 + 4 * row_h

let playlist_area = (1, margin, margin, -margin-scrollbar_w, -bottom_h)
let playlist_table = Ui.table playlist_area gutter_w
let playlist_scroll = Ui.scroll_bar (1, -margin-scrollbar_w, margin, scrollbar_w, -bottom_h) `Vertical
let playlist_wheel = Ui.wheel (1, margin, margin, -margin, -bottom_h)
let playlist_drag = Ui.drag playlist_area

let total_w = -margin-scrollbar_w-gutter_w
let total_x = total_w-100
let playlist_total_box = Ui.box (1, total_x, footer_y, total_w, footer_h) `Black
let playlist_total_text = Ui.text (1, total_x, footer_y, total_w-2, footer_h) `Right

let playlist_resizer = Ui.resizer (1, -resizer_w, -resizer_w, resizer_w, resizer_w) `N_S

let enlarge_key = Ui.key ([`Command], `Char '+')
let reduce_key = Ui.key ([`Command], `Char '-')

let up_key = Ui.key ([], `Arrow `Up)
let down_key = Ui.key ([], `Arrow `Down)
let pageup_key = Ui.key ([], `Page `Up)
let pagedown_key = Ui.key ([], `Page `Down)
let begin_key = Ui.key ([], `End `Up)
let end_key = Ui.key ([], `End `Down)

let selup_key = Ui.key ([`Shift], `Arrow `Up)
let seldown_key = Ui.key ([`Shift], `Arrow `Down)
let selpageup_key = Ui.key ([`Shift], `Page `Up)
let selpagedown_key = Ui.key ([`Shift], `Page `Down)
let selbegin_key = Ui.key ([`Shift], `End `Up)
let selend_key = Ui.key ([`Shift], `End `Down)

let selnone_key = Ui.key ([`Command], `Char 'N')
let selall_key = Ui.key ([`Command], `Char 'A')
let selinv_key = Ui.key ([`Command], `Char 'I')

let moveup_key = Ui.key ([`Command], `Arrow `Up)
let movedown_key = Ui.key ([`Command], `Arrow `Down)
let movepageup_key = Ui.key ([`Command], `Page `Up)
let movepagedown_key = Ui.key ([`Command], `Page `Down)
let movebegin_key = Ui.key ([`Command], `End `Up)
let moveend_key = Ui.key ([`Command], `End `Down)

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


let library_pane = Ui.pane 2
let library_min = 400

let browser_min = 40
let view_min = 40
let browser_max pw = pw-view_min-2*margin-divider_w
let browser_area bw = (2, margin, margin, bw-margin-1, -bottom_h)
let browser_error_box bw = Ui.box (browser_area bw)
let browser_table bw = Ui.table (browser_area bw) 0
let browser_scroll bw = Ui.scroll_bar (2, margin+bw-scrollbar_w, margin, scrollbar_w, -bottom_h) `Vertical
let browser_wheel bw = Ui.wheel (2, margin, margin, bw, -bottom_h)
let browser_drag bw = Ui.drag (browser_area bw)

let header_area bw rh = (2, margin+divider_w+bw, margin, -margin-scrollbar_w-1, rh)
let header_table bw rh = Ui.table (header_area bw rh) gutter_w rh
let header_drag bw rh = Ui.drag (header_area bw rh)
let header_margin = 2

let view_area bw rh = (2, margin+divider_w+bw, margin+rh+header_margin, -margin-scrollbar_w-1, -bottom_h-scrollbar_w-1)
let view_table bw rh = Ui.table (view_area bw rh) gutter_w rh
let view_scroll_v _bw = Ui.scroll_bar (2, -margin-scrollbar_w, margin, scrollbar_w, -bottom_h) `Vertical
let view_scroll_h bw = Ui.scroll_bar (2, margin+divider_w+bw, -bottom_h-scrollbar_w, -margin-scrollbar_w-1, scrollbar_w) `Horizontal
let view_wheel bw = Ui.wheel (2, margin+divider_w+bw, margin, -margin, -bottom_h)
let view_drag bw rh = Ui.drag (view_area bw rh)

let error_w = -margin-scrollbar_w-gutter_w
let error_x = margin+resizer_w
let library_error_box = Ui.box (2, error_x, footer_y, error_w, footer_h) `Black
let library_error_text = Ui.color_text (2, error_x, footer_y, error_w-2, footer_h) `Left

let library_divider_min = margin+browser_min
let library_divider_max pw = margin+browser_max pw
let library_divider bw = Ui.divider (2, margin+bw, margin, divider_w, -bottom_h) `Horizontal

let library_resizer_l = Ui.resizer (2, 1, -resizer_w, resizer_w, resizer_w)
let library_resizer_r = Ui.resizer (2, -resizer_w, -resizer_w, resizer_w, resizer_w)


(* Helpers *)

let rec log10 n = if n < 10 then 0 else 1 + log10 (n / 10)

let clamp min max v =
  if v < min then min else
  if v > max then max else
  v

let float_of_bool b = float (Bool.to_int b)


let start_time = Unix.gettimeofday ()

let time () = Unix.gettimeofday () -. start_time


let fmt = Printf.sprintf

let fmt_time t =
  let t' = int_of_float (Float.trunc t) in
  fmt "%d:%02d" (t' / 60) (t' mod  60)

let _fmt_time2 t =
  let t' = int_of_float (Float.trunc t) in
  fmt "%02d:%02d" (t' / 60) (t' mod  60)

let fmt_time3 t =
  let t' = int_of_float (Float.trunc t) in
  if t' < 3600 then
    fmt_time t
  else
    fmt "%d:%02d:%02d" (t' / 3600) (t' / 60 mod 60) (t' mod  60)


let exec prog args =
  let cmd = Filename.quote_command prog args in
  let cmd' = if not Sys.win32 then cmd else
    "\"start /b ^\"^\" " ^ String.sub cmd 1 (String.length cmd - 1) in
  ignore (Sys.command cmd')


(* Control Section *)

let run_control (st : State.t) =
  let win = Ui.window st.ui in

  let x =
    if st.library.shown && st.library.side = `Left then st.library.width else 0
  in
  control_pane st.ui (x, 0, control_w, control_h);

  (* Exit button *)
  (* This has to come first, otherwise Raylib crashes? *)
  if not (power_button st.ui (Some true)) then exit 0;
  power_label st.ui;

  (* Current status *)
  State.ok st;
  let silence = st.control.sound = Api.Audio.silence st.control.audio in
  let length = Api.Audio.length st.control.audio st.control.sound in
  let elapsed = Api.Audio.played st.control.audio st.control.sound in
  let remaining = length -. elapsed in
  let playing = Api.Audio.is_playing st.control.audio st.control.sound in
  let paused = not playing && elapsed > 0.0 in
  let stopped = not playing && not paused in

  (* LCD *)
  info_box st.ui;
  let sign, d1, d2, d3, d4 =
    if paused && int_of_float (time ()) mod 2 = 0 then
      '+', ' ', ' ', ' ', ' ' else
    let sign, time =
      match st.control.timemode with
      | `Elapse -> '+', elapsed
      | `Remain -> '-', remaining
    in
    lcd_colon st.ui ':';
    let seconds = int_of_float (Float.round (if silence then 0.0 else time)) in
    sign,
    (Char.chr (Char.code '0' + seconds mod 6000 / 600)),
    (Char.chr (Char.code '0' + seconds mod 600 / 60)),
    (Char.chr (Char.code '0' + seconds mod 60 / 10)),
    (Char.chr (Char.code '0' + seconds mod 10))
  in
  lcd_minus st.ui sign;
  lcd1 st.ui d1;
  lcd2 st.ui d2;
  lcd3 st.ui d3;
  lcd4 st.ui d4;

  if lcd_button st.ui then
  (
    (* Click on time LCD: toggle time mode *)
    st.control.timemode <-
      match st.control.timemode with
      | `Elapse -> `Remain
      | `Remain -> `Elapse
  );

  let ncol = Ui.num_palette st.ui in
  let dcol =
    (if color_button_fwd st.ui then +1 else 0) +
    (if color_button_bwd st.ui then -1 else 0)
  in
  Ui.set_palette st.ui ((Ui.get_palette st.ui + dcol + ncol) mod ncol);

  (* FPS *)
  if st.control.fps then
    fps_text st.ui `Regular true (fmt "%d FPS" (Api.Window.fps win));
  if fps_key st.ui then st.control.fps <- not st.control.fps;

  (* Audio properties *)
  if not silence then
  (
    let track = Option.get st.control.current in
    let ext = Filename.extension track.path in
    let format = if ext = "" || ext.[0] <> '.' then "???" else
      String.uppercase_ascii (String.sub ext 1 (String.length ext - 1)) in
    let bitrate = Api.Audio.bitrate st.control.audio st.control.sound in
    let rate = Api.Audio.rate st.control.audio st.control.sound in
    let channels = Api.Audio.channels st.control.audio st.control.sound in
    let depth = bitrate /. float rate /. float channels in
    prop_text st.ui `Regular true
      (fmt "%s   %.0f KBPS   %.1f KHZ   %s BIT   %s"
        format (bitrate /. 1000.0) (float rate /. 1000.0)
        (fmt (if depth = Float.round depth then "%.0f" else "%.1f") depth)
        (match channels with
        | 1 -> "MONO"
        | 2 -> "STEREO"
        | n -> fmt "%d CHAN" n
        )
      );
  );

  (* Title info *)
  let name =
    match st.control.current with
    | Some track when not (Track.is_separator track) ->
      track.name ^ " - " ^ fmt_time track.time
    | _ -> App.(name ^ " " ^ version)
  in
  title_ticker st.ui name;

  (* Volume control *)
  let volume' = volume_bar st.ui st.control.volume +. 0.05 *. volume_wheel st.ui +.
    0.05 *. (float_of_bool (volup_key st.ui) -. float_of_bool (voldown_key st.ui)) in
  mute_text st.ui `Inverted st.control.mute "MUTE";
  let mute' =
    if mute_button st.ui || mute_key st.ui then not st.control.mute else st.control.mute in
  if volume' <> st.control.volume || mute' <> st.control.mute then
  (
    (* Click or drag on volume bar or mute button: adjust volume *)
    (* Hack to overlap volume bar with mute button. *)
    if mute_drag st.ui (0, 0) = `None then st.control.volume <- clamp 0.0 1.0 volume';
    st.control.mute <- mute';
    Api.Audio.volume st.control.audio st.control.sound
      (if st.control.mute then 0.0 else st.control.volume);
  );

  (* Seek bar *)
  let progress =
    if length > 0.0 && not silence then elapsed /. length else 0.0 in
  let progress' = seek_bar st.ui progress +.
    0.05 *. (float_of_bool (ff_key st.ui) -. float_of_bool (rw_key st.ui)) in
  if progress' <> progress && not silence then
  (
    (* Click or drag on seek bar: reposition audio *)
    Control.seek st.control (clamp 0.0 1.0 progress');
  );
(*
  let s1 = fmt_time2 elapsed in
  let s2 = "-" ^ fmt_time2 remaining in
  let w2 = Api.Draw.text_width st.ui 11 (Ui.font st.ui 11) s2 in
  Api.Draw.text st.ui 14 91 11 `White (Ui.font st.ui 11) s1;
  Api.Draw.text st.ui (278 - w2) 91 11 `White (Ui.font st.ui 11) s2;
*)

  (* Looping *)
  (match st.control.loop with
  | `AB (t1, t2) when playing && t2 < elapsed ->
    (* End of loop reached: jump back to start *)
    Control.seek st.control (t1 /. length);
  | _ -> ()
  );

  (* Play controls *)
  let len = Playlist.length st.playlist in
  let bwd = if bwd_button st.ui (Some false) then -1 else 0 in
  let fwd = if fwd_button st.ui (Some false) then +1 else 0 in
  let off = if len = 0 then 0 else bwd + fwd in
  if off <> 0 then
  (
    (* Click on navigation button: jump to track *)
    let more = Playlist.skip st.playlist off (st.control.repeat <> `None) in
    Control.switch st.control (Playlist.current st.playlist) more;
    Playlist.adjust_scroll st.playlist st.playlist.table.pos;
  );

  let playing' = play_button st.ui (Some playing) in
  if stopped && playing' && len > 0 then
  (
    (* Click on play button: start track *)
    Control.switch st.control (Playlist.current st.playlist) true;
    Playlist.adjust_scroll st.playlist st.playlist.table.pos;
  );

  let paused' = pause_button st.ui (Some paused) in
  if playing' && paused' then
  (
    (* Click on pause button when playing: pause track *)
    Api.Audio.pause st.control.audio st.control.sound
  )
  else if (not stopped && not paused' || stopped && paused') && not silence then
  (
    (* Click on pause button when paused: resume track *)
    Api.Audio.resume st.control.audio st.control.sound;
  );

  if stop_button st.ui (Some false) && not stopped then
  (
    (* Click on stop button when playing: stop track *)
    Api.Audio.pause st.control.audio st.control.sound;
    (match Playlist.current_opt st.playlist with
    | None -> Control.eject st.control
    | Some track -> Control.switch st.control track false
    );
    Playlist.adjust_scroll st.playlist st.playlist.table.pos;
  );

  if eject_button st.ui (Some false) then
  (
    (* Click on eject button: stop and clear playlist *)
    Control.eject st.control;
    Playlist.remove_all st.playlist;
  );

  if start_stop_key st.ui then
  (
    (* Press of space key: pause or resume *)
    if playing then
      Api.Audio.pause st.control.audio st.control.sound
    else if paused then
      Api.Audio.resume st.control.audio st.control.sound
    else if stopped && len > 0 then
      Control.switch st.control (Playlist.current st.playlist) true;
    Playlist.adjust_scroll st.playlist st.playlist.table.pos;
  );

  (* End of track *)
  (* Check must occur after possible Audio.resume above,
   * otherwise the last track would be restarted. *)
  if playing && (remaining < 0.2 || silence) then
  (
    (* Close to end: switch to next track *)
    let more =
      match st.control.repeat with
      | `One -> true
      | `All -> Playlist.skip st.playlist (+1) true
      | `None -> Playlist.skip st.playlist (+1) false
    in
    let next_track =
      if st.playlist.table.pos = None
      then Option.get st.control.current
      else Playlist.current st.playlist
    in
    Control.switch st.control next_track more;
    Playlist.adjust_scroll st.playlist st.playlist.table.pos;
  );

  (* Play modes *)
  let shuffle = st.playlist.shuffle <> None in
  shuffle_label st.ui;
  shuffle_indicator st.ui shuffle;
  let shuffle' = shuffle_button st.ui (Some shuffle) in
  if shuffle' <> shuffle then
  (
    (* Click on shuffle button: toggle shuffle *)
    if shuffle' then
    (
      Playlist.shuffle st.playlist
        (if stopped then None else st.playlist.table.pos);
      if stopped && st.playlist.table.pos <> None then
        Control.switch st.control (Playlist.current st.playlist) false;
      Playlist.adjust_scroll st.playlist st.playlist.table.pos;
    )
    else
      Playlist.unshuffle st.playlist
  );

  repeat_label st.ui;
  repeat_indicator1 st.ui (st.control.repeat <> `None);
  repeat_indicator2 st.ui (st.control.repeat = `All);
  let repeat' = repeat_button st.ui (Some (st.control.repeat <> `None)) in
  st.control.repeat <-
    (* Click on repeat button: cycle repeat mode *)
    (match st.control.repeat, repeat' with
    | `None, false | `All, false -> `None
    | `None, true | `One, true -> `One
    | `One, false | `All, true -> `All
    );

  loop_label st.ui;
  loop_indicator1 st.ui (st.control.loop <> `None);
  loop_indicator2 st.ui (match st.control.loop with `AB _ -> true | _ -> false);
  let loop' = loop_button st.ui (Some (st.control.loop <> `None)) in
  st.control.loop <-
    (* Click on loop button: cycle loop mode *)
    (match st.control.loop, loop' with
    | `None, false | `AB _, false -> `None
    | `None, true -> `A elapsed
    | `A t1, true -> `A t1
    | `A t1, false when t1 > elapsed -> `A elapsed
    | `A t1, false -> `AB (t1, elapsed)
    | `AB (t1, t2), true -> `AB (t1, t2)
    );

  (* Pane Activation *)
  playlist_label st.ui;
  playlist_indicator st.ui st.playlist.shown;
  let playlist_shown' = playlist_button st.ui (Some st.playlist.shown) in
  (* Click on playlist activation button: toggle playlist *)
  st.playlist.shown <- playlist_shown';

  library_label st.ui;
  library_indicator st.ui st.library.shown;
  let library_shown' = library_button st.ui (Some st.library.shown) in
  let wx, _ = Api.Window.pos win in
  let sx, _ = Api.Window.min_pos win in
  let sw, _ = Api.Window.max_size win in
  (* Click on library activation button: toggle library *)
  if not st.library.shown && library_shown' && not (Api.Key.is_modifier_down `Shift) then
  (
    (* Library was off: show; switch side if window is at the respective border *)
    if st.library.side = `Left && wx <= sx then st.library.side <- `Right;
    if st.library.side = `Right && wx + control_w >= sx + sw then st.library.side <- `Left;
    st.library.shown <- library_shown';
  )
  else if
    st.library.shown <> library_shown' && Api.Key.is_modifier_down `Shift ||
    library_mouse st.ui || library_key st.ui
  then
  (
    (* Shift-click: switch sides for library pane *)
    st.library.side <- if st.library.side = `Left then `Right else `Left
  )
  else
  (
    (* Library was on: turn off *)
    st.library.shown <- library_shown';
  );

  (* Minimize button *)
  if minimize_button st.ui then
  (
    (* Right-click on power button: minimize window *)
    Api.Window.minimize win
  )


(* Playlist Pane *)

let update_fit (st : State.t) =
  let row_h = st.config.row_height in
  let _, _, _, pl_h = Ui.dim st.ui playlist_area in
  st.playlist.table.fit <-
    max 4 (int_of_float (Float.floor (float pl_h /. float row_h)));
  let _, _, _, br_h = Ui.dim st.ui (browser_area st.library.browser_width) in
  st.library.browser.fit <-
    max 4 (int_of_float (Float.floor (float br_h /. float row_h)));
  let _, _, _, vw_h = Ui.dim st.ui (view_area st.library.browser_width row_h) in
  st.library.view.fit <-
    max 4 (int_of_float (Float.floor (float vw_h /. float row_h)))

let run_playlist (st : State.t) =
  let win = Ui.window st.ui in
  let row_h = st.config.row_height in
  let len = Playlist.length st.playlist in
  let now = Unix.time () in

  let x =
    if st.library.shown && st.library.side = `Left then st.library.width else 0
  in
  playlist_pane st.ui (x, control_h, control_w, st.playlist.height);

  (* Playlist table *)
  let tab = st.playlist.table in
  let digits = log10 (len + 1) + 1 in
  let font = Ui.font st.ui row_h in
  let smax1 = String.make digits '0' ^ "." in
  let cw1 = Api.Draw.text_width win row_h font smax1 + 1 in
  let cw3 = ref 16 in
  let (_, y, w, _) as r = Ui.dim st.ui playlist_area in
  (* Correct scrolling position for possible resize *)
  tab.scroll_v <- clamp 0 (max 0 (len - tab.fit)) tab.scroll_v;
  let rows =
    Array.init (min tab.fit len) (fun i ->
      let i = i + tab.scroll_v in
      let track = tab.entries.(i) in
      if now -. track.last_update > st.config.delay_track_update then
        Track.update st.control.audio track;
      let c =
        match track.status with
        | _ when Some i = tab.pos ->
          if track.path = (Option.get st.control.current).path then `White else `Gray 0xc0
        | _ when Track.is_separator track -> Ui.text_color st.ui
        | `Absent -> Ui.error_color st.ui
        | `Invalid -> Ui.warn_color st.ui
        | `Undet -> Ui.semilit_color (Ui.text_color st.ui)
        | `Predet | `Det -> Ui.text_color st.ui
      in
      let inv = if Table.is_selected tab i then `Inverted else `Regular in
      let time = if track.time = 0.0 then "" else fmt_time track.time in
      cw3 := max !cw3 (Api.Draw.text_width win row_h font time + 1);
      c, inv, [|fmt "%0*d." digits (i + 1); track.name; time|]
    )
  in
  let margin_w = (gutter_w + 1)/2 in  (* matches mw in UI.table *)
  let cw2 = w - cw1 - !cw3 - 2 * gutter_w - 2 * margin_w in
  let cols = [|cw1, `Right; cw2, `Left; !cw3, `Right|] in
  let dragging = playlist_drag st.ui (max_int, row_h) in
  (match playlist_table row_h st.ui cols rows 0 with
  | None -> ()
  | Some i ->
    let i = tab.scroll_v + i in
    if Api.Key.are_modifiers_down [] && Api.Mouse.is_drag `Left then
      Api.Mouse.set_cursor win `Point;
    if
      Api.Key.are_modifiers_down [] &&
      Api.Mouse.is_pressed `Left && dragging = `None
    then
    (
      (* Click on playlist *)
      if i >= len || not (Playlist.is_selected st.playlist i) then
      (
        (* Click on empty space: deselect all *)
        Library.deselect_all st.library;
        Playlist.deselect_all st.playlist;
      );
      if i < len then
      (
        (* Click on track *)
        if Api.Mouse.is_doubleclick `Left then
        (
          (* Double-click on track: switch to track *)
          tab.pos <- Some i;
          Control.switch st.control tab.entries.(i) true;
          if st.playlist.shuffle <> None then 
            Playlist.shuffle_next st.playlist i;
        )
        else
        (
          (* Single-click on track: make it singular selection *)
          Library.deselect_all st.library;
          Playlist.select st.playlist i i;
        )
      )
    )
    else if
      Api.Key.are_modifiers_down [] &&
      not (Api.Mouse.is_pressed `Left) && dragging = `Click
    then
    (
      (* Click-release on playlist: deselect all except for clicked track *)
      Playlist.deselect_all st.playlist;
      if i < len then
      (
        Library.deselect_all st.library;
        Playlist.select st.playlist i i;
      )
    )
    else if
      Api.Key.are_modifiers_down [`Command] && Api.Mouse.is_pressed `Left
    then
    (
      (* Cmd-click on playlist: toggle selection of clicked track *)
      if i < len then
      (
        if Playlist.is_selected st.playlist i then
          Playlist.deselect st.playlist i i
        else
        (
          Library.deselect_all st.library;
          Playlist.select st.playlist i i;
        )
      )
    )
    else if Api.Key.are_modifiers_down [`Shift] && Api.Mouse.is_down `Left then
    (
      (* Shift-click/drag on playlist: adjust selection range *)
      let default = if i < len then (i, i) else (0, 0) in
      let pos1, pos2 = Option.value tab.sel_range ~default in
      let i' = max 0 (min i (len - 1)) in
      if tab.sel_range = None || Playlist.is_selected st.playlist pos1 then
      (
        (* Track was already selected: deselect old range, select new range *)
        Library.deselect_all st.library;
        Playlist.deselect st.playlist pos2 i';
        Playlist.select st.playlist pos1 i'
      )
      else
      (
        (* Track was not selected: select old range, deselect new range *)
        Library.deselect_all st.library;
        Playlist.select st.playlist pos2 i';
        Playlist.deselect st.playlist pos1 i'
      )
    )
  );

  (* Playlist selection *)
  if not (Library.has_selection st.library) then
  (
    let d =
      if begin_key st.ui then -len else
      if end_key st.ui then +len else
      if pageup_key st.ui then -tab.fit else
      if pagedown_key st.ui then +tab.fit else
      if up_key st.ui then -1 else
      if down_key st.ui then +1 else
      0
    in
    if min len (abs d) > 0 then
    (
      (* Plain cursor movement: deselect all, reselect relative to range end *)
      let default = 0, if d < 0 then len else -1 in
      let _, pos2 = Option.value tab.sel_range ~default in
      let i = if d < 0 then max 0 (pos2 + d) else min (len - 1) (pos2 + d) in
      Playlist.deselect_all st.playlist;
      Playlist.select st.playlist i i;
      Playlist.adjust_scroll st.playlist (Some i);
    );

    let d =
      if selbegin_key st.ui then -len else
      if selend_key st.ui then +len else
      if selpageup_key st.ui then -tab.fit else
      if selpagedown_key st.ui then +tab.fit else
      if selup_key st.ui then -1 else
      if seldown_key st.ui then +1 else
      0
    in
    if min len (abs d) > 0 then
    (
      (* Shift-cursor movement: adjust selection range *)
      let default = 0, if d < 0 then len else -1 in
      let pos1, pos2 = Option.value tab.sel_range ~default in
      let i = if d < 0 then max 0 (pos2 + d) else min (len - 1) (pos2 + d) in
      if tab.sel_range = None then
      (
        (* No selection yet: range from end of playlist *)
        Playlist.select st.playlist (len - 1) i
      )
      else if Playlist.is_selected st.playlist pos1 then
      (
        (* Range start was already selected: deselect old range, select new *)
        Playlist.deselect st.playlist (max 0 pos2) i;
        Playlist.select st.playlist pos1 i
      )
      else
      (
        (* Range start was not selected: select old range, deselect new *)
        Playlist.select st.playlist (max 0 pos2) i;
        Playlist.deselect st.playlist pos1 i
      );
      Playlist.adjust_scroll st.playlist (Some i);
    );

    if selall_key st.ui then
    (
      (* Select-all key pressed: select all *)
      Library.deselect_all st.library;
      Playlist.select_all st.playlist;
    )
    else if selnone_key st.ui then
    (
      (* Deselect-all key pressed: deselect all *)
      Playlist.deselect_all st.playlist;
    )
    else if selinv_key st.ui then
    (
      (* Selection inversion key pressed: invert selection *)
      Library.deselect_all st.library;
      Playlist.select_invert st.playlist;
    )
  );

  (* Playlist dragging *)
  let d0 =
    match dragging with
    | `Drag (_, dy) when Api.Key.are_modifiers_down [] -> dy
    | _ -> 0
  in
  let d = d0 +
    if movebegin_key st.ui then -len else
    if moveend_key st.ui then +len else
    if movepageup_key st.ui then -tab.fit else
    if movepagedown_key st.ui then +tab.fit else
    if moveup_key st.ui then -1 else
    if movedown_key st.ui then +1 else
    0
  in
  if min len (abs d) > 0 then
  (
    let d' =
      if d < 0
      then max d (- Option.value (Playlist.first_selected st.playlist) ~default: (len - 1))
      else min d (len - Option.value (Playlist.last_selected st.playlist) ~default: 0 - 1)
    in
    Playlist.move_selected st.playlist d';
    if d0 = 0 then
      tab.scroll_v <- clamp 0 (max 0 (len - tab.fit)) (tab.scroll_v + d);
  );

  (* Playlist scrolling *)
  let h' = tab.fit * row_h in
  let ext = if len = 0 then 1.0 else min 1.0 (float h' /. float (len * row_h)) in
  let pos = if len = 0 then 0.0 else float tab.scroll_v /. float len in
  let pos' = playlist_scroll st.ui pos ext -. 0.05 *. playlist_wheel st.ui in
  tab.scroll_v <- clamp 0 (max 0 (len - tab.fit))
    (int_of_float (Float.round (pos' *. float len)));

  (* Playlist buttons *)
  if save_button st.ui None then
    ();  (* TODO *)

  let selected = Playlist.num_selected st.playlist > 0 in
  let is_executable path =
    try Unix.(access path [X_OK]); true with Unix.Unix_error _ -> false
  in
  let tag_available = selected && is_executable st.config.exec_tag in
  if tag_button st.ui (if tag_available then Some false else None) then
  (
    let tracks = Array.to_list (Playlist.selected st.playlist) in
    Domain.spawn (fun () ->
      let tracks' = List.filter (fun tr -> not (Track.is_separator tr)) tracks in
      let paths = List.map (fun (track : Track.t) -> track.path) tracks' in
      if st.config.exec_tag_max_len = 0 then
        exec st.config.exec_tag paths
      else
      (
        (* Work around Windows command line limits *)
        let args = ref paths in
        let rec pick len max =
          match !args with
          | [] -> []
          | arg1::args' ->
            let len' = len + String.length arg1 + 5 in
            if len <> 0 && len' > max then [] else
            (
              args := args';
              arg1 :: pick len' max
            )
        in
        (* Mp3tag immediately resorts the tracks by current column, unless added
         * with /add. However, /add only works with individual tracks and exec's,
         * which is very slow, so only use that when (a) we have less then a
         * certain number of tracks, or (b) when the command line gets too long
         * for a single call anyways. *)
        let max = if List.length tracks < 20 then 1 else st.config.exec_tag_max_len in
        exec st.config.exec_tag (pick 0 max);
        List.iter (fun arg -> exec st.config.exec_tag ["/add"; arg]) !args;
      )
    ) |> ignore;
  );

  if sep_button st.ui (Some false) then
  (
    let pos = Option.value (Playlist.first_selected st.playlist) ~default: 0 in
    Playlist.insert st.playlist pos [|Track.make_separator ()|];
    Control.switch_if_empty st.control (Playlist.current_opt st.playlist);
    if Playlist.num_selected st.playlist = 1 then
    (
      Library.deselect_all st.library;
      Playlist.deselect_all st.playlist;
      Playlist.select st.playlist pos pos;
    )
  );

  if del_button st.ui (if selected then Some false else None) then
    Playlist.remove_selected st.playlist;

  let unselected = Playlist.num_selected st.playlist < len in
  if crop_button st.ui (if unselected then Some false else None) then
    Playlist.remove_unselected st.playlist;

  if clean_button st.ui (if snd st.playlist.total > 0 then Some false else None) then
    Playlist.remove_invalid st.playlist;

  if undo_button st.ui (if !(tab.undos) <> [] then Some false else None) then
  (
    Playlist.pop_undo st.playlist;
    Control.switch_if_empty st.control (Playlist.current_opt st.playlist);
  );

  if redo_button st.ui (if !(tab.redos) <> [] then Some false else None) then
  (
    Playlist.pop_redo st.playlist;
    Control.switch_if_empty st.control (Playlist.current_opt st.playlist);
  );

  if cut_key st.ui then
  (
    let s = Playlist.string_of_playlist (Playlist.selected st.playlist) in
    Api.Clipboard.write win s;
    Playlist.remove_selected st.playlist;
  );

  if copy_key st.ui then
  (
    let s = Playlist.string_of_playlist (Playlist.selected st.playlist) in
    Api.Clipboard.write win s;
  );

  if paste_key st.ui then
  (
    match Api.Clipboard.read win with
    | None -> ()
    | Some s ->
      let tracks = Playlist.playlist_of_string s in
      let pos = Option.value (Playlist.first_selected st.playlist) ~default: 0 in
      Playlist.insert st.playlist pos tracks;
      Control.switch_if_empty st.control (Playlist.current_opt st.playlist);
      if Playlist.num_selected st.playlist = 1 && tracks <> [||] then
      (
        Library.deselect_all st.library;
        Playlist.deselect_all st.playlist;
        Playlist.select st.playlist pos (pos + Array.length tracks - 1);
      )
  );

  (* Playlist drag & drop *)
  let _, my as m = Api.Mouse.pos win in
  if Api.inside m r then
  (
    let dropped = Api.File.dropped win in
    let pos = min len ((my - y) / row_h + tab.scroll_v) in
    Playlist.insert_paths st.playlist pos dropped st.control.audio;
    Control.switch_if_empty st.control (Playlist.current_opt st.playlist);
  );

  (* Playlist total *)
  if int_of_float (time ()) mod 10 = 0 then Playlist.update_total st.playlist;
  let fmt_total (t, n) = fmt_time3 t ^ if n > 0 then "+" else "" in
  let s1 =
    if st.playlist.total_selected = (0.0, 0) then "" else
    fmt_total st.playlist.total_selected ^ "/"
  in
  let s2 = fmt_total st.playlist.total in
  playlist_total_box st.ui;
  playlist_total_text st.ui `Regular true (s1 ^ s2);

  (* Playlist resizing *)
  let w = control_w + (if st.library.shown then st.library.width else 0) in
  let _, dh =
    playlist_resizer st.ui (w, control_h + playlist_min) (w, -1) in
  st.playlist.height <- st.playlist.height + dh;
  update_fit st


(* Library Pane *)

let symbol_empty = "○"
let symbol_folded = "►" (*"▲"*) (* "▸" *)
let symbol_unfolded = "▼" (* "▾" *)

let run_library (st : State.t) =
  let win = Ui.window st.ui in
  let (mx, my) as m = Api.Mouse.pos win in
  let row_h = st.config.row_height in

  let x = if st.library.side = `Left then 0 else control_w - 5 in
  let dh = if st.playlist.shown then st.playlist.height else 0 in
  library_pane st.ui (x, 0, st.library.width + 5, control_h + dh);

  (* Pane resizing *)
  st.library.browser_width <- st.library.browser_width +
    library_divider st.library.browser_width st.ui
      library_divider_min (library_divider_max st.library.width);
  let bw = st.library.browser_width in

  (* Background rescanning *)
  if Library.rescan_roots_done st.library then
  (
    Library.update_browser st.library;
    Library.update_view st.library;
  );

  (* Browser *)
  let brow = st.library.browser in
  let len = Array.length brow.entries in
  (* Correct scrolling position for possible resize *)
  brow.scroll_v <- clamp 0 (max 0 (len - brow.fit)) brow.scroll_v;
  let (x, y, w, _) as r = Ui.dim st.ui (browser_area bw) in
  let cols = [|w, `Left|] in
  let c = Ui.text_color st.ui in
  let sel = Library.selected_dir st.library in
  let pres =
    Array.map (fun (dir : Data.dir) ->
      let sym =
        if dir.children = [||] then symbol_empty else
        if dir.folded then symbol_folded else symbol_unfolded
      in
      if dir.nest = -1 then "" else String.make (2 * dir.nest) ' ' ^ sym ^ " "
    ) brow.entries
  in
  let rows =
    Array.mapi (fun i (dir : Data.dir) ->
      let inv = if Some i = sel then `Inverted else `Regular in
      c, inv, [|pres.(i) ^ dir.name|]
    ) brow.entries
  in
  let dragging = browser_drag bw st.ui (max_int, row_h) in
  (match browser_table bw row_h st.ui cols rows 0 with
  | None -> ()
  | Some i ->
    let i = brow.scroll_v + i in
    if Api.Key.are_modifiers_down [] && Api.Mouse.is_drag `Left then
    (
      (* Mouse button down: adjust cursor *)
      let browser_area = browser_area bw in
      Api.Mouse.set_cursor win
        (if
          Api.inside m (Ui.dim st.ui browser_area) ||
          Api.inside m (Ui.dim st.ui playlist_area)
        then `Point else `Blocked)
    );
    if
      Api.Key.are_modifiers_down [] &&
      Api.Mouse.is_pressed `Left && dragging = `None
    then
    (
      (* Click into browser table *)
      if i >= len then
      (
        (* Click into empty space: deselect everything *)
        Library.deselect_dir st.library;
        Library.deselect_all st.library;
        Library.update_view st.library;
      )
      else
      (
        let tw = Api.Draw.text_width win row_h (Ui.font st.ui row_h) pres.(i) in
        if mx < x + tw then
        (
          (* CLick on triangle: fold/unfold entry *)
          let dir = brow.entries.(i) in
          dir.folded <- not dir.folded;
          Library.update_browser st.library;
        )
        else
        (
          (* Click on directory name: change view if necessary *)
          if Some i <> Library.selected_dir st.library then
          (
            Library.select_dir st.library i;
            Library.deselect_all st.library;
            Library.update_view st.library;
          );
          if Api.Mouse.is_doubleclick `Left then
          (
            (* Double-click on directory name: send track view to playlist *)
            Control.eject st.control;
            Playlist.remove_all st.playlist;
            let tracks = Array.map Track.make_from_data st.library.view.entries in
            Playlist.insert st.playlist 0 tracks;
            if tracks <> [||] then
              Control.switch st.control tracks.(0) true;
          )
        )
      )
    )
    else if Api.Key.are_modifiers_down [] && dragging = `Drop then
    (
      (* Drag & drop *)
      let (_, y, _, _) as r = Ui.dim st.ui playlist_area in
      if Api.inside m r then
      (
        (* Drag & drop onto playlist: send directory contents to playlist *)
        let tracks = Array.map Track.make_from_data st.library.view.entries in
        let len = Array.length st.playlist.table.entries in
        let pos = min len ((my - y) / row_h + st.playlist.table.scroll_v) in
        Playlist.insert st.playlist pos tracks;
        Library.deselect_all st.library;
        Playlist.deselect_all st.playlist;
        Playlist.select st.playlist pos (pos + Array.length tracks - 1);
        Control.switch_if_empty st.control (Playlist.current_opt st.playlist);
      )
    )
  );

  (* Browser scrolling *)
  let h' = brow.fit * row_h in
  let ext = if len = 0 then 1.0 else min 1.0 (float h' /. float (len * row_h)) in
  let pos = if len = 0 then 0.0 else float brow.scroll_v /. float len in
  let pos' = browser_scroll bw st.ui pos ext -. 0.05 *. browser_wheel bw st.ui in
  brow.scroll_v <- clamp 0 (max 0 (len - brow.fit))
    (int_of_float (Float.round (pos' *. float len)));

  (* Browser drag & drop *)
  let dropped = Api.File.dropped win in
  if dropped <> [] && Api.inside m r then
  (
    let pos = min len ((my - y) / row_h + brow.scroll_v) in
    let rec find_root_pos i j =
      if i = pos then j else
      find_root_pos (i + 1) (if brow.entries.(i).nest = 0 then j + 1 else j)
    in
    if Library.add_roots st.library dropped (find_root_pos 0 0) then
      Library.update_view st.library
    else
      browser_error_box bw (Ui.error_color st.ui) st.ui;  (* flash *)
  );

  (* Error display *)
  library_error_box st.ui;
  let now = Unix.gettimeofday () in
  if now -. st.library.error_time < 10.0 then
    library_error_text st.ui (Ui.error_color st.ui) `Regular true
      st.library.error;

  (* View header *)
  let view = st.library.view in
  let (x, y, w, h) = Ui.dim st.ui (header_area bw row_h) in
  let cols =
    Array.map (fun (attr, cw) -> cw, Library.attr_align attr) st.library.columns
  and headings =
    Array.map (fun (attr, _) -> Library.attr_name attr) st.library.columns
  in
  let rows = [|Ui.text_color st.ui, `Inverted, headings|] in
  ignore (header_table bw row_h st.ui cols rows view.scroll_h);

  (* Header column dividers (HACK) *)
  let margin_w = (gutter_w + 1)/2 in  (* match mw in Ui.table *)
  Api.Draw.clip win x y w h;
  let vw =
    Array.fold_left (fun cx (cw, _) ->
      Api.Draw.fill win (cx + cw + gutter_w/2 - view.scroll_h) y 1 h `Black;
      cx + cw + gutter_w;
    ) (x + margin_w) cols - x - margin_w
  in
  Api.Draw.unclip win;

  let gutter_tolerance = 5 in
  let rec find_gutter' mx i cx =
    if i = Array.length cols then None else
    let cx' = cx + fst cols.(i) in
    if abs (cx' + gutter_w/2 - mx) < gutter_tolerance then Some i else
    if cx' + gutter_w/2 < mx then find_gutter' mx (i + 1) (cx' + gutter_w) else
    None
  in
  let find_gutter mx =
    find_gutter' mx 0 (x + margin_w - view.scroll_h) in

  let rec find_heading' mx i cx =
    if i = Array.length cols then None else
    let cx' = cx + fst cols.(i) in
    if mx >= cx && mx < cx' then Some (fst st.library.columns.(i)) else
    if mx >= cx' then find_heading' mx (i + 1) (cx' + gutter_w) else
    None
  in
  let find_heading mx =
    find_heading' mx 0 (x + margin_w - view.scroll_h) in

  if Api.inside m (Ui.dim st.ui (header_area bw row_h)) then
  (
    match find_gutter mx with
    | None -> ()
    | Some _ -> Api.Mouse.set_cursor win (`Resize `E_W)
  );

  (* View *)
  let len = Array.length st.library.view.entries in
  let current =
    match st.control.current with Some track -> track.path | None -> "" in
  let rows =
    Array.init (min view.fit len) (fun i ->
      let i = i + view.scroll_v in
      let track = view.entries.(i) in
      let values =
        Array.map (fun (attr, _) -> Library.attr_string track attr)
          st.library.columns
      and c =
        match track.status with
        | _ when track.path = current -> `White
        | `Absent -> Ui.error_color st.ui
        | `Invalid -> Ui.warn_color st.ui
        | `Undet -> Ui.semilit_color (Ui.text_color st.ui)
        | `Predet | `Det -> Ui.text_color st.ui
      in
      let inv = if Library.is_selected st.library i then `Inverted else `Regular in
      c, inv, values
    )
  in
  let dragging = view_drag bw row_h st.ui (max_int, row_h) in
  (match view_table bw row_h st.ui cols rows view.scroll_h with
  | None -> ()
  | Some i ->
    let i = view.scroll_v + i in
    if Api.Key.are_modifiers_down [] && Api.Mouse.is_drag `Left then
    (
      let view_area = view_area st.library.browser_width row_h in
      Api.Mouse.set_cursor win
        (if
          Api.inside m (Ui.dim st.ui view_area) ||
          Api.inside m (Ui.dim st.ui playlist_area)
        then `Point else `Blocked)
    );
    if
      Api.Key.are_modifiers_down [] &&
      Api.Mouse.is_pressed `Left && dragging = `None
    then
    (
      if i >= len || not (Library.is_selected st.library i) then
      (
        Playlist.deselect_all st.playlist;
        Library.deselect_all st.library;
      );
      if i < len then
      (
        if Api.Mouse.is_doubleclick `Left then
        (
          Control.eject st.control;
          Playlist.remove_all st.playlist;
          let track = Track.make view.entries.(i).path in
          Playlist.insert st.playlist 0 [|track|];
          Control.switch st.control track true;
        )
        else
        (
          Playlist.deselect_all st.playlist;
          Library.select st.library i i;
        )
      )
    )
    else if Api.Key.are_modifiers_down [] && dragging = `Click then
    (
      Library.deselect_all st.library;
      if i < len then
      (
        Playlist.deselect_all st.playlist;
        Library.select st.library i i;
      )
    )
    else if Api.Key.are_modifiers_down [] && dragging = `Drop then
    (
      let (_, y, _, _) as r = Ui.dim st.ui playlist_area in
      if Api.inside m r then
      (
        let tracks = Library.selected st.library in
        let len = Array.length st.playlist.table.entries in
        let pos = min len ((my - y) / row_h + st.playlist.table.scroll_v) in
        Playlist.insert st.playlist pos (Array.map Track.make_from_data tracks);
        Library.deselect_all st.library;
        Playlist.deselect_all st.playlist;
        Playlist.select st.playlist pos (pos + Array.length tracks - 1);
        Control.switch_if_empty st.control (Playlist.current_opt st.playlist);
      )
    )
    else if
      Api.Key.are_modifiers_down [`Command] && Api.Mouse.is_pressed `Left
    then
    (
      if i < len then
      (
        if Library.is_selected st.library i then
          Library.deselect st.library i i
        else
        (
          Playlist.deselect_all st.playlist;
          Library.select st.library i i;
          if Api.Mouse.is_doubleclick `Left then
          (
            Control.eject st.control;
            Playlist.remove_all st.playlist;
            let tracks = Library.selected st.library in
            Playlist.insert st.playlist 0
              (Array.map (fun (tr : Data.track) -> Track.make tr.path) tracks);
            Control.switch st.control (Playlist.current st.playlist) true;
          )
        )
      )
    )
    else if Api.Key.are_modifiers_down [`Shift] && Api.Mouse.is_down `Left then
    (
      let default = if i < len then (i, i) else (0, 0) in
      let pos1, pos2 = Option.value view.sel_range ~default in
      let i' = max 0 (min i (len - 1)) in
      if view.sel_range = None || Library.is_selected st.library pos1 then
      (
        Playlist.deselect_all st.playlist;
        Library.deselect st.library pos2 i';
        Library.select st.library pos1 i';
      )
      else
      (
        Playlist.deselect_all st.playlist;
        Library.select st.library pos2 i';
        Library.deselect st.library pos1 i';
      )
    )
  );

  (* View selection *)
  if Library.has_selection st.library then
  (
    let d =
      if begin_key st.ui then -len else
      if end_key st.ui then +len else
      if pageup_key st.ui then -view.fit else
      if pagedown_key st.ui then +view.fit else
      if up_key st.ui then -1 else
      if down_key st.ui then +1 else
      0
    in
    if min len (abs d) > 0 then
    (
      let default = 0, if d < 0 then len else -1 in
      let _, pos2 = Option.value view.sel_range ~default in
      let i = if d < 0 then max 0 (pos2 + d) else min (len - 1) (pos2 + d) in
      Library.deselect_all st.library;
      Library.select st.library i i;
      Library.adjust_scroll st.library (Some i);
    );

    let d =
      if selbegin_key st.ui then -len else
      if selend_key st.ui then +len else
      if selpageup_key st.ui then -view.fit else
      if selpagedown_key st.ui then +view.fit else
      if selup_key st.ui then -1 else
      if seldown_key st.ui then +1 else
      0
    in
    if min len (abs d) > 0 then
    (
      let default = 0, if d < 0 then len else -1 in
      let pos1, pos2 = Option.value view.sel_range ~default in
      let i = if d < 0 then max 0 (pos2 + d) else min (len - 1) (pos2 + d) in
      if view.sel_range = None then
      (
        Library.select st.library (len - 1) i
      )
      else if Library.is_selected st.library pos1 then
      (
        Library.deselect st.library (max 0 pos2) i;
        Library.select st.library pos1 i
      )
      else
      (
        Library.select st.library (max 0 pos2) i;
        Library.deselect st.library pos1 i
      );
      Library.adjust_scroll st.library (Some i);
    );

    if selall_key st.ui then
    (
      Playlist.deselect_all st.playlist;
      Library.select_all st.library;
    )
    else if selnone_key st.ui then
    (
      Library.deselect_all st.library;
    )
    else if selinv_key st.ui then
    (
      Playlist.deselect_all st.playlist;
      Library.select_invert st.library;
    )
  );

  (* View scrolling *)
  let h' = view.fit * row_h in
  let ext =
    if len = 0 then 1.0 else min 1.0 (float h' /. float (len * row_h)) in
  let pos =
    if len = 0 then 0.0 else float view.scroll_v /. float len in
  let pos' = view_scroll_v bw st.ui pos ext -. 0.05 *. view_wheel bw st.ui in
  view.scroll_v <- clamp 0 (max 0 (len - view.fit))
    (int_of_float (Float.round (pos' *. float len)));

  let vw' = max vw (view.scroll_h + w) in
  let ext = if vw' = 0 then 1.0 else min 1.0 (float w /. float vw') in
  let pos = if vw' = 0 then 0.0 else float view.scroll_h /. float vw' in
  let pos' = view_scroll_h bw st.ui pos ext in
  view.scroll_h <-
    clamp 0 (max 0 (vw' - w)) (int_of_float (Float.round (pos' *. float vw')));

  (* View row sorting & column resizing *)
  (match header_drag bw row_h st.ui (1, max_int) with
  | `None | `Drop -> ()
  | `Click ->
    (match find_heading mx with
    | None -> ()
    | Some attr -> Library.reorder_view st.library attr
    )
  | `Drag (dx, _) ->
    (match find_gutter (mx - dx) with
    | None -> ()
    | Some i ->
      let add_snd d (x, y) = (x, max 0 (y + d)) in
      st.library.columns.(i) <- add_snd dx st.library.columns.(i);
      if i + 1 < Array.length st.library.columns
      && Api.Key.is_modifier_down `Shift then
        st.library.columns.(i + 1) <- add_snd (-dx) st.library.columns.(i + 1);
    )
  );

  (* Library resizing *)
  let left = st.library.side = `Left in
  if st.playlist.shown && snd (Api.Window.size win) > control_h then
  (
    let dw, dh =
      (if left then library_resizer_l `NE_SW else library_resizer_r `NW_SE)
        st.ui (control_w + library_min, control_h + playlist_min) (-1, -1)
    in
    st.library.width <- st.library.width + dw;
    st.library.browser_width <-
      min st.library.browser_width (browser_max st.library.width);
    st.playlist.height <- st.playlist.height + dh;
    update_fit st
  )
  else
  (
    let h = control_h + (if st.playlist.shown then st.playlist.height else 0) in
    let dw, _ =
      (if left then library_resizer_l else library_resizer_r) `E_W
        st.ui (control_w + library_min, h) (-1, h)
    in
    st.library.width <- st.library.width + dw;
    st.library.browser_width <-
      min st.library.browser_width (browser_max st.library.width);
  )


(* Runner *)

let rec run (st : State.t) =
  let win = Ui.window st.ui in
  if Api.Window.closed win then exit 0;

  Api.Draw.start win (`Trans (`Black, 0x40));

  (* Handle background and window repositioning *)
  Ui.background st.ui;

  (* Remember current geometry for later *)
  let playlist_shown = st.playlist.shown in
  let library_shown = st.library.shown in
  let library_side = st.library.side in
  let library_width = st.library.width in

  (* Run panes *)
  run_control st;
  if not (Api.Window.is_minimized win) then
  (
    if playlist_shown then run_playlist st;
    if library_shown then run_library st;
  );

  (* Adjust font size *)
  let row_height' = st.config.row_height +
    (if enlarge_key st.ui then +1 else 0) +
    (if reduce_key st.ui then -1 else 0)
  in
  st.config.row_height <- max 8 (min 64 row_height');

  (* Adjust window size after resize *)
  let dw = if st.library.shown then st.library.width else 0 in
  let dh = if st.playlist.shown then st.playlist.height else 0 in
  Api.Window.set_size win (control_w + dw) (control_h + dh);

  (* Adjust window position after opening/closing library *)
  let dx =
    match library_shown, st.library.shown, library_side, st.library.side with
    | false, true, _, `Left
    | true, true, `Right, `Left -> -st.library.width  (* opened on the left *)
    | true, false, `Left, _
    | true, true, `Left, `Right -> +library_width     (* closed on the left *)
    | true, true, `Left, `Left -> library_width - st.library.width (* resized *)
    | _ -> 0
  in
  let x, y = Api.Window.pos win in
  if dx <> 0 then Api.Window.set_pos win (x + dx) y;

  Api.Draw.finish win;

  run st


(* Startup *)

let startup () =
  Storage.clear_temp ();
  let db = Db.init () in
  let win = Api.Window.init 0 0 control_w control_h App.name in
  let ui = Ui.make win in
  let audio = Api.Audio.init () in
  let rst = ref (State.make ui audio db) in
  let st = if State.load !rst then !rst else State.make ui audio db in
  update_fit st;
  Library.update_view st.library;
  Playlist.adjust_scroll st.playlist st.playlist.table.pos;
  at_exit (fun () -> State.save st; Storage.clear_temp (); Db.exit db);
  st

let _main =
  try
    Printexc.record_backtrace true;
    let st = startup () in
    run st
  with exn ->
    prerr_endline ("internal error: " ^ Printexc.to_string exn);
    Printexc.print_backtrace stderr;
    Stdlib.exit 2
