(* Main Entry Point *)


(* Configuration *)

let playlist_file_check_freq = 5.0


(* Layout *)

let control_w = 360
let control_h = 160
let label_h = 8

let power_button = Ui.button (-45, 10, 35, 22) "" ([`Command], `Char 'Q')
let power_label = Ui.label (-45, 33, 35, label_h) `Center "POWER"

let minimize_button = Ui.mouse (-45, 10, 35, 22) `Right

let playlist_indicator = Ui.indicator (-30, 48, 7, 7)
let playlist_button = Ui.button (-45, 56, 35, 12) "" ([], `Char 'P')
let playlist_label = Ui.label (-45, 69, 35, label_h) `Center "PLAYLIST"

let library_indicator = Ui.indicator (-30, 85, 7, 7)
let library_button = Ui.button (-45, 93, 35, 12) "" ([], `Char 'L')
let library_label = Ui.label (-45, 106, 35, label_h) `Center "LIBRARY"

let info_box = Ui.box (10, 10, -55, 98) `Black

let lcd_minus = Ui.lcd (15, 15, 14, 20)
let lcd1 = Ui.lcd (32, 15, 14, 20)
let lcd2 = Ui.lcd (49, 15, 14, 20)
let lcd_colon = Ui.lcd (66, 15, 4, 20)
let lcd3 = Ui.lcd (73, 15, 14, 20)
let lcd4 = Ui.lcd (90, 15, 14, 20)
let lcd_button = Ui.mouse (15, 15, 90, 20) `Left

let volume_bar = Ui.volume_bar (-87, 15, 27, 50)
let volume_wheel = Ui.wheel (0, 0, control_w, control_h)
let mute_text = Ui.ticker (-80, 70, 20, 8)
let mute_button = Ui.mouse (-87, 70, 27, 18) `Left
let mute_key = Ui.key ([], `Char '0')
let volup_key = Ui.key ([], `Char '+')
let voldown_key = Ui.key ([], `Char '-')

let color_button_fwd = Ui.mouse (10, 35, -80, 50) `Left
let color_button_bwd = Ui.mouse (10, 35, -80, 50) `Right

let prop_ticker = Ui.ticker (15, 38, -80, 12)
let title_ticker = Ui.ticker (12, 70, -80, 16)
let seek_bar = Ui.progress_bar (12, 90, -60, 14)
let rw_key = Ui.key ([], `Arrow `Left)
let ff_key = Ui.key ([], `Arrow `Right)

let bwd_button = Ui.button (10, 122, 40, 30) ~protrude: false "<<" ([], `Char 'Z')
let play_button = Ui.button (50, 122, 40, 30) ~protrude: false ">" ([], `Char 'X')
let pause_button = Ui.button (90, 122, 40, 30) ~protrude: false "||" ([], `Char 'C')
let stop_button = Ui.button (130, 122, 40, 30) ~protrude: false "[]" ([], `Char 'V')
let fwd_button = Ui.button (170, 122, 40, 30) ~protrude: false ">>" ([], `Char 'B')
let eject_button = Ui.button (210, 122, 40, 30) ~protrude: false "^" ([], `Char 'N')

let shuffle_indicator = Ui.indicator (270, 122, 7, 7)
let shuffle_button = Ui.button (259, 130, 25, 12) "" ([], `Char 'T')
let shuffle_label = Ui.label (259, 143, 25, label_h) `Center "SHUFFLE"

let repeat_indicator1 = Ui.indicator (296, 122, 7, 7)
let repeat_indicator2 = Ui.indicator (307, 122, 7, 7)
let repeat_button = Ui.button (292, 130, 25, 12) "" ([], `Char 'R')
let repeat_label = Ui.label (292, 143, 25, label_h) `Center "REPEAT"

let loop_indicator1 = Ui.indicator (329, 122, 7, 7)
let loop_indicator2 = Ui.indicator (340, 122, 7, 7)
let loop_button = Ui.button (325, 130, 25, 12) "" ([], `Char 'J')
let loop_label = Ui.label (325, 143, 25, label_h) `Center "LOOP"

let playlist_row_h = 13
let playlist_min = 31 + 4 * playlist_row_h
let playlist_rect = (10, 170, -21, -20)
let playlist = Ui.table playlist_rect playlist_row_h
let playlist_scroll = Ui.scroll_bar (-20, 170, 10, -20)
let playlist_wheel = Ui.wheel (10, 170, -10, -20)
let playlist_drag = Ui.drag (10, 170, -20, -20)
let playlist_summary = Ui.ticker (10, -15, 100, 10)
let playlist_resizer = Ui.resizer (-16, -16, 16, 16)

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

let sep_button = Ui.button (117, -18, 25, 17) "SEP" ([`Command], `Char ' ')
let del_button = Ui.button (147, -18, 25, 17) "DEL" ([], `Delete)
let crop_button = Ui.button (172, -18, 25, 17) "CROP" ([`Shift], `Delete)
let clean_button = Ui.button (197, -18, 25, 17) "CLEAN" ([`Command], `Delete)
let undo_button = Ui.button (227, -18, 25, 17) "UNDO" ([`Command], `Char 'Z')
let redo_button = Ui.button (252, -18, 25, 17) "REDO" ([`Shift; `Command], `Char 'Z')
let save_button = Ui.button (282, -18, 25, 17) "SAVE" ([`Command], `Char 'S')
let tag_button = Ui.button (312, -18, 25, 17) "TAG" ([`Command], `Char 'T')

let cut_key = Ui.key ([`Command], `Char 'X')
let copy_key = Ui.key ([`Command], `Char 'C')
let paste_key = Ui.key ([`Command], `Char 'V')


(* Helpers *)

let modulo n m = let k = n mod m in if k < 0 then k + m else k

let rec log10 n = if n < 10 then 0 else 1 + log10 (n / 10)

let clamp min max v =
  if v < min then min else
  if v > max then max else
  v

let float_of_bool b = float (Bool.to_int b)


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
  (* Exit button *)
  (* This has to come first, otherwise Raylib crashes? *)
  if not (power_button st.ui true) then exit 0;
  power_label st.ui;

  (* Current status *)
  State.ok st;
  let length = Api.Audio.length st.audio st.sound in
  let elapsed = Api.Audio.played st.audio st.sound in
  let remaining = length -. elapsed in
  let playing = Api.Audio.is_playing st.audio st.sound in
  let paused = not playing && elapsed > 0.0 in
  let stopped = not playing && not paused in
  let silence = st.sound = Api.Audio.silence st.audio in

  (* LCD *)
  info_box st.ui;
  let sign, d1, d2, d3, d4 =
    if paused && Api.Draw.frame (Ui.window st.ui) / 40 mod 2 = 0 then
      '+', ' ', ' ', ' ', ' ' else
    let sign, time =
      match st.timemode with
      | `Elapse -> '+', elapsed
      | `Remain -> '-', remaining
    in
    lcd_colon st.ui ':';
    let seconds = int_of_float (Float.round time) in
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
    st.timemode <-
      match st.timemode with
      | `Elapse -> `Remain
      | `Remain -> `Elapse
  );

  let ncol = Array.length Ui.color_schemes in
  let dcol =
    (if color_button_fwd st.ui then +1 else 0) +
    (if color_button_bwd st.ui then -1 else 0)
  in
  Ui.set_color_scheme st.ui ((Ui.get_color_scheme st.ui + dcol + ncol) mod ncol);

  (* Audio properties *)
  if not silence then
  (
    let track = Option.get st.current in
    let ext = Filename.extension track.path in
    let format = if ext = "" || ext.[0] <> '.' then "???" else
      String.uppercase_ascii (String.sub ext 1 (String.length ext - 1)) in
    let bitrate = Api.Audio.bitrate st.audio st.sound in
    let rate = Api.Audio.rate st.audio st.sound in
    let channels = Api.Audio.channels st.audio st.sound in
    let depth = bitrate /. float rate /. float channels in
    prop_ticker st.ui
      (fmt "%s    %.0f KBPS    %.1f KHZ    %s BIT    %s"
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
    match st.current with
    | Some track when not (State.is_separator track) ->
      track.name ^ " - " ^ fmt_time track.time
    | _ -> App.(name ^ " " ^ version)
  in
  title_ticker st.ui name;

  (* Volume control *)
  let volume' = volume_bar st.ui st.volume +. 0.05 *. volume_wheel st.ui +.
    0.05 *. (float_of_bool (volup_key st.ui) -. float_of_bool (voldown_key st.ui)) in
  mute_text st.ui "MUTE" ~unlit: (not st.mute);
  let mute' = if mute_button st.ui || mute_key st.ui then not st.mute else st.mute in
  if volume' <> st.volume || mute' <> st.mute then
  (
    st.mute <- mute';
    st.volume <- clamp 0.0 1.0 volume';
    Api.Audio.volume st.audio st.sound (if st.mute then 0.0 else st.volume);
  );

  (* Seek bar *)
  let progress =
    if length > 0.0 && not silence then elapsed /. length else 0.0 in
  let progress' = seek_bar st.ui progress +.
    0.05 *. (float_of_bool (ff_key st.ui) -. float_of_bool (rw_key st.ui)) in
  if progress' <> progress && not silence then
    State.seek_track st (clamp 0.0 1.0 progress');

(*
  let s1 = fmt_time2 elapsed in
  let s2 = "-" ^ fmt_time2 remaining in
  let w2 = Api.Draw.text_width st.ui 11 (Ui.font st.ui 11) s2 in
  Api.Draw.text st.ui 14 91 11 `White (Ui.font st.ui 11) s1;
  Api.Draw.text st.ui (278 - w2) 91 11 `White (Ui.font st.ui 11) s2;
*)

  (* Looping *)
  (match st.loop with
  | `AB (t1, t2) when playing && t2 < elapsed ->
    State.seek_track st (t1 /. length);
  | _ -> ()
  );

  (* End of track *)
  if playing && (remaining < 0.2 || silence) then
  (
    let pos = st.playlist_pos in
    let len = Array.length st.playlist in
    let next_pos, play_on =
      match st.repeat with
      | `One ->
        pos, true
      | _ when not st.shuffled && pos < len - 1 ->
        pos + 1, true
      | _ when st.shuffled && st.shuffle_pos < len - 1 ->
        st.shuffle_pos <- st.shuffle_pos + 1;
        st.shuffle_unobserved <- max st.shuffle_unobserved (st.shuffle_pos + 1);
        st.shuffle.(st.shuffle_pos), true
      | `None ->
        pos, false
      | `All when not st.shuffled ->
        0, true
      | `All ->
        st.shuffle_pos <- 0;
        st.shuffle.(0), true
    in
    st.playlist_pos <- next_pos;
    let next_track =
      if st.playlist = [||]
      then Option.get st.current
      else st.playlist.(st.playlist_pos)
    in
    State.switch_track st next_track play_on;
  );

  (* Play controls *)
  let off =
    (if bwd_button st.ui false && st.playlist <> [||] then -1 else 0) +
    (if fwd_button st.ui false && st.playlist <> [||] then +1 else 0)
  in
  if off <> 0 then
  (
    let len = Array.length st.playlist in
    st.playlist_pos <-
      if not st.shuffled then
        modulo (st.playlist_pos + off) len
      else
      (
        st.shuffle_pos <- modulo (st.shuffle_pos + off) len;
        st.shuffle_unobserved <- max st.shuffle_unobserved (st.shuffle_pos + 1);
        st.shuffle.(st.shuffle_pos)
      );
    State.switch_track st st.playlist.(st.playlist_pos) playing
  );

  let playing' = play_button st.ui playing in
  if stopped && playing' && st.playlist <> [||] then
    State.switch_track st st.playlist.(st.playlist_pos) true;

  let paused' = pause_button st.ui paused in
  if playing' && paused' then
    Api.Audio.pause st.audio st.sound
  else if (not stopped && not paused' || stopped && paused') && not silence then
    Api.Audio.resume st.audio st.sound;

  if stop_button st.ui false && not stopped then
  (
    Api.Audio.pause st.audio st.sound;
    State.switch_track st st.playlist.(st.playlist_pos) false;
  );

  if eject_button st.ui false then
  (
    State.eject_track st;
    State.remove_all st;
  );

  if Api.Key.is_released (`Char ' ') then
  (
    if playing then
      Api.Audio.pause st.audio st.sound
    else if paused then
      Api.Audio.resume st.audio st.sound
    else if stopped && st.playlist <> [||] then
      State.switch_track st st.playlist.(st.playlist_pos) true
  );

  (* Play modes *)
  shuffle_label st.ui;
  shuffle_indicator st.ui st.shuffled;
  let shuffled' = shuffle_button st.ui st.shuffled in
  if shuffled' <> st.shuffled then
  (
    if shuffled' then
    (
      if not stopped then
        State.shuffle st (Some st.playlist_pos)
      else
      (
        State.shuffle st None;
        if Array.length st.playlist > 0 then
        (
          st.playlist_pos <- st.shuffle.(0);
          st.shuffle_unobserved <- 1;
          State.switch_track st st.playlist.(st.playlist_pos) false;
        )
      )
    )
    else
      State.unshuffle st
  );

  repeat_label st.ui;
  repeat_indicator1 st.ui (st.repeat <> `None);
  repeat_indicator2 st.ui (st.repeat = `All);
  let repeat' = repeat_button st.ui (st.repeat <> `None) in
  st.repeat <-
    (match st.repeat, repeat' with
    | `None, false | `All, false -> `None
    | `None, true | `One, true -> `One
    | `One, false | `All, true -> `All
    );

  loop_label st.ui;
  loop_indicator1 st.ui (st.loop <> `None);
  loop_indicator2 st.ui (match st.loop with `AB _ -> true | _ -> false);
  let loop' = loop_button st.ui (st.loop <> `None) in
  st.loop <-
    (match st.loop, loop' with
    | `None, false | `AB _, false -> `None
    | `None, true -> `A elapsed
    | `A t1, true -> `A t1
    | `A t1, false when t1 > elapsed -> `A elapsed
    | `A t1, false -> `AB (t1, elapsed)
    | `AB (t1, t2), true -> `AB (t1, t2)
    );

  (* Window modes *)
  playlist_label st.ui;
  playlist_indicator st.ui st.playlist_open;
  let playlist_open' = playlist_button st.ui st.playlist_open in
  st.playlist_open <- playlist_open';

  (* TODO *)
  library_label st.ui;
  library_indicator st.ui false;
  let _libopen' = library_button st.ui false in

  (* Minimize button *)
  if minimize_button st.ui then
    Api.Window.minimize (Ui.window st.ui)


(* Playlist Section *)

let update_playlist_rows (st : State.t) =
  let _, _, _, h = Ui.dim st.ui playlist_rect in
  st.playlist_rows <- max 4 (int_of_float (Float.floor (float h /. float playlist_row_h)))

let run_playlist (st : State.t) =
  let now = Unix.time () in
  let len = Array.length st.playlist in

  (* Playlist table *)
  let digits = log10 (len + 1) + 1 in
  let font = Ui.font st.ui playlist_row_h in
  let smax1 = String.make digits '0' ^ ". " in
  let cw1 = Api.Draw.text_width (Ui.window st.ui) playlist_row_h font smax1 + 1 in
  let cw3 = ref 16 in
  let (_, y, w, _) as r = Ui.dim st.ui playlist_rect in
  let vlen = st.playlist_rows in
  let h' = vlen * playlist_row_h in
  (* Correct scrolling position for possible resize *)
  st.playlist_scroll <- clamp 0 (max 0 (len - vlen)) st.playlist_scroll;
  let c = Ui.color_schemes.(Ui.get_color_scheme st.ui) in
  let rows =
    Array.init vlen (fun i ->
      let i = i + st.playlist_scroll in
      if i >= len then c.text, `Black, [|""; ""; ""|] else
      let track = st.playlist.(i) in
      if now -. track.last_update > playlist_file_check_freq then
        State.update_track st track;
      let bg = if i mod 2 = 0 then `Black else `Gray 0x10 in
      let fg =
        match track.status with
        | _ when i = st.playlist_pos ->
          if track.path = (Option.get st.current).path then `White else `Gray 0xc0
        | _ when State.is_separator track -> c.text
        | `Absent -> c.error
        | `Invalid -> c.warn
        | `Undet -> c.focus
        | `Predet | `Det -> c.text
      in
      let fg, bg = if State.is_selected st i then bg, fg else fg, bg in
      let time = if track.time = 0.0 then "" else fmt_time track.time in
      cw3 := max !cw3 (Api.Draw.text_width (Ui.window st.ui) playlist_row_h font time + 1);
      fg, bg, [|fmt "%0*d. " digits (i + 1); track.name; time|]
    )
  in
  let cols = [|cw1, `Right; w - cw1 - !cw3 - 2, `Left; !cw3, `Right|] in
  let dragging = playlist_drag st.ui (max_int, playlist_row_h) in
  (match playlist st.ui cols rows with
  | None -> ()
  | Some i ->
    let i = st.playlist_scroll + i in
    let i' = max 0 (min i (len - 1)) in
    let fst' = if i >= len then max_int else i in
    if Api.Key.are_modifiers_down [] && Api.Mouse.is_pressed `Left && dragging = `None then
    (
      st.playlist_range <- fst', i';
      if i >= len || not (State.is_selected st i) then State.deselect_all st;
      if i < len then
      (
        if Api.Mouse.is_doubleclick `Left then
        (
          st.playlist_pos <- i;
          State.switch_track st st.playlist.(i) true;
          if st.shuffled then
          (
            let pos = Option.get (Array.find_index ((=) i) st.shuffle) in
            if pos < st.shuffle_unobserved then
              st.shuffle_pos <- pos
            else
            (
              (* Minimise new observation to one *)
              State.swap st.shuffle pos st.shuffle_unobserved;
              st.shuffle_pos <- st.shuffle_unobserved;
              st.shuffle_unobserved <- st.shuffle_pos + 1;
            )
          )
        )
        else
        (
          State.select st i i;
        )
      )
    )
    else if Api.Key.are_modifiers_down [] && not (Api.Mouse.is_pressed `Left) && dragging = `Click then
    (
      State.deselect_all st;
      if i < len then State.select st i i;
    )
    else if Api.Key.are_modifiers_down [`Command] && Api.Mouse.is_pressed `Left then
    (
      st.playlist_range <- fst', i';
      if i < len then
        if State.is_selected st i then
          State.deselect st i i
        else
          State.select st i i
    )
    else if Api.Key.are_modifiers_down [`Shift] && Api.Mouse.is_down `Left then
    (
      let fst, snd = st.playlist_range in
      st.playlist_range <- fst, i';
      if fst < 0 || fst >= len || State.is_selected st fst then
      (
        State.deselect st snd i';
        State.select st (max 0 fst) i'
      )
      else
      (
        State.select st snd i';
        State.deselect st (max 0 fst) i'
      )
    )
  );

  (* Playlist selection *)
  let d =
    if begin_key st.ui then - Array.length st.playlist else
    if end_key st.ui then + Array.length st.playlist else
    if pageup_key st.ui then -st.playlist_rows else
    if pagedown_key st.ui then +st.playlist_rows else
    if up_key st.ui then -1 else
    if down_key st.ui then +1 else
    0
  in
  if min len (abs d) > 0 then
  (
    let i =
      if d < 0
      then max 0 (Option.value (State.first_selected st) ~default: (len - 1) + d)
      else min (len - 1) (Option.value (State.last_selected st) ~default: 0 + d)
    in
    st.playlist_range <- i, i;
    State.deselect_all st;
    State.select st i i;
    State.scroll_to_view st i;
  );

  let d =
    if selbegin_key st.ui then - Array.length st.playlist else
    if selend_key st.ui then + Array.length st.playlist else
    if selpageup_key st.ui then - st.playlist_rows else
    if selpagedown_key st.ui then + st.playlist_rows else
    if selup_key st.ui then -1 else
    if seldown_key st.ui then +1 else
    0
  in
  if min len (abs d) > 0 then
  (
    let fst, snd = st.playlist_range in
    let i = max 0 (min (len - 1) (snd + d)) in
    st.playlist_range <- fst, i;
    if fst < 0 || fst >= len || State.is_selected st fst then
    (
      State.deselect st snd i;
      State.select st (max 0 fst) i
    )
    else
    (
      State.select st snd i;
      State.deselect st (max 0 fst) i
    );
    State.scroll_to_view st i;
  );

  if selall_key st.ui then
  (
    st.playlist_range <- 0, len - 1;
    State.select_all st;
  )
  else if selnone_key st.ui then
  (
    st.playlist_range <- State.no_range;
    State.deselect_all st;
  )
  else if selinv_key st.ui then
  (
    State.select_inv st;
  );

  (* Playlist reordering *)
  let d0 =
    match dragging with
    | `Drag (_, dy) when Api.Key.are_modifiers_down [] -> dy
    | _ -> 0
  in
  let d = d0 +
    if movebegin_key st.ui then -len else
    if moveend_key st.ui then +len else
    if movepageup_key st.ui then -vlen else
    if movepagedown_key st.ui then +vlen else
    if moveup_key st.ui then -1 else
    if movedown_key st.ui then +1 else
    0
  in
  if min len (abs d) > 0 then
  (
    let d' =
      if d < 0
      then max d (- Option.value (State.first_selected st) ~default: (len - 1))
      else min d (len - Option.value (State.last_selected st) ~default: 0 - 1)
    in
    State.move_selected st d';
    if d0 = 0 then
      st.playlist_scroll <- clamp 0 (max 0 (len - vlen)) (st.playlist_scroll + d);
  );

  (* Playlist scrolling *)
  let ext = if len = 0 then 1.0 else min 1.0 (float h' /. float (len * playlist_row_h)) in
  let pos = if len = 0 then 0.0 else float st.playlist_scroll /. float len in
  let pos' = playlist_scroll st.ui pos ext -. 0.05 *. playlist_wheel st.ui in
  st.playlist_scroll <- clamp 0 (max 0 (len - vlen))
    (int_of_float (Float.round (pos' *. float len)));

  (* Playlist buttons *)
  if sep_button st.ui false then
  (
    let pos = Option.value (State.first_selected st) ~default: 0 in
    State.insert st pos [|State.make_separator ()|];
    if State.num_selected st = 1 then
    (
      State.deselect_all st;
      State.select st pos pos;
    )
  );

  if del_button st.ui false then
    State.remove_selected st;

  if crop_button st.ui false then
    State.remove_unselected st;

  if clean_button st.ui false then
    State.remove_invalid st;

  if undo_button st.ui false then
    State.pop_undo st;

  if redo_button st.ui false then
    State.pop_redo st;

  if save_button st.ui false then
    ();  (* TODO *)

  if tag_button st.ui false && State.num_selected st > 0 && st.exec_tag <> "" then
  (
    let tracks = Array.to_list (State.copy_selected st) in
    Domain.spawn (fun () ->
      let tracks' = List.filter (fun tr -> not (State.is_separator tr)) tracks in
      let paths = List.map (fun tr -> tr.State.path) tracks' in
      if st.exec_tag_max_len = 0 then
        exec st.exec_tag paths
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
        let max = if List.length tracks < 20 then 1 else st.exec_tag_max_len in
        exec st.exec_tag (pick 0 max);
        List.iter (fun arg -> exec st.exec_tag ["/add"; arg]) !args;
      )
    ) |> ignore;
  );

  if cut_key st.ui then
  (
    let s = Persist.string_of_playlist (State.copy_selected st) in
    Api.Clipboard.write (Ui.window st.ui) s;
    State.remove_selected st;
  );

  if copy_key st.ui then
  (
    let s = Persist.string_of_playlist (State.copy_selected st) in
    Api.Clipboard.write (Ui.window st.ui) s;
  );

  if paste_key st.ui then
  (
    match Api.Clipboard.read (Ui.window st.ui) with
    | None -> ()
    | Some s ->
      let tracks = Persist.playlist_of_string s in
      let pos = Option.value (State.first_selected st) ~default: 0 in
      State.remove_selected st;
      State.insert st pos tracks;
  );

  (* Playlist drag & drop *)
  let dropped = Api.File.dropped (Ui.window st.ui) in
  let _, my as m = Api.Mouse.pos (Ui.window st.ui) in
  let pos = if Api.inside m r then
    min len ((my - y) / playlist_row_h + st.playlist_scroll) else len in
  State.insert_paths st pos dropped;

  (* Playlist summary *)
  if Api.Draw.frame (Ui.window st.ui) mod (10 * 60) = 0 then State.update_summary st;
  let fmt_sum (t, n) = fmt_time3 t ^ if n > 0 then "+" else "" in
  playlist_summary st.ui
    (fmt_sum st.playlist_sum_selected ^ " / " ^ fmt_sum st.playlist_sum);

  (* Playlist resizing *)
  let _, h' =
    playlist_resizer st.ui (control_w, control_h + playlist_min) (control_w, -1) in
  st.playlist_height <- h' - control_h;
  update_playlist_rows st


(* Runner *)

let rec run (st : State.t) =
  let win = Ui.window st.ui in
  if Api.Window.closed win then exit 0;
  if not (Api.Window.is_minimized win) then
  (
    st.win_pos <- Api.Window.pos win;
    st.win_size <- Api.Window.size win;
  );
  Api.Draw.start win (`Trans (`Black, 0x40));
  Ui.background st.ui;
  let playlist_open = st.playlist_open in
  run_control st;
  if not (Api.Window.is_minimized win) then
  (
    if playlist_open then run_playlist st;
  );
  Api.Draw.finish win;

  let dh = if st.playlist_open then st.playlist_height else 0 in
  Api.Window.set_size (Ui.window st.ui) control_w (control_h + dh);

  run st


(* Startup *)

let startup () =
  Storage.clear_temp ();
  let win = Api.Window.init 0 0 control_w control_h App.name in
  let ui = Ui.make win in
  let audio = Api.Audio.init () in
  let st = State.make ui audio in
  Persist.load_state st;
  update_playlist_rows st;
  State.scroll_to_view st st.playlist_pos;
  at_exit (fun () -> Persist.save_state st; Storage.clear_temp ());
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
