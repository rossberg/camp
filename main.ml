(* Main Entry Point *)

(* Configuration *)

let playlist_file_check_freq = 5.0


(* Layout *)

let control_w = 360
let control_h = 160
let label_h = 8

let power_button = Ui.control_button (-45, 10, 35, 22) "" ([`Control], `Char 'Q')
let power_label = Ui.label (-45, 33, 35, label_h) `Center "POWER"

let playlist_indicator = Ui.indicator (-30, 48, 7, 7)
let playlist_button = Ui.control_button (-45, 57, 35, 11) "" ([], `Char 'P')
let playlist_label = Ui.label (-45, 69, 35, label_h) `Center "PLAYLIST"

let library_indicator = Ui.indicator (-30, 85, 7, 7)
let library_button = Ui.control_button (-45, 94, 35, 11) "" ([], `Char 'L')
let library_label = Ui.label (-45, 106, 35, label_h) `Center "LIBRARY"

let playlist_resizer = Ui.resizer (-14, -14, 14, 14)

let volume_bar = Ui.progress_bar (200, 21, 90, 12)
let volume_wheel = Ui.wheel (0, 0, control_w, control_h)
let volup_key = Ui.key ([], `Char '+')
let voldown_key = Ui.key ([], `Char '-')

let title_ticker = Ui.ticker (10, 70, -60, 16)
let seek_bar = Ui.progress_bar (10, 90, -60, 14)
let rw_key = Ui.key ([], `Arrow `Left)
let ff_key = Ui.key ([], `Arrow `Right)

let bwd_button = Ui.control_button (10, 122, 40, 30) "<<" ([], `Char 'Z')
let play_button = Ui.control_button (50, 122, 40, 30) ">" ([], `Char 'X')
let pause_button = Ui.control_button (90, 122, 40, 30) "||" ([], `Char 'C')
let stop_button = Ui.control_button (130, 122, 40, 30) "[]" ([], `Char 'V')
let fwd_button = Ui.control_button (170, 122, 40, 30) ">>" ([], `Char 'B')
let eject_button = Ui.control_button (210, 122, 40, 30) "^" ([], `Char 'N')

let shuffle_indicator = Ui.indicator (270, 122, 7, 7)
let shuffle_button = Ui.control_button (259, 131, 25, 11) "" ([], `Char 'T')
let shuffle_label = Ui.label (259, 143, 25, label_h) `Center "SHUFFLE"

let repeat_indicator1 = Ui.indicator (296, 122, 7, 7)
let repeat_indicator2 = Ui.indicator (307, 122, 7, 7)
let repeat_button = Ui.control_button (292, 131, 25, 11) "" ([], `Char 'R')
let repeat_label = Ui.label (292, 143, 25, label_h) `Center "REPEAT"

let loop_indicator1 = Ui.indicator (329, 122, 7, 7)
let loop_indicator2 = Ui.indicator (340, 122, 7, 7)
let loop_button = Ui.control_button (325, 131, 25, 11) "" ([], `Char 'J')
let loop_label = Ui.label (325, 143, 25, label_h) `Center "LOOP"

let playlist_row_h = 13
let playlist_min = 31 + 4 * playlist_row_h
let playlist_rect = (10, 170, -21, -18)
let playlist = Ui.table playlist_rect playlist_row_h
let playlist_scroll = Ui.scroll_bar (-20, 170, 10, -18)
let playlist_wheel = Ui.wheel (10, 170, -10, -18)
let playlist_drag = Ui.drag (10, 170, -20, -18)
let del_key = Ui.key ([], `Delete)

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
let selall_key = Ui.key ([`Control], `Char 'A')

let moveup_key = Ui.key ([`Control], `Arrow `Up)
let movedown_key = Ui.key ([`Control], `Arrow `Down)
let movepageup_key = Ui.key ([`Control], `Page `Up)
let movepagedown_key = Ui.key ([`Control], `Page `Down)
let movebegin_key = Ui.key ([`Control], `End `Up)
let moveend_key = Ui.key ([`Control], `End `Down)

let undo_button = Ui.key ([`Control], `Char 'Z')
let redo_button = Ui.key ([`Shift; `Control], `Char 'Z')


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

let fmt_time2 t =
  let t' = int_of_float (Float.trunc t) in
  fmt "%02d:%02d" (t' / 60) (t' mod  60)


(* Runner *)

let rec run (st : State.t) =
  Api.Draw.start st.win `Black;

  (* Window *)
  Ui.window st.win;

  if not (power_button st.win true) then exit 0;
  power_label st.win;

  (* Current status *)
  State.ok st;
  let length = Api.Audio.length st.audio st.sound in
  let played = Api.Audio.played st.audio st.sound in
  let remain = length -. played in
  let playing = Api.Audio.is_playing st.audio st.sound in
  let paused = not playing && played > 0.0 in
  let stopped = not playing && not paused in
  let silence = st.sound = Api.Audio.silence st.audio in

  (* Looping *)
  (match st.loop with
  | `AB (t1, t2) when playing && t2 < played ->
    State.seek_track st (t1 /. length);
  | _ -> ()
  );

  (* End of track *)
  if playing && (remain < 0.2 || silence) then
  (
    let play_on =
      match st.repeat with
      | `None when st.playpos >= Array.length st.playlist - 1 -> false
      | `None -> st.playpos <- st.playpos + 1; true
      | `One -> true
      | `All -> st.playpos <- 0; true
    in
    let next =
      if st.playlist = [||]
      then Option.get st.current
      else st.playlist.(st.playpos)
    in
    State.switch_track st next play_on;
  );

  (* Play controls *)
  if bwd_button st.win false && st.playlist <> [||] then
  (
    st.playpos <- modulo (st.playpos - 1) (Array.length st.playlist);
    State.switch_track st st.playlist.(st.playpos) playing
  );
  if fwd_button st.win false && st.playlist <> [||] then
  (
    st.playpos <- modulo (st.playpos + 1) (Array.length st.playlist);
    State.switch_track st st.playlist.(st.playpos) playing
  );

  let playing' = play_button st.win playing in
  if not playing && playing' && st.playlist <> [||] then
    State.switch_track st st.playlist.(st.playpos) true;

  let paused' = pause_button st.win paused in
  if playing && paused' then
    Api.Audio.pause st.audio st.sound
  else if not stopped && not paused' then
    Api.Audio.resume st.audio st.sound;

  if stop_button st.win false && not stopped then
  (
    Api.Audio.pause st.audio st.sound;
    State.switch_track st st.playlist.(st.playpos) false;
  );

  if eject_button st.win false then
  (
    State.eject_track st;
    State.remove_all st;
  )
  else if undo_button st.win then
    State.pop_undo st
  else if redo_button st.win then
    State.pop_redo st;

  if Api.Key.is_released (`Char ' ') then
  (
    if playing then
      Api.Audio.pause st.audio st.sound
    else if paused then
      Api.Audio.resume st.audio st.sound
    else if stopped && st.playlist <> [||] then
      State.switch_track st st.playlist.(st.playpos) true
  );

  (* Play modes *)
  shuffle_label st.win;
  (* TODO *)
  shuffle_indicator st.win false;
  let _shuffle' = shuffle_button st.win false in

  repeat_label st.win;
  repeat_indicator1 st.win (st.repeat <> `None);
  repeat_indicator2 st.win (st.repeat = `All);
  let repeat' = repeat_button st.win (st.repeat <> `None) in
  st.repeat <-
    (match st.repeat, repeat' with
    | `None, false | `All, false -> `None
    | `None, true | `One, true -> `One
    | `One, false | `All, true -> `All
    );

  loop_label st.win;
  loop_indicator1 st.win (st.loop <> `None);
  loop_indicator2 st.win (match st.loop with `AB _ -> true | _ -> false);
  let loop' = loop_button st.win (st.loop <> `None) in
  st.loop <-
    (match st.loop, loop' with
    | `None, false | `AB _, false -> `None
    | `None, true -> `A played
    | `A t1, true -> `A t1
    | `A t1, false when t1 > played -> `A played
    | `A t1, false -> `AB (t1, played)
    | `AB (t1, t2), true -> `AB (t1, t2)
    );

  (* Seek bar *)
  let progress =
    if length > 0.0 && not silence then played /. length else 0.0 in
  let progress' = seek_bar st.win progress +.
    0.05 *. (float_of_bool (ff_key st.win) -. float_of_bool (rw_key st.win)) in
  if progress' <> progress && not silence then
    State.seek_track st (clamp 0.0 1.0 progress');

  let s1 = fmt_time2 played in
  let s2 = "-" ^ fmt_time2 remain in
  let w2 = Api.Draw.text_width st.win 11 (Ui.font st.win 11) s2 in
  Api.Draw.text st.win 12 91 11 `White (Ui.font st.win 11) s1;
  Api.Draw.text st.win (298 - w2) 91 11 `White (Ui.font st.win 11) s2;

  (* Title info *)
  let name =
    match st.current with
    | Some track -> track.name ^ " (" ^ fmt_time track.time ^ ")"
    | None -> App.(name ^ " " ^ version)
  in
  title_ticker st.win name;

  (* Volume control *)
  let vol = volume_bar st.win st.volume +. 0.05 *. volume_wheel st.win +.
    0.05 *. (float_of_bool (volup_key st.win) -. float_of_bool (voldown_key st.win)) in
  if vol <> st.volume then
  (
    st.volume <- clamp 0.0 1.0 vol;
    Api.Audio.volume st.audio st.sound st.volume
  );

  (* Coordinate debug info *)
  let wx, wy = Api.Window.pos st.win in
  let s = Printf.sprintf "%d,%d" wx wy in
  Api.Draw.text st.win 15 10 20 (`Gray 0xc0) (Ui.font st.win 20) s;

  let mx, my = Api.Mouse.pos st.win in
  let s = Printf.sprintf "%d,%d" mx my in
  Api.Draw.text st.win 120 10 20 (`Gray 0xc0) (Ui.font st.win 20) s;

  let sw, sh = Api.Window.screen_size st.win in
  let s = Printf.sprintf "%dx%d" sw sh in
  Api.Draw.text st.win 15 35 20 (`Gray 0xc0) (Ui.font st.win 20) s;

  let ww, wh = Api.Window.size st.win in
  let s = Printf.sprintf "%dx%d" ww wh in
  Api.Draw.text st.win 120 35 20 (`Gray 0xc0) (Ui.font st.win 20) s;

  (* Window modes *)
  playlist_label st.win;
  playlist_indicator st.win st.playopen;
  let playopen' = playlist_button st.win st.playopen in
  if st.playopen then
    st.playheight <- snd (Api.Window.size st.win) - control_h;
  (match st.playopen, playopen' with
  | false, true ->
    Api.Window.set_size st.win control_w (control_h + st.playheight)
  | true, false ->
    Api.Window.set_size st.win control_w control_h
  | _, _ -> ()
  );
  st.playopen <- playopen';

  (* TODO *)
  library_label st.win;
  library_indicator st.win false;
  let _libopen' = library_button st.win false in


  (* Playlist *)
if st.playopen then
(
  let now = Unix.time () in
  let len = Array.length st.playlist in
  let digits = log10 (len + 1) + 1 in
  let font = Ui.font st.win playlist_row_h in
  let smax1 = String.make digits '0' ^ ". " in
  let cw1 = Api.Draw.text_width st.win playlist_row_h font smax1 + 1 in
  let cw3 = ref 16 in
  let (_, y, w, h) as r = Ui.dim st.win playlist_rect in
  let vlen = max 0 (int_of_float (Float.floor (float h /. float playlist_row_h))) in
  let h' = vlen * playlist_row_h in
  st.playscroll <- clamp 0 (max 0 (len - vlen)) st.playscroll;  (* correct for possible resize *)
  let rows =
    Array.init vlen (fun i ->
      let i = i + st.playscroll in
      if i >= len then `Green, `Black, [|""; ""; ""|] else
      let track = st.playlist.(i) in
      if now -. track.last_update > playlist_file_check_freq then
        State.update_track st track;
      let bg = if i mod 2 = 0 then `Black else `Gray 0x20 in
      let fg =
        match track.status with
        | _ when i = st.playpos ->
          if track.path = (Option.get st.current).path then `White else `Gray 0xc0
        | `Absent -> `Red
        | `Invalid -> `Yellow
        | `Undet -> `Blue
        | `Predet | `Det -> `Green
      in
      let fg, bg = if track.selected then bg, fg else fg, bg in
      let time = if track.time = 0.0 then "" else fmt_time track.time in
      cw3 := max !cw3 (Api.Draw.text_width st.win playlist_row_h font time + 1);
      fg, bg, [|fmt "%0*d. " digits (i + 1); track.name; time|]
    )
  in
  let cols = [|cw1, `Right; w - cw1 - !cw3 - 2, `Left; !cw3, `Right|] in
  let dragging = playlist_drag st.win (max_int, playlist_row_h) in
  (match playlist st.win cols rows with
  | None -> ()
  | Some i ->
    let i = st.playscroll + i in
    let i' = min i (len - 1) in
    if Api.Key.are_modifiers_down [] && Api.Mouse.is_pressed `Left && dragging = None then
    (
      if i >= len || not st.playlist.(i).selected then
      (
        st.playrange <- (if i >= len then max_int else i), i';
        State.deselect_all st;
      );
      if i < len then
      (
        if Api.Mouse.is_doubleclick `Left then
        (
          st.playpos <- i;
          State.switch_track st st.playlist.(i) true;
          State.deselect st i i;
        )
        else
        (
          State.select st i i;
        )
      )
    )
    else if Api.Key.are_modifiers_down [`Control] && Api.Mouse.is_pressed `Left then
    (
      st.playrange <- (if i >= len then max_int else i), i';
      if i < len then
        if st.playlist.(i).selected then
          State.deselect st i i
        else
          State.select st i i
    )
    else if Api.Key.are_modifiers_down [`Shift] && Api.Mouse.is_down `Left then
    (
      let fst, snd = st.playrange in
      st.playrange <- fst, i';
      if fst < 0 || fst >= len || st.playlist.(fst).selected then
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
    if begin_key st.win then -len else
    if end_key st.win then +len else
    if pageup_key st.win then -vlen else
    if pagedown_key st.win then +vlen else
    if up_key st.win then -1 else
    if down_key st.win then +1 else
    0
  in
  if min len (abs d) > 0 then
  (
    let i =
      if d < 0
      then max 0 (Option.value (State.first_selected st) ~default: (len - 1) + d)
      else min (len - 1) (Option.value (State.last_selected st) ~default: 0 + d)
    in
    State.deselect_all st;
    State.select st i i;
  );

  let d =
    if selbegin_key st.win then -len else
    if selend_key st.win then +len else
    if selpageup_key st.win then -vlen else
    if selpagedown_key st.win then +vlen else
    if selup_key st.win then -1 else
    if seldown_key st.win then +1 else
    0
  in
  if min len (abs d) > 0 then
  (
    let fst, snd = st.playrange in
    let i = max 0 (min (len - 1) (snd + d)) in
    st.playrange <- fst, i;
    if fst < 0 || fst >= len || st.playlist.(fst).selected then
    (
      State.deselect st snd i;
      State.select st (max 0 fst) i
    )
    else
    (
      State.select st snd i;
      State.deselect st (max 0 fst) i
    )
  );

  if selall_key st.win then
  (
    State.select_all st;
    st.playrange <- 0, len - 1;
  );

  (* Playlist reordering *)
  let d0 =
    match dragging with
    | Some (_, dy) when Api.Key.are_modifiers_down [] -> dy
    | _ -> 0
  in
  let d = d0 +
    if movebegin_key st.win then -len else
    if moveend_key st.win then +len else
    if movepageup_key st.win then -vlen else
    if movepagedown_key st.win then +vlen else
    if moveup_key st.win then -1 else
    if movedown_key st.win then +1 else
    0
  in
  if min len (abs d) > 0 then
  (
    State.push_undo st;
    let d' =
      if d < 0
      then max d (- Option.value (State.first_selected st) ~default: (len - 1))
      else min d (len - Option.value (State.last_selected st) ~default: 0 - 1)
    in
    State.move_selected st d';
    if dragging = None then
      st.playscroll <- clamp 0 (max 0 (len - vlen)) (st.playscroll + d);
  );

  (* Playlist scrolling *)
  let ext = if len = 0 then 1.0 else min 1.0 (float h' /. float (len * playlist_row_h)) in
  let pos = if len = 0 then 0.0 else float st.playscroll /. float len in
  let pos' = playlist_scroll st.win pos ext -. 0.05 *. playlist_wheel st.win in
  st.playscroll <- clamp 0 (max 0 (len - vlen))
    (int_of_float (Float.round (pos' *. float len)));

  (* Playlist deletion *)
  if del_key st.win then
    State.remove_selected st;

  (* Playlist drag & drop *)
  let dropped = Api.File.dropped st.win in
  let _, my as m = Api.Mouse.pos st.win in
  let pos = if Api.inside m r then
    min len ((my - y) / playlist_row_h + st.playscroll) else len in
  State.insert st pos dropped;

  playlist_resizer st.win (control_w, control_h + playlist_min) (control_w, -1);
);

  (* All done *)
  Api.Draw.finish st.win;
  run st


(* Startup *)

let startup () =
  File.clear_temp ();
  let win = Api.Window.init 0 0 control_w control_h App.name in
  let audio = Api.Audio.init () in
  let st = State.make win audio in
  Persist.load_state st;
  at_exit (fun () -> Persist.save_state st; File.clear_temp ());
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
