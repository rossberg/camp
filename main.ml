(* Main Entry Point *)

(* Configuration *)

let playlist_file_check_freq = 5.0


(* Layout *)

let control_width = 360
let control_height = 160

let close_button = Ui.button (-14, 0, 14, 14) (`Control, `Char 'Q')
let resizer = Ui.resizer (-14, -14, 14, 14)

let title_scroller = Ui.scroller (10, 70, -10, 16)
let seek_bar = Ui.progress_bar (10, 90, -10, 14)

let bwd_button = Ui.control_button (10, 122, 40, 30) "<<" (`Plain, `Char 'Z')
let play_button = Ui.control_button (50, 122, 40, 30) ">" (`Plain, `Char 'X')
let pause_button = Ui.control_button (90, 122, 40, 30) "||" (`Plain, `Char 'C')
let stop_button = Ui.control_button (130, 122, 40, 30) "[]" (`Plain, `Char 'V')
let fwd_button = Ui.control_button (170, 122, 40, 30) ">>" (`Plain, `Char 'B')
let eject_button = Ui.control_button (210, 122, 40, 30) "^" (`Plain, `Char 'N')
let undo_button = Ui.key (`Control, `Char 'Z')

let volume_bar = Ui.progress_bar (260, 131, 90, 12)

let playlist_rect = (10, 170, -23, -18)
let playlist_row_h = 16
let playlist = Ui.table playlist_rect playlist_row_h
let playlist_scroll = Ui.scroll_bar (-20, 170, 10, -18)


(* Helpers *)

let modulo n m = let k = n mod m in if k < 0 then k + m else k

let rec log10 n = if n < 10 then 0 else 1 + log10 (n / 10)

let clamp min max v =
  if v < min then min else
  if v > max then max else
  v


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
  resizer st.win (control_width, control_height) (control_width, -1);
  if close_button st.win then exit 0;

  (* Current status *)
  State.ok st;
  let length = Api.Audio.length st.audio st.sound in
  let played = Api.Audio.played st.audio st.sound in
  let remain = length -. played in
  let playing = Api.Audio.is_playing st.audio st.sound in
  let paused = not playing && played > 0.0 in
  let stopped = not playing && not paused in
  let silence = st.sound = Api.Audio.silence st.audio in

  (* Detect end of song *)
  if playing && (remain < 0.2 || silence) then
  (
    let last = st.playpos >= Array.length st.playlist - 1 in
    let next =
      if last then Option.get st.current else
      (st.playpos <- st.playpos + 1; st.playlist.(st.playpos))
    in
    State.switch_song st next (not last);
  );

  (* Play controls *)
  if bwd_button st.win false && st.playlist <> [||] then
  (
    st.playpos <- modulo (st.playpos - 1) (Array.length st.playlist);
    State.switch_song st st.playlist.(st.playpos) playing
  );
  if fwd_button st.win false && st.playlist <> [||] then
  (
    st.playpos <- modulo (st.playpos + 1) (Array.length st.playlist);
    State.switch_song st st.playlist.(st.playpos) playing
  );

  let playing' = play_button st.win playing in
  if not playing && playing' && st.playlist <> [||] then
    State.switch_song st st.playlist.(st.playpos) true;

  let paused' = pause_button st.win paused in
  if playing && paused' then
    Api.Audio.pause st.audio st.sound
  else if not stopped && not paused' then
    Api.Audio.resume st.audio st.sound;

  if stop_button st.win false && not stopped then
  (
    Api.Audio.pause st.audio st.sound;
    State.switch_song st st.playlist.(st.playpos) false;
  );

  if eject_button st.win false then
  (
    State.eject_song st;
    State.clear_songs st;
  )
  else if undo_button st.win then
    State.pop_undo st;

  if Api.Key.is_released (`Char ' ') then
  (
    if playing then
      Api.Audio.pause st.audio st.sound
    else if paused then
      Api.Audio.resume st.audio st.sound
    else if stopped && st.playlist <> [||] then
      State.switch_song st st.playlist.(st.playpos) true
  );

  (* Seek bar *)
  let progress =
    if length > 0.0 && not silence then played /. length else 0.0 in
  (match seek_bar st.win progress with
  | Some percent when not silence -> State.seek_song st percent
  | _ -> ()
  );

  let s1 = fmt_time2 played in
  let s2 = "-" ^ fmt_time2 remain in
  let w2 = Api.Draw.text_width st.win 11 (Ui.font st.win 11) s2 in
  Api.Draw.text st.win 12 91 11 `White (Ui.font st.win 11) s1;
  Api.Draw.text st.win (348 - w2) 91 11 `White (Ui.font st.win 11) s2;

  (* Title info *)
  let name =
    match st.current with
    | Some song -> song.name ^ " (" ^ fmt_time song.time ^ ")"
    | None -> App.(name ^ " " ^ version)
  in
  title_scroller st.win name;

  (* Volume control *)
  (match volume_bar st.win st.volume with
  | Some vol when vol <> st.volume ->
    st.volume <- vol;
    Api.Audio.volume st.audio st.sound vol
  | _ -> ()
  );

  (* Coordinate debug info *)
  let wx, wy = Api.Window.pos st.win in
  let s = Printf.sprintf "%d,%d" wx wy in
  Api.Draw.text st.win 20 10 20 (`Gray 0xc0) (Ui.font st.win 20) s;

  let mx, my = Api.Mouse.pos st.win in
  let s = Printf.sprintf "%d,%d" mx my in
  Api.Draw.text st.win 140 10 20 (`Gray 0xc0) (Ui.font st.win 20) s;

  let sw, sh = Api.Window.screen_size st.win in
  let s = Printf.sprintf "%dx%d" sw sh in
  Api.Draw.text st.win 20 35 20 (`Gray 0xc0) (Ui.font st.win 20) s;

  let ww, wh = Api.Window.size st.win in
  let s = Printf.sprintf "%dx%d" ww wh in
  Api.Draw.text st.win 140 35 20 (`Gray 0xc0) (Ui.font st.win 20) s;

  (* Playlist *)
  let now = Unix.time () in
  let len = Array.length st.playlist in
  let digits = log10 (len + 1) + 1 in
  let font = Ui.font st.win playlist_row_h in
  let smax1 = String.make digits '0' ^ ". " in
  let cw1 = Api.Draw.text_width st.win playlist_row_h font smax1 + 1 in
  let cw3 = ref 16 in
  let (_, y, w, h) as r = Ui.dim st.win playlist_rect in
  let vlen = int_of_float (Float.floor (float h /. float playlist_row_h)) in
  let h' = vlen * playlist_row_h in
  st.playscroll <- clamp 0 (max 0 (len - vlen)) st.playscroll;  (* correct for possible resize *)
  let rows =
    Array.init vlen (fun i ->
      let i = i + st.playscroll in
      if i >= len then `Green, `Black, [|""; ""; ""|] else
      let song = st.playlist.(i) in
      if now -. song.last_update > playlist_file_check_freq then State.update_song st song;
      let bg = if i mod 2 = 0 then `Black else `Gray 0x20 in
      let fg =
        match song.status with
        | _ when i = st.playpos ->
          if song.path = (Option.get st.current).path then `White else `Gray 0xc0
        | `Absent -> `Red
        | `Invalid -> `Yellow
        | `Undet -> `Blue
        | `Predet | `Det -> `Green
      in
      let time = if song.time = 0.0 then "" else fmt_time song.time in
      cw3 := max !cw3 (Api.Draw.text_width st.win playlist_row_h font time + 1);
      fg, bg, [|fmt "%0*d. " digits (i + 1); song.name; time|]
    )
  in
  let cols = [|cw1, `Right; w - cw1 - !cw3, `Left; !cw3, `Right|] in
  (match playlist st.win cols rows with
  | Some i when st.playscroll + i < Array.length st.playlist ->
    st.playpos <- st.playscroll + i;
    State.switch_song st st.playlist.(st.playpos) playing
  | _ -> ()
  );

  let ext = if len = 0 then 1.0 else min 1.0 (float h' /. float (len * playlist_row_h)) in
  let pos = if len = 0 then 0.0 else float st.playscroll /. float len in
  (match playlist_scroll st.win pos ext with
(* TODO: remove
| exception (Assert_failure _ as exn) ->
Printf.printf "[scroll] len=%d pos=%d->%.2f ext=%d->%.2f\n%!"
len st.playscroll pos vlen ext
;raise exn
*)
  | Some pos' ->
(* TODO: remove
Printf.printf "[scroll] len=%d pos=%d->%.2f ext=%d->%.2f pos'=%.2f->%d\n%!"
len st.playscroll pos vlen ext pos' (int_of_float (pos' *. float len))
;
*)
    st.playscroll <- min (len - vlen) (int_of_float (Float.round (pos' *. float len)));
  | _ -> ()
  );

  (* Handle drag & drop *)
  let dropped = Api.File.dropped st.win in
  let _, my as m = Api.Mouse.pos st.win in
  let pos = if Api.inside m r then
    min len ((my - y) / playlist_row_h + st.playscroll) else len in
  State.insert_songs st pos dropped;

  Api.Draw.finish st.win;
  run st


(* Startup *)

let startup () =
  File.clear_temp ();
  let win = Api.Window.init 0 0 control_width control_height App.name in
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
