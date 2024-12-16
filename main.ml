(* Main Entry Point *)

(* Layout *)

let control_width = 360
let control_height = 160

let close_button = Ui.button (-14, 0, 14, 14) (`Char 'Q')
let resizer = Ui.resizer (-14, -14, 14, 14)

let title_scroller = Ui.scroller (10, 70, 340, 16)
let seek_bar = Ui.progress_bar (10, 90, 340, 14)

let bwd_button = Ui.control_button (10, 122, 40, 30) "<<" (`Char 'Z')
let play_button = Ui.control_button (50, 122, 40, 30) ">" (`Char 'X')
let pause_button = Ui.control_button (90, 122, 40, 30) "||" (`Char 'C')
let stop_button = Ui.control_button (130, 122, 40, 30) "[]" (`Char 'V')
let fwd_button = Ui.control_button (170, 122, 40, 30) ">>" (`Char 'B')
let eject_button = Ui.control_button (210, 122, 40, 30) "^" (`Char 'N')
let undo_button = Ui.overlay_button (210, 122, 40, 30) (`Char 'Z')

let volume_bar = Ui.progress_bar (260, 131, 90, 12)


(* Helpers *)

let modulo n m = let k = n mod m in if k < 0 then k + m else k

let rec log10 n = if n < 10 then 0 else 1 + log10 (n / 10)

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
  let x = 10 in
  let w = control_width - 2*x in
  let h = 16 in
  let y0 = control_height in
  let font = Ui.font st.win h in
  let len = Array.length st.playlist in
  let digits = log10 (len + 1) + 1 in
  for i = 0 to min (len - 1) ((snd (Api.Window.size st.win) - y0) / h + 1) do
    let y = y0 + i * h in
    let song = st.playlist.(i) in
    if not (State.is_loaded_song song) then State.update_song song;
    let bg =
      match i mod 2 = 0, Sys.file_exists song.path with
      | true, true -> `Black
      | false, true -> `Gray 0x20
      | true, false -> `RGB 0x500000
      | false, false -> `RGB 0x680000
    in
    if bg <> `Black then Api.Draw.fill st.win x y w h bg;
    let color =
      match st.current with
      | Some song when i = st.playpos ->
        if song.path = st.playlist.(i).path then `White else `Gray 0xc0
      | _ -> `Green
    in
    let entry = fmt "%0*d. %s" digits (i + 1) song.name in
    let time = if song.time = 0.0 then "" else fmt_time song.time in
    let w1 = Api.Draw.text_width st.win h font entry in
    let w2 = Api.Draw.text_width st.win h font time in
    Api.Draw.clip st.win (x + 1, y, w - w2 - 3, h);
    Api.Draw.text st.win (x + 1) y h color font entry;
    if w1 > w - w2 then
      Api.Draw.gradient st.win (x + w - w2 - 18) y 16 h
        (`Trans (bg, 0)) `Horizontal bg;
    Api.Draw.unclip st.win;
    Api.Draw.text st.win (x + w - w2 - 2) y h color font time;
  done;

  (* Handle drag & drop *)
  let dropped = Api.File.dropped st.win in
  let _, my as m = Api.Mouse.pos st.win in
  let pos = if Api.inside m (x, y0, w, len * h) then (my - y0) / h else len in
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
