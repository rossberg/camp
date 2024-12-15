(* Main Entry Point *)

(* Layout *)

let control_width = 360
let control_height = 160

let close_button = Ui.button (5, 30, 20, 20) (`Char 'Q')

let play_button = Ui.control_button (5, 5, 20, 20) (`Char ' ')
let title_scroller = Ui.scroller (10, 53, 340, 16)
let seek_bar = Ui.progress_bar (30, 8, 300, 14)
let volume_bar = Ui.progress_bar (30, 33, 300, 14)

let resizer = Ui.resizer (-14, -14, 14, 14)


(* Helpers *)

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

  (* Play controls *)
  let on = (st.playing <> None) in
  let is_playing = on && Api.Audio.is_playing st.audio st.sound in
  let is_playing' = play_button st.win is_playing in
  if on && is_playing' then
    Api.Audio.resume st.audio st.sound
  else if on then
    Api.Audio.pause st.audio st.sound
  else if is_playing' && st.playlist <> [||] then
    State.switch_song st st.playlist.(st.playpos) true;

  (* Seek bar *)
  let length = Api.Audio.length st.audio st.sound in
  let played = Api.Audio.played st.audio st.sound in
  let remain = length -. played in
  let progress = if length > 0.0 then played /. length else 0.0 in
  (match seek_bar st.win progress with
  | Some percent when on -> State.seek_song st percent
  | _ -> ()
  );

  let s1 = fmt_time2 played in
  let s2 = "-" ^ fmt_time2 remain in
  let w2 = Api.Draw.text_width st.win 11 (Ui.font st.win 11) s2 in
  Api.Draw.text st.win 32 10 11 `White (Ui.font st.win 11) s1;
  Api.Draw.text st.win (328 - w2) 10 11 `White (Ui.font st.win 11) s2;

  (* Title info *)
  let name =
    match st.playing with
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
  Api.Draw.text st.win 20 75 20 (`Gray 0xc0) (Ui.font st.win 20) s;

  let mx, my = Api.Mouse.pos st.win in
  let s = Printf.sprintf "%d,%d" mx my in
  Api.Draw.text st.win 140 75 20 (`Gray 0xc0) (Ui.font st.win 20) s;

  let sw, sh = Api.Window.screen_size st.win in
  let s = Printf.sprintf "%dx%d" sw sh in
  Api.Draw.text st.win 20 100 20 (`Gray 0xc0) (Ui.font st.win 20) s;

  let ww, wh = Api.Window.size st.win in
  let s = Printf.sprintf "%dx%d" ww wh in
  Api.Draw.text st.win 140 100 20 (`Gray 0xc0) (Ui.font st.win 20) s;

  (* Playlist *)
  let x = 10 in
  let w = 340 in
  let h = 16 in
  let y0 = 120 in
  let font = Ui.font st.win h in
  let len = Array.length st.playlist in
  let digits = log10 (len + 1) + 1 in
  for i = 0 to len - 1 do
    let y = y0 + i * h in
    let song = st.playlist.(i) in
    let bg =
      match i mod 2 = 0, Sys.file_exists song.path with
      | true, true -> `Black
      | false, true -> `Gray 0x18
      | true, false -> `RGB 0x500000
      | false, false -> `RGB 0x680000
    in
    if bg <> `Black then Api.Draw.fill st.win x y w h bg;
    let color =
      match st.playing with
      | Some song when i = st.playpos && song.path = st.playlist.(i).path ->
        `White
      | _ -> `Green
    in
    let entry = fmt "%0*d. %s" digits (i + 1) song.name in
    let time = fmt_time song.time in
    let w1 = Api.Draw.text_width st.win h font entry in
    let w2 = Api.Draw.text_width st.win h font time in
    Api.Draw.clip st.win (x + 1, y, w - w2 - 2, h);
    Api.Draw.text st.win (x + 1) y h color font entry;
    if w1 > w - w2 then
      Api.Draw.gradient st.win (x + w - w2 - 17) y 16 h
        (`Trans (bg, 0)) `Horizontal bg;
    Api.Draw.unclip st.win;
    Api.Draw.text st.win (x + w - w2 + 1) y h color font time;
  done;

  (* Handle drag & drop *)
  let dropped = Api.File.dropped st.win in
  let songs = Array.map Song.make (Array.of_list dropped) in
  st.playlist <- Array.append st.playlist songs;

Printf.printf "[pl'=%b played=%.2f remain=%.2f playpos=%d]\n%!" is_playing' played remain st.playpos;
  (* Detect end of song *)
  if is_playing' && remain < 0.2 then
  (
    let last = st.playpos >= Array.length st.playlist - 1 in
    if not last then st.playpos <- st.playpos + 1;
    State.switch_song st st.playlist.(st.playpos) (not last);
Printf.printf "  [played=%.2f playpos=%d]\n%!" (Api.Audio.played st.audio st.sound) st.playpos;
  );

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
