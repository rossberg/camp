(* Main Entry Point *)

(* Layout *)

let name = "Kamp"
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
  let on = (st.playing <> -1) in
  let is_playing = on && Api.Audio.is_playing st.audio st.sound in
  let is_playing' = play_button st.win is_playing in
  if on then Api.Audio.pause st.audio st.sound (not is_playing');

  (* Seek bar *)
  let length = if on then Api.Audio.length st.audio st.sound else 0.0 in
  let played = if on then Api.Audio.played st.audio st.sound else 0.0 in
  let remain = length -. played in
  let progress = if on then played /. length else 0.0 in
  (match seek_bar st.win progress with
  | Some x when on -> Api.Audio.seek st.audio st.sound (x *. length)
  | _ -> ()
  );

  let s1 = fmt_time2 played in
  let s2 = "-" ^ fmt_time2 remain in
  let w2 = Api.Draw.text_width st.win 11 (Ui.font st.win 11) s2 in
  Api.Draw.text st.win 32 10 11 `White (Ui.font st.win 11) s1;
  Api.Draw.text st.win (328 - w2) 10 11 `White (Ui.font st.win 11) s2;

  (* Title info *)
  let name = if not on then "Kamp 0.0.1" else
    let playing = st.playlist.(st.playing) in
    playing.artist ^ " - " ^ playing.title ^
    " (" ^ fmt_time playing.time ^ ")"
  in
  title_scroller st.win name;

  (* Volume control *)
  (match volume_bar st.win st.volume with
  | Some vol when vol <> st.volume ->
    st.volume <- vol;
    if on then Api.Audio.volume st.audio st.sound vol
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
    let bg = if i mod 2 = 0 then `Black else `Gray 0x18 in
    if bg <> `Black then Api.Draw.fill st.win x y w h bg;
    let color = if i = st.playing then `White else `Green in
    let song = st.playlist.(i) in
    let name =
      if song.artist = "" then song.title else
      song.artist ^ " - " ^ song.title
    in
    let entry = fmt "%0*d. %s" digits (i + 1) name in
    let time = fmt_time song.time in
    let w1 = Api.Draw.text_width st.win h font entry in
    let w2 = Api.Draw.text_width st.win h font time in
    Api.Draw.clip st.win (Some (x, y, w - w2, h));
    Api.Draw.text st.win x y h color font entry;
    if w1 > w - w2 then
      Api.Draw.gradient st.win (x + w - w2 - 16) y 16 h
        (`Trans (bg, 0)) `Horizontal bg;
    Api.Draw.clip st.win None;
    Api.Draw.text st.win (x + w - w2) y h color font time;
  done;

  (* Handle drag & drop *)
  let dropped = Api.File.dropped st.win in
  let songs = Array.map Song.make (Array.of_list dropped) in
  st.playlist <- Array.append st.playlist songs;

  (* Detect end of song *)
  if on && remain < 0.02 then
  (
    Api.Audio.stop st.audio st.sound;
    if st.playing >= Array.length st.playlist - 1 then
      st.playing <- -1
    else
    (
      st.playing <- st.playing + 1;
      st.sound <- Api.Audio.load st.audio st.playlist.(st.playing).path;
      Api.Audio.play st.audio st.sound;
      Api.Audio.volume st.audio st.sound st.volume;
    )
  );

  Api.Draw.finish st.win;
  run st


(* Startup *)

let song = Song.
{
  artist = "Vaal";
  title = "Interference (from the album Vaal - Nosferatu)";
  time = 342.0;
  path = "song.mp3";
}

let startup () =
  let win = Api.Window.init 0 0 control_width 1000 name in

  let audio = Api.Audio.init () in
  let sound = Api.Audio.load audio song.path in
      
  let volume = 0.2 in
  Api.Audio.play audio sound;
  Api.Audio.pause audio sound true;
  Api.Audio.volume audio sound volume;
  State.{win; audio; sound; playing = 0; playlist = [|song|]; volume}


let _main =
  try
    Printexc.record_backtrace true;
    let st = startup () in
    run st
  with exn ->
    prerr_endline ("internal error: " ^ Printexc.to_string exn);
    Printexc.print_backtrace stderr;
    Stdlib.exit 2
