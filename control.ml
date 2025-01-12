(* Control State *)

type time = float
type track = Track.t

type t =
{
  audio : Api.audio;
  mutable mute : bool;
  mutable volume : float;
  mutable sound : Api.sound;
  mutable current : track option;
  mutable timemode : [`Elapse | `Remain];
  mutable repeat : [`None | `One | `All];
  mutable loop : [`None | `A of time | `AB of time * time];
  mutable fps : bool;
}


(* Constructor *)

let make audio =
  {
    audio;
    mute = false;
    volume = 0.5;
    sound = Api.Audio.silence audio;
    current = None;
    timemode = `Elapse;
    repeat = `None;
    loop = `None;
    fps = false;
  }


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]

let ok ctl =
  let length = Api.Audio.length ctl.audio ctl.sound in
  let played = Api.Audio.played ctl.audio ctl.sound in
  let playing = Api.Audio.is_playing ctl.audio ctl.sound in
  let paused = not playing && played > 0.0 in
  let stopped = not playing && not paused in
  let silence = ctl.sound = Api.Audio.silence ctl.audio in
  check "volume in range" (ctl.volume >= 0.0 && ctl.volume <= 1.0) @
  check "silence when no current track" (ctl.current <> None || silence) @
  check "stopped when no current track" (ctl.current <> None || stopped) @
  check "no loop when no current track"
    (ctl.current <> None || ctl.loop = `None) @
  check "lower loop boundary in range"
    (match ctl.loop with
    | `A t1
    | `AB (t1, _) -> t1 >= 0.0 && t1 <= length
    | _ -> true
    ) @
  check "upper loop boundary in range"
    (match ctl.loop with
    | `AB (t1, t2) -> t1 <= t2 && t2 <= length
    | _ -> true
    )


(* Track Control *)

let eject ctl =
  Api.Audio.stop ctl.audio ctl.sound;
  ctl.current <- None;
  ctl.loop <- `None;
  if ctl.sound <> Api.Audio.silence ctl.audio then
  (
    Api.Audio.free ctl.audio ctl.sound;
    ctl.sound <- Api.Audio.silence ctl.audio;
  )

let switch ctl (track : track) play =
  eject ctl;
  ctl.sound <- Api.Audio.load ctl.audio track.path;
  ctl.current <- Some track;
  ctl.loop <- `None;
  track.time <-
    if ctl.sound = Api.Audio.silence ctl.audio then 0.0
    else Api.Audio.length ctl.audio ctl.sound;
  Track.update ctl.audio track;
  Api.Audio.volume ctl.audio ctl.sound (if ctl.mute then 0.0 else ctl.volume);
  Api.Audio.play ctl.audio ctl.sound;
  if not play then Api.Audio.pause ctl.audio ctl.sound

let seek ctl percent =
  if ctl.sound <> Api.Audio.silence ctl.audio then
  (
    let length = Api.Audio.length ctl.audio ctl.sound in
    Api.Audio.seek ctl.audio ctl.sound (percent *. length)
  )

let switch_if_empty ctl track_opt =
  match ctl.current, track_opt with
  | None, Some track -> switch ctl track false
  | _, _ -> ()


(* Persistance *)

let to_string' ctl =
  let buf = Buffer.create 1024 in
  let output fmt  = Printf.bprintf buf fmt in
  output "volume = %.2f\n" ctl.volume;
  output "mute = %d\n" (Bool.to_int ctl.mute);
  output "play = %s\n" (match ctl.current with Some s -> s.path | None -> "");
  let length = Api.Audio.length ctl.audio ctl.sound in
  let played = Api.Audio.played ctl.audio ctl.sound in
  output "seek = %.4f\n" (if length > 0.0 then played /. length else 0.0);
  output "timemode = %d\n" (Bool.to_int (ctl.timemode = `Remain));
  output "repeat = %d\n"
    (match ctl.repeat with
    | `None -> 0
    | `One -> 1
    | `All -> 2
    );
  let a, b =
    match ctl.loop with
    | `None -> -1.0, -1.0
    | `A t1 -> t1, -1.0
    | `AB tt -> tt
  in
  output "loop = %.4f, %.4f\n" a b;
  Buffer.contents buf

let to_string ctl =
  to_string' ctl ^
  let buf = Buffer.create 1024 in
  let output fmt = Printf.bprintf buf fmt in
  output "fps = %d\n" (Bool.to_int ctl.fps);
  Buffer.contents buf


let save ctl file =
  Out_channel.output_string file (to_string' ctl)


let fscanf file =
  match In_channel.input_line file with
  | None -> raise End_of_file
  | Some s -> Scanf.sscanf s

let value = Fun.id
let bool x = x <> 0
let num l h x = max l (min h x)
let pair x y = x, y

let load ctl file =
  let input fmt = fscanf file fmt in
  ctl.volume <- input " volume = %f " (num 0.0 1.0);
  ctl.mute <- input " mute = %d " bool;
  let current = input " play = %[\x20-\xff]" String.trim in
  ctl.current <-
    if current = "" then None else Some (Track.make current);
  let seek' = input " seek = %f " (num 0.0 1.0) in
  ctl.timemode <-
    if input " timemode = %d " bool then `Remain else `Elapse;
  ctl.repeat <-
    (match input " repeat = %d " value with
    | 1 -> `One
    | 2 -> `All
    | _ -> `None
    );
  ctl.loop <-
    (match input " loop = %f, %f " pair with
    | t1, _t2 when t1 < 0.0 -> `None
    | t1, t2 when t2 < 0.0 -> `A t1
    | t1, t2 -> `AB (t1, max t1 t2)
    );
  if ctl.current <> None then switch ctl (Option.get ctl.current) false;
  seek ctl seek'
