(* Control State *)

type time = float
type track = Data.track

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
  mutable cover : bool;
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
    cover = true;
    fps = false;
  }


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]

let ok ctl =
  let length = Api.Audio.length ctl.audio in
  let played = Api.Audio.played ctl.audio in
  let playing = Api.Audio.is_playing ctl.audio in
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
  Api.Audio.stop ctl.audio;
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
(*
  track.time <-
    if ctl.sound = Api.Audio.silence ctl.audio then 0.0
    else Api.Audio.length ctl.audio ctl.sound;
*)
  Track.update track;
  Api.Audio.play ctl.audio ctl.sound;
  Api.Audio.volume ctl.audio (if ctl.mute then 0.0 else ctl.volume);
  if not play then Api.Audio.pause ctl.audio

let seek ctl percent =
  if ctl.sound <> Api.Audio.silence ctl.audio then
  (
    let length = Api.Audio.length ctl.audio in
    Api.Audio.seek ctl.audio (percent *. length)
  )

let switch_if_empty ctl track_opt =
  match ctl.current, track_opt with
  | None, Some track -> switch ctl track false
  | _, _ -> ()


(* Persistance *)

open Storage
let fmt = Printf.sprintf
let scan = Scanf.sscanf

let value = Fun.id
let bool x = x <> 0
let num l h x = max l (min h x)
let pair x y = x, y


let to_map ctl =
  let length = Api.Audio.length ctl.audio in
  let played = Api.Audio.played ctl.audio in
  Map.of_list
  [
    "volume", fmt "%.2f" ctl.volume;
    "mute", fmt "%d" (Bool.to_int ctl.mute);
    "play", fmt "%s" (match ctl.current with Some s -> s.path | None -> "");
    "seek", fmt "%.4f" (if length > 0.0 then played /. length else 0.0);
    "timemode", fmt "%d" (Bool.to_int (ctl.timemode = `Remain));
    "info_cover", fmt "%d" (Bool.to_int ctl.cover);
    "repeat", fmt "%d"
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
    "loop", fmt "%.4f, %.4f" a b;
  ]

let to_map_extra ctl =
  Map.of_list
  [
    "fps", fmt "%d" (Bool.to_int ctl.fps);
  ]


let of_map ctl m =
  read_map m "volume" (fun s -> ctl.volume <- scan s "%f" (num 0.0 1.0));
  read_map m "mute" (fun s -> ctl.mute <- scan s "%d" bool);
  read_map m "play" (fun s ->
    if s <> "" then switch ctl (Data.make_track s) false
  );
  read_map m "seek" (fun s -> seek ctl (scan s "%f" (num 0.0 1.0)));
  read_map m "repeat" (fun s ->
    ctl.repeat <-
      (match scan s "%d" value with
      | 1 -> `One
      | 2 -> `All
      | _ -> `None
      )
  );
  read_map m "loop" (fun s ->
    ctl.loop <-
      (match scan s "%f, %f" pair with
      | t1, _t2 when t1 < 0.0 -> `None
      | t1, t2 when t2 < 0.0 -> `A t1
      | t1, t2 -> `AB (t1, max t1 t2)
      )
  );
  read_map m "timemode" (fun s ->
    ctl.timemode <- if scan s "%d" bool then `Remain else `Elapse);
  read_map m "info_cover" (fun s -> ctl.cover <- scan s "%d" bool)
