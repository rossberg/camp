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
