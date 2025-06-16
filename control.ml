(* Control State *)

type time = float
type track = Data.track

type t =
{
  audio : Api.audio;
  mutable mute : bool;
  mutable volume : float;
  mutable progress : float;
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
    progress = 0.0;
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


(* Persistence *)

let timemode_enum = ["elapsed", `Elapse; "remain", `Remain]
let repeat_enum = ["none", `None; "one", `One; "all", `All]

let print_loop =
  let open Text.Print in
  variant (function
    | `None -> "none", unit ()
    | `A t -> "a", float t
    | `AB tt -> "ab", pair float float tt
  )

let parse_loop =
  let open Text.Parse in
  variant (function
    | "none", t -> unit t; `None
    | "a", t -> `A (float t)
    | "ab", t ->
      `AB ((pair float float >-> fun (t1, t2) -> t1, max t1 t2) t)
    | _ -> raise Text.Type_error
  )

let print_state ctl =
  let open Text.Print in
  let length = Api.Audio.length ctl.audio in
  let played = Api.Audio.played ctl.audio in
  record (fun ctl -> [
    "volume", float ctl.volume;
    "mute", bool ctl.mute;
    "play", option string (Option.map (fun (t : track) -> t.path) ctl.current);
    "seek", float (if length > 0.0 then played /. length else 0.0);
    "repeat", enum repeat_enum ctl.repeat;
    "loop", print_loop ctl.loop;
    "timemode", enum timemode_enum ctl.timemode;
    "cover", bool ctl.cover;
  ]) ctl

let print_intern ctl =
  let open Text.Print in
  print_state ctl @@@
  record (fun ctl -> [
    "fps", bool ctl.fps;
  ]) ctl

let parse_state ctl =
  let open Text.Parse in
  record (fun r ->
    apply (r $? "volume") (interval 0.0 1.0)
      (fun q -> ctl.volume <- q);
    apply (r $? "mute") bool
      (fun b -> ctl.mute <- b);
    apply (r $? "play") (option string)
      (Option.iter (fun s -> switch ctl (Data.make_track s) false));
    apply (r $? "seek") (interval 0.0 1.0)
      (fun q -> seek ctl q);
    apply (r $? "repeat") (enum repeat_enum)
      (fun r -> ctl.repeat <- r);
    apply (r $? "loop") parse_loop
      (fun l -> ctl.loop <- l);
    apply (r $? "timemode") (enum timemode_enum)
      (fun m -> ctl.timemode <- m);
    apply (r $? "cover") bool
      (fun b -> ctl.cover <- b)
  )
