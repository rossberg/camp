(* Control State *)

type time = float
type track = Data.track

type visual = [`None | `Cover | `Wave | `Oscilloscope]

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
  mutable visual : visual;
  mutable fps : bool;
  mutable osc_x : float;
  mutable osc_y : float;
  mutable data : float array;
}


(* Constructor *)

let osc_x = 0.5
let osc_y = 1.5

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
    visual = `Cover;
    fps = false;
    osc_x; osc_y;
    data = [|0.0|];
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
  let silence = (ctl.sound = Api.Audio.silence ctl.audio) in
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


(* Volume Control *)

let adjust_volume ctl =
  Api.Audio.volume ctl.audio (if ctl.mute then 0.0 else ctl.volume)

let mute ctl b =
  ctl.mute <- b;
  adjust_volume ctl

let volume ctl v =
  ctl.volume <- max 0.0 (min 1.0 v);
  adjust_volume ctl

let clamp lo hi x = max lo (min hi x)


(* Visuals *)

let audio_processor ctl fs = ctl.data <- fs

let needs_processor = function
  | `None | `Cover -> false
  | `Wave | `Oscilloscope -> true

let set_visual ctl vis =
  let old_need = needs_processor ctl.visual in
  let new_need = needs_processor vis in
  ctl.visual <- vis;
  if old_need <> new_need then
  (
    if new_need then
      Api.Audio.add_processor ctl.audio (audio_processor ctl)
    else
      Api.Audio.remove_all_processors ctl.audio
  )

let set_osc ctl x y =
  ctl.osc_x <- clamp 0.2 10.0 x;
  ctl.osc_y <- clamp 0.2 10.0 y

let reset_osc ctl =
  ctl.osc_x <- osc_x;
  ctl.osc_y <- osc_y


(* Track Control *)

let silent ctl = (ctl.sound = Api.Audio.silence ctl.audio)
let length ctl = if silent ctl then 0.0 else Api.Audio.length ctl.audio
let elapsed ctl = if silent ctl then 0.0 else Api.Audio.played ctl.audio
let bitrate ctl = Api.Audio.bitrate ctl.audio ctl.sound
let rate ctl = Api.Audio.rate ctl.audio ctl.sound
let channels ctl = Api.Audio.channels ctl.audio ctl.sound

let status ctl =
  if ctl.current = None then
    `Ejected
  else if Api.Audio.is_playing ctl.audio then
    `Playing
  else if Api.Audio.played ctl.audio > 0.0 then
    `Paused
  else
    `Stopped


let pause ctl = Api.Audio.pause ctl.audio
let resume ctl = Api.Audio.resume ctl.audio

let play ctl =
  let track = Option.get ctl.current in
  try
    ctl.sound <- Api.Audio.load ctl.audio track.path;
    Api.Audio.play ctl.audio ctl.sound;
  with Sys_error _ -> ()

let stop ctl =
  Api.Audio.stop ctl.audio;
  ctl.progress <- 0.0;
  if ctl.sound <> Api.Audio.silence ctl.audio then
  (
    Api.Audio.free ctl.audio ctl.sound;
    ctl.sound <- Api.Audio.silence ctl.audio;
  )

let eject ctl =
  stop ctl;
  ctl.current <- None;
  ctl.loop <- `None

let switch ctl (track : track) =
  eject ctl;
  ctl.current <- Some track;
(*
  track.time <-
    if ctl.sound = Api.Audio.silence ctl.audio then 0.0
    else Api.Audio.length ctl.audio ctl.sound;
*)
  Track.update track

let seek ctl percent =
  if silent ctl && ctl.current <> None then
  (
    play ctl;
    pause ctl;
  );
  if not (silent ctl) then
  (
    let percent' = max 0.0 (min 1.0 percent) in
    let length = Api.Audio.length ctl.audio in
    Api.Audio.seek ctl.audio (percent' *. length);
    ctl.progress <- percent';
  )

let switch_if_empty ctl track_opt =
  match ctl.current, track_opt with
  | None, Some track -> switch ctl track; true
  | _, _ -> false


(* Persistence *)

let timemode_enum = ["elapsed", `Elapse; "remain", `Remain]
let repeat_enum = ["none", `None; "one", `One; "all", `All]
let visual_enum =
  ["none", `None; "cover", `Cover; "wave", `Wave; "oscilloscope", `Oscilloscope]

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
    "visual", enum visual_enum ctl.visual;
    "osc_x", float ctl.osc_x;
    "osc_y", float ctl.osc_y;
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
      (Option.iter (fun s -> switch ctl (Data.make_track s)));
    apply (r $? "seek") (interval 0.0 1.0)
      (fun q -> seek ctl q);
    apply (r $? "repeat") (enum repeat_enum)
      (fun r -> ctl.repeat <- r);
    apply (r $? "loop") parse_loop
      (fun l -> ctl.loop <- l);
    apply (r $? "timemode") (enum timemode_enum)
      (fun m -> ctl.timemode <- m);
    apply (r $? "visual") (enum visual_enum)
      (fun v -> set_visual ctl v);
    apply (r $? "osc_x") float
      (fun x -> set_osc ctl x ctl.osc_y);
    apply (r $? "osc_y") float
      (fun y -> set_osc ctl ctl.osc_x y);
  )
