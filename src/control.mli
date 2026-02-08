(* Control State *)

type time = float
type track = Data.track

type visual = [`None | `Cover | `Spectrum | `Wave | `Oscilloscope]

type t =
{
  audio : Api.audio;
  mutable mute : bool;
  mutable volume : float;
  mutable progress : float;
  mutable sound : Api.sound;
  mutable current : track option;
  mutable timemode : [`Elapse | `Remain];
  mutable repeat : [`None | `One | `All | `Marked];
  mutable loop : [`None | `A of time | `AB of time * time];
  mutable visual : visual;
  mutable fps : bool;
  mutable spec_bands : int;
  mutable osc_x : float;
  mutable osc_y : float;
  mutable raw : float array;
  mutable data : float array;
}

(* Constructor *)

val make : Api.audio -> t


(* Validation *)

type error = string

val ok : t -> error list


(* Persistence *)

val print_state : t -> Text.t
val print_intern : t -> Text.t
val parse_state : t -> Text.t -> unit


(* Volume Control *)

val mute : t -> bool -> unit
val volume : t -> float -> unit


(* Visuals *)

val set_visual : t -> visual -> unit

val set_osc : t -> float -> float -> unit
val reset_osc : t -> unit


(* Track Control *)

val pause : t -> unit
val resume : t -> unit
val stop : t -> unit
val play : t -> unit
val eject : t -> unit
val switch : t -> track -> unit
val switch_if_empty : t -> track option -> bool
val seek : t -> float (* fraction *) -> unit

val status : t -> [`Ejected | `Stopped | `Paused | `Playing]
val silent : t -> bool
val length : t -> time
val elapsed : t -> time
val bitrate : t -> float
val rate : t -> int
val depth : t -> float
val channels : t -> int
