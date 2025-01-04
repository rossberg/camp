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

val make : Api.audio -> t


(* Validation *)

type error = string

val ok : t -> error list


(* Track Control *)

val eject : t -> unit
val switch : t -> track -> bool (* play *) -> unit
val switch_if_empty : t -> track option -> unit
val seek : t -> float (* fraction *) -> unit
