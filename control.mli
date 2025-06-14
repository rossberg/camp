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

val make : Api.audio -> t


(* Validation *)

type error = string

val ok : t -> error list


(* Persistance *)

val print_state : t -> Struct.t
val print_intern : t -> Struct.t
val parse_state : t -> Struct.t -> unit


(* Track Control *)

val eject : t -> unit
val switch : t -> track -> bool (* play *) -> unit
val switch_if_empty : t -> track option -> unit
val seek : t -> float (* fraction *) -> unit
