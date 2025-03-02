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

val make : Api.audio -> t


(* Validation *)

type error = string

val ok : t -> error list


(* Persistance *)

val to_map : t -> Storage.map
val of_map : t -> Storage.map -> unit

val to_map_extra : t -> Storage.map


(* Track Control *)

val eject : t -> unit
val switch : t -> track -> bool (* play *) -> unit
val switch_if_empty : t -> track option -> unit
val seek : t -> float (* fraction *) -> unit
