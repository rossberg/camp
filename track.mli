(* Tracks *)

type path = string
type time = float

type t =
{
  path : path;
  mutable name : string;
  mutable time : time;
  mutable status : [`Undet | `Predet | `Det | `Invalid | `Absent];
  mutable last_update : time;
}


(* Constructors *)

val make : path -> t
val make_predet : path -> string -> time -> t
val make_separator : unit -> t


(* Properties *)

val is_separator : t -> bool
val is_invalid : t -> bool


(* Updating queue *)

val update : Api.audio -> t -> unit
