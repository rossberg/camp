(* Program Configuration *)

type time = float
type path = string

type t =
{
  mutable delay_track_update : time;
  mutable exec_tag : path;
  mutable exec_tag_max_len : int;
}


(* Constructor *)

val make : unit -> t


(* Validation *)

type error = string

val ok : t -> error list


(* Persistance *)

val print_state : t -> Struct.t
val print_intern : t -> Struct.t
val parse_state : t -> Struct.t -> unit
