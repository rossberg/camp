(* Program Configuration *)

type time = float
type path = string

type t =
{
  mutable row_height : int;
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

val to_string : t -> string

val load : t -> in_channel -> unit
val save : t -> out_channel -> unit
