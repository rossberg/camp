(* Program Configuration *)

type path = string

type t =
{
  mutable exec_tag : path;
  mutable exec_tag_max_len : int;
}


(* Constructor *)

val make : unit -> t


(* Validation *)

type error = string

val ok : t -> error list
