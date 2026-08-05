(* Settings *)

type t =
{
  mutable vscroll : int;
}


(* Constructor *)

val make : unit -> t


(* Validation *)

type error = string

val ok : t -> error list


(* Focus *)

val defocus : t -> unit


(* Persistence *)

val print_state : t -> Text.t
val print_intern : t -> Text.t
val parse_state : t -> Text.t -> unit
