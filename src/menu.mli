(* Pop-uup Menu *)

type op = int -> unit

type t = private
{
  mutable pos : int * int;
  mutable op : op option;
  mutable items : Ui.menu_entry iarray;
}


(* Constructor *)

val make : unit -> t

val set : t -> int * int -> op -> Ui.menu_entry iarray -> unit
val clear : t -> unit


(* Validation *)

type error = string

val ok : t -> error list


(* Persistence *)

val print_state : t -> Text.t
val print_intern : t -> Text.t
val parse_state : t -> Text.t -> unit
