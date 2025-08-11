(* Pop-uup Menu *)

type 'op t = private
{
  mutable pos : int * int;
  mutable op : 'op option;
  mutable items : Ui.menu_entry array;
}


(* Constructor *)

val make : unit -> 'a t

val set : 'a t -> int * int -> 'a -> Ui.menu_entry array -> unit
val clear : 'a t -> unit


(* Validation *)

type error = string

val ok : 'a t -> error list


(* Persistence *)

val print_state : 'a t -> Text.t
val print_intern : 'a t -> Text.t
val parse_state : 'a t -> Text.t -> unit
