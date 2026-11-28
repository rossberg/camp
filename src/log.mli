(* Log state *)

type entry = Ui.color * Ui.cell iarray

type t =
{
  table : (entry, Cache.t) Table.t;
  mutable columns : Ui.column iarray;
  mutable heading : Ui.heading option;
  mutable info : string;
  mutable cancel : bool;
  mutable completed : bool;
  mutable sortable : bool;
  mutable on_completion : t -> unit;
  mutable on_click : t -> int -> unit;
  mutable on_menu : t -> int option * int option -> unit;
}


(* Constructor *)

val make : Ui.heading option -> Ui.column iarray ->
  (t -> unit) -> (t -> int -> unit) ->
  (t -> int option * int option -> unit) -> t


(* Manipulation *)

val length : t -> int
val append : t -> entry array -> unit
val insert : t -> int -> entry array -> unit
val update : t -> int -> entry -> unit
val remove : t -> int -> int -> unit

val text : t -> int -> int -> string

val complete : t -> unit


(* Validation *)

type error = string

val ok : t -> error list
