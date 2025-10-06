(* Log state *)

type entry = Ui.color * Ui.cell array

type 'cache t =
{
  table : (entry, 'cache) Table.t;
  columns : Ui.column array;
  mutable heading : Ui.heading option;
  mutable info : string;
  mutable cancel : bool;
  mutable completed : bool;
  mutable on_completion : 'cache t -> unit;
  mutable on_menu : 'cache t -> int option * int option -> unit;
}


(* Constructor *)

val make : Ui.heading option -> Ui.column array ->
  ('a t -> unit) -> ('a t -> int option * int option -> unit) -> 'a t


(* Manipulation *)

val length : 'a t -> int
val append : 'a t -> entry array -> unit
val insert : 'a t -> int -> entry array -> unit

val text : 'a t -> int -> int -> string

val complete : 'a t -> unit


(* Validation *)

type error = string

val ok : 'a t -> error list
