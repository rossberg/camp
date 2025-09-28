(* Log state *)

type 'cache t =
{
  table : (Ui.color * Ui.cell array, 'cache) Table.t;
  columns : Ui.column array;
  mutable heading : Ui.heading option;
  mutable info : string;
  mutable completed : bool;
  mutable on_completion : 'cache t -> [`Ok | `Cancel] -> unit;
}


(* Constructor *)

val make : Ui.heading option -> Ui.column array ->
  ('a t -> [`Ok | `Cancel] -> unit) -> 'a t

val add : 'a t -> (Ui.color * Ui.cell array) array -> unit


(* Validation *)

type error = string

val ok : 'a t -> error list
