(* Edit state *)

type undo

type t =
{
  mutable text : string;
  mutable focus : bool;
  mutable scroll : int;
  mutable sel_range : (int * int) option;  (* primary and secondary pos *)
  mutable undos : undo list ref;
  mutable redos : undo list ref;
  mutable undo_depth : int;
}


(* Constructor *)

val make : int -> t


(* Editing *)

val set : t -> string -> unit
val insert : t -> int -> string -> unit
val remove : t -> int -> int -> unit
val clear : t -> unit


(* Undo *)

val push_undo : t -> unit
val pop_undo : t -> unit
val pop_redo : t -> unit

val drop_undo : t -> unit
val drop_redo : t -> unit

val clear_undo : t -> unit
