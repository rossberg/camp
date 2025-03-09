(* Edit state *)

type undo

type t = private
{
  mutable text : string;
  mutable focus : bool;
  mutable scroll : int;
  mutable sel_range : (int * int) option;  (* primary and secondary pos *)
  undos : undo list ref;
  redos : undo list ref;
  undo_depth : int;
}


(* Constructor *)

val make : int -> t


(* Accessors *)

val focus : t -> unit
val defocus : t -> unit

val scroll : t -> int -> unit
val select : t -> (int * int) option -> unit


(* Editing *)

val set : t -> string -> unit
val insert : t -> int -> string -> unit
val remove : t -> int -> int -> unit
val clear : t -> unit

val move_begin : t -> unit
val move_end : t -> unit


(* Undo *)

val push_undo : t -> unit
val pop_undo : t -> unit
val pop_redo : t -> unit

val drop_undo : t -> unit
val drop_redo : t -> unit

val clear_undo : t -> unit
