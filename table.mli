(* Table state *)

module IntSet : module type of Set.Make(Int)

type 'a undo

type 'a t = private
{
  mutex : Mutex.t;
  mutable entries : 'a array;
  mutable pos : int option;                (* current position in table *)
  mutable focus : bool;
  mutable vscroll : int;                   (* in number of rows *)
  mutable hscroll : int;                   (* in pixels *)
  mutable sel_range : (int * int) option;  (* primary and secondary pos *)
  mutable selected : IntSet.t;
  undos : 'a undo list ref;
  redos : 'a undo list ref;
  undo_depth : int;
  undo_save : (unit -> unit -> unit) option;
}


(* Constructor *)

val make : ?save : (unit -> unit -> unit) -> int -> 'a t


(* Validation *)

type error = string

val ok : string -> 'a t -> error list


(* Accessors *)

val length : 'a t -> int
val current : 'a t -> 'a
val current_opt : 'a t -> 'a option

val set_pos : 'a t -> int option -> unit
val set_hscroll : 'a t -> int -> unit
val set_vscroll : 'a t -> int -> int -> unit
val adjust_vscroll : 'a t -> int -> int -> unit

val focus : 'a t -> unit
val defocus : 'a t -> unit


(* Selection *)

val has_selection : 'a t -> bool
val num_selected : 'a t -> int
val first_selected : 'a t -> int option
val last_selected : 'a t -> int option
val is_selected : 'a t -> int -> bool
val reset_selected : 'a t -> IntSet.t -> unit  (* unchecked! *)
val selected : 'a t -> 'a array

val select_all : 'a t -> unit
val deselect_all : 'a t -> unit
val select_invert : 'a t -> unit

val select : 'a t -> int -> int -> unit
val deselect : 'a t -> int -> int -> unit

val save_selection : 'a t -> 'a array
val restore_selection : 'a t -> 'a array -> ('a -> string) -> unit


(* Editing *)

val set : 'a t -> 'a array -> unit
val insert : 'a t -> int -> 'a array -> unit

val remove_all : 'a t -> unit
val remove_if : (int -> bool) -> 'a t -> int -> int array

val move_selected : 'a t -> int -> int array


(* Undo *)

val push_undo : 'a t -> unit
val pop_undo : 'a t -> unit
val pop_redo : 'a t -> unit

val drop_undo : 'a t -> unit
val drop_redo : 'a t -> unit

val clear_undo : 'a t -> unit
