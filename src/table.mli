(* Table state *)

module IntSet : module type of Set.Make(Int)

type 'data undo

type ('data, 'cache) t = private
{
  mutex : Mutex.t;
  mutable entries : 'data array;
  mutable pos : int option;                (* current position in table *)
  mutable focus : bool;
  mutable vscroll : int;                   (* in number of rows *)
  mutable hscroll : int;                   (* in pixels *)
  mutable sel_range : (int * int) option;  (* primary and secondary pos *)
  mutable selected : IntSet.t;
  mutable cache : 'cache option;
  mutable dirty : bool;
  undos : 'data undo list ref;
  redos : 'data undo list ref;
  undo_depth : int;
  undo_save : (unit -> unit -> unit) option;
}


(* Constructor *)

val make : ?save : (unit -> unit -> unit) -> int -> ('a, 'b) t


(* Validation *)

type error = string

val ok : string -> ('a, 'b) t -> error list


(* Accessors *)

val length : ('a, 'b) t -> int
val current : ('a, 'b) t -> 'a
val current_opt : ('a, 'b) t -> 'a option

val set_pos : ('a, 'b) t -> int option -> unit
val set_hscroll : ('a, 'b) t -> int -> unit
val set_vscroll : ('a, 'b) t -> int -> int -> unit
val adjust_vscroll : ('a, 'b) t -> int -> int -> unit

val focus : ('a, 'b) t -> unit
val defocus : ('a, 'b) t -> unit

val dirty : ('a, 'b) t -> unit
val clean : ('a, 'b) t -> unit
val cache : ('a, 'b) t -> 'b -> unit
val uncache : ('a, 'b) t -> unit


(* Selection *)

val has_selection : ('a, 'b) t -> bool
val num_selected : ('a, 'b) t -> int
val first_selected : ('a, 'b) t -> int option
val last_selected : ('a, 'b) t -> int option
val is_selected : ('a, 'b) t -> int -> bool
val reset_selected : ('a, 'b) t -> IntSet.t -> unit  (* unchecked! *)
val selected : ('a, 'b) t -> 'a array

val select_all : ('a, 'b) t -> unit
val deselect_all : ('a, 'b) t -> unit
val select_invert : ('a, 'b) t -> unit

val select : ('a, 'b) t -> int -> int -> unit
val deselect : ('a, 'b) t -> int -> int -> unit

val save_selection : ('a, 'b) t -> 'a array
val restore_selection : ('a, 'b) t -> 'a array -> ('a -> string) -> unit


(* Editing *)

val set : ('a, 'b) t -> 'a array -> unit
val insert : ('a, 'b) t -> int -> 'a array -> unit

val remove_all : ('a, 'b) t -> unit
val remove_if : (int -> bool) -> ('a, 'b) t -> int -> int array

val move_selected : ('a, 'b) t -> int -> int array
val reverse_selected : ('a, 'b) t -> int array
val reverse_all : ('a, 'b) t -> unit


(* Undo *)

val push_undo : ('a, 'b) t -> unit
val pop_undo : ('a, 'b) t -> unit
val pop_redo : ('a, 'b) t -> unit

val drop_undo : ('a, 'b) t -> unit
val drop_redo : ('a, 'b) t -> unit

val clean_undo : ('a, 'b) t -> unit
val clear_undo : ('a, 'b) t -> unit
