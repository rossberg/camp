(* Table state *)

module IntSet : module type of Set.Make(Int)

type 'a undo

type 'a t =
{
  mutable entries : 'a array;
  mutable pos : int option;
  mutable fit : int;       (* number of rows currently fitting view *)
  mutable scroll_v : int;  (* in number of rows *)
  mutable scroll_h : int;  (* in pixels *)
  mutable sel_range : (int * int) option;  (* primary and secondary pos *)
  mutable selected : IntSet.t;
  mutable undos : 'a undo list ref;
  mutable redos : 'a undo list ref;
  mutable undo_depth : int;
}


(* Constructor *)

val make : unit -> 'a t


(* Validation *)

type error = string

val ok : string -> 'a t -> error list


(* Accessors *)

val current : 'a t -> 'a
val current_opt : 'a t -> 'a option

val adjust_pos : 'a t -> unit
val adjust_scroll : 'a t -> int option -> unit


(* Selection *)

val num_selected : 'a t -> int
val first_selected : 'a t -> int option
val last_selected : 'a t -> int option
val is_selected : 'a t -> int -> bool

val select_all : 'a t -> unit
val deselect_all : 'a t -> unit
val select_invert : 'a t -> unit

val select : 'a t -> int -> int -> unit
val deselect : 'a t -> int -> int -> unit


(* Editing *)

val insert : 'a t -> int -> 'a array -> unit

val remove_all : 'a t -> unit
val remove_if : (int -> bool) -> 'a t -> int -> int array

val move_selected : 'a t -> int -> int array


(* Undo *)

val pop_undo : 'a t -> unit
val pop_redo : 'a t -> unit
