(* Playlist *)

module IntSet : module type of Set.Make(Int)

type time = Track.time
type track = Track.t

type undo
type shuffle

type t =
{
  mutable tracks : track array;  (* external *)
  mutable shown : bool;  (* external *)
  mutable height : int;  (* external *)
  mutable rows : int;  (* external *)
  mutable scroll : int;  (* external *)
  mutable pos : int option;  (* external *)
  mutable sel_range : (int * int) option;  (* external *)
  mutable selected : IntSet.t;  (* r external *)
  mutable total : time * int;  (* r external *)
  mutable total_selected : time * int;  (* r external *)
  mutable shuffle : shuffle option;  (* r external *)
  mutable undos : undo list ref;
  mutable redos : undo list ref;
}


(* Constructor *)

val make : unit -> t


(* Validation *)

type error = string

val ok : t -> error list


(* Total *)

val update_total : t -> unit


(* Navigation *)

val current : t -> track
val current_opt : t -> track option

val skip : t -> int (* delta *) -> bool (* repeat *) -> bool

val adjust_scroll : t -> int option -> unit

val swap : 'a array -> int -> int -> unit


(* Shuffle *)

val shuffle : t -> int option (* first track *) -> unit
val unshuffle : t -> unit

val shuffle_next : t -> int -> unit


(* Selection *)

val num_selected : t -> int
val first_selected : t -> int option
val last_selected : t -> int option
val is_selected : t -> int -> bool

val select_all : t -> unit
val deselect_all : t -> unit
val select_invert : t -> unit

val select : t -> int -> int -> unit
val deselect : t -> int -> int -> unit


(* Editing *)

val copy_selected : t -> track array

val insert : t -> int -> track array -> unit
val insert_paths : t -> int -> Track.path list -> Api.audio -> unit

val remove_all : t -> unit
val remove_selected : t -> unit
val remove_unselected : t -> unit
val remove_invalid : t -> unit

val move_selected : t -> int -> unit


(* Undo *)

val pop_undo : t -> unit
val pop_redo : t -> unit
