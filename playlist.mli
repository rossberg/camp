(* Playlist *)

type path = Data.path
type time = Data.time
type track = Data.track

type shuffle

type 'cache t =
{
  table : (track, 'cache) Table.t;
  mutable total : time * int;
  mutable total_selected : time * int;
  mutable shuffle : shuffle option;
}


(* Constructor *)

val make : unit -> 'a t


(* Validation *)

type error = string

val ok : 'a t -> error list


(* Persistance *)

val to_map : 'a t -> Storage.map
val of_map : 'a t -> Storage.map -> unit  (* assumes roots already set *)

val to_map_extra : 'a t -> Storage.map

val load_playlist : 'a t -> unit
val save_playlist : 'a t -> unit


(* Accessors *)

val length : 'a t -> int
val tracks : 'a t -> track array
val table : 'a t -> (track, 'a) Table.t

val current : 'a t -> track
val current_opt : 'a t -> track option

val focus : 'a t -> unit
val defocus : 'a t -> unit
val adjust_scroll : 'a t -> int -> unit


(* Total *)

val refresh_total : 'a t -> unit
val refresh_total_selected : 'a t -> unit


(* Navigation *)

val skip : 'a t -> int (* delta *) -> bool (* repeat *) -> bool

val swap : 'a array -> int -> int -> unit


(* Shuffle *)

val shuffle : 'a t -> int option (* first track *) -> unit
val unshuffle : 'a t -> unit

val shuffle_next : 'a t -> int -> unit


(* Selection *)

val has_selection : 'a t -> bool
val num_selected : 'a t -> int
val first_selected : 'a t -> int option
val last_selected : 'a t -> int option
val is_selected : 'a t -> int -> bool
val selected : 'a t -> track array

val select_all : 'a t -> unit
val deselect_all : 'a t -> unit
val select_invert : 'a t -> unit

val select : 'a t -> int -> int -> unit
val deselect : 'a t -> int -> int -> unit


(* Editing *)

val insert : 'a t -> int -> track array -> unit
val replace_all : 'a t -> track array -> unit

val remove_all : 'a t -> unit
val remove_selected : 'a t -> unit
val remove_unselected : 'a t -> unit
val remove_invalid : 'a t -> unit

val move_selected : 'a t -> int -> unit

val undo : 'a t -> unit
val redo : 'a t -> unit


(* Undo *)

val pop_undo : 'a t -> unit
val pop_redo : 'a t -> unit
