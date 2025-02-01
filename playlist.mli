(* Playlist *)

type time = Track.time
type track = Track.t

type shuffle

type t =
{
  table : track Table.t;
  mutable total : time * int;
  mutable total_selected : time * int;
  mutable shuffle : shuffle option;
}


(* Constructor *)

val make : unit -> t


(* Validation *)

type error = string

val ok : t -> error list


(* Persistance *)

val to_map : t -> Storage.map
val of_map : t -> Storage.map -> unit  (* assumes roots already set *)

val to_map_extra : t -> Storage.map

val load_playlist : t -> unit
val save_playlist : t -> unit

val string_of_playlist : track array -> string
val playlist_of_string : string -> track array


(* Accessors *)

val length : t -> int
val current : t -> track
val current_opt : t -> track option

val focus : t -> bool -> unit
val adjust_scroll : t -> int -> unit


(* Total *)

val update_total : t -> unit
val update_total_selected : t -> unit


(* Navigation *)

val skip : t -> int (* delta *) -> bool (* repeat *) -> bool

val swap : 'a array -> int -> int -> unit


(* Shuffle *)

val shuffle : t -> int option (* first track *) -> unit
val unshuffle : t -> unit

val shuffle_next : t -> int -> unit


(* Selection *)

val has_selection : t -> bool
val num_selected : t -> int
val first_selected : t -> int option
val last_selected : t -> int option
val is_selected : t -> int -> bool
val selected : t -> track array

val select_all : t -> unit
val deselect_all : t -> unit
val select_invert : t -> unit

val select : t -> int -> int -> unit
val deselect : t -> int -> int -> unit


(* Editing *)

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
