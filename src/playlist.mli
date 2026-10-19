(* Playlist *)

type path = Data.path
type time = Data.time
type track = Data.track
type track_attr_ex = Data.track_attr_ex

type shuffle

type t = private
{
  table : (track, Cache.t) Table.t;
  mutable total : time * int;
  mutable total_selected : time * int;
  mutable shuffle : shuffle option;
  view : (track, track_attr_ex) Library.view;
}


(* Constructor *)

val make : unit -> t


(* Validation *)

type error = string

val ok : t -> error list


(* Persistence *)

val print_state : t -> Text.t
val print_intern : t -> Text.t
val parse_state : t -> Text.t -> unit  (* assumes roots already set *)

val load_playlist : t -> unit
val save_playlist : t -> unit


(* Accessors *)

val length : t -> int
val tracks : t -> track array
val table : t -> (track, Cache.t) Table.t

val current : t -> track
val current_opt : t -> track option

val focus : t -> unit
val defocus : t -> unit
val adjust_scroll : t -> int -> unit


(* Total *)

val refresh_total : t -> unit
val refresh_total_selected : t -> unit


(* Navigation *)

val jump : t -> int -> unit
val skip : t -> int (* delta *) -> [`None | `One | `All | `Marked] -> bool

val swap : 'a array -> int -> int -> unit


(* Shuffle *)

val shuffle : t -> int option (* first track *) -> unit
val unshuffle : t -> unit


(* Selection *)

val has_selection : t -> bool
val num_selected : t -> int
val first_selected : t -> int option
val last_selected : t -> int option
val is_selected : t -> int -> bool
val selected : t -> track array

val select : t -> int -> int -> unit
val deselect : t -> int -> int -> unit
val select_all : t -> unit
val deselect_all : t -> unit
val select_marked : t -> unit
val select_invert : t -> unit


(* Editing *)

val insert : t -> int -> track array -> unit
val append : t -> track array -> unit
val replace_map : t -> track Map.Make(String).t -> bool -> unit
val replace_all : t -> track array -> unit

val remove_all : t -> unit
val remove_selected : t -> unit
val remove_unselected : t -> unit
val remove_invalid : t -> bool -> unit
val remove_duplicates : t -> bool -> unit

val move_selected : t -> int -> unit
val reverse_selected : t -> unit
val reverse_all : t -> unit
val reorder_all : t -> unit

val undo : t -> unit
val redo : t -> unit
