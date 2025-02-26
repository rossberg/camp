(* Library *)

open Data
type db = Db.t
type scan

type t =
{
  db : db;
  scan : scan;
  mutable root : dir;
  mutable current : dir option;
  mutable browser : dir Table.t;
  mutable artists : artist Table.t;
  mutable albums : album Table.t;
  mutable tracks : track Table.t;
  mutable search : Edit.t;
  mutable error : string;
  mutable error_time : time;
}


(* Constructor *)

val make : db -> t


(* Validation *)

type error = string

val ok : t -> error list


(* Error Message *)

val error : t -> string -> unit


(* Persistance *)

val to_map : t -> Storage.map
val of_map : t -> Storage.map -> unit  (* assumes roots already set *)

val to_map_extra : t -> Storage.map


(* Scanning *)

type scan_mode = [`Fast | `Thorough]

val rescan_root : t -> scan_mode -> unit
val rescan_dirs : t -> scan_mode -> dir array -> unit
val rescan_tracks : t -> scan_mode -> track array -> unit

val rescan_busy : t -> string option

val refresh_after_rescan : t -> unit


(* Browser *)

val length_browser : t -> int

val fold_dir : t -> dir -> bool -> unit

val focus_search : t -> unit
val focus_browser : t -> unit
val refresh_browser : t -> unit

val selected_dir : t -> int option
val select_dir : t -> int -> unit
val deselect_dir : t -> unit

val update_dir : t -> dir -> unit
val load_dirs : t -> unit
val add_dirs : t -> path list -> int -> bool
val remove_dirs : t -> path list -> unit

val current_is_playlist : t -> bool
val current_is_shown_playlist : t -> bool

val has_track : t -> track -> bool

val set_search : t -> search -> unit


(* Views *)

val attr_name : [< any_attr] -> string
val attr_align : [< any_attr] -> [> `Left | `Right]

val artist_attr_string : artist -> artist_attr -> string
val album_attr_string : album -> album_attr -> string
val track_attr_string : track -> track_attr -> string

val refresh_artists_sync : t -> unit
val refresh_albums_sync : t -> unit
val refresh_tracks_sync : t -> unit
val refresh_artists : t -> unit
val refresh_albums : t -> unit
val refresh_tracks : t -> unit

val refresh_artists_busy : t -> bool
val refresh_albums_busy : t -> bool
val refresh_tracks_busy : t -> bool

val reorder_artists : t -> unit
val reorder_albums : t -> unit
val reorder_tracks : t -> unit

val focus_artists : t -> unit
val focus_albums : t -> unit
val focus_tracks : t -> unit
val defocus : t -> unit

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


(* Playlist Editing *)

val length : t -> int
val tracks : t -> track array
val table : t -> track Table.t

val insert : t -> int -> track array -> unit
val replace_all : t -> track array -> unit

val remove_all : t -> unit
val remove_selected : t -> unit
val remove_unselected : t -> unit
val remove_invalid : t -> unit

val move_selected : t -> int -> unit

val undo : t -> unit
val redo : t -> unit

val adjust_scroll : t -> int option -> int -> unit
