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
  mutable error : string;
  mutable error_time : time;
}


(* Constructor *)

val make : db -> t


(* Validation *)

type error = string

val ok : t -> error list


(* Persistance *)

val to_map : t -> Storage.map
val of_map : t -> Storage.map -> unit  (* assumes roots already set *)

val to_map_extra : t -> Storage.map


(* Accessors *)

val adjust_scroll : t -> int option -> int -> unit


(* Roots *)

val is_root : t -> dir -> bool

val load_roots : t -> unit

val add_roots : t -> path list -> int -> bool
val remove_roots : t -> path list -> unit


(* Scanning *)

type scan_mode = [`Fast | `Thorough]

val rescan_roots : t -> scan_mode -> unit
val rescan_dirs : t -> scan_mode -> dir array -> unit
val rescan_tracks : t -> scan_mode -> track array -> unit

val rescan_busy : t -> bool

val update_after_rescan : t -> unit


(* Browser *)

val length_browser : t -> int

val update_browser : t -> unit
val update_dir : t -> dir -> unit

val fold_dir : t -> dir -> bool -> unit

val focus_browser : t -> unit

val selected_dir : t -> int option
val select_dir : t -> int -> unit
val deselect_dir : t -> unit


(* Views *)

val attr_name : [< any_attr] -> string
val attr_align : [< any_attr] -> [> `Left | `Right]

val artist_attr_string : artist -> artist_attr -> string
val album_attr_string : album -> album_attr -> string
val track_attr_string : track -> track_attr -> string

val update_artists : t -> unit
val update_albums : t -> unit
val update_tracks : t -> unit
val update_views : t -> unit

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

val insert : t -> int -> track array -> unit
val insert_paths : t -> int -> Data.path list -> unit

val remove_all : t -> unit
val remove_selected : t -> unit
val remove_unselected : t -> unit

val replace_all : t -> track array -> unit

val move_selected : t -> int -> unit
