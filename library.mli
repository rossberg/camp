(* Library *)

open Data

type 'attr view =
{
  mutable shown : display option;
  mutable columns : 'attr columns;
  mutable sorting : 'attr sorting;
}

type views =
{
  mutable search : string;
  mutable query : Query.query option;
  mutable folded : bool;
  mutable divider_width : int;
  mutable divider_height : int;
  artists : artist_attr view;
  albums : album_attr view;
  tracks : track_attr view;
}

type dir = views Data.dir
type scan
type cover

type 'cache t = private
{
  mutable root : dir;
  mutable current : dir option;
  mutable error : string;
  mutable error_time : time;
  mutable refresh_time : time;
  mutable cover : bool;
  search : Edit.t;
  browser : (dir, 'cache) Table.t;
  artists : (artist, 'cache) Table.t;
  albums : (album, 'cache) Table.t;
  tracks : (track, 'cache) Table.t;
  covers : cover Map.Make(String).t Atomic.t;
  scan : scan;
}


(* Constructor *)

val make : unit -> 'a t
val make_views : string -> views
val copy_views : views -> views


(* Validation *)

type error = string

val ok : 'a t -> error list


(* Error Message *)

val error : 'a t -> string -> unit


(* Persistence *)

val print_state : 'a t -> Text.t
val print_intern : 'a t -> Text.t
val parse_state : 'a t -> Text.t -> unit  (* assumes roots already set *)

val save_playlist : 'a t -> unit

val save_db : 'a t -> unit
val load_db : 'a t -> unit

val save_browser : 'a t -> unit
val load_browser : 'a t -> unit


(* Scanning *)

type scan_mode = [`Quick | `Thorough]

val rescan_root : 'a t -> scan_mode -> unit
val rescan_dirs : 'a t -> scan_mode -> dir array -> unit
val rescan_tracks : 'a t -> scan_mode -> track array -> unit

val rescan_busy : 'a t -> string option

val refresh_after_rescan : 'a t -> unit


(* Browser *)

val length_browser : 'a t -> int

val focus_search : 'a t -> unit
val focus_browser : 'a t -> unit
val refresh_browser : 'a t -> unit

val selected_dir : 'a t -> int option
val select_dir : 'a t -> int -> unit
val deselect_dir : 'a t -> unit

val insert_roots : 'a t -> path list -> int -> bool
val remove_roots : 'a t -> path list -> bool
val move_root : 'a t -> int -> int -> unit

val find_dir : 'a t -> path -> dir option
val insert_dir : 'a t -> path -> dir option
val remove_dir : 'a t -> path -> bool
val save_dir : 'a t -> dir -> unit
val fold_dir : 'a t -> dir -> bool -> unit

val current_is_playlist : 'a t -> bool
val current_is_viewlist : 'a t -> bool
val current_is_shown_playlist : 'a t -> bool
val current_is_shown_viewlist : 'a t -> bool

val has_track : 'a t -> track -> bool


(* Views *)

val attr_name : [< any_attr] -> string
val attr_align : [< any_attr] -> [> `Left | `Right]

val refresh_tracks_sync : 'a t -> unit
val refresh_albums_tracks_sync : 'a t -> unit
val refresh_artists_albums_tracks_sync : 'a t -> unit

val refresh_tracks : ?busy: bool -> 'a t -> unit
val refresh_albums_tracks : ?busy: bool -> 'a t -> unit
val refresh_artists_albums_tracks : ?busy: bool -> 'a t -> unit

val refresh_artists_busy : 'a t -> bool
val refresh_albums_busy : 'a t -> bool
val refresh_tracks_busy : 'a t -> bool

val reorder_artists : 'a t -> unit
val reorder_albums : 'a t -> unit
val reorder_tracks : 'a t -> unit

val focus_artists : 'a t -> unit
val focus_albums : 'a t -> unit
val focus_tracks : 'a t -> unit
val defocus : 'a t -> unit

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

val set_search : 'a t -> string -> unit


(* Playlist Editing *)

val length : 'a t -> int
val tracks : 'a t -> track array
val table : 'a t -> (track, 'a) Table.t

val insert : 'a t -> int -> track array -> unit
val replace_all : 'a t -> track array -> unit

val remove_all : 'a t -> unit
val remove_selected : 'a t -> unit
val remove_unselected : 'a t -> unit
val remove_invalid : 'a t -> unit

val move_selected : 'a t -> int -> unit

val undo : 'a t -> unit
val redo : 'a t -> unit


(* Covers *)

val load_cover : 'a t -> Api.window -> path -> Api.image option
val purge_covers : 'a t -> unit
val activate_covers : 'a t -> bool -> unit
