(* Library *)

open Data

type display = [`Table | `Grid]

type ('data, 'attr) view =
{
  mutable shown : display option;
  mutable columns : 'attr columns;
  mutable sorting : 'attr sorting;
  mutable table : ('data, Cache.t) Table.t option;
}

type artists_view = (artist, artist_attr_ex) view
type albums_view = (album, album_attr_ex) view
type tracks_view = (track, track_attr_ex) view

type views =
{
  mutable folded : bool;
  mutable custom : bool;
  mutable divider_width : int;
  mutable divider_height : int;
  artists : artists_view;
  albums : albums_view;
  tracks : tracks_view;
}

type dir = views Data.dir
type scan
type cover

type t = private
{
  mutable root : dir;
  mutable current : dir option;
  mutable error : string;
  mutable error_time : time;
  mutable refresh_time : time;
  mutable covers_shown : bool;
  mutable renaming : int option;
  mutable log : Log.t option;
  mutable query : Query.query option;
  search : Edit.t;
  rename : Edit.t;
  browser : (dir, Cache.t) Table.t;
  mutable artists : (artist, Cache.t) Table.t;
  mutable albums : (album, Cache.t) Table.t;
  mutable tracks : (track, Cache.t) Table.t;
  covers : cover Map.Make(String).t Atomic.t;
  mutable age_covers : (path * cover) Seq.t;
  mutable views_clip : views option;
  mutable views_dir_default : views;
  mutable views_album_default : views;
  mutable views_playlist_default : views;
  scan : scan;
}


(* Constructor *)

val make : unit -> t
val make_views : unit -> views
val make_views_none : unit -> views
val clone_views : views -> views
val update_views : views -> views -> unit


(* Validation *)

type error = string

val ok : t -> error list


(* Error Message *)

val error : t -> string -> unit


(* Persistence *)

val print_state : t -> Text.t
val print_intern : t -> Text.t
val parse_state : t -> Text.t -> unit  (* assumes roots already set *)

val save_playlist : t -> unit

val save_db : t -> unit
val load_db : t -> unit

val save_browser : t -> unit
val load_browser : t -> unit


(* Scanning *)

type scan_mode = [`Quick | `Thorough]

val rescan_root : t -> scan_mode -> unit
val rescan_dirs : t -> scan_mode -> dir array -> unit
val rescan_tracks : t -> scan_mode -> track array -> unit

val rescan_busy : t -> string option

val refresh_after_rescan : t -> unit


(* Browser *)

val length_browser : t -> int

val focus_search : t -> unit
val focus_browser : t -> unit
val refresh_browser : t -> unit

val selected_dir : t -> int option
val select_dir : t -> int -> unit
val deselect_dir : t -> unit

val insert_roots : t -> path list -> int -> bool
val remove_roots : t -> path list -> bool

val find_dir : t -> path -> dir option
val insert_dir : t -> path -> dir option
val remove_dir : t -> path -> bool
val reverse_dir : t -> dir -> unit
val move_dir : t -> dir -> int -> int -> unit
val save_dir : t -> dir -> unit
val fold_dir : t -> dir -> bool -> unit
val find_entry_dir : t -> dir -> int option

val find_parent : t -> dir -> dir option
val find_parents : t -> dir -> dir list
val find_parent_pos : t -> dir -> int

val current_is_all : t -> bool
val current_is_root : t -> bool
val current_is_dir : t -> bool
val current_is_album : t -> bool
val current_is_playlist : t -> bool
val current_is_viewlist : t -> bool
val current_is_shown_playlist : t -> bool  (* tracks view is active *)
val current_is_shown_viewlist : t -> bool  (* tracks view is active *)
val current_is_plain_playlist : t -> bool  (* tracks view is sorted by pos *)

val has_track : t -> track -> bool

val start_rename : t -> int -> unit
val end_rename : t -> bool -> unit
val start_log : t -> Log.t -> unit
val end_log : t -> unit


(* Views *)

val attr_name : [< any_attr_ex] -> string
val attr_align : [< any_attr_ex] -> [`Left | `Right | `Center]

val refresh_tracks_sync : t -> unit
val refresh_albums_tracks_sync : t -> unit
val refresh_artists_albums_tracks_sync : t -> unit

val refresh_tracks :
  ?busy: bool -> ?after: (unit -> unit) -> t -> unit
val refresh_albums_tracks :
  ?busy: bool -> ?after: (unit -> unit) -> t -> unit
val refresh_artists_albums_tracks :
  ?busy: bool -> ?after: (unit -> unit) -> t -> unit

val refresh_artists_is_busy : t -> bool
val refresh_albums_is_busy : t -> bool
val refresh_tracks_is_busy : t -> bool

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

val select : t -> int -> int -> unit
val deselect : t -> int -> int -> unit
val select_all : t -> unit
val deselect_all : t -> unit
val select_invert : t -> unit

val set_search : t -> string -> unit
val clear_search : t -> unit
val refresh_search : t -> unit

val copy_views : t -> unit
val paste_views : t -> unit
val default_views : t -> unit
val template_views : t -> unit
val inherit_views : t -> bool -> bool -> unit
val propagate_views : t -> bool -> bool -> unit

val make_viewlist : t -> dir -> string


(* Playlist Editing *)

val length : t -> int
val tracks : t -> track array
val table : t -> (track, Cache.t) Table.t

val insert : t -> int -> track array -> unit
val append : t -> track array -> unit
val replace_all : t -> track array -> unit
val replace_map : t -> track Map.Make(String).t -> bool -> unit

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


(* Playlist Repair *)

type repair_map = track list Map.Make(String).t

val repair_key : path -> string
val repair_map : t -> (dir -> unit) -> repair_map
val repair_path : repair_map -> path -> path ->
  [`Ok | `Replace of track | `Ambiguous | `Missing]


(* Covers *)

val load_cover : t -> Api.window -> path -> Api.image option
val purge_covers : t -> unit
val activate_covers : t -> bool -> unit
