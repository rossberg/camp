(* Library *)

open Data
type db = Db.t

type t =
{
  db : db;
  mutable roots : dir array;
  mutable shown : bool;
  mutable side : Api.side;
  mutable width : int;
  mutable browser_width : int;
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


(* Persistance *)

val to_string : t -> string

val load : t -> in_channel -> unit  (* assumes roots already set *)
val save : t -> out_channel -> unit


(* Accessors *)

val adjust_scroll : t -> int option -> int -> unit


(* Roots *)

val load_roots : t -> unit

val add_roots : t -> path list -> int -> bool
val remove_roots : t -> path list -> unit

val rescan_roots : t -> unit
val rescan_dirs : t -> dir array -> unit
val rescan_tracks : t -> track array -> unit

val rescan_busy : t -> bool
val rescan_done : t -> bool


(* Browser *)

val update_browser : t -> unit
val update_dir : t -> dir -> unit

val fold_dir : t -> dir -> bool -> unit

val selected_dir : t -> int option
val select_dir : t -> int -> unit
val deselect_dir : t -> unit


(* View *)

val attr_name : [< any_attr] -> string
val attr_align : [< any_attr] -> [> `Left | `Right]

val track_attr_string : track -> track_attr -> string

val update_tracks : t -> unit
val reorder_tracks : t -> unit

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


(* Validation *)

type error = string

val ok : t -> error list
