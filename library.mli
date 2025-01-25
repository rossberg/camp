(* Library *)

open Data
type db = Db.t

type attr =
[
  | `FilePath | `FileSize | `FileTime
  | `Codec | `Channels | `Depth | `SampleRate | `Bitrate | `Rate
  | `Artist | `Title | `Length | `Rating
  | `AlbumArtist | `AlbumTitle | `Track | `Disc
  | `Date | `Year | `Country | `Label
]

type t =
{
  db : db;
  mutable shown : bool;
  mutable side : Api.side;
  mutable width : int;
  mutable browser_width : int;
  mutable browser : dir Table.t;
  mutable view : track Table.t;
  mutable error : string;
  mutable error_time : time;
  mutable roots : dir array;
  mutable columns : (attr * int) array;
}


(* Constructor *)

val make : db -> t


(* Persistance *)

val to_string : t -> string

val load : t -> in_channel -> unit  (* assumes roots already set *)
val save : t -> out_channel -> unit


(* Accessors *)

val adjust_scroll : t -> int option -> unit


(* Roots *)

val load_roots : t -> unit

val add_roots : t -> path list -> int -> bool

val count_roots : t -> int
val iter_roots : t -> (dir -> unit) -> unit

val scan_roots : t -> dir array -> unit


(* View *)

val attr_name : attr -> string
val attr_align : attr -> [> `Left | `Right]
val attr_string : track -> attr -> string

val update_view : t -> unit
val reorder_view : t -> attr -> unit

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
