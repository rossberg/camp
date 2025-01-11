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
  mutable shown : bool;         (* external *)
  mutable side : Api.side;      (* external *)
  mutable width : int;          (* external *)
  mutable browser_width : int;  (* external *)
  mutable browser_rows : int;   (* external *)
  mutable browser_scroll : int; (* external *)
  mutable view_rows : int;      (* external *)
  mutable view_scroll : int;    (* external *)
  mutable view_scroll_h : int;  (* external *)
  mutable error : string;       (* external *)
  mutable error_time : time;    (* external *)
  mutable roots : dir array;    (* external *)
  mutable tracks : track array; (* external *)
  mutable columns : (attr * int) array; (* external *)
}


(* Constructor *)

val make : db -> t


(* Roots *)

val load_roots : t -> unit

val add_roots : t -> path list -> int -> bool

val count_roots : t -> int
val iter_roots : t -> (dir -> unit) -> unit

val scan_roots : t -> dir array -> unit


(* View *)

val update_view : t -> unit

val attr_name : attr -> string
val attr_align : attr -> [> `Left | `Right]
val attr_string : track -> attr -> string


(* Validation *)

type error = string

val ok : t -> error list
