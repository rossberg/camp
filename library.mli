(* Library *)

open Data
type db = Db.t

type t =
{
  db : db;
  mutable shown : bool;         (* external *)
  mutable side : Api.side;      (* external *)
  mutable width : int;          (* external *)
  mutable browser_width : int;  (* external *)
  mutable browser_rows : int;   (* external *)
  mutable browser_scroll : int; (* external *)
  mutable error : string;       (* external *)
  mutable error_time : time;    (* external *)
  mutable roots : dir array;    (* external *)
}


(* Constructor *)

val make : db -> t


(* Roots *)

val load_roots : t -> unit

val add_roots : t -> path list -> int -> bool

val count_roots : t -> int
val iter_roots : t -> (dir -> unit) -> unit

val scan_roots : t -> dir array -> unit


(* Songs *)

val iter_tracks : t -> (track -> unit) -> unit


(* Validation *)

type error = string

val ok : t -> error list
