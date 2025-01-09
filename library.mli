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
  mutable error : string;       (* external *)
  mutable error_time : time;    (* external *)
}


(* Constructor *)

val make : db -> t


(* Roots *)

val add_root : t -> dir -> (id, string) result

val count_roots : t -> int
val iter_roots : t -> (dir -> unit) -> unit
val iter_root_paths : t -> (path -> unit) -> unit


(* Validation *)

type error = string

val ok : t -> error list
