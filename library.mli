(* Library *)

type db = Db.t

type t =
{
  db : db;
  mutable shown : bool;     (* external *)
  mutable width : int;      (* external *)
  mutable side : Api.side;  (* external *)
}


(* Constructor *)

val make : db -> t


(* Validation *)

type error = string

val ok : t -> error list
