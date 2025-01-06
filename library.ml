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

let make db =
  {
    db;
    shown = false;
    width = 600;
    side = `Right;
  }


(* Validation *)

type error = string

let ok _lib =
  []
