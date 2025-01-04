(* Library *)

type t =
{
  mutable shown : bool;     (* external *)
  mutable width : int;      (* external *)
  mutable side : Api.side;  (* external *)
}


(* Constructor *)

val make : unit -> t


(* Validation *)

type error = string

val ok : t -> error list
