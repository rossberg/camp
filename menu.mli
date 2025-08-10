(* Pop-uup Menu *)

type 'op t =
{
  mutable op : 'op option;
  mutable items : (string * string) array;
}


(* Constructor *)

val make : unit -> 'a t


(* Validation *)

type error = string

val ok : 'a t -> error list
