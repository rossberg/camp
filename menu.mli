(* Pop-uup Menu *)

type 'op t =
{
  mutable pos : int * int;
  mutable op : 'op option;
  mutable items : (Api.color * string * string) option array;
}


(* Constructor *)

val make : unit -> 'a t


(* Validation *)

type error = string

val ok : 'a t -> error list
