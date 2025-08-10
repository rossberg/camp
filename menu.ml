(* Pop-uup Menu *)

type 'op t =
{
  mutable pos : int * int;
  mutable op : 'op option;
  mutable items : (string * string) option array;
}


(* Constructor *)

let make () =
  {
    pos = 0, 0;
    op = None;
    items = [||];
  }


(* Validation *)

type error = string

let ok _menu = []
