(* Pop-uup Menu *)

type 'op t =
{
  mutable op : 'op option;
  mutable items : (string * string) array;
}


(* Constructor *)

let make () =
  {
    op = None;
    items = [||];
  }


(* Validation *)

type error = string

let ok _menu = []
