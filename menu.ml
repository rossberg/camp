(* Pop-uup Menu *)

type op = int -> unit

type t =
{
  mutable pos : int * int;
  mutable op : op option;
  mutable items : Ui.menu_entry iarray;
}


(* Constructor *)

let make () =
  {
    pos = 0, 0;
    op = None;
    items = [||];
  }


let set menu pos op items =
  menu.pos <- pos;
  menu.op <- Some op;
  menu.items <- items

let clear menu =
  menu.op <- None;
  menu.items <- [||]


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]

let ok menu =
  check "menu items when op"
    ((menu.op <> None) = (Iarray.length menu.items > 0)) @
  []


(* Persistence *)

let print_state menu =
  let open Text.Print in
  record (fun _menu -> []) menu

let print_intern menu =
  let open Text.Print in
  print_state menu @@@
  record (fun menu -> [
    "pos", pair nat nat menu.pos;
    "op", bool (menu.op <> None);
    "items", nat (Iarray.length menu.items);
  ]) menu

let parse_state _menu =
  let open Text.Parse in
  record (fun _ -> ())
