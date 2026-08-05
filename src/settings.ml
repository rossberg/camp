(* Settings *)

type t =
{
  mutable vscroll : int;
}


(* Constructor *)

let make () =
  {
    vscroll = 0;
  }


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]

let ok set =
  check "scroll in range" (set.vscroll >= 0) @
  []


(* Focus *)

let defocus _set = ()


(* Persistence *)

let print_state =
  let open Text.Print in
    record (fun _set -> [])

let print_intern set =
  let open Text.Print in
  print_state set @@@
  record (fun set -> [
    "vscroll", nat set.vscroll;
  ]) set

let parse_state _set = ignore
