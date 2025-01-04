(* Program Configuration *)

type path = string

type t =
{
  mutable exec_tag : path;
  mutable exec_tag_max_len : int;
}


(* Constructor *)

let make () =
  {
    exec_tag = "";
    exec_tag_max_len = 0;
  }


(* Validation *)

type error = string

let ok _cfg =
  []
