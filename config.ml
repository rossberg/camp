(* Program Configuration *)

type time = float
type path = string

type t =
{
  mutable delay_track_update : time;
  mutable exec_tag : path;
  mutable exec_tag_max_len : int;
}


(* Constructor *)

let make () =
  {
    delay_track_update = 10.0;
    exec_tag = "";
    exec_tag_max_len = 0;
  }


(* Validation *)

type error = string

let ok _cfg =
  []
