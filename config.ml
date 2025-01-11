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
    exec_tag_max_len = 8000;
  }


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]

let ok cfg =
  check "track update delay in range" (cfg.delay_track_update >= 0.0) @
  []
