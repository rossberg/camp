(* Meta data about songs *)

type time = float
type path = string

type t =
{
  path : path;
  name : string;
  time : time;
}


let make path =
{
  path;
  name = Filename.(remove_extension (basename path));  (* TODO *)
  time = 0.0;  (* TODO *)
}
