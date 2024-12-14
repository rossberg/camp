(* Meta data about songs *)

type time = float
type path = string

type t =
{
  path : path;
  artist : string;
  title : string;
  time : time;
}


let make path =
{
  path;
  artist = "";  (* TODO *)
  title = Filename.basename path;  (* TODO *)
  time = 0.0;  (* TODO *)
}
