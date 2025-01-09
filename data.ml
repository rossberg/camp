(* Library Data Representation *)

type path = string
type time = float
type blob = string
type id = int64
type 'a link = [`Id of id | `Val of 'a] ref

let link id = ref (`Id id)

type dir =
{
  mutable id : id;
  path : path;  (* primary *)
  name : string;
  parent : dir link option;  (* none for root *)
  mutable pos : int;
  mutable folded : bool;
}

type album =
{
  mutable id : id;
  path : path;  (* primary *)
  dir : dir link;
  artist  : string option;
  title : string option;
  tracks : int option;
  discs : int option;
  date : string option;
  label : string option;
  country : string option;
  cover : blob option;
}

type song =
{
  mutable id : id;
  path : path;  (* primary *)
  dir : dir link;
  album : album link option;
  size : int option;  (* B *)
  time : time option;
  artist  : string option;
  title : string option;
  track : int option;
  disc : int option;
  albumartist : string option;
  albumtitle : string option;
  date : string option;
  label : string option;
  country : string option;
  length : time option;
  rating : int option;
  cover : blob option;
  channels : int option;
  depth : int option;
  rate : int option;       (* Hz *)
  bitrate : float option;  (* b/s *)
}

type playlist =
{
  mutable id : id;
  path : path;  (* primary *)
  dir : dir link;
  entries : id array;
}
