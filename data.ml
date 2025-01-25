(* Library Data Representation *)

open Audio_file

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
  mutable children : dir link array;
  mutable pos : int;
  mutable folded : bool;
}

type album =
{
  mutable id : id;
  path : path;  (* primary *)
  artist  : string option;
  title : string option;
  tracks : int option;
  discs : int option;
  date : string option;
  label : string option;
  country : string option;
  cover : blob option;
}

type playlist =
{
  mutable id : id;
  path : path;  (* primary *)
}

type track =
{
  mutable id : id;
  path : path;  (* primary *)
  album : album link option;
  filetime : time;
  filesize : int;
  fileage : time;
  status : [`Undet | `Predet | `Det | `Invalid | `Absent];
  format : Format.t option;
  meta : Meta.t option;
}
