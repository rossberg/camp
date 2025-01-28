(* Library Data Representation *)

open Audio_file

type path = string
type time = float
type blob = string
type id = int64
type 'a link = [`Id of id | `Val of 'a] ref

let link_id id = ref (`Id id)
let link_val x = ref (`Val x)

let id_of_link = function {contents = `Id id} -> id | _ -> assert false
let val_of_link = function {contents = `Val x} -> x | _ -> assert false

type dir =
{
  mutable id : id;
  path : path;  (* primary *)
  name : string;
  mutable children : dir link array;
  mutable pos : int;
  mutable nest : int;
  mutable folded : bool;
}

type album =
{
  mutable id : id;
  path : path;  (* primary *)
  mutable artist  : string option;
  mutable title : string option;
  mutable tracks : int option;
  mutable discs : int option;
  mutable date : string option;
  mutable label : string option;
  mutable country : string option;
  mutable cover : blob option;
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
  mutable album : album link option;
  mutable filesize : int;
  mutable filetime : time;
  mutable fileage : time;
  mutable status : [`Undet | `Predet | `Det | `Invalid | `Absent];
  mutable format : Format.t option;
  mutable meta : Meta.t option;
}
