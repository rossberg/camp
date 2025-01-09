(* Library *)

type time = float
type db = Db.t

type t =
{
  db : db;
  mutable shown : bool;         (* external *)
  mutable side : Api.side;      (* external *)
  mutable width : int;          (* external *)
  mutable browser_width : int;  (* external *)
  mutable error : string;       (* external *)
  mutable error_time : time;    (* external *)
}


(* Constructor *)

let make db =
  {
    db;
    shown = false;
    side = `Right;
    width = 600;
    browser_width = 100;
    error = "";
    error_time = 0.0;
  }


(* Roots *)

let add_root lib (dir : Data.dir) =
  if not (Sys.file_exists dir.path) then
    Result.error (dir.path ^ " does not exist")
  else if not (Sys.is_directory dir.path) then
    Result.error (dir.path ^ " is not a directory")
  else
  (
    let overlaps = ref [] in
    Db.iter_similar_roots lib.db dir.path (fun (path, name) ->
      overlaps := (path, name) :: !overlaps;
    );
    if !overlaps <> [] then
    (
      let s = String.concat ", "
        (List.map (fun (path, name) -> name ^ " (" ^ path ^ ")")
          (List.rev !overlaps))
      in Result.error (dir.path ^ " overlaps with " ^ s)
    )
    else
    (
      Db.insert_root lib.db dir;
Printf.printf "inserted %Ld\n%!" dir.id;
      Result.ok dir.id;
    )
  )

let count_roots lib = Db.count_roots lib.db
let iter_roots lib f = Db.iter_roots lib.db f
let iter_root_paths lib f = Db.iter_root_paths lib.db f


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]

let ok lib =
  check "browser width in range" (lib.browser_width <= lib.width - 40) @
  []
