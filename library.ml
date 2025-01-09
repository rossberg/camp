(* Library *)

open Data

type time = float
type db = Db.t

type t =
{
  db : db;
  mutable shown : bool;         (* external *)
  mutable side : Api.side;      (* external *)
  mutable width : int;          (* external *)
  mutable browser_width : int;  (* external *)
  mutable browser_rows : int;   (* external *)
  mutable browser_scroll : int; (* external *)
  mutable error : string;       (* external *)
  mutable error_time : time;    (* external *)
  mutable roots : dir array;    (* external *)
}


(* Constructor *)

let make db =
  {
    db;
    shown = false;
    side = `Right;
    width = 600;
    browser_width = 100;
    browser_rows = 4;
    browser_scroll = 0;
    error = "";
    error_time = 0.0;
    roots = [||];
  }


(* Roots *)

let count_roots lib = Db.count_roots lib.db
let iter_roots lib f = Db.iter_roots lib.db f

let load_roots lib =
  let roots = ref [] in
  iter_roots lib (fun root -> roots := root :: !roots);
  lib.roots <- Array.of_list !roots;
  Array.sort (fun r1 r2 -> compare r1.pos r2.pos) lib.roots

let make_root lib path pos =
  if not (Sys.file_exists path) then
    failwith (path ^ " does not exist")
  else if not (Sys.is_directory path) then
    failwith (path ^ " is not a directory")
  else
  (
    match
      Array.find_opt (fun (root : dir) ->
        path = root.path ||
        String.starts_with root.path ~prefix: (Filename.concat path "") ||
        String.starts_with path ~prefix: (Filename.concat root.path "")
      ) lib.roots
    with
    | Some root ->
      failwith
        (path ^ " overlaps with " ^ root.name ^ " (" ^ root.path ^ ")")
    | None ->
      let name = Filename.basename path in
      Data.{id = 0L; path; name; parent = None; pos; folded = true}
  )

let add_roots lib paths pos =
  let paths = Array.of_list paths in
  lib.error <- "";
  try
    let roots' = Array.mapi (fun i path -> make_root lib path (pos + i)) paths in
    let len = Array.length lib.roots in
    let len' = Array.length roots' in
    lib.roots <-
      Array.init (len + len') (fun i ->
        if i < pos then
          lib.roots.(i)
        else if i < pos + len' then
          roots'.(i - pos)
        else
          let root = lib.roots.(i - len') in
          root.pos <- i; root
      );
    Db.update_roots_pos lib.db pos (+len);  (* avoid temporary pos clash *)
    Db.update_roots_pos lib.db pos (-len + len');
    Array.iter (Db.insert_root lib.db) roots';
    true
  with Failure msg ->
    lib.error <- msg;
    lib.error_time <- Unix.gettimeofday ();
    false


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]

let ok lib =
  check "browser width in range" (lib.browser_width <= lib.width - 40) @
  []
