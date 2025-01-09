(* Library *)

open Audio_file
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


(* Scanning *)

let nonzero_int i = if i = 0 then None else Some i
let nonzero_float z = if z = 0.0 then None else Some z
let nonempty_text s = if s = "" then None else Some s

let scan_roots lib roots =
  let rec scan_path parent path =
    try
      if Sys.file_exists path then
      (
        if Sys.is_directory path then
        (
          if Format.is_known_ext path then
            scan_album parent path
          else
            scan_dir (Some parent) path
        )
        else
        (
          if Format.is_known_ext path then
            scan_song parent path
          else if M3u.is_known_ext path then
            scan_playlist parent path
          else
            ()
        )
      )
    with Sys_error _ -> ()
  and scan_dir parent path =
    let dir : Data.dir =
      {
        id = -1L;
        path;
        name = Filename.basename path;
        parent;
        children = [||];
        pos = 0;
        folded = true;
      }
    in
    Array.iter (fun file ->
      scan_path (ref (`Val dir)) (Filename.concat path file)
    ) (Sys.readdir path)
  and scan_album parent path =
    scan_dir (Some parent) path  (* TODO *)
  and scan_song parent path =
    let format = Format.read path in
    let meta = Meta.load_meta path in
    let song : Data.song =
      {
        id = -1L;
        path;
        dir = parent;
        album = None;
        size = nonzero_int format.size;
        time = nonzero_float format.time;
        artist = nonempty_text meta.artist;
        title = nonempty_text meta.title;
        track = nonzero_int meta.track;
        disc = nonzero_int meta.disc;
        albumartist = nonempty_text meta.albumartist;
        albumtitle = nonempty_text meta.albumtitle;
        date = nonempty_text meta.date;
        label = nonempty_text meta.label;
        country = nonempty_text meta.country;
        length = nonzero_float meta.length;
        rating = nonzero_int meta.rating;
        cover = None;
        format = nonempty_text format.name;
        channels = nonzero_int format.channels;
        depth = nonzero_int format.depth;
        rate = nonzero_int format.rate;
        bitrate = nonzero_float format.bitrate;
      }
    in
    Db.insert_song lib.db song
  and scan_playlist parent path =
    let playlist : Data.playlist =
      {
        id = -1L;
        path;
        dir = parent;
        entries = [||];
        size = None;  (* TODO *)
        time = None;  (* TODO *)
      }
    in
    Db.insert_playlist lib.db playlist
  in
  Array.iter (fun (root : dir) -> scan_dir None root.path) roots


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
      Data.{
        id = 0L;
        path;
        name = Filename.basename path;
        parent = None;
        children = [||];
        pos;
        folded = true
      }
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
    scan_roots lib roots';
    true
  with Failure msg ->
    lib.error <- msg;
    lib.error_time <- Unix.gettimeofday ();
    false


(* Songs *)

let iter_songs lib f = Db.iter_songs lib.db f


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]

let ok lib =
  check "browser width in range" (lib.browser_width <= lib.width - 40) @
  []
