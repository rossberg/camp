(* Tracks *)

open Audio_file

type path = string
type time = float

type t =
{
  path : path;
  mutable name : string;
  mutable time : time;
  mutable status : [`Undet | `Predet | `Det | `Invalid | `Absent];
  mutable last_update : time;
}


(* Constructors *)

let name_separator = String.make 80 '-'

let name_of_path path =
  let file = Filename.basename path in
  if Format.is_known_ext file then Filename.remove_extension file else file

let name_of_meta path (meta : Meta.t) =
  if meta.artist <> "" && meta.title <> "" then
    meta.artist ^ " - " ^ meta.title
  else if meta.title <> "" then
    meta.title
  else
    name_of_path path

let artist_title_of_name name =
  let rec find i =
    match String.index_from_opt name i '-' with
    | None -> None
    | Some i ->
      if i >= String.length name - 1 then None else
      if name.[i - 1] <> ' ' || name.[i + 1] <> ' ' then find (i + 2) else
      Some String.(sub name 0 (i - 1), sub name (i + 2) (length name - i - 2))
  in find 1

let artist_title_of_path path =
  if M3u.is_separator path then Some (name_separator, name_separator) else
  let name = Filename.(remove_extension (basename path)) in
  match artist_title_of_name name with
  | None -> None
  | Some (_, title) as result ->
    (* In case there is a position as well *)
    let result' = artist_title_of_name title in
    if result' = None then result else result'


let make' path name time status =
  {path; name; time; status; last_update = 0.0}

let make_separator () = make' "separator://" name_separator 0.0 `Det

let make_predet path name time = make' path name time `Predet

let make path =
  if M3u.is_separator path then make_separator () else
  make' path (name_of_path path) 0.0 `Undet


(* Properties *)

let is_separator track = M3u.is_separator track.path

let is_invalid track =
  match track.status with
  | `Invalid | `Absent -> true
  | `Det | `Predet | `Undet -> false


(* Updating queue *)

let queue = Safe_queue.create ()

let update track =
  if track.last_update >= 0.0 then
  (
    track.last_update <- -1.0;
    Safe_queue.add track queue;
  )

let rec updater () =
  let track = Safe_queue.take queue in
  if M3u.is_separator track.path then
  (
    track.status <- `Det;
    track.time <- 0.0;
    track.name <- name_separator;
  )
  else if not (Sys.file_exists track.path) then
  (
    track.status <- `Absent;
    track.name <- name_of_path track.path
  )
  else if not (Format.is_known_ext track.path) then
  (
    track.status <- `Invalid;
    track.name <- name_of_path track.path
  )
  else
  (
    try
      let meta = Meta.load track.path ~with_cover: false in
      if meta.loaded then track.status <- `Det;
      if track.time = 0.0 then
      (
        if meta.length <> 0.0 then track.time <- meta.length else
        let format = Format.read track.path in
        track.time <- format.time;
      );
      track.name <- name_of_meta track.path meta
    with
    | Sys_error _ -> track.status <- `Invalid
    | exn ->
      Printf.fprintf stderr "uncaught exception in updater thread: %s\n%!"
        (Printexc.to_string exn)
  );
  track.last_update <- Unix.time ();
  updater ()

let _ = Domain.spawn updater
