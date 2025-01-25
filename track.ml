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


let make' path name time status =
  {path; name; time; status; last_update = 0.0}

let make_separator () = make' "separator://" name_separator 0.0 `Det

let make_predet path name time = make' path name time `Predet

let make path =
  if M3u.is_separator path then make_separator () else
  make' path (name_of_path path) 0.0 `Undet

let make_from_data (track : Data.track) =
  {
    path = track.path;
    name =
      (match track.meta with
      | Some meta -> name_of_meta track.path meta
      | None -> name_of_path track.path
      );
    time =
      (match track.format with
      | Some format -> format.time
      | None -> 0.0
      );
    status = track.status;
    last_update = track.fileage;
  }


(* Properties *)

let is_separator track = M3u.is_separator track.path

let is_invalid track =
  match track.status with
  | `Invalid | `Absent -> true
  | `Det | `Predet | `Undet -> false


(* Updating queue *)

let queue = Safe_queue.create ()

let update audio track =
  if track.last_update >= 0.0 then
  (
    track.last_update <- -1.0;
    Safe_queue.add (audio, track) queue;
  )

let rec updater () =
  let audio, track = Safe_queue.take queue in
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
      let meta = Meta.load track.path in
      if meta.loaded then track.status <- `Det;
      if track.time = 0.0 then
      (
        if meta.length <> 0.0 then track.time <- meta.length else
        let sound = Api.Audio.load audio track.path in
        track.time <-
          if sound = Api.Audio.silence audio then 0.0 else
          (
            let t = Api.Audio.length audio sound in
            Api.Audio.free audio sound;
            t
          )
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
