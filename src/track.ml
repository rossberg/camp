(* Tracks *)

open Audio_file
open Data


(* Names *)

let name_of_path path =
  let file = File.name path in
  if Format.is_known_ext file then File.remove_extension file else file

let name_of_artist_title artist title =
  let artist = if artist = "" then "[unknown]" else artist in
  let title = if title = "" then "[unknown]" else title in
  artist ^ " - " ^ title

let name_of_meta path (meta : Meta.t) =
  if meta.artist <> "" && meta.title <> "" then
    meta.artist ^ " - " ^ meta.title
  else if meta.title <> "" then
    "[unknown] - " ^ meta.title
  else
    name_of_path path

let name track =
  if is_separator track then name_separator else
  match track.meta with
  | Some meta -> name_of_meta track.path meta
  | None -> name_of_path track.path


let split_name s =
  let len = String.length s in
  let rec loop i j ss =
    if i = len then ss else
    match String.index_from_opt s j ' ' with
    | Some k when k <> i && k < len - 3 ->
      if s.[k + 1] = '-' && s.[k + 2] = ' ' then
        loop (k + 3) (k + 3) (String.sub s i (k - i) :: ss)
      else
        loop i (k + 1) ss
    | Some k -> loop i (k + 1) ss
    | None -> if i = 0 then [] else String.sub s i (len - i) :: ss
  in List.rev (loop 0 0 [])


let time track =
  match track.format with
  | Some format -> format.time
  | None ->
    match track.meta with
    | Some meta -> meta.length
    | None -> 0.0


(* Conversion *)

let to_m3u_item (track : Data.track) =
  let info =
    Option.map (fun (meta : Meta.t) ->
      let title = name_of_meta track.path meta in
      let time = time track in
      M3u.{title; time = int_of_float time}
    ) track.meta
  in
  M3u.{path = track.path; info}

let of_m3u_item (item : M3u.item) =
  if M3u.is_separator item.path then
    Data.make_separator ()
  else
  (
    let track = Data.make_track item.path in
    if not (Format.is_known_ext track.path) then
      track.status <- `Invalid
    else
      Option.iter (fun (info : M3u.info) ->
        Option.iter (fun (artist, title) ->
          let meta = Meta.meta track.path None in
          track.meta <- Some {meta with artist; title; length = float info.time}
        ) (artist_title (fields_of_name info.title))
      ) item.info;
    track
  )

let of_pos_m3u_item i item =
  let track = of_m3u_item item in
  track.pos <- i;
  track

let to_m3u tracks = M3u.make_ext (Array.to_list (Array.map to_m3u_item tracks))
let of_m3u s = Array.mapi of_pos_m3u_item (Array.of_list (M3u.parse_ext s))


(* Updating queue *)

let queue : track Safe_queue.t = Safe_queue.create ()

let update (track : track) =
  if track.file.age >= 0.0 then
  (
    track.file.age <- -1.0;
    Safe_queue.add track queue;
  )

let rec updater () =
  let track = Safe_queue.take queue in
  if M3u.is_separator track.path then
    track.status <- `Det
  else if not (File.exists track.path) then
    track.status <- `Absent
  else if not (Format.is_known_ext track.path) then
    track.status <- `Invalid
  else
  (
    try
      track.format <- Some (Format.read track.path);
      let meta = Meta.load track.path in
      if meta.loaded then track.meta <- Some meta;
      track.status <- `Det;
    with
    | Sys_error _ -> track.status <- `Invalid
    | exn ->
      Storage.log_exn "file" exn ("updating playlist entry " ^ track.path);
      track.status <- `Invalid
  );
  track.file.age <- Unix.time ();
  updater ()

let _ = Domain.spawn updater
