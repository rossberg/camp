(* Tracks *)

open Audio_file
open Data


(* Names *)

let name_separator = String.make 80 '-'

let name_of_path path =
  let file = Filename.basename path in
  if Format.is_known_ext file then Filename.remove_extension file else file

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


let artist_title_of_name name =
  let rec find i =
    match String.index_from_opt name i '-' with
    | None -> None
    | Some i ->
      if i >= String.length name - 1 then None else
      if name.[i - 1] <> ' ' || name.[i + 1] <> ' ' then find (i + 2) else
      Some String.(sub name 0 (i - 1), sub name (i + 2) (length name - i - 2))
  in find 1

let is_digit_or_dot c = c = '.' || c >= '0' && c <= '9'

let artist_title_of_path path =
  if M3u.is_separator path then Some (name_separator, name_separator) else
  let name = Filename.(remove_extension (basename path)) in
  match artist_title_of_name name with
  | None -> None
  | Some (pre, rest) as result when String.for_all is_digit_or_dot pre ->
    (* In case there is a position as well *)
    let result' = artist_title_of_name rest in
    if result' = None then result else result'
  | result -> result


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
        ) (artist_title_of_name info.title)
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

let queue = Safe_queue.create ()

let update track =
  if track.file.age >= 0.0 then
  (
    track.file.age <- -1.0;
    Safe_queue.add track queue;
  )

let rec updater () =
  let track = Safe_queue.take queue in
  if M3u.is_separator track.path then
    track.status <- `Det
  else if not (Sys.file_exists track.path) then
    track.status <- `Absent
  else if not (Format.is_known_ext track.path) then
    track.status <- `Invalid
  else
  (
    try
      track.format <- Some (Format.read track.path);
      let meta = Meta.load track.path ~with_cover: false in
      if meta.loaded then
      (
        track.meta <- Some meta;
        track.status <- `Det;
      )
    with
    | Sys_error _ -> track.status <- `Invalid
    | exn ->
      Storage.log_exn "file" exn ("updating playlist entry " ^ track.path)
  );
  track.file.age <- Unix.time ();
  updater ()

let _ = Domain.spawn updater
