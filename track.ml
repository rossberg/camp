(* Tracks *)

open Audio_file
open Data


(* Names *)

let name_separator = String.make 80 '-'

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


let fields_of_name name =
  let len = String.length name in
  let rec find i j =
    if i >= len then [] else
    if j >= len then [String.sub name i (len - i)] else
    match String.index_from_opt name j '-' with
    | Some j when j < len - 1 ->
      if name.[j - 1] <> ' ' || name.[j + 1] <> ' ' then
        find i (j + 2)
      else
        String.sub name i (j - i - 1) :: find (j + 2) (j + 4)
    | _ -> [String.sub name i (len - i)]
  in find 0 2

let fields_of_path path =
  if M3u.is_separator path then
    [name_separator; name_separator]
  else
    fields_of_name File.(remove_extension (name path))

let int_of_pos s =
  match String.index_opt s '.' with
  | None -> int_of_string_opt s
  | Some i -> int_of_string_opt (String.sub s (i + 1) (String.length s - i - 1))

let artist_title = function
  | artist :: title :: rest -> Some (artist, String.concat " - " (title::rest))
  | title :: [] -> Some ("[unknown]", title)
  | [] -> None

let pos_artist_title = function
  | [] -> None
  | ([_] | [_; _]) as fields ->
    Option.map (fun (artist, title) -> -1, artist, title) (artist_title fields)
  | pos :: rest ->
    match int_of_pos pos, artist_title rest with
    | Some pos, Some (artist, title) -> Some (pos - 1, artist, title)
    | _, _ ->
      Option.map (fun (artist, title) -> -1, artist, title) (artist_title rest)


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
  else if not (File.exists track.path) then
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
