(* M3U Playlist Creation & Parsing *)

type path = string
type playlist = string

type info = {time : int; title : string}
type item = {path : path; info : info option}


let exts = [".m3u"; ".m3u8"]

let is_known_ext path =
  List.mem (String.lowercase_ascii (Filename.extension path)) exts


let is_separator path = String.starts_with ~prefix:"separator:" path


let string_of_item {path; info} =
  match info with
  | None -> path
  | Some {time; title} ->
    "#EXTINF:" ^ string_of_int time ^ "," ^ title ^ "\n" ^ path

let make_ext items =
  String.concat "\n" ("#EXTM3U" :: List.map string_of_item items) ^ "\n"

let make paths =
  String.concat "\n" paths ^ "\n"


let bom = "\xef\xbb\xbf"

let lines s =
  let s' =
    if String.starts_with ~prefix: bom s then
      String.sub s (String.length bom) (String.length s - String.length bom)
    else s
  in
  let lines = List.map String.trim (String.split_on_char '\n' s') in
  List.filter ((<>) "") lines

let parse s =
  List.filter (fun s -> s.[0] <> '#') (lines s)

let parse_info s =
  let (let*) = Option.bind in
  if not (String.starts_with ~prefix: "#EXTINF:" s) then None else
  let* col = String.index_opt s ':' in
  let* com = String.index_from_opt s col ',' in
  let* time = int_of_string_opt (String.sub s (col + 1) (com - col - 1)) in
  let title = String.sub s (com + 1) (String.length s - com - 1) in
  Some {time; title}

let parse_ext s =
  List.fold_right
    (fun ln (items, info) ->
      if String.starts_with ~prefix: "#" ln
      then items, parse_info ln
      else {path = ln; info}::items, None
    ) (List.rev (lines s)) ([], None) |> fst |> List.rev
