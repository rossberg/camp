(* M3U Playlist Creation & Parsing *)

type path = string
type playlist = string

type info = {time : int; title : string}
type item = {path : path; info : info option}


let exts = [".m3u"; ".m3u8"]

let is_known_ext path =
  List.mem (String.lowercase_ascii (Filename.extension path)) exts


let separator = "separator://"

let is_separator path = String.starts_with ~prefix: "separator:" path


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
  Some {time = max 0 time; title}

let parse_ext s =
  List.fold_right
    (fun ln (items, info) ->
      if String.starts_with ~prefix: "#" ln
      then items, parse_info ln
      else {path = ln; info}::items, None
    ) (List.rev (lines s)) ([], None) |> fst |> List.rev


let split_drive path =
  if Sys.win32 && String.length path >= 2 && path.[1] = ':' then
    String.sub path 0 2, String.sub path 2 (String.length path - 2)
  else
    "", path

let normalise path =
  let open Filename in
  assert (String.length dir_sep = 1);
  let arcs = String.split_on_char dir_sep.[0] path in
  let rec iter ls rs =
    match ls, rs with
    | _, [] -> List.rev ls
    | _, r1::rs' when r1 = current_dir_name -> iter ls rs'
    | _::_, r1::rs' when r1 = "" -> iter ls rs'
    | l1::l2::ls', r1::r2::rs'
      when r1 = parent_dir_name && l1 <> parent_dir_name ->
      iter (l2::ls') (r2::rs')
    | _, r1::rs' -> iter (r1::ls) rs'
  in
  String.concat dir_sep (iter [] arcs)

let resolve dir item =
  if is_separator item.path then item else
  let ddrive, dpath = split_drive dir in
  let idrive, ipath = split_drive item.path in
  let drive = if idrive = "" && ddrive <> "" then ddrive else idrive in
  let path =
    if Filename.is_relative ipath then Filename.concat dpath ipath else ipath in
  {item with path = drive ^ normalise path}
