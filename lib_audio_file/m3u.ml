(* M3U Playlist Creation & Parsing *)

type path = string
type playlist = string

type info = {time : int; title : string}
type item = {path : path; info : info option}


(* Auxiliaries *)
let exts = [".m3u"; ".m3u8"]

let is_known_ext path =
  List.mem (String.lowercase_ascii (Filename.extension path)) exts


let separator = "separator://"

let is_separator path = String.starts_with ~prefix: "separator:" path


(* Constructors *)

let string_of_item {path; info} =
  match info with
  | None -> path
  | Some {time; title} ->
    "#EXTINF:" ^ string_of_int time ^ "," ^ title ^ "\n" ^ path

let make_ext items =
  String.concat "\n" ("#EXTM3U" :: List.map string_of_item items) ^ "\n"

let make paths =
  String.concat "\n" paths ^ "\n"


(* Parsing *)

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

let parse_path s =
  assert (String.length File.sep = 1);
  String.map (function '/' | '\\' -> File.sep.[0] | c -> c) s

let parse_ext s =
  List.fold_right
    (fun ln (items, info) ->
      if String.starts_with ~prefix: "#" ln
      then items, parse_info ln
      else {path = parse_path ln; info}::items, None
    ) (List.rev (lines s)) ([], None) |> fst |> List.rev


(* Loading & Saving *)

let local_path dir path =
  let path' = File.resolve dir path in
  if File.drive path' = File.drive dir then File.remove_drive path' else path'

let resolve_path dir path =
  if is_separator path then path else File.normalize (File.resolve dir path)

let relative_path dir path =
  if is_separator path then path else
  File.normalize (File.relative dir (File.resolve dir path))

let local_item dir item = {item with path = local_path dir item.path}
let resolve_item dir item = {item with path = resolve_path dir item.path}
let relative_item dir item = {item with path = relative_path dir item.path}

let local dir items = List.map (local_item dir) items
let resolve dir items = List.map (resolve_item dir) items
let relative dir items = List.map (relative_item dir) items

let load path =
  let path' =
    if File.is_relative path then File.(current_dir () // path) else path in
  resolve (File.dir path') (parse_ext (File.load `Bin path'))

let save path items : unit =
  let path' =
    if File.is_relative path then File.(current_dir () // path) else path in
  File.save `Bin path (make_ext (relative (File.dir path') items))
