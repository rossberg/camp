(* M3U Playlist Creation & Parsing *)

type t = string
type path = string

type info = {time : int; title : string}
type item = {path : path; info : info option}

type format = Raw | Native | Portable
type reloc = Absolute | Driveless | Relative


(* Auxiliaries *)
let exts = [".m3u"; ".m3u8"]

let is_known_ext path =
  List.mem (String.lowercase_ascii (File.extension path)) exts


let separator = "separator://"

let is_separator path = String.starts_with ~prefix: "separator:" path


let native_path s =
  assert (String.length File.sep = 1);
  String.map (function '/' | '\\' -> File.sep.[0] | c -> c) s

let portable_path s =
  assert (String.length File.sep = 1);
  String.map (fun c -> if c = File.sep.[0] then '/' else c) s

let absolute_path dir path =
  if is_separator path then path else
  File.normalize (File.resolve dir path)

let driveless_path dir path =
  if is_separator path then path else
  let path' = absolute_path dir path in
  if File.drive path' = File.drive dir then File.remove_drive path' else path'

let relative_path dir path =
  if is_separator path then path else
  File.normalize (File.relative dir (File.resolve dir path))


let format_path = function
  | Raw -> Fun.id
  | Native -> native_path
  | Portable -> portable_path

let reloc_path = function
  | Absolute -> absolute_path
  | Driveless -> driveless_path
  | Relative -> relative_path


(* Constructors *)

let make format paths =
  let paths' =
    if format = Raw then
      paths  (* optimisation *)
    else
      List.map (format_path format) paths
  in String.concat "\n" paths' ^ "\n"

let string_of_item f {path; info} =
  match info with
  | None -> f path
  | Some {time; title} ->
    "#EXTINF:" ^ string_of_int time ^ "," ^ title ^ "\n" ^ f path

let make_ext' f items =
  String.concat "\n" ("#EXTM3U" :: List.map (string_of_item f) items) ^ "\n"

let make_ext format items =
  make_ext' (format_path format) items


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

let parse format s =
  let paths = List.filter (fun s -> s.[0] <> '#') (lines s) in
  if format = Raw then
    paths  (* optimisation *)
  else
    List.map (format_path format) paths

let parse_info s =
  let (let*) = Option.bind in
  if not (String.starts_with ~prefix: "#EXTINF:" s) then None else
  let* col = String.index_opt s ':' in
  let* com = String.index_from_opt s col ',' in
  let* time = int_of_string_opt (String.sub s (col + 1) (com - col - 1)) in
  let title = String.sub s (com + 1) (String.length s - com - 1) in
  Some {time = max 0 time; title}

let parse_ext' f s =
  List.fold_right
    (fun ln (items, info) ->
      if String.starts_with ~prefix: "#" ln
      then items, parse_info ln
      else {path = f ln; info}::items, None
    ) (List.rev (lines s)) ([], None) |> fst |> List.rev

let parse_ext format s =
  parse_ext' (format_path format) s


(* Loading & Saving *)

let load format path =
  let path' =
    if File.is_relative path then File.(current_dir () // path) else path in
  let dir = File.dir path' in
  let f = format_path format in
  let g path = f (absolute_path dir path) in
  parse_ext' g (File.load `Bin path')

let save format reloc path items : unit =
  let path' =
    if File.is_relative path then File.(current_dir () // path) else path in
  let dir = File.dir path' in
  let f = format_path format in
  let g = reloc_path reloc in
  let h path = f (g dir path) in
  File.save `Bin path (make_ext' h items)
