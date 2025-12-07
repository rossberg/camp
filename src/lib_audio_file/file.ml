(* Types *)

type name = string
type path = string
type dir = path
type drive = path
type time = float


(* Paths *)

let current = Filename.current_dir_name
let parent = Filename.parent_dir_name
let sep = Filename.dir_sep

let (//) = Filename.concat
let dir = Filename.dirname
let name = Filename.basename
let extension = Filename.extension
let remove_extension = Filename.remove_extension
let temp temp_dir ext = Filename.temp_file ?temp_dir ext

let has_drive path = String.length path >= 2 && path.[1] = ':'
let drive path = if has_drive path then String.sub path 0 2 else ""
let remove_drive path =
  if has_drive path then String.sub path 2 (String.length path - 2) else path

let is_proper name = name <> "" && name.[0] <> '.' && name.[0] <> '$'

let is_url path =
  match String.index_opt path ':' with
  | None -> false
  | Some i -> String.length path >= i + 2 && path.[i+1] = '/' && path.[i+2] = '/'


let explode path = assert (String.length sep = 1); String.split_on_char sep.[0] path
let implode names = if names = [] then current else String.concat sep names

let normalize path =
  let rec iter ls rs =
    match ls, rs with
    | _, [] -> List.rev ls
    | _, r1::rs' when r1 = current -> iter ls rs'  (* remove "." *)
    | _::_, r1::rs' when r1 = "" -> iter ls rs'  (* remove "", except for first *)
    | l1::l2::ls', r1::r2::rs' when r1 = parent && l1 <> parent ->
      iter (l2::ls') (r2::rs')  (* resolve "..", except after unresolved ".." *)
    | _, r1::rs' -> iter (r1::ls) rs'
  in implode (iter [] (explode path))


let is_relative = Filename.is_relative

let rec relative' names1 names2 =
  match names1, names2 with
  | name1::names1', name2::names2' when name1 = name2 ->
    relative' names1' names2'
  | _, _ ->
    List.init (List.length names1) (fun _ -> parent) @ names2

let relative dir path =
  let ddrive, dpath = drive dir, remove_drive dir in
  let idrive, ipath = drive path, remove_drive path in
  if idrive <> ddrive && idrive <> "" then path else
  implode (relative' (explode dpath) (explode ipath))

let resolve dir path =
  let ddrive, dpath = drive dir, remove_drive dir in
  let idrive, ipath = drive path, remove_drive path in
  let drive = if idrive = "" && ddrive <> "" then ddrive else idrive in
  drive ^ if is_relative ipath then dpath // ipath else ipath


(* Path escaping (for Windows) *)

let escape path =
  if not Sys.win32 || String.length path < 256 then path else
  let path' = String.map (function '/' -> '\\' | c -> c) path in
  "\\\\?\\" ^  (* Work around Windows MAX_PATH *)
    if not (is_relative path') then path' else resolve (Sys.getcwd ()) path'


(* Attributes *)

let real path =
  (* Unix.realpath doesn't like Windows UNC prefix *)
  Unix.realpath (escape path)

let stat path =
  (* Unix.stat doesn't like Windows UNC prefix *)
  Unix.stat (escape path)

let exists path =
  (* Sys.file_exists doesn't like Windows UNC prefix *)
  try ignore (stat path); true with Unix.Unix_error _ -> false

let is_dir path =
  let path = escape path in
  String.ends_with ~suffix: sep path ||
  if not Sys.win32 || String.length path < 256 then Sys.is_directory path else
  (* Sys.is_directory doesn't like Windows UNC prefix *)
  try (stat path).st_kind = S_DIR with Unix.Unix_error _ -> false

let size path = (stat path).Unix.st_size
let time path = (stat path).Unix.st_mtime
let set_time path time = Unix.utimes (escape path) (Unix.time ()) time

let run cmd =
  let inp = Unix.open_process_in cmd in
  let s = In_channel.input_all inp in
  In_channel.close inp; s

let init_tz () =
  (* On Windows, TZ is not usually set, which makes time functions fail *)
  try ignore (Unix.getenv "TZ") with Not_found ->
    let cmd =
      if Sys.win32 then
        "tzutil /g"
      else
        "date +%Z"
    in Unix.putenv "TZ" (run cmd)

let year = 365.0 *. 24.0 *. 60.0 *. 60.0
let local_time t =
  (* Windows localtime cannot handle negative dates *)
  init_tz ();
  let ydelta = if t >= 0.0 then 0.0 else ceil (-.t /. year) in
  let tm = Unix.localtime (t +. ydelta *. year) in
  Unix.{tm with tm_year = tm.tm_year - int_of_float ydelta}

let make_time tm =
  init_tz ();
  let tm' =
    if tm.Unix.tm_year >= 71 then tm else
    (* Windows mktime cannot handle dates before 1971 *)
    Unix.{tm with tm_year = 71}
  in
  let t = fst (Unix.mktime tm') in
  if tm.Unix.tm_year >= 71 then t else t -. float (71 - tm.Unix.tm_year) *. year

let zero_time =
  Unix.{
    tm_year = 0;
    tm_mon = 0;
    tm_mday = 1;
    tm_hour = 0;
    tm_min = 0;
    tm_sec = 0;
    tm_yday = 0;
    tm_wday = 0;
    tm_isdst = false;
  }


(* Files *)

type mode = [`Bin | `Text]

let check path =
  if not (exists path) then
    raise (Unix.Unix_error (Unix.ENOENT, "check", path))

let open_in mode path =
  In_channel.(match mode with `Bin -> open_bin | `Text -> open_text)
    (escape path)

let open_out mode path  =
  Out_channel.(match mode with `Bin -> open_bin | `Text -> open_text)
    (escape path)

let open_append mode path  =
  let mode = Out_channel.(match mode with `Bin -> Open_binary | `Text -> Open_text) in
  Out_channel.(open_gen [mode; Open_creat; Open_append; Open_nonblock])
    0o660 (escape path)

let with_open_in mode path  f =
  In_channel.(match mode with `Bin -> with_open_bin | `Text -> with_open_text)
    (escape path) f

let with_open_out mode path f =
  Out_channel.(match mode with `Bin -> with_open_bin | `Text -> with_open_text)
    (escape path) f

let with_open_append mode path f =
  let mode = Out_channel.(match mode with `Bin -> Open_binary | `Text -> Open_text) in
  Out_channel.(with_open_gen [mode; Open_creat; Open_append; Open_nonblock])
    0o660 (escape path) f


let load mode path = with_open_in mode path In_channel.input_all

let save mode path s =
  with_open_out mode path (fun file -> Out_channel.output_string file s)


let chunk_size = 0x1_0000_0000
let buf = Bytes.create chunk_size
let big_flag = ref true

let copy src_path dst_path =
  let stat = Unix.stat src_path in
  if !big_flag && stat.Unix.st_size > chunk_size then
  (
    let perm = stat.Unix.st_perm in
    let src = Unix.(openfile (escape src_path) [O_RDONLY] 0) in
    let dst = Unix.(openfile (escape dst_path) [O_RDWR; O_CREAT; O_TRUNC] perm) in
    let src_a = Bigarray.(Unix.map_file src char c_layout false [|-1|]) in
    let dims = Bigarray.Genarray.dims src_a in
    let dst_a = Bigarray.(Unix.map_file dst char c_layout true dims) in
    Bigarray.Genarray.blit src_a dst_a;
    Unix.close src;
    Unix.close dst
  )
  else
  (
    with_open_in `Bin src_path (fun src ->
      with_open_out `Bin dst_path (fun dst ->
        let len = ref 1 in
        while !len > 0 do
          len := input src buf 0 chunk_size;
          Out_channel.output dst buf 0 !len
        done
      )
    )
  );
  set_time dst_path (time src_path)

let move src_path dst_path = Unix.rename (escape src_path) (escape dst_path)

let delete path = Sys.remove (escape path)

let make_writable path = Unix.chmod (escape path) 0o660


let save_safe mode path s =
  let old_path = path ^ ".old" in
  let new_path = path ^ ".new" in
  save mode new_path s;
  if exists path then move path old_path;
  move new_path path;
  if exists old_path then delete old_path


(* Directories *)

let current_dir () = Sys.getcwd ()
let change_dir path = Sys.chdir (escape path)

let rec normalize_dir path =
  if not Sys.win32
  || String.length path = 3 && path.[1] = ':' && path.[2] = '\\' then
    path
  else if String.ends_with ~suffix: sep path then
    normalize_dir String.(sub path 0 (length path - length sep))
  else
    path

let check_dir path =
  let path' = normalize_dir path in
  if not (exists path') then
    raise (Unix.Unix_error (Unix.ENOENT, "check_dir", path))
  else if not (is_dir path') then
    raise (Unix.Unix_error (Unix.ENOTDIR, "check_dir", path))

let exists_dir path =
  let path' = normalize_dir path in
  exists path' && is_dir path'

let rec create_dir path =
  let parent = dir path in
  if not (exists parent) then create_dir parent;
  Unix.mkdir path 0o750

let delete_dir path = Unix.rmdir path

let read_dir path =
  check_dir path;
  try Sys.readdir (escape path) with _ -> [||]
