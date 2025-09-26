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

let drive path =
  if String.length path >= 2 && path.[1] = ':' then
    String.sub path 0 2
  else
    ""

let is_proper name = name <> "" && name.[0] <> '.' && name.[0] <> '$'

let remove_drive path =
  let drive = drive path in
  if drive = "" then path else
  let n = String.length drive in
  String.sub path n (String.length path - n)


let remove_suffix s1 s2 =
  if String.ends_with ~suffix: s1 s2 then
    String.(sub s2 0 (length s2 - length s1))
  else s2

let rec explode path =
  let dir = Filename.dirname path in
  if dir = path then
    [dir]
  else if dir = Filename.current_dir_name then
    [remove_suffix sep path]
  else
    explode dir @ [Filename.basename path]

let rec implode names =
  match names with
  | [] -> Filename.current_dir_name
  | [name] -> name
  | name::names -> name // implode names

let is_relative = Filename.is_relative

let rec make_relative' names1 names2 =
  match names1, names2 with
  | name1::names1', name2::names2' when name1 = name2 ->
    make_relative' names1' names2'
  | _, _ ->
    List.init (List.length names1) (fun _ -> Filename.parent_dir_name) @ names2

let make_relative dir path =
  implode (make_relative' (explode dir) (explode path))

let make_resolvable dir path =
  if Filename.is_relative path then
    make_relative dir path
  else
    path

let resolve dir path =
  if Filename.is_relative path then dir//path else path

let is_url path =
  match String.index_opt path ':' with
  | None -> false
  | Some i -> String.length path >= i + 2 && path.[i+1] = '/' && path.[i+2] = '/'


(* Path normalization (for Windows) *)

let normalize path =
  if not Sys.win32 || String.length path < 256 then path else
  let path' = String.map (function '/' -> '\\' | c -> c) path in
  "\\\\?\\" ^  (* Work around Windows MAX_PATH *)
    if not (is_relative path') then path' else resolve (Sys.getcwd ()) path'


(* Attributes *)

let stat path =
  (* Unix.stat doesn't like Windows UNC prefix *)
  Unix.stat (normalize path)

let exists path =
  (* Sys.file_exists doesn't like Windows UNC prefix *)
  try ignore (stat path); true with Unix.Unix_error _ -> false

let is_dir path =
  let path = normalize path in
  String.ends_with ~suffix: sep path ||
  if not Sys.win32 || String.length path < 256 then Sys.is_directory path else
  (* Sys.is_directory doesn't like Windows UNC prefix *)
  try (stat path).st_kind = S_DIR with Unix.Unix_error _ -> false

let size path = (stat path).Unix.st_size
let time path = (stat path).Unix.st_mtime
let set_time path time = Unix.utimes (normalize path) (Unix.time ()) time

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
    (normalize path)

let open_out mode path  =
  Out_channel.(match mode with `Bin -> open_bin | `Text -> open_text)
    (normalize path)

let open_append mode path  =
  let mode = Out_channel.(match mode with `Bin -> Open_binary | `Text -> Open_text) in
  Out_channel.(open_gen [mode; Open_creat; Open_append; Open_nonblock])
    0o660 (normalize path)

let with_open_in mode path  f =
  In_channel.(match mode with `Bin -> with_open_bin | `Text -> with_open_text)
    (normalize path) f

let with_open_out mode path f =
  Out_channel.(match mode with `Bin -> with_open_bin | `Text -> with_open_text)
    (normalize path) f

let with_open_append mode path f =
  let mode = Out_channel.(match mode with `Bin -> Open_binary | `Text -> Open_text) in
  Out_channel.(with_open_gen [mode; Open_creat; Open_append; Open_nonblock])
    0o660 (normalize path) f


let load mode path = with_open_in mode path In_channel.input_all

let store mode path s =
  with_open_out mode path (fun file -> Out_channel.output_string file s)


let chunk_size = 0x1_0000_0000
let buf = Bytes.create chunk_size
let big_flag = ref true

let copy src_path dst_path =
  if !big_flag then
  (
    let perm = (Unix.stat src_path).Unix.st_perm in
    let src = Unix.(openfile (normalize src_path) [O_RDONLY] 0) in
    let dst = Unix.(openfile (normalize dst_path) [O_RDWR; O_CREAT; O_TRUNC] perm) in
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

let move src_path dst_path = Unix.rename (normalize src_path) (normalize dst_path)

let delete path = Sys.remove (normalize path)

let make_writable path = Unix.chmod (normalize path) 0o660


(* Directories *)

let current_dir () = Sys.getcwd ()
let change_dir path = Sys.chdir (normalize path)

let normalize_dir path =
  if not Sys.win32
  || String.length path = 3 && path.[1] = ':' && path.[2] = '\\' then
    path
  else
    remove_suffix sep path

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
  try Sys.readdir (normalize path) with _ -> [||]
