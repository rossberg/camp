(* File Management *)

(* Directories *)

type path = string

module Dirs = Directories.Base_dirs ()

let home =
  match Dirs.home_dir with
  | Some path -> Fpath.to_string path
  | None -> Sys.getcwd ()

let dir =
  match Dirs.data_local_dir with
  | Some path -> Filename.concat (Fpath.to_string path) App.name
  | None -> Filename.concat home ("." ^ App.name)

let path filename =
  Filename.concat dir filename


(* Temporary Files *)

let buf_size = 0x400_000
let buf = Bytes.create buf_size
let temp_dir = Filename.concat dir "temp"

let copy_to_temp path =
  if not (Sys.file_exists temp_dir) then Sys.mkdir temp_dir 0o770;
  let ext = Filename.extension path in
  let path' = Filename.temp_file ~temp_dir "temp" ext in
  In_channel.with_open_bin path (fun ic ->
    Out_channel.with_open_bin path' (fun oc ->
      let rec loop () =
        let i = In_channel.input ic buf 0 buf_size in
        if i > 0 then (Out_channel.output oc buf 0 i; loop ())
      in loop ()
    )
  );
  path'

let remove_temp path =
  try Sys.remove path with Sys_error _ -> ()

let clear_temp () =
  if Sys.file_exists temp_dir && Sys.is_directory temp_dir then
  (
    Array.iter (fun file ->
      try Sys.remove (Filename.concat temp_dir file) with Sys_error _ -> ()
    ) (Sys.readdir temp_dir)
  )


(* Logging *)

let log_file = "error.log"

let log_clear () =
  Out_channel.with_open_bin (path log_file) ignore

let append_fwd = ref (fun _ -> assert false)

let log msg =
  let tm = Unix.(localtime (time ())) in
  let msg' = Printf.sprintf
    "%04d-%02d-%02d %02d:%02d:%02d "
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec ^ msg ^ "\n"
  in
  Out_channel.output_string stderr msg';
  !append_fwd log_file (fun file -> Out_channel.output_string file msg');
  Out_channel.flush_all ()

let log_exn cause exn msg =
  let msg' = if msg = "" then "" else " " ^ msg in
  log (cause ^ " error " ^ Printexc.to_string exn ^ msg' ^ "\n" ^
    Printexc.get_backtrace ())


(* Loading & Saving *)

let log_io_error op filename exn =
  log_exn "file" exn ("while " ^ op ^ " " ^ filename)

let load filename f =
  try
    In_channel.with_open_bin (path filename) f
  with Sys_error _ | End_of_file | Scanf.Scan_failure _ | Failure _ as exn ->
    log_io_error "loading" filename exn

let save filename f =
  try
    if not (Sys.file_exists dir) then Sys.mkdir dir 0o770;
    Out_channel.with_open_bin (path filename) f
  with Sys_error _ as exn ->
    log_io_error "saving" filename exn

let append filename f =
  try
    if not (Sys.file_exists dir) then Sys.mkdir dir 0o770;
    Out_channel.(with_open_gen [Open_binary; Open_creat; Open_append; Open_nonblock])
      0o660 (path filename) f
  with Sys_error _ as exn ->
    log_io_error "appending to" filename exn

let _ = append_fwd := append


(* Key/value file *)

module Map = Map.Make(String)
type map = string Map.t

let read_map map key f =
  match Map.find_opt key map with
  | None -> ()
  | Some value -> try f value with exn -> log_io_error "reading key" key exn

let combine_map map1 map2 =
  Map.union (fun key _ _ -> failwith ("conflicting key: " ^ key)) map1 map2

let map_of_string s =
  let map = ref Map.empty in
  List.iteri (fun i line ->
    match String.index_opt line '=' with
    | Some n ->
      let key = String.(trim (sub line 0 n)) in
      let value = String.(trim (sub line (n + 1) (length line - n - 1))) in
      map := Map.add key value !map
    | None when String.trim line = "" -> ()
    | None -> failwith ("line " ^ string_of_int i ^ ": syntax error")
  ) (String.split_on_char '\n' s);
  !map

let string_of_map map =
  let buf = Buffer.create 1024 in
  Map.iter (fun key value ->
    Buffer.add_string buf key;
    Buffer.add_string buf " = ";
    Buffer.add_string buf value;
    Buffer.add_string buf "\n";
  ) map;
  Buffer.contents buf

let load_map filename =
  let map = ref Map.empty in
  load filename (fun file -> map := map_of_string (In_channel.input_all file));
  !map

let save_map filename map =
  save filename (fun file -> Out_channel.output_string file (string_of_map map))
