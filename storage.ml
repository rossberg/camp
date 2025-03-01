(* File Management *)

open Audio_file


(* Directories *)

type path = string

module Dirs = Directories.Base_dirs ()

let home_dir =
  match Dirs.home_dir with
  | Some path -> Fpath.to_string path
  | None -> File.current_dir ()

let data_dir =
  match Dirs.data_local_dir with
  | Some path -> File.(Fpath.to_string path // App.name)
  | None -> File.(home_dir // ("." ^ App.name))

let path filename =
  File.(data_dir // filename)


(* Temporary Files *)

let temp_dir = File.(data_dir // "temp")

let copy_to_temp path =
  if not (File.exists temp_dir) then File.create_dir temp_dir;
  let ext = File.extension path in
  let path' = File.temp (Some temp_dir) "temp" ext in
  File.copy path path';
  path'

let delete_temp path =
  try File.delete path with Sys_error _ -> ()

let clear_temp () =
  if File.exists_dir temp_dir then
  (
    Array.iter (fun file ->
      try File.delete File.(temp_dir // file) with Sys_error _ -> ()
    ) (File.read_dir temp_dir)
  )


(* Logging *)

let log_file = "error.log"
let log_mutex = Mutex.create ()

let append_fwd = ref (fun _ -> assert false)

let log_clear () =
  Mutex.protect log_mutex (fun () -> File.store `Bin (path log_file) "")

let log msg =
  Mutex.protect log_mutex (fun () ->
    let tm = Unix.(localtime (time ())) in
    let msg' = Printf.sprintf
      "%04d-%02d-%02d %02d:%02d:%02d "
      (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
      tm.tm_hour tm.tm_min tm.tm_sec ^ msg ^ "\n"
    in
    Out_channel.output_string stderr msg';
    !append_fwd log_file (fun file -> Out_channel.output_string file msg');
    Out_channel.flush_all ()
  )

let log_exn cause exn msg =
  let msg' = if msg = "" then "" else " " ^ msg in
  log (cause ^ " error " ^ Printexc.to_string exn ^ msg' ^ "\n" ^
    Printexc.get_backtrace ())


(* Loading & Saving *)

let log_io_error op filename exn =
  log_exn "file" exn ("while " ^ op ^ " " ^ filename)

let load filename f =
  try
    File.with_open_in `Bin (path filename) f
  with Sys_error _ | End_of_file | Scanf.Scan_failure _ | Failure _ as exn ->
    log_io_error "loading" filename exn

let save filename f =
  try
    if not (File.exists data_dir) then File.create_dir data_dir;
    File.with_open_out `Bin (path filename) f
  with Sys_error _ as exn ->
    log_io_error "saving" filename exn

let append filename f =
  try
    if not (File.exists data_dir) then File.create_dir data_dir;
    File.with_open_append `Bin (path filename) f
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
