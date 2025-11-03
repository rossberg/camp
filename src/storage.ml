(* File Management *)

open Audio_file


(* Directories *)

type file = string
type path = string
type time = float

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

let make_dir path =
  if not (File.exists path) then File.create_dir path

let _ = make_dir data_dir


(* Temporary Files *)

let temp_dir = File.(data_dir // "temp")

let create_temp () =
  if not (File.exists temp_dir) then File.create_dir temp_dir;
  File.temp (Some temp_dir) "temp" ""

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

let save_string_append_fwd = ref (fun _ -> assert false)

let log_clear () =
  Mutex.protect log_mutex (fun () -> File.save `Bin (path log_file) "")

let log msg =
  Mutex.protect log_mutex (fun () ->
    let tm = File.local_time (Unix.time ()) in
    let msg' = Printf.sprintf
      "%04d-%02d-%02d %02d:%02d:%02d "
      (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
      tm.tm_hour tm.tm_min tm.tm_sec ^ msg ^ "\n"
    in
    Out_channel.output_string stderr msg';
    !save_string_append_fwd log_file (fun () -> msg');
    Out_channel.flush_all ()
  )

let log_exn cause exn msg =
  let msg' = if msg = "" then "" else " " ^ msg in
  log (cause ^ " error " ^ Printexc.to_string exn ^ msg' ^ "\n" ^
    Printexc.get_backtrace ())

let log_time op f =
  if not !App.debug_perf then f () else
  (
    let t1 = Unix.gettimeofday () in
    let x = f () in
    let t2 = Unix.gettimeofday () in
    Printf.printf "    [%s] %.3f ms\n%!" op (t2 -. t1);
    x
  )


(* Loading & Saving *)

let log_io_error op filename exn =
  log_exn "file" exn ("while " ^ op ^ " " ^ filename)

let load filename f =
  try
    File.with_open_in `Bin (path filename) f
  with Sys_error _ | End_of_file | Scanf.Scan_failure _ | Failure _ as exn ->
    log_io_error "loading" filename exn

let load_opt filename f =
  try
    if File.exists (path filename) then load filename f
  with Sys_error _ | Failure _ as exn ->
    log_io_error "loading" filename exn

let save filename f =
  try
    if not (File.exists data_dir) then File.create_dir data_dir;
    let path = path filename in
    let old_path = path ^ ".old" in
    let new_path = path ^ ".new" in
    if File.exists new_path then File.delete new_path;
    File.with_open_out `Bin new_path f;
    if File.exists path then File.move path old_path;
    File.move new_path path;
    if File.exists old_path then File.delete old_path;
  with Sys_error _ as exn ->
    log_io_error "saving" filename exn

let save_append filename f =
  try
    if not (File.exists data_dir) then File.create_dir data_dir;
    File.with_open_append `Bin (path filename) f
  with Sys_error _ as exn ->
    log_io_error "appending to" filename exn

let load_string filename f =
  load filename (fun ic -> f (In_channel.input_all ic))

let load_string_opt filename f =
  load_opt filename (fun ic -> f (In_channel.input_all ic))

let save_string filename f =
  save filename (fun oc -> Out_channel.output_string oc (f ()))

let save_string_append filename f =
  save_append filename (fun oc -> Out_channel.output_string oc (f ()))

let _ = save_string_append_fwd := save_string_append

let time filename =
  try
    Some (File.time (path filename))
  with Sys_error _ | Failure _ as exn ->
    log_io_error "stating" filename exn;
    None

let exists filename =
  try
    File.exists (path filename)
  with Sys_error _ | Failure _ as exn ->
    log_io_error "probing" filename exn;
    false

let delete filename =
  let path = path filename in
  try
    if File.exists path then File.delete path
  with Sys_error _ | Failure _ as exn ->
    log_io_error "deleting" filename exn
