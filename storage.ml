(* File Management *)

(* Directories *)

type path = string

module Dirs = Directories.Base_dirs ()

let dir =
  match Dirs.data_local_dir with
  | Some path -> Filename.concat path App.name
  | None -> "."


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


(* Loading & Saving *)

let save filename f =
  try
    if not (Sys.file_exists dir) then Sys.mkdir dir 0o770;
    Out_channel.with_open_bin (Filename.concat dir filename) f
  with Sys_error _ -> ()

let load filename f =
  try
    In_channel.with_open_bin (Filename.concat dir filename) f
  with Sys_error _ | End_of_file | Scanf.Scan_failure _ | Failure _ -> ()
