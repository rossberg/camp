(* Program Configuration *)

type time = float
type path = string

type t =
{
  mutable delay_track_update : time;
  mutable exec_tag : path;
  mutable exec_tag_flags : string;
  mutable exec_max_len : int;
}


(* Constructor *)

let make () =
  {
    delay_track_update = 10.0;
    exec_tag = if Sys.win32 then "C:\\Program Files\\Mp3tag\\Mp3tag.exe" else "";
    exec_tag_flags = if Sys.win32 then "/add" else "";
    exec_max_len = if Sys.win32 then 8000 else 100_000;
  }


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]

let ok cfg =
  check "track update delay in range" (cfg.delay_track_update >= 0.0) @
  []


(* Persistence *)

let print_state =
  let open Text.Print in
  record (fun cfg -> [
    "delay_track_update", float cfg.delay_track_update;
    "exec_tag", string cfg.exec_tag;
    "exec_tag_flags", string cfg.exec_tag_flags;
    "exec_max_len", nat cfg.exec_max_len;
  ])

let print_intern = print_state

let parse_state cfg =
  let open Text.Parse in
  record (fun r ->
    apply (r $? "delay_track_update") (interval 1.0 Float.infinity)
      (fun t -> cfg.delay_track_update <- t);
    apply (r $? "exec_tag") string
      (fun s -> cfg.exec_tag <- s);
    apply (r $? "exec_tag_flags") string
      (fun s -> cfg.exec_tag_flags <- s);
    apply (r $? "exec_max_len") nat
      (fun n -> cfg.exec_max_len <- n);
  )
