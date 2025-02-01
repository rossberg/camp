(* Program Configuration *)

type time = float
type path = string

type t =
{
  mutable delay_track_update : time;
  mutable exec_tag : path;
  mutable exec_tag_max_len : int;
}


(* Constructor *)

let make () =
  {
    delay_track_update = 10.0;
    exec_tag = "C:\\Program Files\\Mp3tag\\Mp3tag.exe";
    exec_tag_max_len = 8000;
  }


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]

let ok cfg =
  check "track update delay in range" (cfg.delay_track_update >= 0.0) @
  []


(* Persistance *)

open Storage
let fmt = Printf.sprintf
let scan = Scanf.sscanf

let num l h x = max l (min h x)

let to_map cfg =
  Map.of_list
  [
    "delay_track_update", fmt "%.1f" cfg.delay_track_update;
    "exec_tag", cfg.exec_tag;
    "exec_tag_max_len", fmt "%d" cfg.exec_tag_max_len;
  ]

let of_map cfg m =
  read_map m "delay_track_update" (fun s ->
    cfg.delay_track_update <- scan s "%f" (num 1.0 Float.infinity));
  read_map m "exec_tag" (fun s -> cfg.exec_tag <- s);
  read_map m "exec_tag_max_len" (fun s ->
    cfg.exec_tag_max_len <- scan s "%d" (num 0 max_int))
