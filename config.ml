(* Program Configuration *)

type time = float
type path = string

type t =
{
  mutable row_height : int;
  mutable delay_track_update : time;
  mutable exec_tag : path;
  mutable exec_tag_max_len : int;
}


(* Constructor *)

let make () =
  {
    row_height = 13;
    delay_track_update = 10.0;
    exec_tag = "C:\\Program Files\\Mp3tag\\Mp3tag.exe";
    exec_tag_max_len = 8000;
  }


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]

let ok cfg =
  check "row height in range" (cfg.row_height >= 8 && cfg.row_height <= 64) @
  check "track update delay in range" (cfg.delay_track_update >= 0.0) @
  []


(* Persistance *)

let to_string cfg =
  let buf = Buffer.create 1024 in
  let output fmt = Printf.bprintf buf fmt in
  output "row_height = %d\n" cfg.row_height;
  output "delay_track_update = %.1f\n" cfg.delay_track_update;
  output "exec_tag = %s\n" cfg.exec_tag;
  output "exec_tag_max_len = %d\n" cfg.exec_tag_max_len;
  Buffer.contents buf

let save cfg file =
  Out_channel.output_string file (to_string cfg)


let fscanf file =
  match In_channel.input_line file with
  | None -> raise End_of_file
  | Some s -> Scanf.sscanf s

let num l h x = max l (min h x)

let load cfg file =
  let input fmt = fscanf file fmt in
  cfg.row_height <- input " row_height = %d " (num 8 64);
  cfg.delay_track_update <- input " delay_track_update = %f "
    (num 1.0 Float.infinity);
  cfg.exec_tag <- input " exec_tag = %[\x20-\xff]" String.trim;
  cfg.exec_tag_max_len <- input " exec_tag_max_len = %d " (num 0 max_int)
