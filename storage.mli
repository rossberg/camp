(* File Management *)

type file = string
type path = string

(* Path to local storage *)

val path : file -> path

val home_dir : path
val make_dir : path -> unit

(* Temporary Files *)

val copy_to_temp : path -> path
val delete_temp : path -> unit
val clear_temp : unit -> unit

(* Logging *)

val log : string -> unit
val log_exn : string -> exn -> string -> unit
val log_time : string -> (unit -> 'a) -> 'a
val log_clear : unit -> unit

(* Loading & Saving *)

val load : file -> (in_channel -> unit) -> unit
val load_opt : file -> (in_channel -> unit) -> unit
val save : file -> (out_channel -> unit) -> unit
val save_append : file -> (out_channel -> unit) -> unit

val load_string : file -> (string -> unit) -> unit
val load_string_opt : file -> (string -> unit) -> unit
val save_string : file -> (unit -> string) -> unit
val save_string_append : file -> (unit -> string) -> unit

val delete : file -> unit
