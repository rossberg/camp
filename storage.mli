(* File Management *)

type path = string

(* Path to local storage *)

val path : string -> path

(* Temporary Files *)

val copy_to_temp : path -> path
val remove_temp : path -> unit
val clear_temp : unit -> unit

(* Logging *)

val log : string -> unit
val log_exn : string -> exn -> string -> unit
val log_clear : unit -> unit

(* Loading & Saving *)

val load : path -> (in_channel -> unit) -> unit
val save : path -> (out_channel -> unit) -> unit
val append : path -> (out_channel -> unit) -> unit

(* Key/value files *)

module Map : Map.S with type key = string
type map = string Map.t

val read_map : map -> string -> (string -> unit) -> unit
val combine_map : map -> map -> map

val load_map : path -> map
val save_map : path -> map -> unit

val string_of_map : map -> string
val map_of_string : string -> map
