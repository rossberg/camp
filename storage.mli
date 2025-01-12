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
val log_clear : unit -> unit

(* Loading & Saving *)

val load : string -> (in_channel -> unit) -> unit
val save : string -> (out_channel -> unit) -> unit
val append : string -> (out_channel -> unit) -> unit
