(* File Management *)

type path = string

(* Temporary Files *)

val copy_to_temp : path -> path
val remove_temp : path -> unit
val clear_temp : unit -> unit

(* Loading & Saving *)

val save : string -> (out_channel -> unit) -> unit
val load : string -> (in_channel -> unit) -> unit
