(* File Selection UI *)

open Audio_file

val filesel : State.t ->
  [`Write | `Read] -> [`File | `Dir] -> File.path -> string ->
  (File.path -> unit) -> unit

val run : State.t -> unit
