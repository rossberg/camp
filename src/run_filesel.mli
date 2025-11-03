(* File Selection UI *)

open Audio_file

(* Runner *)

val run : State.t -> unit

(* Initiate File Selection *)

val filesel : State.t ->
  [`File | `Dir] -> [`Write | `Read] -> File.path -> string ->
  (File.path -> unit) -> unit
