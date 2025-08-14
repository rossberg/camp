(* File Selection UI *)

open Audio_file

(* Runner *)

val run : State.t -> unit

(* Initiate File Selection *)

val filesel : State.t ->
  [`Write | `Read] -> [`File | `Dir] -> File.path -> string ->
  (File.path -> unit) -> unit
