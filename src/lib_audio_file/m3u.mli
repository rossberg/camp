(* M3U Playlist Creation & Parsing *)

type t = string
type path = string

type info = {time : int; title : string}
type item = {path : path; info : info option}

type format = Raw | Native | Portable
type reloc = Absolute | Driveless | Relative

val separator : path
val is_separator : path -> bool

val make : format -> path list -> t
val make_ext : format -> item list -> t

val parse : format -> t -> path list
val parse_ext : format -> t -> item list

val native_path : path -> path
val portable_path : path -> path
val absolute_path : path -> path -> path
val driveless_path : path -> path -> path
val relative_path : path -> path -> path
val format_path : format -> path -> path
val reloc_path : reloc -> path -> path -> path

val load : format -> path -> item list
val save : format -> reloc -> path -> item list -> unit

val is_known_ext : path -> bool
