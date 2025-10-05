(* M3U Playlist Creation & Parsing *)

type path = string
type playlist = string

type info = {time : int; title : string}
type item = {path : path; info : info option}

val separator : path
val is_separator : path -> bool

val make : path list -> playlist
val make_ext : item list -> playlist

val parse : playlist -> path list
val parse_ext : playlist -> item list

val local_path : path -> path -> path
val local_item : path -> item -> item
val local : path -> item list -> item list
val resolve_path : path -> path -> path
val resolve_item : path -> item -> item
val resolve : path -> item list -> item list
val relative_path : path -> path -> path
val relative_item : path -> item -> item
val relative : path -> item list -> item list

val load : path -> item list
val save : path -> item list -> unit

val is_known_ext : path -> bool
