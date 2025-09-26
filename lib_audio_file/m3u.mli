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

val resolve : path -> item -> item

val load : path -> item list

val is_known_ext : path -> bool
