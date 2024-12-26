(* M3U Playlist Creation & Parsing *)

type path = string
type playlist = string

type info = {time : int; title : string}
type item = {path : path; info : info option}

val is_separator : path -> bool

val make : path list -> playlist
val make_ext : item list -> playlist

val parse : playlist -> path list
val parse_ext : playlist -> item list
