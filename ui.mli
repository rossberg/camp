(* Direct-style GUI abstractions *)

open Api

type align = [`Left | `Center | `Right]
type color_scheme = {text : color; warn : color; error : color; focus : color}

val color_schemes : color_scheme array
val get_color_scheme : unit -> int
val set_color_scheme : int -> unit

val font : window -> int -> Api.font
val dim : window -> rect -> rect

val window : window -> unit

val key : modifier list * key -> window -> bool
val mouse : rect -> side -> window -> bool
val drag : rect -> window -> int * int -> [`Drag of int * int | `Click | `None]
val wheel : rect -> window -> float

val box : rect -> color -> window -> unit
val label : rect -> align -> string -> window -> unit
val indicator : rect -> window -> bool -> unit
val lcd : rect -> window -> char -> unit

val resizer : rect -> window -> size -> size -> size
val button : rect -> ?protrude: bool -> string -> modifier list * key -> window -> bool -> bool
val progress_bar : rect -> window -> float -> float
val volume_bar : rect -> window -> float -> float
val scroll_bar : rect -> window -> float -> float -> float
val ticker : rect -> window -> string -> unit

type column = int * align
type row = color * color * string array

val table : rect -> int -> window -> column array -> row array -> int option
