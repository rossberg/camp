(* Direct-style GUI abstractions *)

open Api

type t

type align = [`Left | `Center | `Right]
type color_scheme = {text : color; warn : color; error : color; focus : color}

val make : window -> t
val window : t -> window

val color_schemes : color_scheme array
val get_color_scheme : t -> int
val set_color_scheme : t -> int -> unit

val font : t -> int -> Api.font
val dim : t -> rect -> rect

val background : t -> unit

val key : modifier list * key -> t -> bool
val mouse : rect -> side -> t -> bool
val drag : rect -> t -> int * int -> [`Drag of int * int | `Click | `None]
val wheel : rect -> t -> float

val box : rect -> color -> t -> unit
val label : rect -> align -> string -> t -> unit
val indicator : rect -> t -> bool -> unit
val lcd : rect -> t -> char -> unit

val resizer : rect -> t -> size -> size -> size
val button : rect -> ?protrude: bool -> string -> modifier list * key -> t -> bool -> bool
val progress_bar : rect -> t -> float -> float
val volume_bar : rect -> t -> float -> float
val scroll_bar : rect -> t -> float -> float -> float
val ticker : rect -> t -> ?unlit: bool -> string -> unit

type column = int * align
type row = color * color * string array

val table : rect -> int -> t -> column array -> row array -> int option
