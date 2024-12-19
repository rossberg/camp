(* Direct-style GUI abstractions *)

open Api

val font : window -> int -> Api.font
val dim : window -> rect -> rect

val window : window -> unit

val key : modifier list * key -> window -> bool
val mouse : rect -> side -> window -> bool
val wheel : rect -> window -> float

val resizer : rect -> window -> size -> size -> unit
val button : rect -> modifier list * key -> window -> bool
val control_button : rect -> string -> modifier list * key -> window -> bool -> bool
val progress_bar : rect -> window -> float -> float
val scroll_bar : rect -> window -> float -> float -> float
val scroller : rect -> window -> string -> unit

type align = [`Left | `Center | `Right]
type column = int * align
type row = color * color * string array

val table : rect -> int -> window -> column array -> row array -> int option
