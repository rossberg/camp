(* Direct-style GUI abstractions *)

open Api

val font : window -> int -> Api.font

val window : window -> unit
val resizer : rect -> window -> size -> size -> unit
val button : rect -> key -> window -> bool
val overlay_button : rect -> key -> window -> bool
val control_button : rect -> string -> key -> window -> bool -> bool
val progress_bar : rect -> window -> float -> float option
val scroller : rect -> window -> string -> unit
