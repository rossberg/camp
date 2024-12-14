(* Direct-style GUI abstractions *)

open Api

val font : window -> int -> Api.font

val window : window -> unit
val resizer : window -> int -> int -> int -> int -> int * int -> int * int -> unit
val button : window -> int -> int -> int -> int -> key -> bool
val control_button : window -> int -> int -> int -> int -> key -> bool -> bool
val progress_bar : window -> int -> int -> int -> int -> float -> float option
val scroller : window -> int -> int -> int -> int -> string -> unit
