(* Immediate-style GUI abstractions *)

open Api


(* State *)

type t

val make : window -> t

val window : t -> window
val window_pos : t -> point
val window_size : t -> size

(* Sub Windows *)

type subwindow = int

val subwindow : subwindow -> t -> rect -> unit

(* Areas *)

type area = subwindow * int * int * int * int

val dim : t -> area -> rect

(* Colors *)

val num_color_scheme : t -> int
val get_color_scheme : t -> int
val set_color_scheme : t -> int -> unit

val text_color : t -> color
val warn_color : t -> color
val error_color : t -> color
val focus_color : t -> color
val unlit_color : color -> color

(* Fonts *)

val font : t -> int -> Api.font

(* Input elements *)

val key : modifier list * key -> t -> bool
val mouse : area -> side -> t -> bool
val drag : area -> t -> int * int -> [`Drag of int * int | `Click | `None]
val wheel : area -> t -> float

(* UI elements *)

type align = [`Left | `Center | `Right]

val background : t -> unit

val label : area -> align -> string -> t -> unit
val indicator : area -> t -> bool -> unit
val lcd : area -> t -> char -> unit

val box : area -> color -> t -> unit
val text : area -> align -> t -> bool -> string -> unit
val ticker : area -> t -> string -> unit

val button : area -> ?protrude: bool -> modifier list * key -> t -> bool -> bool
val labeled_button : area -> ?protrude: bool -> int -> string -> modifier list * key -> t -> bool -> bool

val progress_bar : area -> t -> float -> float
val volume_bar : area -> t -> float -> float
val scroll_bar : area -> t -> float -> float -> float

val resizer : area -> Api.resize -> t -> size -> size -> size

type column = int * align
type row = color * color * string array
val table : area -> int -> t -> column array -> row array -> int option
