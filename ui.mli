(* Immediate-style GUI abstractions *)

open Api


(* State *)

type t

val make : window -> t
val window : t -> window

(* Panes *)

type pane = int

val pane : pane -> t -> rect -> unit

(* Areas *)

type area = pane * int * int * int * int

val dim : t -> area -> rect

(* Colors *)

val num_palette : t -> int
val get_palette : t -> int
val set_palette : t -> int -> unit

val text_color : t -> color
val warn_color : t -> color
val error_color : t -> color
val hover_color : t -> color
val unlit_color : color -> color
val semilit_color : color -> color

(* Fonts *)

val font : t -> int -> Api.font

(* Input elements *)

val key : modifier list * key -> t -> bool
val mouse : area -> side -> t -> bool
val drag : area -> t -> size -> [`Drag of size | `Drop | `Click | `None]
val wheel : area -> t -> float

(* UI elements *)

type align = [`Left | `Center | `Right]
type inversion = [`Regular | `Inverted]

val start : t -> unit
val finish : t -> int -> size -> size -> unit

val label : area -> align -> string -> t -> unit
val indicator : area -> t -> bool -> unit
val lcd : area -> t -> char -> unit

val box : area -> color -> t -> unit
val text : area -> align -> t -> inversion -> bool -> string -> unit
val color_text : area -> align -> t -> color -> inversion -> bool -> string -> unit
val ticker : area -> t -> string -> unit

val button : area -> ?protrude: bool -> modifier list * key -> t -> bool option -> bool
val labeled_button : area -> ?protrude: bool -> int -> string -> modifier list * key -> t -> bool option -> bool

val progress_bar : area -> t -> float -> float
val volume_bar : area -> t -> float -> float
val scroll_bar : area -> Api.orientation -> t -> float -> float -> float

val divider : area -> Api.orientation -> t -> int -> int -> int -> int

type column = int * align
type row = color * inversion * string array
val table : area -> int -> int -> t -> column array -> row array -> int -> int option
val header : area -> int -> t -> column array -> string array -> int -> [`Click of int | `Arrange | `None]

val rich_table :
  area ->
  int ->  (* gutter width *)
  int ->  (* row height *)
  int ->  (* vertical scroll bar width *)
  int ->  (* horizontal scroll bar height (can be 0) *)
  t -> column array -> string array option -> 'a Table.t -> (int -> color * string array) ->
    [`Click of int option | `Select | `Scroll | `Sort of int | `Arrange | `Move of int | `Drop | `None]
