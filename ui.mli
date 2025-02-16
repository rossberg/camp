(* Immediate-style GUI abstractions *)

open Api


(* State *)

type t

val make : window -> t
val window : t -> window

(* Panes *)

type pane = int

val pane : t -> pane -> rect -> unit

(* Areas *)

type area = pane * int * int * int * int

val dim : t -> area -> rect
val mouse_inside : t -> area -> bool


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

type way = [`Start | `Inside | `Outside | `Outward | `Inward]

val key : t -> modifier list * key -> bool
val mouse : t -> area -> side -> bool
val drag : t -> area -> size -> [`Drag of size * way | `Drop | `Click | `None]
val wheel : t -> area -> float

(* UI elements *)

type align = [`Left | `Center | `Right]
type inversion = [`Regular | `Inverted]

val start : t -> unit
val finish : t -> int -> size -> size -> unit

val label : t -> area -> align -> string -> unit
val indicator : t -> color -> area -> bool -> unit
val lcd : t -> area -> char -> unit

val box : t -> area -> color -> unit
val text : t -> area -> align -> inversion -> bool -> string -> unit
val color_text : t -> area -> align -> color -> inversion -> bool -> string -> unit
val ticker : t -> area -> string -> unit

val button : t -> area -> ?protrude: bool -> modifier list * key -> bool option -> bool
val labeled_button : t -> area -> ?protrude: bool -> int -> string -> modifier list * key -> bool option -> bool

val progress_bar : t -> area -> float -> float
val volume_bar : t -> area -> float -> float
val scroll_bar : t -> area -> Api.orientation -> float -> float -> float

val divider : t -> area -> Api.orientation -> int -> int -> int -> int

type order = [`Asc | `Desc]
type sorting = (int * order) list
type column = int * align
type row = color * inversion * string array
type heading = string array * sorting

val table : t -> area -> int -> int -> column array -> row array -> int ->
  int option
val header : t -> area -> int -> column array -> heading -> int ->
  [`Click of int | `Arrange | `None]

val rich_table :
  t -> 
  area ->
  int ->  (* gutter width *)
  int ->  (* row height *)
  int ->  (* vertical scroll bar width *)
  int ->  (* horizontal scroll bar height (can be 0) *)
  column array ->                    (* column layout *)
  heading option ->                  (* headers *)
  'a Table.t ->                      (* data *)
  (int -> color * string array) ->   (* row generator *)
    [ `Click of int option
    | `Select
    | `Scroll
    | `Sort of int
    | `Arrange
    | `Move of int
    | `Drag of int * way
    | `Drop
    | `None
    ]

val rich_table_inner : t -> area -> int -> int  -> int -> int -> bool -> area
val rich_table_mouse : t -> area -> int -> int  -> int -> int -> bool ->
  'a Table.t -> int option
