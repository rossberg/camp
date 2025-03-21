(* Immediate-style GUI widgets *)

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
val active_color : t -> color
val inactive_color : t -> color
val unlit_color : color -> color
val semilit_color : color -> color

(* Fonts *)

val font : t -> int -> Api.font

(* Images *)

val nocover : t -> Api.image

(* Input elements *)

type way = [`Start | `Inside | `Outside | `Outward | `Inward]

val key : t -> modifier list * key -> bool -> bool
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

val mouse_reflection : t -> area -> int -> unit

val box : t -> area -> color -> unit
val text : t -> area -> align -> inversion -> bool -> string -> unit
val color_text : t -> area -> align -> color -> inversion -> bool -> string -> unit
val ticker : t -> area -> string -> unit
val edit_text : t -> area -> string -> int -> (int * int) option -> string * int * (int * int) option * Uchar.t
val rich_edit_text : t -> area -> Edit.t -> Uchar.t

val button : t -> area -> ?protrude: bool -> modifier list * key -> bool -> bool option -> bool
val labeled_button : t -> area -> ?protrude: bool -> int -> color -> string -> modifier list * key -> bool -> bool option -> bool

val progress_bar : t -> area -> float -> float
val volume_bar : t -> area -> float -> float
val scroll_bar : t -> area -> Api.orientation -> float -> float -> float

val divider : t -> area -> Api.orientation -> int -> int -> int -> int

type order = [`Asc | `Desc]
type sorting = (int * order) list
type column = int * align
type cell = [`Text of string | `Image of Api.image]
type row = color * inversion * cell array
type heading = string array * sorting

val table : t -> area -> int -> int -> column array -> row array -> int ->
  int option
val header : t -> area -> int -> column array -> heading -> int ->
  [`Click of int | `Arrange | `None]

type cached

val rich_table :
  t -> 
  area ->
  int ->  (* gutter width *)
  int ->  (* row height *)
  int ->  (* vertical scroll bar width *)
  int ->  (* horizontal scroll bar height (can be 0) *)
  int ->  (* mouse reflection radius *)
  column array ->                    (* column layout *)
  heading option ->                  (* headers *)
  ('a, cached) Table.t ->            (* data *)
  (int -> color * cell array) ->     (* row generator *)
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
  ('a, cached) Table.t -> int option

val browser :
  t ->
  area ->
  int ->  (* row height *)
  int ->  (* vertical scroll bar width *)
  int ->  (* horizontal scroll bar height (can be 0) *)
  int ->  (* mouse reflection radius *)
  ('a, cached) Table.t ->                         (* data *)
  (int -> int * bool option * color * string) ->  (* entry generator *)
    [ `Click of int option
    | `Select
    | `Scroll
    | `Move of int
    | `Fold of int
    | `Drag of int * way
    | `Drop
    | `None
    ]

val grid : t -> area -> int -> int -> int ->
  (image * color * inversion * string) option array array -> (int * int) option

val grid_table :
  t ->
  area ->
  int ->    (* gutter width *)
  int ->    (* image width/height *)
  int ->    (* text height *)
  int ->    (* vertical scroll bar width *)
  int ->    (* mouse reflection radius *)
  heading option ->
  ('a, cached) Table.t ->
  (int -> Api.image * string) ->
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
