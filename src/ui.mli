(* Immediate-style GUI widgets *)

open Api


(* State *)

type t

val make : window -> t
val window : t -> window

val buffered : t -> bool -> unit
val is_buffered : t -> bool

val modal : t -> unit
val nonmodal : t -> unit
val is_modal : t -> bool

(* Panes *)

type pane = int

val pane : t -> pane -> rect -> unit

(* Areas *)

type area = pane * int * int * int * int

val dim : t -> area -> rect
val mouse_inside : t -> area -> bool

(* Colors *)

type color = Api.color

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

val font_sdf : t -> bool -> unit
val font_is_sdf : t -> bool

(* Images *)

val nocover : t -> Api.image

(* Input *)

type motion = [`Unmoved | `Moving | `Moved]
type trajectory = [`Inside | `Outside | `Outward | `Inward]

val key : t -> modifier list * key -> bool -> bool
val mouse : t -> area -> side -> bool
val drag : t -> area -> size ->
  [`None | `Click | `Take | `Drag of size * motion * trajectory | `Drop | `Abort]
val wheel : t -> area -> float * float

(* Focus *)

val focus : t -> area -> unit
val mouse_focus : t -> area -> int -> int -> int -> unit

(* Window *)

val start : t -> unit
val finish : t -> int -> size -> size -> (Api.screen -> unit) -> unit

val resize : t -> point -> size -> unit
val rescale : t -> size -> unit
val pin : t -> screen -> unit

val delay : t -> (unit -> unit) -> unit

(* Simple Widgets *)

type align = [`Left | `Center | `Right]
type inversion = [`Regular | `Inverted]
type adjustment = [`Crop of orientation | `Shrink]

val label : t -> area -> align -> string -> unit
val indicator : t -> color -> area -> bool -> unit
val lcd : t -> area -> char -> unit
val image : t -> area -> adjustment -> Api.image -> unit
val image_size : t -> area -> adjustment -> Api.image -> int * int

val box : t -> area -> color -> unit
val text : t -> area -> align -> inversion -> bool -> string -> unit
val color_text : t -> area -> align -> color -> inversion -> bool -> string -> unit
val ticker : t -> area -> string -> unit

val edit_text : t -> area -> int -> string -> int -> (int * int * int) option -> color -> bool -> string * int * (int * int * int) option * Uchar.t
val rich_edit_text : t -> area -> int -> color -> Edit.t -> Uchar.t

val button : t -> area -> ?protrude: bool -> modifier list * key -> bool -> bool option -> bool
val labeled_button : t -> area -> ?protrude: bool -> int -> color -> string -> modifier list * key -> bool -> bool option -> bool
val invisible_button : t -> area -> modifier list -> modifier list * key -> bool -> bool

val progress_bar : t -> area -> float -> float
val volume_bar : t -> area -> float -> float
val scroll_bar : t -> area -> Api.orientation -> float -> float -> float

val divider : t -> area -> Api.orientation -> int -> int -> int -> int

(* Table *)

type order = [`Asc | `Desc]
type sorting = (int * order) list
type column = int * align
type cell = [`Text of string | `Image of Api.image]
type row = color * inversion * cell iarray
type heading = string iarray * sorting

val table : t -> area -> int -> int -> int -> column iarray -> row iarray -> int ->
  int option * int option
val header : t -> area -> int -> int -> column iarray -> heading -> int ->
  [`Click of int | `Resize of int iarray | `Reorder of int iarray | `Menu of int option | `None]

type cached

type rich_table =
  { gutter_w : int;
    text_h : int;
    pad_h : int;
    scroll_w : int ;
    scroll_h : int;
    refl_r : int;
    has_heading : bool
  }

type table_action =
  [ `Click of int option * int option   (* row, column *)
  | `Select
  | `Scroll
  | `Move of int                        (* delta *)
  | `Drag of int * motion * trajectory  (* delta, motion, trajectory *)
  | `Drop
  | `Abort
  | `Menu of int option * int option    (* row, column *)
  | `None
  ]

type rich_table_action =
  [ table_action
  | `Sort of int             (* column *)
  | `Resize of int iarray    (* new sizes *)
  | `Reorder of int iarray   (* permutation *)
  | `HeadMenu of int option  (* column *)
  ]

val rich_table :
  t -> 
  area ->
  rich_table ->
  column iarray ->                 (* column layout *)
  heading option ->                (* headers (None if has_heading = false) *)
  ('a, cached) Table.t ->          (* data *)
  (int -> color * cell iarray) ->  (* row generator *)
    rich_table_action

val rich_table_inner_area : t -> area -> rich_table -> area
val rich_table_mouse : t -> area -> rich_table -> column iarray ->
  ('a, cached) Table.t -> (int option * int option) option
val rich_table_drag : t -> area -> rich_table -> [`Above | `Inside] ->
  ('a, cached) Table.t -> unit

type browser_action =
  [ table_action
  | `Fold of int
  ]

(* Browser *)

val browser :
  t ->
  area ->
  rich_table ->  (* gutter_w unused *)
  ('a, cached) Table.t ->                         (* data *)
  (int -> int * bool option * color * string) ->  (* entry generator *)
    browser_action

val browser_entry_text_area :
  t -> area -> rich_table -> ('a, cached) Table.t -> int -> int ->
  bool option -> area

(* Grid *)

val grid :
  t -> area -> int -> int -> int -> int ->
  (image * color * inversion * string) option iarray iarray ->
    (int * int) option

type grid_table =
  { gutter_w : int;
    img_h : int;
    text_h : int;
    pad_h : int;
    scroll_w : int ;
    refl_r : int;
    has_heading : bool
  }

type grid_table_action = rich_table_action

val grid_table :
  t ->
  area ->
  grid_table ->
  heading option ->  (* None if has_heading = false*)
  ('a, cached) Table.t ->
  (int -> Api.image * color * string) ->
    grid_table_action

val grid_table_inner_area : t -> area -> grid_table -> area
val grid_table_mouse : t -> area -> grid_table ->
  ('a, cached) Table.t -> (int option * int option) option
val grid_table_drag : t -> area -> grid_table -> [`Left | `Inside] ->
  ('a, cached) Table.t -> unit

(* Pop-ups *)

val popup : t -> int -> int -> int -> int -> int -> area

type menu_entry =
  [`Separator | `Entry of color * string * (modifier list * key) * bool]

val menu : t -> int -> int -> int -> int -> int -> int -> menu_entry iarray ->
  [`None | `Close | `Click of int]
