(* Immediate-style GUI widgets *)

open Api


(* State *)

type t

val make : window -> t
val window : t -> window

val buffered : t -> bool -> unit
val is_buffered : t -> bool

val modal : t -> string -> unit
val nonmodal : t -> string -> unit
val is_modal : t -> bool
val modal_rect : t -> string -> rect option -> unit
val has_modal_rect : t -> bool
val except_modal : t -> string -> (unit -> 'a) -> 'a

(* Snapping *)

val snap : int -> int -> int -> int

(* Panes *)

type pane
type owner = string

val pane : t -> owner -> rect -> pane
val popup :
  t -> owner -> rect -> int -> bool * bool * bool -> bool ->
  pane * (rect * (bool * bool * bool * bool)) option

val popup_rect : t -> rect -> int -> rect


(* Areas *)

type area = pane * int * int * int * int

val dim : t -> area -> rect
val mouse_inside : t -> area -> bool

(* Colors *)

type color = Api.color

val num_palette : t -> int
val get_palette : t -> int
val set_palette : t -> int -> unit
val name_palette : t -> int -> string

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
val mouse : t -> owner -> area -> side -> bool
val drag : t -> owner -> area -> size ->
  [`None | `Click | `Take | `Drag of size * motion * trajectory | `Drop | `Abort]
val wheel : t -> area -> float * float

(* Focus *)

val focus : t -> area -> int -> unit
val mouse_focus : t -> area -> int -> int -> int -> unit

(* Window *)

val reset : t -> rect -> unit
val start : t -> rect -> unit
val finish : t -> int -> bool * bool ->
  rect * (bool * bool * bool * bool) * screen option

val rescale : t -> size -> unit
val pin : t -> screen -> unit

val delay : t -> (unit -> unit) -> unit

val resize_repos : t -> point -> size -> size

(* Simple Widgets *)

type align = [`Left | `Center | `Right]
type inversion = [`Regular | `Inverted]
type adjustment = [`Crop of orientation | `Shrink]

val label : t -> area -> align -> string -> unit
val indicator : t -> color -> area -> bool -> unit
val lcd : t -> area -> char -> unit
val image : t -> area -> adjustment -> Api.image -> unit
val image_size : t -> size -> adjustment -> Api.image -> size

val box : t -> area -> color -> unit
val text : t -> area -> align -> inversion -> bool -> string -> unit
val color_text : t -> area -> align -> color -> inversion -> bool -> string -> unit
val ticker : t -> area -> string -> unit

(*
val edit_text : t -> owner -> area -> int -> string -> int -> (int * int * int) option -> color -> bool -> string * int * (int * int * int) option * Uchar.t
*)
val rich_edit_text : t -> owner -> area -> int -> bool -> color -> Edit.t -> Uchar.t

val button : t -> owner -> area -> ?protrude: bool -> modifier list * key -> bool -> bool option -> bool
val labeled_button : t -> owner -> area -> ?protrude: bool -> int -> color -> string -> modifier list * key -> bool -> bool option -> bool
val invisible_button : t -> owner -> area -> modifier list -> modifier list * key -> bool -> bool

val progress_bar : t -> owner -> area -> int -> (float -> string * int * color) option -> float -> float
val volume_bar : t -> owner -> area -> int -> float -> float
val scroll_bar : t -> owner -> area -> int -> Api.orientation -> float -> float -> float

val divider : t -> owner -> area -> Api.orientation -> int -> int -> int -> int * bool
val divider2 : t -> owner -> area -> Api.resize -> size -> size -> size -> size -> size -> size * bool

(* Table *)

type order = [`Asc | `Desc]
type sorting = (int * order) list
type column = int * align
type cell = [`Text of string | `Image of Api.image]
type row = color * inversion * cell iarray
type heading = string iarray * sorting

(*
val table : t -> area -> string -> int -> int -> int -> column iarray -> row iarray -> int ->
  int option * int option
val header : t -> area -> string -> int -> int -> column iarray -> heading -> int ->
  [`Click of int | `Resize of int iarray | `Reorder of int iarray | `Menu of int option | `None]
*)

type cached

type rich_table_style =
  { gutter_w : int;
    text_h : int;
    pad_h : int;
    scroll_w : int;
    scroll_h : int;
    scroll_l : int;
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
  owner ->
  area ->
  rich_table_style ->
  column iarray ->                 (* column layout *)
  heading option ->                (* headers (None if has_heading = false) *)
  ('a, cached) Table.t ->          (* data *)
  (int -> color * cell iarray) ->  (* row generator *)
    rich_table_action

val rich_table_inner_area : t -> area -> rich_table_style -> area
val rich_table_mouse : t -> area -> rich_table_style -> column iarray ->
  ('a, cached) Table.t -> (int option * int option) option
val rich_table_drag : t -> area -> rich_table_style -> [`Above | `Inside] ->
  ('a, cached) Table.t -> unit

(* Browser *)

type browser_action =
  [ table_action
  | `Fold of int
  ]

val browser :
  t ->
  owner ->
  area ->
  rich_table_style ->  (* gutter_w unused *)
  ('a, cached) Table.t ->                         (* data *)
  (int -> int * bool option * color * string) ->  (* entry generator *)
    browser_action

val browser_entry_text_area :
  t -> area -> rich_table_style -> ('a, cached) Table.t -> int -> int ->
  bool option -> area

(* Grid *)

(*
val grid :
  t -> area -> string -> int -> int -> int -> int ->
  (image * color * inversion * string) option iarray iarray ->
    (int * int) option
*)

type grid_table_style =
  { gutter_w : int;
    img_h : int;
    text_h : int;
    pad_h : int;
    scroll_w : int;
    scroll_l : int;
    refl_r : int;
    has_heading : bool
  }

type grid_table_action = rich_table_action

val grid_table :
  t ->
  owner ->
  area ->
  grid_table_style ->
  heading option ->  (* None if has_heading = false*)
  ('a, cached) Table.t ->
  (int -> Api.image * color * string) ->
    grid_table_action

val grid_table_inner_area : t -> area -> grid_table_style -> area
val grid_table_mouse : t -> area -> grid_table_style ->
  ('a, cached) Table.t -> (int option * int option) option
val grid_table_drag : t -> area -> grid_table_style -> [`Left | `Inside] ->
  ('a, cached) Table.t -> unit


(* Settings *)

type setting = string * setting_item
and setting_item =
  [ `Flag of bool * (bool -> unit)
  | `Choice of (string * bool * (string -> unit)) list
  | `Text of Edit.t * color * (Edit.t -> unit) * (string -> unit)
  | `Number of string * Edit.t * int * int * int * (Edit.t -> unit) * (int -> unit)
  | `Button of string * (unit -> unit)
  | `Section of setting list
  ]

type settings_style =
  { margin : int;
    item_h : int;     (* size for buttons, indicators, edit fields etc. *)
    label_h : int;    (* text size for choice labels *)
    pad_w : int;      (* padding between button and label or indicator *)
    pad_h : int;      (* padding between lines *)
    sep_h : int;      (* space between sections *)
    sep_w : int;      (* space between label and item column *)
    indent_w : int;   (* indentation width for section items *)
    scroll_w : int;   (* scrollbar width *)
    scroll_l : int;   (* scrollbar line width *)
  }

val settings :
  t -> owner -> area -> settings_style -> int -> bool -> setting list -> int


(* Menus *)

type menu_style =
  { margin : int;
    gutter_w : int;
    text_h : int;
    pad_h : int;
    scroll_w : int;
    scroll_h : int;
    scroll_l : int;
    refl_r : int;
  }

type menu_entry =
  [`Separator | `Entry of color * string * (modifier list * key) * bool]

val menu : t -> int -> int -> menu_style -> int -> int -> menu_entry iarray ->
  [`None | `Close | `Click of int | `Scroll of int * int]
