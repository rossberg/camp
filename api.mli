(* Graphics/sound API abstraction *)


(* Base types *)

type path = string
type time = float


(* General stuff *)

val is_mac : bool


(* Geometry helpers *)

type point = int * int
type size = int * int
type rect = int * int * int * int

type side = [`Left | `Right]
type face = [`Up | `Down]
type dir = [side | face]
type orientation = [`Horizontal | `Vertical]
type corner = [`NW | `NE | `SW | `SE]

val add : point -> point -> point
val sub : point -> point -> point
val mul : point -> point -> point

val inside : point -> rect -> bool


(* Window *)

type window
type icon

module Window :
sig
  val init : int -> int -> int -> int -> string -> window

  val closed : window -> bool

  val pos : window -> point
  val size : window -> size
  val set_pos : window -> int -> int -> unit
  val set_size : window -> int -> int -> unit
  val set_icon : window -> icon -> unit

  val min_pos : window -> point
  val max_size : window -> size

  val minimize : window -> unit
  val restore : window -> unit
  val is_minimized : window -> bool

  val hide : window -> unit
  val reveal : window -> unit
  val is_hidden : window -> bool

  val screen_size : window -> size
  val is_hires : window -> bool
  val scale : window -> int * int
  val rescale : window -> int -> int -> unit
  val fps : window -> int
end


(* Colors *)

type color =
[
  | `Blank | `Black | `White
  | `Red | `Orange | `Yellow | `Green | `Blue
  | `Gray of int
  | `RGB of int
  | `Trans of color * int
]

module Color :
sig
  val darken : int -> color -> color
end


(* Fonts *)

type font

module Font :
sig
  val load : window -> path -> int -> int -> int -> bool -> font
end


(* Image *)

type image

module Image :
sig
  type raw = icon
  type prepared = image

  val load : window -> path -> image
  val load_from_memory : window -> string (* mime *) -> string -> image

  val load_raw : path -> raw
  val load_raw_from_memory : string -> string -> raw

  val extract : raw -> int -> int -> int -> int -> raw
  val prepare : window -> raw -> image

  val size : image -> size
end


(* Drawing *)

type buffer

module Buffer :
sig
  val create : window -> int -> int -> buffer
  val dispose : buffer -> unit
  val size : buffer -> size
  val scale : buffer -> size
  val needed_scale : window -> size
end

module Draw :
sig
  val start : window -> color -> unit
  val finish : window -> unit

  val clip : window -> int -> int -> int -> int -> unit
  val unclip : window -> unit

  val buffered : window -> buffer -> unit
  val unbuffered : window -> unit

  val frame : window -> int

  val fill : window -> int -> int -> int -> int -> color -> unit
  val rect : window -> int -> int -> int -> int -> color -> unit
  val fill_circ : window -> int -> int -> int -> int -> color -> unit
  val circ : window -> int -> int -> int -> int -> color -> unit
  val tri : window -> int -> int -> int -> int -> int -> int -> color -> unit
  val arrow : window -> int -> int -> int -> int -> color -> dir -> unit
  val gradient : window -> int -> int -> int -> int -> color -> orientation -> color -> unit
  val gradient_circ : window -> int -> int -> int -> int -> color -> color -> unit
  val text : window -> int -> int -> int -> color -> font -> string -> unit
  val text_width : window -> int -> font -> string -> int
  val text_spacing : window -> int -> font -> int
  val image : window -> int -> int -> float -> image -> unit
  val image_part : window -> int -> int -> int -> int -> int -> int -> int -> int -> image -> unit
  val buffer : window -> int -> int -> buffer -> unit
end


(* Input devices *)

type key =
[
  | `None
  | `Char of char
  | `Arrow of dir
  | `Page of face
  | `End of face
  | `Return
  | `Enter
  | `Tab
  | `Escape
  | `Backspace
  | `Delete
  | `Insert
  | `F of int
  | `Shift of side
  | `Command of side
  | `Alt of side
  | `Caps
]

type modifier = [`Shift | `Command | `Alt]

type resize = [`N_S | `E_W | `NE_SW | `NW_SE | `All]
type cursor =
[
  | `Default
  | `Arrow
  | `Busy
  | `Blocked
  | `Beam
  | `Crosshair
  | `Point
  | `Resize of resize
]

module Mouse :
sig
  val pos : window -> point
  val delta : window -> point
  val screen_pos : window -> point
  val screen_delta : window -> point
  val wheel : window -> float * float
  val is_down : side -> bool
  val is_pressed : side -> bool
  val is_released : side -> bool
  val is_doubleclick : side -> bool
  val is_tripleclick : side -> bool
  val is_drag : side -> bool

  val set_cursor : window -> cursor -> unit
end

module Key :
sig
  val is_down : key -> bool
  val is_pressed : key -> bool
  val is_repeated : key -> bool
  val is_pressed_or_repeated : key -> bool
  val is_released : key -> bool
  val is_modifier_down : modifier -> bool
  val are_modifiers_down : modifier list -> bool

  val char : unit -> Uchar.t

  val key_name : key -> string
  val modifier_name : modifier -> string
end


(* Audio *)

type audio
type sound

module Audio :
sig
  val init : unit -> audio

  val silence : audio -> sound
  val load : audio -> path -> sound  (* returns silence on error *)
  val free : audio -> sound -> unit

  val play : audio -> sound -> unit
  val stop : audio -> unit
  val pause : audio -> unit
  val resume : audio -> unit
  val seek : audio -> time -> unit
  val volume : audio -> float -> unit

  val length : audio -> time
  val played : audio -> time
  val is_playing : audio -> bool

  val channels : audio -> sound -> int
  val depth : audio -> sound -> int
  val rate : audio -> sound -> int
  val bitrate : audio -> sound -> float
end


(* Files *)

module Files :
sig
  val dropped : window -> path list
end


(* Clipboard *)

module Clipboard :
sig
  val read : window -> string option
  val write : window -> string -> unit
end
