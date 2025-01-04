(* Graphics/sound API abstraction *)


(* Base types *)

type path = string
type time = float


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

  val pump : window -> unit
  val closed : window -> bool

  val pos : window -> point
  val size : window -> size
  val set_pos : window -> int -> int -> unit
  val set_size : window -> int -> int -> unit
  val set_icon : window -> icon -> unit

  val minimize : window -> unit
  val restore : window -> unit
  val is_minimized : window -> bool

  val screen_size : window -> size
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


(* Fonts *)

type font

module Font :
sig
  val load : window -> path -> int -> int -> int -> font
end


(* Image *)

type image

module Image :
sig
  type raw = icon
  type prepared = image

  val load : window -> path -> image

  val load_raw : path -> raw
  val extract : raw -> int -> int -> int -> int -> raw
  val prepare : window -> raw -> image

  val size : image -> size
end


(* Drawing *)

module Draw :
sig
  val start : window -> color -> unit
  val finish : window -> unit

  val clip : window -> int -> int -> int -> int -> unit
  val unclip : window -> unit

  val frame : window -> int

  val line : window -> int -> int -> int -> int -> color -> unit
  val fill : window -> int -> int -> int -> int -> color -> unit
  val rect : window -> int -> int -> int -> int -> color -> unit
  val fill_circ : window -> int -> int -> int -> int -> color -> unit
  val circ : window -> int -> int -> int -> int -> color -> unit
  val tri : window -> int -> int -> int -> int -> color -> corner -> unit
  val arrow : window -> int -> int -> int -> int -> color -> dir -> unit
  val gradient : window -> int -> int -> int -> int -> color -> orientation -> color -> unit
  val text : window -> int -> int -> int -> color -> font -> string -> unit
  val text_width : window -> int -> font -> string -> int
  val image : window -> int -> int -> int -> image -> unit
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
  | `Resize of resize
  | `Link
]

module Mouse :
sig
  val pos : window -> point
  val delta : window -> point
  val wheel : window -> float * float
  val is_down : side -> bool
  val is_pressed : side -> bool
  val is_released : side -> bool
  val is_doubleclick : side -> bool

  val set_cursor : window -> cursor -> unit
end

module Key :
sig
  val is_down : key -> bool
  val is_pressed : key -> bool
  val is_repeated : key -> bool
  val is_released : key -> bool
  val is_modifier_down : modifier -> bool
  val are_modifiers_down : modifier list -> bool
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
  val stop : audio -> sound -> unit
  val pause : audio -> sound -> unit
  val resume : audio -> sound -> unit
  val volume : audio -> sound -> float -> unit
  val is_playing : audio -> sound -> bool
  val length : audio -> sound -> time
  val played : audio -> sound -> time
  val seek : audio -> sound -> time -> unit

  val channels : audio -> sound -> int
  val depth : audio -> sound -> int
  val rate : audio -> sound -> int
  val bitrate : audio -> sound -> float
end


(* Files *)

module File :
sig
  val dropped : window -> path list
end


(* Clipboard *)

module Clipboard :
sig
  val read : window -> string option
  val write : window -> string -> unit
end
