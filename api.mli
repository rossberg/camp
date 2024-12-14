(* Graphics/sound API abstraction *)


(* Base types *)

type path = string
type time = float


(* Geometry helpers *)

type point = int * int
type size = int * int
type rect = int * int * int * int
type orientation = [`Horizontal | `Vertical]

val add : point -> point -> point
val sub : point -> point -> point

val inside : point -> rect -> bool


(* Window *)

type window

module Window :
sig
  val init : int -> int -> int -> int -> string -> window

  val pos : window -> point
  val size : window -> size
  val set_pos : window -> int -> int -> unit
  val set_size : window -> int -> int -> unit

  val screen_size : window -> size
end


(* Colors *)

type color =
[
  | `Black | `White
  | `Red | `Orange | `Yellow | `Green | `Blue
  | `Gray of int
  | `RGB of int
  | `Trans of color * int
]


(* Fonts *)

type font

module Font :
sig
  val load : window -> path -> int -> int -> font
end


(* Drawing *)

module Draw :
sig
  val start : window -> color -> unit
  val finish : window -> unit
  val frame : window -> int
  val clip : window -> rect option -> unit

  val fill : window -> int -> int -> int -> int -> color -> unit
  val rect : window -> int -> int -> int -> int -> color -> unit
  val fill_circ : window -> int -> int -> int -> int -> color -> unit
  val circ : window -> int -> int -> int -> int -> color -> unit
  val gradient : window -> int -> int -> int -> int -> color -> orientation -> color -> unit
  val text : window -> int -> int -> int -> color -> font -> string -> unit
  val text_width : window -> int -> font -> string -> int
end


(* Input devices *)

type side = [`Left | `Right]
type face = [`Up | `Down]
type dir = [side | face]
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
  | `Control of side
  | `Alt of side
  | `Caps
]

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
  val is_down : side -> bool
  val is_released : side -> bool

  val set_cursor : window -> cursor -> unit
end

module Key :
sig
  val is_down : key -> bool
  val is_released : key -> bool
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
end


(* Files *)

module File :
sig
  val dropped : window -> path list
end
