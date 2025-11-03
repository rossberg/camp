(* Structured Values *)

(* De/serialisation *)

type t

exception Syntax_error of int
exception Type_error

val print : t -> string
val parse : string -> t  (* raises Syntax_error, Type_error *)


(* Typed Conversion *)

module Print :
sig
  type nonrec t = t

  val unit : unit -> t
  val bool : bool -> t
  val int : int -> t
  val nat : int -> t
  val num : int -> int -> int -> t
  val float : float -> t
  val interval : float -> float -> float -> t
  val string : string -> t
  val enum : (string * 'a) list -> 'a -> t
  val option : ('a -> t) -> 'a option -> t
  val list : ('a -> t) -> 'a list -> t
  val array : ('a -> t) -> 'a array -> t
  val iarray : ('a -> t) -> 'a iarray -> t
  val map : ('a -> t) -> (string * 'a) list -> t
  val tuple : ('a -> t list) -> 'a -> t
  val pair : ('a1 -> t) -> ('a2 -> t) -> 'a1 * 'a2 -> t
  val triple : ('a1 -> t) -> ('a2 -> t) -> ('a3 -> t) -> 'a1 * 'a2 * 'a3 -> t
  val record : ('a -> (string * t) list) -> 'a -> t
  val variant : ('a -> string * t) -> 'a -> t
  val transform : ('a -> t) -> ('b -> 'a) -> 'b -> t
  val (@@@) : t -> t -> t  (* raise Invalid_argument *)
end

module Parse :
sig
  type nonrec t = t

  val unit : t -> unit
  val bool : t -> bool
  val int : t -> int
  val nat : t -> int
  val num : int -> int -> t -> int
  val float : t -> float
  val interval : float -> float -> t -> float
  val string : t -> string
  val enum : (string * 'a) list -> t -> 'a
  val option : (t -> 'a) -> t -> 'a option
  val list : (t -> 'a) -> t -> 'a list
  val array : (t -> 'a) -> t -> 'a array
  val iarray : (t -> 'a) -> t -> 'a iarray
  val tuple : (t list -> 'a) -> t -> 'a
  val pair : (t -> 'a1) -> (t -> 'a2) -> t -> 'a1 * 'a2
  val triple : (t -> 'a1) -> (t -> 'a2) -> (t -> 'a3) -> t -> 'a1 * 'a2 * 'a3
  val map : (t -> 'a) -> t -> (string * 'a) list
  val record : ((string * t) list -> 'a) -> t -> 'a
  val ($) : (string * t) list -> string -> t
  val ($?) : (string * t) list -> string -> t option
  val variant : (string * t -> 'a) -> t -> 'a
  val (|||) : (t -> 'a) -> (t -> 'a) -> t -> 'a
  val (>->) : (t -> 'a) -> ('a -> 'b) -> t -> 'b
  val default : 'a -> (t -> 'a) -> t option -> 'a
  val apply : t option -> (t -> 'a) -> ('a -> unit) -> unit
end
