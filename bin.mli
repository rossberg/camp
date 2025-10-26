(* Binary Values *)

module Encode :
sig
  type buf

  val unit : unit -> buf -> unit
  val bool : bool -> buf -> unit
  val nat : int -> buf -> unit
  val nat8 : int -> buf -> unit
  val nat16 : int -> buf -> unit
  val nat32 : int -> buf -> unit
  val nat64 : int -> buf -> unit
  val int8 : int -> buf -> unit
  val int16 : int -> buf -> unit
  val int32 : int -> buf -> unit
  val int64 : int -> buf -> unit
  val float : float -> buf -> unit
  val string : string -> buf -> unit
  val enum : (int * 'a) list -> 'a -> buf -> unit
  val option : ('a -> buf -> unit) -> 'a option -> buf -> unit
  val list : ('a -> buf -> unit) -> 'a list -> buf -> unit
  val array : ('a -> buf -> unit) -> 'a array -> buf -> unit
  val iarray : ('a -> buf -> unit) -> 'a iarray -> buf -> unit
  val map : ('a -> buf -> unit) -> (string * 'a) list -> buf -> unit
  val tuple : ('a -> (buf -> unit) list) -> 'a -> buf -> unit
  val pair : ('a1 -> buf -> unit) -> ('a2 -> buf -> unit) -> 'a1 * 'a2 -> buf -> unit
  val triple : ('a1 -> buf -> unit) -> ('a2 -> buf -> unit) -> ('a3 -> buf -> unit) -> 'a1 * 'a2 * 'a3 -> buf -> unit
  val record : ('a -> (buf -> unit) list) -> 'a -> buf -> unit
  val variant : ('a -> int * (buf -> unit)) -> 'a -> buf -> unit
end

module Decode :
sig
  type buf

  exception Decode_error of int

  val error : buf -> 'a

  val unit : buf -> unit
  val bool : buf -> bool
  val nat : buf -> int
  val nat8 : buf -> int
  val nat16 : buf -> int
  val nat32 : buf -> int
  val nat64 : buf -> int
  val int8 : buf -> int
  val int16 : buf -> int
  val int32 : buf -> int
  val int64 : buf -> int
  val num : int -> int -> buf -> int
  val num8 : int -> int -> buf -> int
  val num16 : int -> int -> buf -> int
  val num32 : int -> int -> buf -> int
  val num64 : int -> int -> buf -> int
  val float : buf -> float
  val interval : float -> float -> buf -> float
  val string : buf -> string
  val enum : (int * 'a) list -> buf -> 'a
  val option : (buf -> 'a) -> buf -> 'a option
  val list : (buf -> 'a) -> buf -> 'a list
  val array : (buf -> 'a) -> buf -> 'a array
  val iarray : (buf -> 'a) -> buf -> 'a iarray
  val tuple : (buf -> 'a) -> buf -> 'a
  val pair : (buf -> 'a1) -> (buf -> 'a2) -> buf -> 'a1 * 'a2
  val triple : (buf -> 'a1) -> (buf -> 'a2) -> (buf -> 'a3) -> buf -> 'a1 * 'a2 * 'a3
  val map : (buf -> 'a) -> buf -> (string * 'a) list
  val record : (int -> buf -> 'a) -> buf -> 'a
  val variant : (int -> buf -> 'a) -> buf -> 'a
end

val encode : ('a -> Encode.buf -> unit) -> 'a -> string
val decode : (Decode.buf -> 'a) -> string -> 'a  (* raises Decode_error *)
