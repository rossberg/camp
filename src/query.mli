(* Queries *)

type key = Data.query_attr
type track = Data.track
type album = Data.album
type artist = Data.artist
type order = Data.order
type sorting = Data.track_attr Data.sorting

type fnop = Min | Max | Avg | If
type unop = Not | Neg
type binop =
  | And | Or | EQ | NE | LT | GT | LE | GE | IN | NI | Add | Sub | Mul | Cat
type expr =
  | Text of string
  | Int of int * string
  | Time of Data.time * string
  | Date of Data.date * string
  | Key of key
  | Fn of fnop * expr list
  | Un of unop * expr
  | Bin of binop * expr * expr

type value =
  | BoolV of bool
  | IntV of int * string option
  | TimeV of Data.time * string option
  | DateV of Data.date * string option
  | TextV of string

type query = {expr : expr; sort : sorting}

val empty_query : query
val full_query : query

val quote : string -> string

val parse_expr : string -> (expr, string) result
val parse_query : string -> (query, string) result

val value : key -> track -> value
val check : expr -> track -> bool
val sort : sorting -> track array -> unit

val exec : query -> (track -> bool * bool * bool) -> 'a Data.dir ->
  artist array * album array * track array

val string_of_key : key -> string
val string_of_value : value -> string
val string_of_expr : expr -> string
val string_of_query : query -> string

module AlbumKey :
sig
  type t = string * string * string * string
  val compare : t -> t -> int
end
module AlbumSet : module type of Set.Make(AlbumKey)
module AlbumMap : module type of Map.Make(AlbumKey)

val album_key : Data.album -> AlbumKey.t
val track_album_key : Data.track -> AlbumKey.t
