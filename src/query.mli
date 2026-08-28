(* Queries *)

type key = Data.any_attr
type track = Data.track
type album = Data.album
type artist = Data.artist
type order = Data.order
type 'a sorting = 'a Data.sorting

type fnop = True | False | Now | Random | Min | Max | Avg | If | Id
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

type type_ =
  | BoolT
  | TextT
  | IntT
  | TimeT
  | DateT

type query = {expr : expr; sort : Data.track_attr sorting}

val empty_query : query
val full_query : query

val quote : string -> string

val parse_query : string -> (query, string) result
val parse_expr : ('x, 'a) Data.kind -> string -> (expr * type_, string) result
val parse_custom : ('x, 'a) Data.kind -> string -> (expr list * type_, string) result

val value : ('x, 'a) Data.kind -> 'a -> 'x -> value
val check : ('x, 'a) Data.kind -> expr -> 'x -> bool
val sort : ('x, 'a) Data.kind -> 'a sorting -> 'x array -> unit

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

val album_key : album -> AlbumKey.t
val track_album_key : track -> AlbumKey.t

val artist_attr_ex_string : artist -> Data.artist_attr_ex -> string
val album_attr_ex_string : album -> Data.album_attr_ex -> string
val track_attr_ex_string : track -> Data.track_attr_ex -> string
val any_attr_ex_type : Data.any_attr_ex -> type_ option
