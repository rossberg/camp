(* Queries *)

type key = Data.query_attr
type track = Data.track
type order = Data.order
type sorting = Data.track_attr Data.sorting

type unop = Not | Neg
type binop =
  And | Or | EQ | NE | LT | GT | LE | GE | IN | NI | Add | Sub | Mul | Cat
type expr =
  | Text of string
  | Int of int * string
  | Time of Data.time * string
  | Date of Data.date
  | Key of key
  | Un of unop * expr
  | Bin of binop * expr * expr

type value =
  | BoolV of bool
  | IntV of int
  | TimeV of Data.time
  | DateV of Data.date
  | TextV of string

type query = {expr : expr; sort : sorting}

val empty_query : query
val full_query : query

val parse_expr : string -> (expr, string) result
val parse_query : string -> (query, string) result

val value : key -> track -> value
val check : expr -> track -> bool
val exec : query -> (track -> bool) -> 'a Data.dir -> track array
val sort : sorting -> track array -> unit

val string_of_key : key -> string
val string_of_value : value -> string
val string_of_expr : expr -> string
val string_of_query : query -> string
