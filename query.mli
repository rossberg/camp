(* Queries *)

type key = [ Data.any_attr | `True | `False | `Now | `Random ]
type order = Asc | Desc

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

type query = {expr : expr; sort : (key * order) list}

val parse_expr : string -> (expr, string) result
val parse_query : string -> (query, string) result

val check : expr -> Data.track -> bool
val value : key -> Data.track -> value

val string_of_key : key -> string
val string_of_value : value -> string
val string_of_expr : expr -> string
val string_of_query : query -> string
