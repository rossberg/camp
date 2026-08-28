(* Pop-up Menu *)

type menu =
{
  mutable hscroll : int;
  mutable vscroll : int;
  items : Ui.menu_entry iarray;
  op : int -> unit;
}

type cover =
  | Current
  | Track of Data.track
  | Album of Data.album

type custom =
{
  name : Edit.t;
  expr : Edit.t;
  valid : string -> bool;
  ok : string -> string -> unit;
}

type t = private
{
  mutable kind : [`Menu of menu | `Cover of cover | `Custom of custom] option;
}


(* Constructor *)

val make : unit -> t

val clear : t -> unit
val set_menu : t -> Ui.menu_entry iarray -> (int -> unit) -> unit
val set_cover : t -> cover -> unit
val set_custom : t -> string -> string ->
  (string -> bool) -> (string -> string -> unit) -> unit


(* Validation *)

type error = string

val ok : t -> error list


(* Focus *)

val defocus : t -> unit
val foci : t -> Edit.t list


(* Persistence *)

val print_state : t -> Text.t
val print_intern : t -> Text.t
val parse_state : t -> Text.t -> unit
