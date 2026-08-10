(* Settings *)

type t =
{
  mutable vscroll : int;
  text_size : Edit.t;
  text_padding : Edit.t;
  text_gutter : Edit.t;
  grid_tracks : Edit.t;
  grid_albums : Edit.t;
  popup_size : Edit.t;
  scroll_width : Edit.t;
  spec_bands : Edit.t;
  exec_tag : Edit.t;
  exec_tag_flags : Edit.t;
}


(* Constructor *)

val make : unit -> t


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
