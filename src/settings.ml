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
  reflect_radius : Edit.t;
  spec_bands : Edit.t;
  exec_tag : Edit.t;
}


(* Constructor *)

let make () =
  {
    vscroll = 0;
    text_size = Edit.make 10;
    text_padding = Edit.make 10;
    text_gutter = Edit.make 10;
    grid_tracks = Edit.make 10;
    grid_albums = Edit.make 10;
    popup_size = Edit.make 10;
    scroll_width = Edit.make 10;
    reflect_radius = Edit.make 10;
    spec_bands = Edit.make 10;
    exec_tag = Edit.make 100;
  }


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]

let ok set =
  check "scroll in range" (set.vscroll >= 0) @
  []


(* Focus *)

let defocus _set = ()


(* Persistence *)

let print_state =
  let open Text.Print in
    record (fun _set -> [])

let print_intern set =
  let open Text.Print in
  print_state set @@@
  record (fun set -> [
    "vscroll", nat set.vscroll;
  ]) set

let parse_state _set = ignore
