(* Tracks *)

open Audio_file
open Data


(* Properties *)

val is_separator : track -> bool
val is_invalid : track -> bool


(* Names *)

val name_of_artist_title : string -> string -> string
val name_of_path : path -> string
val name_of_meta : path -> Meta.t -> string
val name : track -> string

val artist_title_of_name : string -> (string * string) option
val artist_title_of_path : path -> (string * string) option

val time : track -> time


(* Conversion *)

val to_m3u_item : track -> M3u.item
val of_m3u_item : M3u.item -> track

val to_m3u : track array -> string
val of_m3u : string -> track array


(* Updating queue *)

val update : track -> unit
