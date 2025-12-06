(* Tracks *)

open Audio_file
open Data


(* Names *)

val name_of_artist_title : string -> string -> string
val name_of_path : path -> string
val name_of_meta : path -> Meta.t -> string
val name : track -> string

val split_name : string -> string list

val time : track -> time


(* Conversion *)

val to_m3u_item : track -> M3u.item
val of_m3u_item : M3u.item -> track

val to_m3u : track array -> string
val of_m3u : string -> track array


(* Updating queue *)

val update : track -> unit
