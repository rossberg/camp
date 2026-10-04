(* Tracks *)

open Audio_file
open Data


(* Cloning *)

val copy : track -> track
val copy_array : track array -> track array


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

val is_updated : track -> bool

val update : track -> unit
val update_if_undet : track -> unit

val queue_update : track -> unit
val queue_update_if_undet : track -> unit
val await_update : time -> track -> unit
