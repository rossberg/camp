(* Types *)

type name = string
type path = string
type dir = path
type drive = path
type time = float

(* Paths *)

val current : string
val parent : string
val sep : string

val (//) : path -> path -> path

val drive : path -> drive
val dir : path -> path
val name : path -> name
val extension : path -> path
val temp : dir option -> string -> string -> path

val remove_extension : path -> path
val remove_drive : path -> path

val explode : path -> name list
val implode : name list -> path

val is_relative : path -> bool
val make_relative : dir -> path -> path
val make_resolvable : dir -> path -> path
val resolve : dir -> path -> path

val is_url : path -> bool
val is_proper : path -> bool

(* Attributes *)

val exists : path -> bool
val is_dir : path -> bool

val stat : path -> Unix.stats
val size : path -> int
val time : path -> time
val set_time : path -> time -> unit

val localtime : time -> Unix.tm

(* Files *)

type mode = [`Bin | `Text]

val check : path -> unit
val open_in : mode -> path -> in_channel
val open_out : mode -> path -> out_channel
val open_append : mode -> path -> out_channel
val with_open_in : mode -> path -> (in_channel -> 'a) -> 'a
val with_open_out : mode -> path -> (out_channel -> 'a) -> 'a
val with_open_append : mode -> path -> (out_channel -> 'a) -> 'a

val load : mode -> path -> string
val store : mode -> path -> string -> unit
val copy : path -> path -> unit
val delete : path -> unit

val make_writable : path -> unit

(* Directories *)

val current_dir : unit -> dir
val change_dir : dir -> unit

val check_dir : dir -> unit
val exists_dir : dir -> bool
val create_dir : dir -> unit
val delete_dir : dir -> unit
val read_dir : dir -> name array
