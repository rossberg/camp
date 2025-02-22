(* Playlist *)

type path = Data.path
type time = Data.time

type file =
{
  name : string;
  size : int;
  time : time;
  is_dir : bool;
}

type dir =
{
  path : path;
  nest : int;
  mutable folded : bool;
  mutable children : dir array;
  mutable files : file array;
}

type 'a t =
{
  mutable op : 'a option;
  mutable path : path;
  roots : dir array;
  dirs : dir Table.t;
  files : file Table.t;
  input : Edit.t;
  mutable columns : int array;
}


(* Constructor *)

val make : unit -> 'a t


(* Validation *)

type error = string

val ok : 'a t -> error list


(* Persistance *)

val to_map : 'a t -> Storage.map
val of_map : 'a t -> Storage.map -> unit

val to_map_extra : 'a t -> Storage.map


(* Focus *)

val defocus : 'a t -> unit
val focus_directories : 'a t -> unit
val focus_files : 'a t -> unit
val focus_input : 'a t -> unit


(* Refresh *)

val refresh_files : 'a t -> unit
val refresh_dirs : 'a t -> unit


(* Navigation *)

val selected_dir : 'a t -> int
val selected_file : 'a t -> int option
val select_dir : 'a t -> int -> unit
val select_file : 'a t -> int -> unit
val deselect_file : 'a t -> unit
val deselect_file_if_input_differs : 'a t -> unit

val set_dir_path : 'a t -> path -> unit

val fold_dir : 'a t -> dir -> bool -> unit
val reorder_files : 'a t -> int -> unit


(* Input *)

val reset : 'a t -> unit

val current_file_path : 'a t -> path option
val current_file_exists : 'a t -> bool
val current_sel_is_dir : 'a t -> bool


(* Formatting *)

val columns : 'a t -> (int * [> `Left | `Right]) array
val row : file -> string array
val headings : string array
