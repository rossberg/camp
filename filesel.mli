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

type ('op, 'cache) t =
{
  mutable op : 'op option;
  mutable path : path;
  roots : dir array;
  dirs : (dir, 'cache) Table.t;
  files : (file, 'cache) Table.t;
  input : Edit.t;
  mutable columns : int array;
}


(* Constructor *)

val make : unit -> ('a, 'b) t


(* Validation *)

type error = string

val ok : ('a, 'b) t -> error list


(* Persistance *)

val print_state : ('a, 'b) t -> Text.t
val print_intern : ('a, 'b) t -> Text.t
val parse_state : ('a, 'b) t -> Text.t -> unit


(* Focus *)

val defocus : ('a, 'b) t -> unit
val focus_directories : ('a, 'b) t -> unit
val focus_files : ('a, 'b) t -> unit
val focus_input : ('a, 'b) t -> unit


(* Refresh *)

val refresh_files : ('a, 'b) t -> unit
val refresh_dirs : ('a, 'b) t -> unit


(* Navigation *)

val selected_dir : ('a, 'b) t -> int
val selected_file : ('a, 'b) t -> int option
val select_dir : ('a, 'b) t -> int -> unit
val select_file : ('a, 'b) t -> int -> unit
val deselect_file : ('a, 'b) t -> unit
val deselect_file_if_input_differs : ('a, 'b) t -> unit

val set_dir_path : ('a, 'b) t -> path -> unit

val fold_dir : ('a, 'b) t -> dir -> bool -> unit
val reorder_files : ('a, 'b) t -> int -> unit


(* Input *)

val reset : ('a, 'b) t -> unit

val current_file_path : ('a, 'b) t -> path option
val current_file_exists : ('a, 'b) t -> bool
val current_sel_is_dir : ('a, 'b) t -> bool


(* Formatting *)

val columns : ('a, 'b) t -> (int * [> `Left | `Right]) array
val row : file -> string array
val headings : string array
