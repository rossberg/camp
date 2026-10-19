(* File Selector *)

type path = Data.path
type time = Data.time

type file = private
{
  name : string;
  size : int;
  time : time;
  is_dir : bool;
  accessible : bool;
}

type dir = private
{
  path : path;
  nest : int;
  accessible : bool;
  mutable folded : bool;
  mutable children : dir array;
  mutable files : file array;
}

type op =
{
  kind : [`File | `Dir];
  access : [`Write | `Read];
  run : path -> unit;
}

type t =
{
  mutable op : op option;
  mutable path : path;
  mutable roots : dir array;
  dirs : (dir, Cache.t) Table.t;
  files : (file, Cache.t) Table.t;
  input : Edit.t;
  mutable columns : int iarray;
}


(* Constructor *)

val make : unit -> t


(* Validation *)

type error = string

val ok : t -> error list


(* Persistence *)

val print_state : t -> Text.t
val print_intern : t -> Text.t
val parse_state : t -> Text.t -> unit


(* Focus *)

val defocus : t -> unit
val focus_directories : t -> unit
val focus_files : t -> unit
val focus_input : t -> unit


(* Refresh *)

val refresh_files : t -> unit
val refresh_dirs : t -> unit
val refresh_roots : t -> unit


(* Navigation *)

val selected_dir : t -> int
val selected_file : t -> int option
val select_dir : t -> int -> unit
val select_file : t -> int -> unit
val deselect_file : t -> unit
val deselect_file_if_input_differs : t -> unit

val set_dir_path : t -> path -> unit

val fold_dir : t -> dir -> bool -> unit
val reorder_files : t -> int -> unit


(* Input *)

val init : t -> unit
val reset : t -> unit

val current_file_path : t -> path option
val current_file_exists : t -> bool
val current_sel_is_dir : t -> bool


(* Formatting *)

val columns : t -> (int * [> `Left | `Right]) iarray
val row : file -> string iarray
val heading : string iarray * (int * [`Asc | `Desc]) list
