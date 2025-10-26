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

type 'cache t =
{
  mutable op : op option;
  mutable path : path;
  mutable roots : dir array;
  dirs : (dir, 'cache) Table.t;
  files : (file, 'cache) Table.t;
  input : Edit.t;
  mutable columns : int iarray;
}


(* Constructor *)

val make : unit -> 'a t


(* Validation *)

type error = string

val ok : 'a t -> error list


(* Persistence *)

val print_state : 'a t -> Text.t
val print_intern : 'a t -> Text.t
val parse_state : 'a t -> Text.t -> unit


(* Focus *)

val defocus : 'a t -> unit
val focus_directories : 'a t -> unit
val focus_files : 'a t -> unit
val focus_input : 'a t -> unit


(* Refresh *)

val refresh_files : 'a t -> unit
val refresh_dirs : 'a t -> unit
val refresh_roots : 'a t -> unit


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

val init : 'a t -> unit
val reset : 'a t -> unit

val current_file_path : 'a t -> path option
val current_file_exists : 'a t -> bool
val current_sel_is_dir : 'a t -> bool


(* Formatting *)

val columns : 'a t -> (int * [> `Left | `Right]) iarray
val row : file -> string iarray
val heading : string iarray * (int * [`Asc | `Desc]) list
