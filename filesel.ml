(* Playlist *)

open Audio_file

type path = File.path
type time = File.time

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

type ('a, 'b) t =
{
  mutable op : 'a option;
  mutable path : path;
  roots : dir array;
  dirs : (dir, 'b) Table.t;
  files : (file, 'b) Table.t;
  input : Edit.t;
  mutable columns : int array;
}


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]

let ok fs =
  check "one directory selected" (Table.num_selected fs.dirs = 1) @
  check "at most one directory selected" (Table.num_selected fs.files <= 1) @
  check "dir selection consistent with path"
    ( fs.dirs.entries.(Option.get (Table.first_selected fs.dirs)).path = fs.path ) @
  check "file selection empty or consistent with input"
    ( match Table.first_selected fs.files with
      | None -> true
      | Some i -> fs.files.entries.(i).is_dir || fs.files.entries.(i).name = fs.input.text ) @
  []


(* Refresh *)

let refresh_files fs =
  let i = Option.get (Table.first_selected fs.dirs) in
  let dir = fs.dirs.entries.(i) in
  Table.deselect_all fs.files;
  Table.set fs.files dir.files;
  Table.set_vscroll fs.files 0 4

let refresh_dirs fs =
  let rec entries (dirs : dir array) i acc =
    if i = Array.length dirs then acc else
    let dir = dirs.(i) in
    let acc' = entries dirs (i + 1) acc in
    dir :: (if dir.folded then acc' else entries dir.children 0 acc')
  in
  let selection = Table.save_selection fs.dirs in
  Table.set fs.dirs (Array.of_list (entries fs.roots 0 []));
  Table.restore_selection fs.dirs selection (fun dir -> dir.path);
  Table.adjust_vscroll fs.files
    (Option.value (Table.first_selected fs.files) ~default: 0) 4


(* Constructor *)

let make_file path name =
  try
    let st = File.stat path in
    let is_dir = (st.st_kind = Unix.S_DIR) in
    Unix.access path (if is_dir then Unix.[R_OK; X_OK] else Unix.[R_OK]);
    Some {
      name;
      size = st.st_size;
      time = st.st_mtime;
      is_dir;
    }
  with _ -> None

let make_dir path nest =
  {
    path;
    nest;
    folded = true;
    children = [||];
    files = [||];
  }

let roots () =
  if not Sys.win32 then [make_dir File.sep 0] else
  let rec detect c =
    if c > 'Z' then [] else
    let drive = String.make 1 c ^ ":\\" in
    let roots' = detect (Char.chr (Char.code c + 1)) in
    if File.exists drive then
      (make_dir drive 0)::roots'
    else
      roots'
  in detect 'A'

let dir_of_file path nest file =
  make_dir File.(path // file.name) nest

let populate_dir (dir : dir) =
  if dir.files = [||] && dir.children = [||] then
  try
    let names = File.read_dir dir.path in
    Array.sort Data.compare_utf_8 names;
    let names = List.filter File.is_proper (Array.to_list names) in
    let paths = List.map (File.(//) dir.path) names in
    let files_opt = List.map2 make_file paths names in
    let files = List.filter_map Fun.id files_opt in
    let dirs, files' = List.partition (fun file -> file.is_dir) files in
    dir.children <-
      Array.of_list (List.map (dir_of_file dir.path (dir.nest + 1)) dirs);
    dir.files <- Array.append (Array.of_list dirs) (Array.of_list files');
  with Sys_error _ -> ()

let rec populate_path' root path : dir =
  let dirpath = File.dir path in
  let parent =
    if dirpath = path then root else
    populate_path' root dirpath
  in
  parent.folded <- false;
  Array.iter populate_dir parent.children;
  Option.get
    (Array.find_opt (fun (dir : dir) -> dir.path = path) parent.children)

let populate_path fs =
  let root = make_dir "" (-1) in
  root.children <- fs.roots;
  let dir = populate_path' root fs.path in
  Array.iter populate_dir dir.children;
  dir

let make () =
  let roots = Array.of_list (roots ()) in
  let fs =
    {
      op = None;
      path = Storage.home_dir;
      roots;
      dirs = Table.make 0;
      files = Table.make 0;
      input = Edit.make 100;
      columns = [|12; 200; 80; 80|];
    }
  in
  ignore (populate_path fs);
  refresh_dirs fs;
  let i = Array.find_index (fun (dir : dir) -> dir.path = fs.path) fs.dirs.entries in
  Table.select fs.dirs (Option.get i) (Option.get i);
  refresh_files fs;
  fs


(* Focus *)

let defocus fs =
  Table.defocus fs.dirs;
  Table.defocus fs.files;
  Edit.defocus fs.input

let focus_directories fs =
  defocus fs;
  Table.focus fs.dirs

let focus_files fs =
  defocus fs;
  Table.focus fs.files

let focus_input fs =
  defocus fs;
  Edit.focus fs.input


(* Navigation *)

let selected_dir fs =
  Option.get (Table.first_selected fs.dirs)

let selected_file fs = Table.first_selected fs.files

let select_dir fs i =
  Table.deselect_all fs.dirs;
  Table.deselect_all fs.files;
  Table.select fs.dirs i i;
  let dir = fs.dirs.entries.(i) in
  fs.path <- dir.path;
  populate_dir dir;
  refresh_files fs

let set_dir_path fs path =
  if path <> fs.path then
  (
    let old = fs.path in
    try
      fs.path <- path;
      let dir = populate_path fs in
      refresh_dirs fs;
      let i = Array.find_index ((==) dir) fs.dirs.entries in
      select_dir fs (Option.get i);
    with Sys_error _ ->
      fs.path <- old;
  )

let select_file fs i =
  Table.deselect_all fs.files;
  Table.select fs.files i i;
  let file = fs.files.entries.(i) in
  Edit.set fs.input file.name

let deselect_file fs =
  Table.deselect_all fs.files

let deselect_file_if_input_differs fs =
  match Table.first_selected fs.files with
  | Some i when fs.files.entries.(i).name <> fs.input.text ->
    deselect_file fs
  | _ -> ()


let fold_dir fs dir status =
  if status <> dir.folded then
  (
    dir.folded <- status;
    if status
    && String.starts_with ~prefix: (File.(//) dir.path "") fs.path then
      set_dir_path fs dir.path
    else
      refresh_dirs fs;
  )


(* Input *)

let current_file_path fs =
  if fs.input.text = "" then None else
  Some File.(fs.path // fs.input.text)

let current_file_exists fs =
  match current_file_path fs with
  | None -> false
  | Some path -> File.exists path && not (File.is_dir path)

let current_sel_is_dir fs =
  match selected_file fs with
  | None -> false
  | Some i -> fs.files.entries.(i).is_dir


let reset fs =
  fs.op <- None;
  deselect_file fs;
  Edit.clear fs.input


(* Formatting *)

let columns fs =
  let w = fs.columns in
  [|w.(0), `Left; w.(1), `Left; w.(2), `Right; w.(3), `Right|]

let headings = [|""; "File Name"; "File Size"; "File Date"|]

let string_of_mode is_dir = if is_dir then "►" else "   "
let string_of_size size = Data.fmt "%3.1f MB" (float size /. 2.0 ** 20.0)

let string_of_col file = function
  | 0 -> string_of_mode file.is_dir
  | 1 -> file.name
  | 2 -> string_of_size file.size
  | 3 -> Data.string_of_date_time file.time
  | _ -> ""

let row file = Array.init 4 (string_of_col file)


(* Ordering *)

let reorder_files fs k =
  let key file =
    if k = 0 then
      string_of_col file 0 ^ string_of_col file 1
    else
      string_of_col file k
  in
  let enriched =
    Array.map (fun (file : file) -> key file, file) fs.files.entries in
  let cmp e1 e2 = Data.compare_utf_8 (fst e1) (fst e2) in
  let cmp' =
    if
      Array.find_mapi (fun i e ->
        if i = 0 || cmp enriched.(i - 1) e <= 0 then None else Some ()
      ) enriched = None
    then
      cmp
    else
      fun e1 e2 -> cmp e2 e1
  in
  Array.stable_sort cmp' enriched;
  let selection = Table.save_selection fs.files in
  Table.set fs.files (Array.map snd enriched);
  Table.restore_selection fs.files selection (fun (file : file) -> file.name)


(* Persistence *)

let print_state fs =
  let open Text.Print in
  record (fun fs -> [
    "path", string fs.path;
    "columns", array nat fs.columns;
  ]) fs

let print_intern fs =
  let open Text.Print in
  print_state fs @@@
  record (fun fs -> [
    "roots", array string (Array.map (fun (dir : dir) -> dir.path) fs.roots);
    "dirs", nat (Table.length fs.dirs);
    "files", nat (Table.length fs.files);
    "dir", option string
      (Option.map (fun i -> fs.dirs.entries.(i).path) (Table.first_selected fs.dirs));
    "file", option string
      (Option.map (fun i -> fs.files.entries.(i).name) (Table.first_selected fs.files));
    "input", string fs.input.text;
  ]) fs

let parse_state fs =
  let open Text.Parse in
  record (fun r->
    apply (r $? "path") string
      (fun s -> set_dir_path fs s);
    apply (r $? "columns") (array nat)
      (fun ws -> fs.columns <- ws);
  )
