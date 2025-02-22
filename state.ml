(* Program State *)

type filesel_op = [`LoadPlaylist | `SavePlaylist of Data.track Table.t]

type t =
{
  config : Config.t;
  layout : Layout.t;
  control : Control.t;
  playlist : Playlist.t;
  library : Library.t;
  filesel : filesel_op Filesel.t;
}


(* Constructor *)

let make ui audio db =
  {
    config = Config.make ();
    layout = Layout.make ui;
    control = Control.make audio;
    playlist = Playlist.make ();
    library = Library.make db;
    filesel = Filesel.make ();
  }


(* Validation *)

let dumped_before = ref None
let to_string_fwd = ref (fun _ -> assert false)

let check msg b = if b then [] else [msg]

let layout_ok layout =
  let open Layout in
  check "text size in range" (layout.text >= 6 && layout.text <= 64) @
  check "playlist height positive" (layout.playlist_height > 0) @
  check "library width positive" (layout.library_width > 0) @
  check "browser width in range" (layout.browser_width <= layout.library_width - 40) @
  []

let focus st =
  let pl = st.playlist in
  let lib = st.library in
  let fs = st.filesel in
  List.filter snd [
    "playlist", pl.table.focus;
    "browser", lib.browser.focus;
    "tracks", lib.tracks.focus;
    "albums", lib.albums.focus;
    "artists", lib.artists.focus;
    "search", lib.search.focus;
    "directories", fs.dirs.focus;
    "files", fs.files.focus;
    "input", fs.input.focus;
  ]

let rec ok st =
  match
    Config.ok st.config @
    layout_ok st.layout @
    Control.ok st.control @
    Playlist.ok st.playlist @
    Library.ok st.library @
    Filesel.ok st.filesel @
    check "at most one focus" (List.length (focus st) <= 1) @
    check "playlist empty when no current track"
      (st.control.current <> None || st.playlist.table.entries = [||]) @
    check "at most one selection"
      (not (Table.has_selection st.playlist.table &&
        Table.has_selection st.library.tracks)) @
    check "file selection with op"
      (st.layout.filesel_shown = (st.filesel.op <> None)) @
    []
  with
  | errors when errors <> [] ->
    dump st (List.map ((^) "Invariant violated: ") errors)
  | exception exn ->
    dump st ["Exception during validation: " ^ Printexc.to_string exn ^ "\n" ^
      Printexc.get_backtrace ()]
  | _ -> ()

and dump st errors =
  let msgs = errors @ ["State:\n" ^ !to_string_fwd st] in
  if !dumped_before <> Some msgs then
  (
    if !dumped_before = None then Storage.log_clear ();
    List.iter Storage.log msgs;
    dumped_before := Some msgs;
  )


(* Focus *)

let defocus_all st =
  Playlist.defocus st.playlist;
  Library.defocus st.library;
  Filesel.defocus st.filesel

let focus_playlist st =
  Library.deselect_all st.library;
  defocus_all st;
  Playlist.focus st.playlist

let focus_library (table : _ Table.t) st =
  Playlist.deselect_all st.playlist;
  defocus_all st;
  table.focus <- true

let focus_filesel (table : _ Table.t) st =
  Playlist.deselect_all st.playlist;
  Library.deselect_all st.library;
  defocus_all st;
  table.focus <- true


(* Layout Persistance *)

open Storage
let fmt = Printf.sprintf
let scan = Scanf.sscanf

let value = Fun.id
let bool x = x <> 0
let num l h x = max l (min h x)
let num_pair lx ly hx hy x y = num lx hx x, num ly hy y

let layout_to_map lay =
  let open Layout in
  let x, y = Api.Window.pos (Ui.window lay.ui) in
  let x =
    if lay.library_shown || not lay.filesel_shown then x
    else x + lay.library_width
  in
  Map.of_list
  [
    "win_pos", fmt "%d, %d" x y;
    "color_palette", fmt "%d" (Ui.get_palette lay.ui);
    "text_size", fmt "%d" lay.text;
    "play_open", fmt "%d" (Bool.to_int lay.playlist_shown);
    "play_height", fmt "%d" lay.playlist_height;
    "lib_open", fmt "%d" (Bool.to_int lay.library_shown);
    "lib_side", fmt "%d" (Bool.to_int (lay.library_side = `Right));
    "lib_width", fmt "%d" lay.library_width;
    "browser_width", fmt "%d" lay.browser_width;
    "upper_height", fmt "%d" lay.upper_height;
    "left_width", fmt "%d" lay.left_width;
    "directories_width", fmt "%d" lay.directories_width;
  ]

let layout_of_map lay m =  (* assumes playlist and library already loaded *)
  let open Layout in
  let win = Ui.window lay.ui in
  let ww, wh = Api.Window.size win in
  let sx, sy = Api.Window.min_pos win in
  let sw, sh = Api.Window.max_size win in
  let xy = ref None in
  read_map m "win_pos" (fun s -> xy :=
    Some (scan s "%d , %d" (num_pair sx sy (sx + sw - 20) (sy + sh - 20))));
  read_map m "color_palette" (fun s ->
    Ui.set_palette lay.ui (scan s "%d" (num 0 (Ui.num_palette lay.ui - 1))));
  read_map m "text" (fun s -> lay.text <- scan s "%d" (num 6 64));
  read_map m "play_open" (fun s -> lay.playlist_shown <- scan s "%d" bool);
  read_map m "play_height" (fun s ->
    lay.playlist_height <- scan s "%d" (num (playlist_min lay) (sh - sy)));
  read_map m "lib_open" (fun s -> lay.library_shown <- scan s "%d" bool);
  read_map m "lib_side" (fun s ->
    lay.library_side <- if scan s "%d" bool then `Right else `Left);
  read_map m "lib_width" (fun s ->
    lay.library_width <- scan s "%d" (num (library_min lay) (sw - ww)));
  read_map m "browser_width" (fun s ->
    lay.browser_width <- scan s "%d" (num (browser_min lay) (browser_max lay)));
  read_map m "left_width" (fun s ->
    lay.left_width <- scan s "%d" (num (left_min lay) (left_max lay)));
  read_map m "upper_height" (fun s ->
    lay.upper_height <- scan s "%d" (num (upper_min lay) (upper_max lay)));
  read_map m "directories_width" (fun s ->
    lay.directories_width <- scan s "%d"
      (num (directories_min lay) (directories_max lay)));
  Api.Draw.start win `Black;
  Option.iter (fun (x, y) -> Api.Window.set_pos win x y) !xy;
  Api.Window.set_size win (ww + lay.library_width) (wh + lay.playlist_height);
  Api.Draw.finish win


(* Persistance *)

let state_file = "state.conf"
let state_header = App.name

let to_map st =
  List.fold_left combine_map Map.empty
  [
    layout_to_map st.layout;
    Config.to_map st.config;
    Control.to_map st.control;
    Playlist.to_map st.playlist;
    Library.to_map st.library;
    Filesel.to_map st.filesel;
  ]

let to_map_extra st =
  List.fold_left combine_map Map.empty
  [
    to_map st;
    Control.to_map_extra st.control;
    Playlist.to_map_extra st.playlist;
    Library.to_map_extra st.library;
    Filesel.to_map_extra st.filesel;
  ]

let to_string st = string_of_map (to_map_extra st)
let _ = to_string_fwd := to_string

let save st =
  Playlist.save_playlist st.playlist;
  Storage.save_map state_file (to_map st)

let load st =
  Playlist.load_playlist st.playlist;
  Library.load_dirs st.library;

  let map = Storage.load_map state_file in
  layout_of_map st.layout map;
  Config.of_map st.config map;
  Control.of_map st.control map;
  Playlist.of_map st.playlist map;
  Library.of_map st.library map;
  Filesel.of_map st.filesel map;

  focus_playlist st;
  if st.control.current = None && Playlist.length st.playlist > 0 then
  (
    st.control.current <- Table.current_opt st.playlist.table;
    Option.iter (fun track -> Control.switch st.control track false)
      st.control.current;
  );

  try ok st; true with _ -> false
