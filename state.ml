(* Program State *)

type 'a filesel_op = [`LoadPlaylist | `SavePlaylist of (Data.track, 'a) Table.t]

type 'cache t =
{
  config : Config.t;
  layout : Layout.t;
  control : Control.t;
  playlist : 'cache Playlist.t;
  library : 'cache Library.t;
  filesel : ('cache filesel_op, 'cache) Filesel.t;
}


(* Constructor *)

let make ui audio =
  {
    config = Config.make ();
    layout = Layout.make ui;
    control = Control.make audio;
    playlist = Playlist.make ();
    library = Library.make ();
    filesel = Filesel.make ();
  }


(* Focus *)

let defocus_all st =
  Playlist.defocus st.playlist;
  Library.defocus st.library;
  Filesel.defocus st.filesel

let focus_table (tab : _ Table.t) st =
  defocus_all st;
  Table.focus tab

let focus_edit (ed : Edit.t) st =
  defocus_all st;
  Edit.focus ed

let focus_playlist st =
  Library.deselect_all st.library;
  focus_table st.playlist.table st

let focus_library (tab : _ Table.t) st =
  Playlist.deselect_all st.playlist;
  focus_table tab st

let focus_filesel (tab : _ Table.t) st =
  Playlist.deselect_all st.playlist;
  Library.deselect_all st.library;
  focus_table tab st

let foci_table f (tab : _ Table.t) = tab.focus, f tab
let foci_edit (ed : Edit.t) = ed.focus, focus_edit ed

let foci_playlist (pl : _ Playlist.t) =
  let f = fun _ -> focus_playlist in
  [foci_table f pl.table]

let foci_library (lib : _ Library.t) =
  let f = focus_library in
  [foci_edit lib.search; foci_table f lib.browser] @
  match lib.current with
  | None -> []
  | Some dir ->
    (if dir.artists_shown && not (Library.refresh_artists_busy lib) then
      [foci_table f lib.artists] else []) @
    (if dir.albums_shown <> None && not (Library.refresh_albums_busy lib) then
      [foci_table f lib.albums] else []) @
    (if dir.tracks_shown <> None && not (Library.refresh_tracks_busy lib) then
      [foci_table f lib.tracks] else [])

let foci_filesel (fs : _ Filesel.t) =
  let f = focus_filesel in
  [foci_edit fs.input; foci_table f fs.dirs; foci_table f fs.files]

let foci st =
  (if st.layout.playlist_shown then foci_playlist st.playlist else []) @
  (if st.layout.filesel_shown then foci_filesel st.filesel else
   if st.layout.library_shown then foci_library st.library else [])

let focus_switch st foci =
  let rec find = function
    | f::f'::_ when fst f -> f'
    | _::fs -> find fs
    | [] -> List.hd foci
  in snd (find foci) st

let focus_next st = focus_switch st (foci st)
let focus_prev st = focus_switch st (List.rev (foci st))


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
    "albums_grid", fmt "%d" lay.albums_grid;
    "tracks_grid", fmt "%d" lay.tracks_grid;
  ]

let layout_of_map lay m =  (* assumes playlist and library already loaded *)
  let open Layout in
  let win = Ui.window lay.ui in
  let ww, wh = Layout.(control_min_w, control_min_h) in
  let sx, sy = Api.Window.min_pos win in
  let sw, sh = Api.Window.max_size win in
  let pos = ref (0, 0) in
  read_map m "win_pos" (fun s -> pos :=
    scan s "%d , %d" (num_pair sx sy (sx + sw - 20) (sy + sh - 20)));
  read_map m "color_palette" (fun s ->
    Ui.set_palette lay.ui (scan s "%d" (num 0 (Ui.num_palette lay.ui - 1))));
  read_map m "text" (fun s -> lay.text <- scan s "%d" (num 6 64));
  read_map m "play_open" (fun s -> lay.playlist_shown <- scan s "%d" bool);
  read_map m "play_height" (fun s ->
    lay.playlist_height <- scan s "%d" (num (playlist_min lay) (sh - wh)));
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
  read_map m "albums_grid"
    (fun s -> lay.albums_grid <- scan s "%d" (num 10 1000));
  read_map m "tracks_grid"
    (fun s -> lay.tracks_grid <- scan s "%d" (num 10 1000));
  !pos


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


(* Validation *)

let dumped_before = ref None

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
  let msgs = errors @ ["State:\n" ^ to_string st] in
  if !dumped_before <> Some msgs then
  (
    if !dumped_before = None then Storage.log_clear ();
    List.iter Storage.log msgs;
    dumped_before := Some msgs;
  )


(* Persistance pt 2 *)

let save st =
  Library.save_db st.library;
  Playlist.save_playlist st.playlist;
  Storage.save_map state_file (to_map st)

let load st =
  Random.self_init ();

  Playlist.load_playlist st.playlist;
  Library.load_db st.library;

  let map = Storage.load_map state_file in
  let pos = layout_of_map st.layout map in
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

  try ok st; true, pos with _ -> false, pos
