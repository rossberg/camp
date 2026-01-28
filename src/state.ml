open Audio_file

(* Program State *)

type config = Config.t
type geometry = Geometry.t
type control = Control.t
type playlist = Ui.cached Playlist.t
type library = Ui.cached Library.t
type filesel = Ui.cached Filesel.t
type menu = Menu.t

type t =
{
  config : config;
  geometry : geometry;
  control : control;
  playlist : playlist;
  library : library;
  filesel : filesel;
  menu : menu;
  mutable popup : [`Current | `Track of Data.track | `Album of Data.album];
  mutable saved : File.time;
  mutable delayed : (unit -> unit) list;
}


(* Constructor *)

let make ui audio =
  {
    config = Config.make ();
    geometry = Geometry.make ui;
    control = Control.make audio;
    playlist = Playlist.make ();
    library = Library.make ();
    filesel = Filesel.make ();
    menu = Menu.make ();
    popup = `Current;
    saved = Unix.gettimeofday ();
    delayed = [];
  }

let delay st f = st.delayed <- f :: st.delayed


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
  match lib.current, lib.log with
  | Some dir, None ->
    let view = dir.view in
    (if view.artists.shown <> None && not (Library.refresh_artists_busy lib) then
      [foci_table f lib.artists] else []) @
    (if view.albums.shown <> None && not (Library.refresh_albums_busy lib) then
      [foci_table f lib.albums] else []) @
    (if view.tracks.shown <> None && not (Library.refresh_tracks_busy lib) then
      [foci_table f lib.tracks] else [])
  | _, _ -> []

let foci_filesel (fs : _ Filesel.t) =
  let f = focus_filesel in
  [foci_table f fs.dirs; foci_table f fs.files; foci_edit fs.input]

let foci st =
  (if st.geometry.playlist_shown then foci_playlist st.playlist else []) @
  (if st.geometry.filesel_shown then foci_filesel st.filesel else
   if st.geometry.library_shown then foci_library st.library else [])

let focus_switch st foci =
  let rec find = function
    | f::f'::_ when fst f -> f'
    | _::fs -> find fs
    | [] -> List.hd foci
  in snd (find foci) st

let focus_next st = focus_switch st (foci st)
let focus_prev st = focus_switch st (List.rev (foci st))


(* Persistence *)

let state_file = "state.conf"
let state_header = App.name

let print_state st =
  let open Text.Print in
  record (fun st -> [
    "layout", Geometry.print_state st.geometry;
    "config", Config.print_state st.config;
    "control", Control.print_state st.control;
    "playlist", Playlist.print_state st.playlist;
    "library", Library.print_state st.library;
    "filesel", Filesel.print_state st.filesel;
    "menu", Menu.print_state st.menu;
  ]) st

let print_intern st =
  let open Text.Print in
  record (fun st -> [
    "layout", Geometry.print_intern st.geometry;
    "config", Config.print_intern st.config;
    "control", Control.print_intern st.control;
    "playlist", Playlist.print_intern st.playlist;
    "library", Library.print_intern st.library;
    "filesel", Filesel.print_intern st.filesel;
    "menu", Menu.print_intern st.menu;
  ]) st

let to_string st = Text.print (print_intern st)

let parse_state st pos =
  let open Text.Parse in
  record (fun r ->
    apply (r $? "layout") (Geometry.parse_state st.geometry pos) ignore;
    apply (r $? "config") (Config.parse_state st.config) ignore;
    apply (r $? "control") (Control.parse_state st.control) ignore;
    apply (r $? "playlist") (Playlist.parse_state st.playlist) ignore;
    apply (r $? "library") (Library.parse_state st.library) ignore;
    apply (r $? "filesel") (Filesel.parse_state st.filesel) ignore;
    apply (r $? "menu") (Menu.parse_state st.menu) ignore;
  )


(* Validation *)

let dumped_before = ref None

let check msg b = if b then [] else [msg]

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
    Geometry.ok st.geometry @
    Control.ok st.control @
    Playlist.ok st.playlist @
    Library.ok st.library @
    Filesel.ok st.filesel @
    Menu.ok st.menu @
    check "at most one focus" (List.length (focus st) <= 1) @
    check "playlist empty when no current track"
      (st.control.current <> None || st.playlist.table.entries = [||]) @
    check "at most one selection"
      (not (Table.has_selection st.playlist.table &&
        Table.has_selection st.library.tracks)) @
    check "file selection with op"
      (st.geometry.filesel_shown = (st.filesel.op <> None)) @
    check "menu with op"
      (st.geometry.menu_shown = (st.menu.op <> None)) @
    check "menu modal"
      (not st.geometry.menu_shown || Ui.is_modal st.geometry.ui) @
    check "rename modal"
      (st.library.renaming = None || Ui.is_modal st.geometry.ui) @
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


(* Persistence pt 2 *)

let save_after st tdelta =
  let now = Unix.gettimeofday () in
  if st.saved < now -. tdelta then
  (
    Storage.save_string state_file (fun () -> Text.print (print_state st));
    st.saved <- now;
  )

let save st =
  Library.save_db st.library;
  Library.save_browser st.library;
  Playlist.save_playlist st.playlist;
  save_after st 0.0

let load st =
  Random.self_init ();

  let pos = ref (0, 0) in
  Library.load_db st.library;
  Library.load_browser st.library;
  Library.rescan_root st.library `Quick;
  Playlist.load_playlist st.playlist;
  Storage.load_string_opt state_file (fun s ->
    try parse_state st pos (Text.parse s)
    with Text.Syntax_error _ | Text.Type_error as exn ->
      Storage.log_exn "parse" exn "while loading state"
  );
  st.saved <- Unix.gettimeofday ();

  if st.geometry.playlist_shown then focus_playlist st;
  if st.control.current = None && Playlist.length st.playlist > 0 then
  (
    st.control.current <- Table.current_opt st.playlist.table;
    Option.iter (fun track ->
      Control.switch st.control track;
    ) st.control.current;
  );

  (try ok st; true with _ -> false), !pos
