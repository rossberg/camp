open Audio_file

(* Program State *)

type 'a filesel_op =
[
  | `LoadPlaylist
  | `SavePlaylist of (Data.track, 'a) Table.t
  | `CreatePlayViewlist of string * string * Library.views option
  | `InsertRoot
]

type menu_op = [`Todo]

type 'cache t =
{
  config : Config.t;
  layout : Layout.t;
  control : Control.t;
  playlist : 'cache Playlist.t;
  library : 'cache Library.t;
  filesel : ('cache filesel_op, 'cache) Filesel.t;
  menu : menu_op Menu.t;
  mutable saved : File.time;
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
    menu = Menu.make ();
    saved = Unix.gettimeofday ();
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
    let view = dir.view in
    (if view.artists.shown <> None && not (Library.refresh_artists_busy lib) then
      [foci_table f lib.artists] else []) @
    (if view.albums.shown <> None && not (Library.refresh_albums_busy lib) then
      [foci_table f lib.albums] else []) @
    (if view.tracks.shown <> None && not (Library.refresh_tracks_busy lib) then
      [foci_table f lib.tracks] else [])

let foci_filesel (fs : _ Filesel.t) =
  let f = focus_filesel in
  [foci_table f fs.dirs; foci_table f fs.files; foci_edit fs.input]

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


(* Layout Persistence *)

let side_enum = ["left", `Left; "right", `Right]

let print_layout lay =
  let open Layout in
  let open Text.Print in
  let win = Ui.window lay.ui in
  let x, y = Api.Window.pos win in
  let w, h = Api.Window.size win in
  let sw, sh = Api.Window.max_size win in
  let lx, ly = Api.Window.min_pos win in
  let rx, ry = lx + sw, ly + sh in
  let x =
    if lay.library_shown || not lay.filesel_shown then x
    else x + lay.library_width
  in
  let dx = if x = lx || x + w < rx then lx else rx in
  let dy = if y = ly || y + h < ry then ly else ry in
  record (fun lay -> [
    "win_pos", pair int int (x - dx, y - dy);
    "color_palette", nat (Ui.get_palette lay.ui);
    "text_size", nat lay.text;
    "play_open", bool lay.playlist_shown;
    "play_height", nat lay.playlist_height;
    "lib_open", bool lay.library_shown;
    "lib_side", enum side_enum lay.library_side;
    "lib_width", nat lay.library_width;
    "browser_width", nat lay.browser_width;
    "upper_height", nat lay.upper_height;
    "left_width", nat lay.left_width;
    "directories_width", nat lay.directories_width;
    "albums_grid", nat lay.albums_grid;
    "tracks_grid", nat lay.tracks_grid;
  ]) lay

let parse_layout lay pos =  (* assumes playlist and library already loaded *)
  let open Layout in
  let open Text.Parse in
  let win = Ui.window lay.ui in
  let ww, wh = Layout.(control_min_w, control_min_h) in
  let sw, sh = Api.Window.max_size win in
  let lx, ly = Api.Window.min_pos win in
  let rx, ry = lx + sw, ly + sh in
  record (fun r ->
    apply (r $? "win_pos")
      (pair (num (lx - rx) (rx - lx + 20)) (num (ly - ry) (ry - ly - 20)))
      (fun (x, y) ->
        let dx = if x >= 0 then lx else rx in
        let dy = if y >= 0 then ly else ry in
        pos := x + dx, y + dy
      );
    apply (r $? "color_palette") (num 0 (Ui.num_palette lay.ui - 1))
      (fun i -> Ui.set_palette lay.ui i);
    apply (r $? "text") (num 6 64)
      (fun h -> lay.text <- h);
    apply (r $? "play_open") bool
      (fun b -> lay.playlist_shown <- b);
    apply (r $? "play_height") (num (playlist_min lay) (sh - wh))
      (fun h -> lay.playlist_height <- h);
    apply (r $? "lib_open") bool
      (fun b -> lay.library_shown <- b);
    apply (r $? "lib_side") (enum side_enum)
      (fun s -> lay.library_side <- s);
    apply (r $? "lib_width") (num (library_min lay) (sw - ww))
      (fun w -> lay.library_width <- w);
    apply (r $? "browser_width") (num (browser_min lay) (browser_max lay))
      (fun w -> lay.browser_width <- w);
    apply (r $? "left_width") (num (left_min lay) (left_max lay))
      (fun w -> lay.left_width <- w);
    apply (r $? "upper_height") (num (upper_min lay) (upper_max lay))
      (fun h -> lay.upper_height <- h);
    apply (r $? "directories_width") (num (directories_min lay) (directories_max lay))
      (fun w -> lay.directories_width <- w);
    apply (r $? "albums_grid") (num 10 1000)
      (fun w -> lay.albums_grid <- w);
    apply (r $? "tracks_grid") (num 10 1000)
      (fun w -> lay.tracks_grid <- w);
  )


(* Persistence *)

let state_file = "state.conf"
let state_header = App.name

let print_state st =
  let open Text.Print in
  record (fun st -> [
    "layout", print_layout st.layout;
    "config", Config.print_state st.config;
    "control", Control.print_state st.control;
    "playlist", Playlist.print_state st.playlist;
    "library", Library.print_state st.library;
    "filesel", Filesel.print_state st.filesel;
  ]) st

let print_intern st =
  let open Text.Print in
  record (fun st -> [
    "layout", print_layout st.layout;
    "config", Config.print_intern st.config;
    "control", Control.print_intern st.control;
    "playlist", Playlist.print_intern st.playlist;
    "library", Library.print_intern st.library;
    "filesel", Filesel.print_intern st.filesel;
  ]) st

let to_string st = Text.print (print_intern st)

let parse_state st pos =
  let open Text.Parse in
  record (fun r ->
    apply (r $? "layout") (parse_layout st.layout pos) ignore;
    apply (r $? "config") (Config.parse_state st.config) ignore;
    apply (r $? "control") (Control.parse_state st.control) ignore;
    apply (r $? "playlist") (Playlist.parse_state st.playlist) ignore;
    apply (r $? "library") (Library.parse_state st.library) ignore;
    apply (r $? "filesel") (Filesel.parse_state st.filesel) ignore;
  )


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
    Menu.ok st.menu @
    check "at most one focus" (List.length (focus st) <= 1) @
    check "playlist empty when no current track"
      (st.control.current <> None || st.playlist.table.entries = [||]) @
    check "at most one selection"
      (not (Table.has_selection st.playlist.table &&
        Table.has_selection st.library.tracks)) @
    check "file selection with op"
      (st.layout.filesel_shown = (st.filesel.op <> None)) @
    check "menu with op"
      (st.layout.menu_shown = (st.menu.op <> None)) @
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

  focus_playlist st;
  if st.control.current = None && Playlist.length st.playlist > 0 then
  (
    st.control.current <- Table.current_opt st.playlist.table;
    Option.iter (fun track -> Control.switch st.control track false)
      st.control.current;
  );

  (try ok st; true with _ -> false), !pos
