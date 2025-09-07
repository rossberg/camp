(* Generic Handling of Editable Track View UI *)

open Audio_file

module Set = Set.Make(String)

type path = File.path
type state = State.t
type table = (Data.track, Ui.cached) Table.t


(* Helpers *)

let exec prog args =
  let cmd = Filename.quote_command prog args in
  let cmd' = if not Sys.win32 then cmd else
    "\"start /b ^\"^\" " ^ String.sub cmd 1 (String.length cmd - 1) in
  ignore (Sys.command cmd')


(* Generic abstraction *)

module type View =
sig
  open Data

  type 'cache t
  type table

  module Select :  (* ops on the target table *)
  sig
    val length : table -> int
    val num_selected : table -> int
    val select_all : table -> unit
    val deselect_all : table -> unit
    val select_invert : table -> unit
  end

  val it : Ui.cached t
  val tab : table
  val is_same : bool
  val focus : State.t -> unit
  val deselect_other : unit -> unit
  val refresh_deps : 'a Library.t -> unit

  (* Ops on the underlying tracks table *)

  val length : Ui.cached t -> int
  val tracks : Ui.cached t -> track array
  val table : Ui.cached t -> (track, Ui.cached) Table.t

  val num_selected : Ui.cached t -> int
  val first_selected : Ui.cached t -> int option
  val selected : Ui.cached t -> track array
  val select_all : Ui.cached t -> unit
  val deselect_all : Ui.cached t -> unit
  val select_invert : Ui.cached t -> unit
(*  val select : Ui.cached t -> int -> int -> unit*)
(*  val deselect : Ui.cached t -> int -> int -> unit*)

  val insert : Ui.cached t -> int -> track array -> unit
  val replace_all : Ui.cached t -> track array -> unit
  val remove_all : Ui.cached t -> unit
  val remove_selected : Ui.cached t -> unit
  val remove_unselected : Ui.cached t -> unit
  val remove_invalid : Ui.cached t -> unit
  val remove_duplicates : Ui.cached t -> unit
(*  val move_selected : Ui.cached t -> int -> unit*)
  val reverse_selected : Ui.cached t -> unit
  val reverse_all : Ui.cached t -> unit
  val undo : Ui.cached t -> unit
  val redo : Ui.cached t -> unit
end

type view = (module View)

let playlist_view (st : state) : view =
  (module struct
    include Playlist
    module Select = Playlist
    type table = Ui.cached t
    let is_same = true
    let it = st.playlist
    let tab = st.playlist
    let focus = State.focus_playlist
    let deselect_other () = Library.deselect_all st.library
    let refresh_deps = ignore
  end)

let tracks_view (st : state) : view =
  (module struct
    include Library
    module Select = Library
    type table = Ui.cached t
    let is_same = true
    let it = st.library
    let tab = st.library
    let focus = State.focus_library st.library.tracks
    let deselect_other () = Playlist.deselect_all st.playlist
    let refresh_deps = ignore
  end)

let albums_view (st : state) : view =
  (module struct
    include Library
    module Select = Table
    type table = (Data.album, Ui.cached) Table.t
    let is_same = false
    let it = st.library
    let tab = it.albums
    let focus = State.focus_library tab
    let deselect_other = ignore
    let refresh_deps lib = Library.refresh_tracks lib
  end)

let artists_view (st : state) : view =
  (module struct
    include Library
    module Select = Table
    type table = (Data.artist, Ui.cached) Table.t
    let is_same = false
    let it = st.library
    let tab = it.artists
    let focus = State.focus_library tab
    let deselect_other = ignore
    let refresh_deps lib = Library.refresh_albums_tracks lib
  end)


(* Drag & Drop *)

let update_control (st : state) =
  if Control.switch_if_empty st.control (Playlist.current_opt st.playlist) then
  (
    Table.dirty st.library.tracks;  (* current song has changed *)
    Table.dirty st.library.browser;
  )

let current_is_grid (st : state) =
  match st.library.current with
  | None -> false
  | Some dir -> dir.view.tracks.shown = Some `Grid

let drag (st : state) table_drag (module View : View) =
  let lay = st.layout in
  let tab = View.table View.it in
  (* Drag over table: highlight target entry *)
  Ui.delay lay.ui (fun () -> table_drag lay tab)

let drag_on_playlist (st : state) =
  if st.layout.playlist_shown then
    drag st Layout.playlist_drag (playlist_view st)

let library_drag (st : state) (lay : Layout.t) =
  let drag, grid_drag =
    if lay.lower_shown then Layout.(lower_drag, lower_grid_drag) else
    if lay.right_shown then Layout.(right_drag, right_grid_drag) else
    Layout.(left_drag, left_grid_drag)
  in
  if current_is_grid st then grid_drag lay lay.track_grid else drag lay

let drag_on_tracks (st : state) =
  if st.layout.library_shown && Library.current_is_shown_playlist st.library then
    drag st (library_drag st) (tracks_view st)

let drop (st : state) tracks table_mouse (module View : View) =
  if tracks <> [||] then
  (
    let lay = st.layout in
    let view = View.it in
    let tab = View.table view in
    Option.iter (fun pos ->
      (* Drop onto table: send tracks there *)
      View.insert view pos tracks;
      State.defocus_all st;
      View.focus st;
      update_control st;
    ) (table_mouse lay tab)
  )

let drop_on_playlist (st : state) tracks =
  if st.layout.playlist_shown then
    drop st tracks Layout.playlist_mouse (playlist_view st)

let library_mouse (st : state) (lay : Layout.t) =
  let mouse, grid_mouse =
    if lay.lower_shown then Layout.(lower_mouse, lower_grid_mouse) else
    if lay.right_shown then Layout.(right_mouse, right_grid_mouse) else
    Layout.(left_mouse, left_grid_mouse)
  in
  if current_is_grid st then grid_mouse lay lay.track_grid else mouse lay

let drop_on_tracks (st : state) tracks =
  if st.layout.library_shown && Library.current_is_shown_playlist st.library then
    drop st tracks (library_mouse st) (tracks_view st)


let expand_paths (st : state) paths =
  let tracks = ref [] in
  let add_track (track : Data.track) =
    tracks := track :: !tracks
  in
  let add_playlist path =
    let s = File.load `Bin path in
    List.iter (fun item -> add_track (Track.of_m3u_item item)) (M3u.parse_ext s)
  in
  let add_viewlist path =
    let s = File.load `Bin path in
    match Query.parse_query s with
    | Error msg -> Library.error st.library msg
    | Ok query ->
      let _, _, tracks' =
        Query.exec query
          (fun track -> false, false, not (Data.is_separator track))
          st.library.root
      in
      for i = 0 to Array.length tracks' / 2 - 1 do
        let j = Array.length tracks' - i - 1 in
        let temp = tracks'.(i) in
        tracks'.(i) <- tracks'.(j);
        tracks'.(j) <- temp;
      done;
      let tracks'' = Array.to_list tracks' in
      tracks := if !tracks = [] then tracks'' else tracks'' @ !tracks
  in
  let rec add_path path =
    try
      if File.exists_dir path then
        Array.iter (fun file ->
          add_path File.(path // file)
        ) (File.read_dir path)
      else if Data.is_playlist_path path then
        add_playlist path
      else if Data.is_viewlist_path path then
        add_viewlist path
      else if Data.is_track_path path then
        add_track (Data.make_track path)
    with Sys_error _ -> ()
  in
  List.iter add_path paths;
  Array.of_list (List.rev !tracks)

let external_drop drop_on (st : state) (module View : View) =
  let dropped = Api.Files.dropped (Ui.window st.layout.ui) in
  if dropped <> [] then
    drop_on st (expand_paths st dropped)

let external_drop_on_playlist st = external_drop drop_on_playlist st (playlist_view st)
let external_drop_on_tracks st = external_drop drop_on_tracks st (tracks_view st)

let external_queue_on_playlist st paths =
  let tracks = expand_paths st paths in
  if tracks <> [||] then
  (
    let len = Playlist.length st.playlist in
    Playlist.insert st.playlist len tracks;
    let status = Control.status st.control in
    if status = `Stopped || status = `Ejected then
    (
      Playlist.jump st.playlist len;
      Control.switch st.control tracks.(0) true;
      Playlist.adjust_scroll st.playlist 4;
      Table.dirty st.library.tracks;  (* current song has changed *)
      Table.dirty st.library.browser;
    )
  )


let set_drop_cursor (st : state) =
  let lay = st.layout in
  let pl = st.playlist in
  let lib = st.library in
  let droppable =
    lay.playlist_shown &&
      (* over playlist *)
      Layout.playlist_mouse lay pl.table <> None
    ||
    lay.library_shown && (
      (* over library playlist view? *)
      Library.current_is_playlist lib &&
        library_mouse st lay lib.tracks <> None
      ||
      (* over browser entry that is a playlist? *)
      match Layout.browser_mouse lay lib.browser with
      | None -> false
      | Some i ->
        i < Table.length lib.browser && Data.is_playlist lib.browser.entries.(i)
    )
  in
  Api.Mouse.set_cursor (Ui.window lay.ui)
    (if droppable then `Point else `Blocked)


(* Edit Operations *)

let editable (st : state) (module View : View) =
  View.(table it) == st.playlist.table ||
  Library.current_is_playlist st.library

let separator_avail st view =
  editable st view
let separator _st (module View : View) pos =
  View.(insert it) pos [|Data.make_separator ()|];
  View.deselect_other ()

let remove_avail st (module View : View) =
  editable st (module View) && View.(num_selected it > 0)
let remove _st (module View : View) =
  View.(remove_selected it)

let crop_avail st (module View : View) =
  editable st (module View) &&
  View.(num_selected it > 0 && num_selected it < length it)
let crop _st (module View : View) =
  View.(remove_unselected it)

let wipe_avail st (module View : View) =
  editable st (module View) &&
  Array.exists (fun track -> track.Data.status = `Absent) View.(table it).entries
let wipe _st (module View : View) =
  View.(remove_invalid it)

let dedupe_avail st (module View : View) =
  editable st (module View) &&
  let mems = ref Set.empty in
  Array.exists (fun (track : Data.track) ->
    Set.mem track.path !mems || (mems := Set.add track.path !mems; false)
  ) View.(tracks it)
let dedupe _st (module View : View) =
  View.(remove_duplicates it)

let clear_avail st (module View : View) =
  editable st (module View) && View.(length it > 0)
let clear _st (module View : View) =
  View.(remove_all it)

let undo_avail _st (module View : View) =
  !(View.(table it).undos) <> []
let undo (st : state) (module View : View) =
  View.(undo it);
  update_control st

let redo_avail _st (module View : View) =
  !(View.(table it).redos) <> []
let redo (st : state) (module View : View) =
  View.(redo it);
  update_control st

let copy_avail _st (module View : View) =
  View.(num_selected it > 0)
let copy (st : state) (module View : View) =
  let s = Track.to_m3u View.(selected it) in
  Api.Clipboard.write (Ui.window st.layout.ui) s

let cut_avail st view =
  copy_avail st view && remove_avail st view
let cut st view =
  copy st view;
  remove st view

let paste_avail (st : state) view =
  editable st view && Api.Clipboard.read (Ui.window st.layout.ui) <> None
let paste (st : state) (module View : View) =
  let s = Option.value (Api.Clipboard.read (Ui.window st.layout.ui)) ~default: "" in
  let tracks = Track.of_m3u s in
  let found_proper =
    Array.exists (fun (track : Data.track) ->
      Data.is_track_path track.path
    ) tracks
  in
  if found_proper && tracks <> [||] then
  (
    let pos = Option.value (View.first_selected View.it) ~default: 0 in
    View.(insert it) pos tracks;
    View.deselect_other ();
    update_control st;
  )

let reverse_avail _st (module View : View) =
  View.(num_selected it > 1)
let reverse _st (module View : View) =
  View.(reverse_selected it)

let reverse_all_avail _st (module View : View) =
  View.(length it > 1)
let reverse_all _st (module View : View) =
  View.(reverse_all it)

let load_avail (st : state) (module View : View) =
  editable st (module View) && not st.layout.filesel_shown
let load (st : state) (module View : View) =
  Run_filesel.filesel st `Read `File "" ".m3u" (fun path ->
    let tracks = Track.of_m3u (File.load `Bin path) in
    View.(replace_all it) tracks;
    View.(focus st);
    if View.(table it) == st.playlist.table then
    (
      Control.eject st.control;
      Control.switch st.control tracks.(0) true;
      Table.dirty st.library.tracks;
      Table.dirty st.library.browser;
    )
  )

let save_avail (st : state) _view =
  not st.layout.filesel_shown
let save (st : state) (module View : View) =
  Run_filesel.filesel st `Write `File "" ".m3u" (fun path ->
    File.store `Bin path (Track.to_m3u View.(table it).entries)
  )

let save_view_avail (st : state) _view =
  not st.layout.filesel_shown && st.layout.library_shown &&
  not st.playlist.table.focus &&
  ( st.library.search.text <> "" ||
    Table.num_selected st.library.artists > 0 ||
    Table.num_selected st.library.albums > 0 )
let save_view (st : state) _view =
  Option.iter (fun dir ->
    Run_filesel.filesel st `Write `File "" ".m3v" (fun path ->
      File.store `Bin path (Library.make_viewlist dir ^ "\n")
    )
  ) st.library.current


let queue_avail _st (module View : View) =
  View.(length it > 0)
let queue (st : state) (module View : View) tracks =
  Playlist.insert st.playlist (Playlist.length st.playlist) tracks;
  Playlist.deselect_all st.playlist;
  ignore (Control.switch_if_empty st.control (Some tracks.(0)))


let rescan_avail _st (module View : View) =
  View.(length it > 0)
let rescan (st : state) tracks =
  Library.rescan_tracks st.library `Thorough tracks

let tag_avail (st : state) (module View : View) =
  View.(length it > 0) &&
  try Unix.(access st.config.exec_tag [X_OK]); true
  with Unix.Unix_error _ -> false

let tag (st : state) tracks additive =
  let paths = Array.map (fun (track : Data.track) -> track.path) tracks in
  Domain.spawn (fun () ->
    let paths' =
      List.filter (fun p -> not (M3u.is_separator p)) (Array.to_list paths) in
    if st.config.exec_tag_max_len = 0 && not additive then
      exec st.config.exec_tag paths'
    else
    (
      (* Work around Windows command line limits *)
      let args = ref paths' in
      let rec pick len max =
        match !args with
        | [] -> []
        | arg1::args' ->
          let len' = len + String.length arg1 + 5 in
          if len <> 0 && len' > max then [] else
          (
            args := args';
            arg1 :: pick len' max
          )
      in
      (* Mp3tag immediately resorts the tracks by current column, unless added
       * with /add. However, /add only works with individual tracks and exec's,
       * which is very slow, so only use that when (a) we have less then a
       * certain number of tracks, or (b) when the command line gets too long
       * for a single call anyways. *)
      let max =
        if List.length paths' < 20 then 1 else st.config.exec_tag_max_len in
      if not additive then exec st.config.exec_tag (pick 0 max);
      List.iter (fun arg -> exec st.config.exec_tag ["/add"; arg]) !args;
    )
  ) |> ignore


let search_avail (st : state) =
  st.library.current <> None

let search (st : state) =
  State.focus_edit st.library.search st

let select_all_avail _st (module View : View) =
  View.(Select.(num_selected tab < length tab))
let select_all (st : state) (module View : View) =
  View.(Select.select_all tab; refresh_deps st.library)

let select_none_avail _st (module View : View) =
  View.(Select.num_selected tab > 0)
let select_none (st : state) (module View : View) =
  View.(Select.deselect_all tab; refresh_deps st.library)

let select_invert_avail _st (module View : View) =
  View.(Select.num_selected tab > 0)
let select_invert (st : state) (module View : View) =
  View.(Select.select_invert tab; refresh_deps st.library)


(* Initiate Menus *)

let subject_tracks (module View : View) =
  if View.(Select.num_selected tab) > 0
  then false, "", fun () -> View.(if is_same then selected else tracks) View.it
  else true, " All", fun () -> View.(tracks it)

let list_menu (st : state) view =
  let lay = st.layout in
  let module View = (val view : View) in

  let c = Ui.text_color lay.ui in
  let all, quant, get_tracks = subject_tracks view in
  Run_menu.command_menu st [|
    `Entry (c, "Tag" ^ quant, Layout.key_tag, tag_avail st view),
      (fun () -> tag st (get_tracks ()) (not all));
    `Entry (c, "Rescan" ^ quant, Layout.key_rescan, rescan_avail st view),
      (fun () -> rescan st (get_tracks ()));
    `Separator, ignore;
    `Entry (c, "Select All", Layout.key_all, select_all_avail st view),
      (fun () -> select_all st view);
    `Entry (c, "Select None", Layout.key_none, select_none_avail st view),
      (fun () -> select_none st view);
    `Entry (c, "Invert Selection", Layout.key_invert, select_invert_avail st view),
      (fun () -> select_invert st view);
    `Separator, ignore;
    `Entry (c, "Search...", Layout.key_search, search_avail st),
      (fun () -> search st);
    `Separator, ignore;
    `Entry (c, "Save as Playlist...", Layout.key_save, save_avail st view),
      (fun () -> save st view);
    `Entry (c, "Save as Viewlist...", Layout.key_save2, save_view_avail st view),
      (fun () -> save_view st view);
    `Entry (c, "Queue" ^ quant ^ " to Playlist...", Layout.key_queue,
      queue_avail st view),
      (fun () -> queue st view (get_tracks ()));
  |]

let edit_menu (st : state) view pos_opt =
  let lay = st.layout in
  let module View = (val view : View) in 

  let pos = Option.value pos_opt ~default: View.(length it) in
  let c = Ui.text_color lay.ui in
  let all, quant, get_tracks = subject_tracks view in
  Run_menu.command_menu st (Array.append [|
    `Entry (c, "Insert Separator", Layout.key_sep, separator_avail st view),
      (fun () -> separator st view pos);
    `Separator, ignore;
    `Entry (c, "Tag" ^ quant, Layout.key_tag, tag_avail st view),
      (fun () -> tag st (get_tracks ()) (not all));
    `Entry (c, "Rescan" ^ quant, Layout.key_rescan, rescan_avail st view),
      (fun () -> rescan st (get_tracks ()));
    `Entry (c, "Remove" ^ quant, Layout.key_del,
      if all then clear_avail st view else remove_avail st view),
      (fun () -> (if all then clear else remove) st view);
    `Entry (c, "Reverse" ^ quant, Layout.key_rev,
      if all then reverse_all_avail st view else reverse_avail st view),
      (fun () -> (if all then reverse_all else reverse) st view);
    `Separator, ignore;
    `Entry (c, "Cut", Layout.key_cut, cut_avail st view),
      (fun () -> cut st view);
    `Entry (c, "Copy", Layout.key_copy, copy_avail st view),
      (fun () -> copy st view);
    `Entry (c, "Paste", Layout.key_paste, paste_avail st view),
      (fun () -> paste st view);
    `Entry (c, "Crop", Layout.key_crop, crop_avail st view),
      (fun () -> crop st view);
    `Separator, ignore;
    `Entry (c, "Select All", Layout.key_all, select_all_avail st view),
      (fun () -> select_all st view);
    `Entry (c, "Select None", Layout.key_none, select_none_avail st view),
      (fun () -> select_none st view);
    `Entry (c, "Invert Selection", Layout.key_invert, select_invert_avail st view),
      (fun () -> select_invert st view);
    `Separator, ignore;
    `Entry (c, "Wipe", Layout.key_wipe, wipe_avail st view),
      (fun () -> wipe st view);
    `Entry (c, "Dedupe", Layout.key_dedupe, dedupe_avail st view),
      (fun () -> dedupe st view);
    `Separator, ignore;
    `Entry (c, "Undo", Layout.key_undo, undo_avail st view),
      (fun () -> undo st view);
    `Entry (c, "Redo", Layout.key_redo, redo_avail st view),
      (fun () -> redo st view);
    `Separator, ignore;
    `Entry (c, "Load...", Layout.key_load, load_avail st view),
      (fun () -> load st view);
    `Entry (c, "Save...", Layout.key_save, save_avail st view),
      (fun () -> save st view);
  |] (
    if View.(table it) == st.playlist.table then [||] else
    [|
      `Entry (c, "Save View...", Layout.key_save2, save_view_avail st view),
        (fun () -> save_view st view);
      `Entry (c, "Queue" ^ quant ^ " to Playlist...", Layout.key_queue,
        queue_avail st view),
        (fun () -> queue st view (get_tracks ()));
    |])
  )


(* Runner *)

let run_edit_panel (st : state) =
  let pl = st.playlist in
  let lib = st.library in
  let lay = st.layout in

  Layout.edit_pane lay;

  let lib_shows_tracks =
    match lib.current with
    | None -> false
    | Some dir -> dir.view.tracks.shown <> None
  in
  let pl_focus = pl.table.focus in
  let lib_focus =
    (lib.tracks.focus || lib.albums.focus || lib.artists.focus || lib.browser.focus)
  in
  let focus = pl_focus || lib_focus && lib_shows_tracks in

  assert (not (pl_focus && lib_focus));
  assert (lay.playlist_shown || not pl_focus);
  assert (lay.library_shown || not lib_focus);

  let playlist = playlist_view st in
  let library = tracks_view st in
  let view = if pl_focus then playlist else library in
  let module View = (val view) in

  let active_if avail = if focus && avail st view then Some false else None in

  (* Separator button *)
  if Layout.sep_button lay (active_if separator_avail) then
  (
    (* Click on Separator button: insert separator *)
    let pos = Option.value View.(first_selected it) ~default: 0 in
    separator st view pos
  );

  (* Edit buttons *)
  if Layout.del_button lay (active_if remove_avail)
  || remove_avail st view && Layout.del_button_alt lay then
  (
    (* Click on Delete button: remove selected tracks from playlist *)
    remove st view
  );

  if Layout.crop_button lay (active_if crop_avail) then
  (
    (* Click on Crop button: remove unselected tracks from playlist *)
    crop st view
  );

  if Layout.wipe_button lay (active_if wipe_avail) then
  (
    (* Click on Wipe button: remove invalid tracks from playlist *)
    wipe st view
  );

  if focus && Layout.dedupe_button lay then
  (
    (* Dedupe key pressed or Shift-click on Wipe button: dedupe *)
    dedupe st view
  );

  if Layout.undo_button lay (active_if undo_avail) then
  (
    (* Click on Undo button: pop undo *)
    undo st view
  );

  if focus && redo_avail st view && Layout.redo_button lay then
  (
    (* Redo key pressed or Shift-click on Undo button: pop redo *)
    redo st view
  );

  (* Edit keys *)
  if focus && cut_avail st view && Layout.cut_key lay then
  (
    (* Press of Cut key: remove selected tracks and write them to clipboard *)
    cut st view
  );

  if (focus || lib_focus) && copy_avail st view && Layout.copy_key lay then
  (
    (* Press of Copy key: write selected tracks to clipboard *)
    copy st view
  );

  if focus && paste_avail st view && Layout.paste_key lay then
  (
    (* Press of Paste key: insert tracks from clipboard *)
    paste st view
  );

  (* Tag button *)
  if Layout.tag_button lay (active_if tag_avail) then
  (
    (* Click on Tag button: execute tagging program *)
    let _, _, get_tracks = subject_tracks view in
    tag st (get_tracks ()) false;
  );

  if focus && tag_avail st view && Layout.tag_add_button lay then
  (
    (* Shift-click on Tag button: execute tagging program, additively *)
    let _, _, get_tracks = subject_tracks view in
    tag st (get_tracks ()) true;
  );

  (* Load button *)
  if Layout.load_button lay (active_if load_avail) then
  (
    (* Click on Load button: load playlist *)
    load st view
  );

  (* Save Playlist button *)
  if Layout.save_button lay (active_if save_avail)
  && Api.Key.are_modifiers_down [] then
  (
    (* Click on Save button: save playlist *)
    save st view
  );

  (* Save Viewlist button *)
  if lib_focus && Layout.save_view_button lay then
  (
    (* Press of Save-View key or Shift-Click on Save button: save viewlist *)
    if save_view_avail st view then
      save_view st view
  );

  (* Queue key *)
  if lib_focus && queue_avail st view && Layout.queue_key lay then
  (
    (* Press of Queue key: queue tracks *)
    let _, _, get_tracks = subject_tracks view in
    queue st view (get_tracks ())
  );

  (* Focus buttons *)
  if Layout.focus_next_key lay then State.focus_next st;
  if Layout.focus_prev_key lay then State.focus_prev st
