(* Library UI *)

open Audio_file

type state = State.t
type dir = Library.dir


(* Helpers *)

let refresh_delay = 9

let clamp = Layout.clamp

let fmt = Printf.sprintf


(* Commands *)

let rescan_all_avail (st : state) =
  st.library.root.children <> [||]
let rescan_all (st : state) mode =
  Option.iter (fun dir ->
    if Data.is_dir dir then Library.rescan_dirs st.library mode [|dir|]
  ) st.library.current

let rescan_one_avail (st : state) =
  st.library.current <> None && rescan_all_avail st
let rescan_one (st : state) mode =
  Option.iter (fun dir ->
    if Data.is_dir dir then Library.rescan_dirs st.library mode [|dir|]
  ) st.library.current

let insert_avail (st : state) =
  not st.layout.filesel_shown
let insert (st : state) =
  Run_filesel.filesel st `Read `Dir "" "" (fun path ->
    let roots = st.library.root.children in
    if not (Library.insert_roots st.library [path] (Array.length roots)) then
      Layout.browser_error_box st.layout;  (* flash *)
    State.focus_library st.library.browser st;
  )

let rename_avail (st : state) i_opt =
  match i_opt with
  | None -> false
  | Some i -> not (Data.is_all st.library.browser.entries.(i))

let rename (st : state) i_opt =
  Option.iter (fun i ->
    if i > 0 then
    (
      let dir = st.library.browser.entries.(i) in
      State.defocus_all st;
      Edit.focus st.library.rename;
      Edit.set st.library.rename dir.name;
      Ui.delay st.layout.ui (fun () -> Ui.modal st.layout.ui);
      Library.start_rename st.library i;
    )
  ) i_opt


let remove_avail (st : state) =
  Library.current_is_root st.library

let remove (st : state) =
  Option.iter (fun (dir : dir) ->
    if not (Library.remove_roots st.library [dir.path]) then
      Layout.browser_error_box st.layout  (* flash *)
  ) st.library.current

let remove_list_avail (st : state) =
  Library.current_is_playlist st.library ||
  Library.current_is_viewlist st.library

let remove_list (st : state) =
  let lib = st.library in
  Option.iter (fun (dir : dir) ->
    if Data.is_playlist dir && dir.tracks <> [||] then
    (
      Library.error lib "Playlist is not empty";
      Layout.browser_error_box st.layout  (* flash *)
    )
    else
    (
      assert (Data.is_playlist dir || Data.is_viewlist dir);
      (try File.delete dir.path with Sys_error msg ->
        Library.error lib ("Error deleting file " ^ dir.path ^ ", " ^ msg);
        Layout.browser_error_box st.layout  (* flash *)
      );
      if not (Library.remove_dir lib dir.path) then
        Layout.browser_error_box st.layout  (* flash *)
      else
        Library.refresh_artists_albums_tracks lib
    )
  ) lib.current


let create_list (st : state) ext s view_opt path =
  let lib = st.library in
  (match Library.find_dir lib File.(dir path // "") with
  | None ->
    Library.error lib
      ("Error creating file " ^ path ^ ", path is outside library");
    Layout.browser_error_box st.layout;  (* flash *)
  | Some parent ->
    let path =
      if String.lowercase_ascii (File.extension path) = ext
      then path
      else path ^ ext
    in
    File.store `Bin path s;
    match Library.insert_dir lib path with
    | None -> raise (Sys_error "library is out of sync")
    | Some dir ->
      Library.fold_dir lib parent false;
      Option.iter (fun view -> dir.view <- view) view_opt;
      Option.iter (Library.select_dir lib)
        (Array.find_index ((==) dir) lib.browser.entries)
  );
  State.focus_table lib.tracks st

let create_playlist_avail (st : state) =
  Library.current_is_dir st.library &&
  not (Library.current_is_all st.library)

let create_playlist (st : state) =
  Option.iter (fun (dir : dir) ->
    let path = if Data.is_dir dir then dir.path else File.dir dir.path in
    Run_filesel.filesel st `Write `File path ".m3u"
      (create_list st ".m3u" "" None);
  ) st.library.current

let create_viewlist_avail (st : state) =
  st.library.tracks.entries <> [||] &&
  ( st.library.search.text <> "" ||
    Table.num_selected st.library.artists > 0 ||
    Table.num_selected st.library.albums > 0 )

let create_viewlist (st : state) =
  Option.iter (fun (dir : dir) ->
    let query = Library.make_viewlist dir in
    let view = Library.copy_views dir.view in
    view.search <- "";
    let path = if Data.is_dir dir then dir.path else File.dir dir.path in
    Run_filesel.filesel st `Write `File path ".m3v"
      (create_list st ".m3v" query (Some view));
  ) st.library.current


(* Drag & Drop *)

let drag_on_browser (st : state) =
  let lay = st.layout in
  let lib = st.library in
  let browser = lib.browser in
  if st.layout.library_shown then
  (
    Option.iter (fun i ->
      if i < Array.length browser.entries then
      (
        let dir = browser.entries.(i) in
        if Data.is_playlist dir then
        (
          (* Drag over playlist browser entry: highlight target entry *)
          Ui.delay lay.ui (fun () -> Layout.browser_drag lay `Inside browser)
        )
      )
    ) (Layout.browser_mouse lay browser)
  )

let drop_on_browser (st : state) tracks =
  let lay = st.layout in
  let lib = st.library in
  let browser = lib.browser in
  if st.layout.library_shown then
  (
    Option.iter (fun i ->
      if i < Array.length browser.entries then
      (
        let dir = browser.entries.(i) in
        if Data.is_playlist dir then
        (
          (* Drop onto playlist browser entry: send tracks there *)
          (* Since the dir might not be selected, and updating views is
           * asynchronous, write to file directly *)
          (try
            let s = File.load `Bin dir.path in
            let s' = Track.to_m3u (Array.append (Track.of_m3u s) tracks) in
            File.store `Bin dir.path s'
          with exn ->
            Storage.log_exn "file" exn ("modifying playlist " ^ dir.path)
          );
          if Library.selected_dir lib = Some i then
          (
            Library.deselect_dir lib;  (* force reload *)
            Library.select_dir lib i;
            Library.refresh_artists_albums_tracks lib;
          );
        )
      )
    ) (Layout.browser_mouse lay browser)
  )


(* Generic view runner *)

let spin_delay = 3
let spins = [|"|"; "/"; "-"; "\\"|]
let spin win = spins.(Api.Draw.frame win / spin_delay mod Array.length spins)

let convert_sorting columns sorting =
  let index attr = Array.find_index (fun (a, _) -> a = attr) columns in
  List.map (fun (attr, order) -> Option.get (index attr), order) sorting


let run_view (st : state)
    layout grid_w
    (tab : _ Table.t) busy_tab dep_tab
    refresh_busy refresh_deps
    reorder (view : _ Library.view)
    attr_string prim_attr all_attrs
    path_of text_of color_of
    selected_tracks clicked_tracks
    editable make_view =
  let lib = st.library in
  let lay = st.layout in
  let win = Ui.window lay.ui in

  let pane, area, table, grid, spinner = layout in
  pane lay;

  let busy = refresh_busy lib in
  let tab = if busy then busy_tab else tab in
  let old_selected = tab.selected in
  let cols =
    Array.map (fun (attr, cw) -> cw, Library.attr_align attr) view.columns in
  let headings =
    Array.map (fun (attr, _) -> Library.attr_name attr) view.columns in

  if Option.get view.shown = `Grid
  && Api.Draw.frame win mod refresh_delay = 5 then
    Table.dirty tab;  (* to capture cover updates *)

  let entries = tab.entries in  (* could change concurrently *)
  let pp_row i =
    let entry = entries.(i) in
    color_of entry,
    Array.map (fun (attr, _) ->
      match (attr :> Data.any_attr) with
      | `Cover ->
        if not lib.cover then `Text "" else
        (match Library.load_cover lib win (path_of entry) with
        | Some img -> `Image img
        | None -> `Text ""
        )
      | _ -> `Text (attr_string entry attr)
    ) view.columns
  in

  let pp_cell i =
    let entry = entries.(i) in
    let img =
      match Library.load_cover lib win (path_of entry) with
      | Some img -> img
      | None -> Ui.nocover lay.ui
    in img, Ui.text_color lay.ui, text_of entry
  in

  let sorting = convert_sorting view.columns view.sorting in
  let header = Some (headings, sorting) in
  (match
    match Option.get view.shown with
    | `Table -> table lay cols header tab pp_row
    | `Grid -> grid lay grid_w header tab pp_cell
  with
  | `None | `Scroll -> ()

  | `Select ->
    (* New selection: grab focus, update filter *)
    State.focus_library tab st;
    refresh_deps lib;

  | `Sort i ->
    (* Click on column header: reorder view accordingly *)
    let attr = fst view.columns.(i) in
    let k =
      Bool.to_int (Api.Key.is_modifier_down `Shift) +
      Bool.to_int (Api.Key.is_modifier_down `Alt) * 2 +
      Bool.to_int (Api.Key.is_modifier_down `Command) * (-4)
    in
    view.sorting <- Data.insert_sorting prim_attr attr k 4 view.sorting;
    Option.iter (Library.save_dir lib) lib.current;
    reorder lib;

  | `Resize ws ->
    (* Column resizing: update column widths *)
    Array.mapi_inplace (fun i (attr, _) -> attr, ws.(i)) view.columns;
    Option.iter (Library.save_dir lib) lib.current;

  | `Reorder perm ->
    (* Column reordering: update columns *)
    Data.permute perm view.columns;
    Option.iter (Library.save_dir lib) lib.current;

  | `Click (Some i) when Api.Mouse.is_doubleclick `Left ->
    (* Double-click on entry: clear playlist and send tracks to it *)
    let n = Table.num_selected dep_tab in
    if n <> 0 && n <> Table.length dep_tab then
    (
      Table.deselect_all dep_tab;       (* deactivate inner filter *)
      Library.refresh_tracks_sync lib;  (* could be slow... *)
    );
    let tracks =
      if Api.Key.are_modifiers_down [`Command]
      then selected_tracks lib
      else clicked_tracks lib i
    in
    if tracks <> [||] then
    (
      Playlist.replace_all st.playlist (Array.copy tracks);
      (*
      Playlist.select_all st.playlist;
      State.focus_playlist st;
      *)
      Control.eject st.control;
      Control.switch st.control tracks.(0) true;
      Table.dirty st.library.tracks;  (* redraw for current track *)
      Table.dirty st.library.browser;
    )

  | `Click _ ->
    (* Single-click: grab focus, update filter *)
    if Api.Mouse.is_pressed `Left then State.focus_library tab st;
    if not (Table.IntSet.equal tab.selected old_selected) then
      refresh_deps lib;

  | `Move delta ->
    (* Cmd-cursor movement: move selection *)
    if editable then
      Library.move_selected lib delta;

  | `Drag (delta, motion, traj) ->
    (* Drag: adjust cursor *)
    if Api.Key.are_modifiers_down [] then
    (
      (* State.focus_library tab st; *)  (* don't steal after double-click! *)
      if Table.num_selected tab > 0 && lib.tracks.entries <> [||] then
      (
        if motion <> `Unmoved then Run_view.set_drop_cursor st;
        (match traj with
        | `Inside | `Inward when editable -> ()
        | `Inside | `Inward | `Outside | `Outward ->
          Run_view.drag_on_playlist st;
          drag_on_browser st;
        );

        if editable then
        (
          assert (layout == Layout.(if lay.lower_shown then lower_view else left_view));

          (* Invariant as for playlist view *)
          if motion = `Moving then
          (
            (* Start of drag & drop: remember original configuration *)
            Table.push_undo lib.tracks;
          );
          (match traj with
          | `Outward ->
            (* Leaving area: snap back to original state *)
            Library.undo lib;
            Library.save_playlist lib;
          | `Inward ->
            (* Reentering area: restore updated state *)
            Library.redo lib
          | `Inside | `Outside -> ()
          );

          (* Positional movement *)
          if delta <> 0 then
          (
            match traj with
            | `Inside | `Inward ->
              Library.move_selected lib delta;
              (* Erase intermediate new state *)
              Table.drop_undo lib.tracks;
            | `Outside | `Outward -> ()  (* ignore *)
          );
        )
      )
    );

  | `Drop ->
    if Api.Key.are_modifiers_down [] then
    (
      if Ui.mouse_inside lay.ui (area lay) then
      (
        (* Dropping inside own: drop aux undo if no change *)
        if editable then
          Table.clean_undo lib.tracks
      )
      else
      (
        (* Drag & drop originating from current view *)

        (* Dropping outside tracks: drop aux redo for new state *)
        if editable then
          Table.drop_redo lib.tracks;

        (* Drag & drop onto playlist or browser: send tracks to playlist *)
        let tracks = selected_tracks lib in
        Run_view.drop_on_playlist st tracks;
        drop_on_browser st tracks;
      )
    )

  | `Menu i_opt ->
    (* Right-click on content: context menu *)
    State.focus_library tab st;
    if editable then
      Run_view.(edit_menu st (make_view st) i_opt)
    else
      Run_view.(list_menu st (make_view st))

  | `HeadMenu i_opt ->
    (* Right-click on header: header menu *)
    State.focus_library tab st;
    let used_attrs = Array.to_list (Array.map fst view.columns) in
    let unused_attrs = Data.diff_attrs all_attrs used_attrs in
    let i, current_attrs =
      match i_opt with
      | None -> Array.length view.columns, []
      | Some i -> i, [fst view.columns.(i)]
    in
    Run_menu.header_menu st view i current_attrs unused_attrs
  );

  if busy then
    spinner lay (spin win)


(* Runner *)

let busy_artists = Table.make 0
let busy_albums = Table.make 0
let busy_tracks = Table.make 0

let run (st : state) =
  let pl = st.playlist in
  let lib = st.library in
  let lay = st.layout in
  let win = Ui.window lay.ui in

  (* Update after possible window resize *)
  lay.browser_width <-
    clamp (Layout.browser_min lay) (Layout.browser_max lay) lay.browser_width;
  lay.left_width <-
    clamp (Layout.left_min lay) (Layout.left_max lay) lay.left_width;
  lay.upper_height <-
    clamp (Layout.upper_min lay) (Layout.upper_max lay) lay.upper_height;

  Layout.browser_pane lay;

  (* Background rescanning *)
  Library.refresh_after_rescan lib;

  (* Browser *)
  let browser = lib.browser in
  let current_path =
    match st.control.current with Some track -> track.path | None -> "" in

  if Library.rescan_busy lib <> None
  && Api.Draw.frame win mod spin_delay = 0 then
    Table.dirty browser;   (* to draw spinner *)

  let entries = browser.entries in  (* could change concurrently *)
  let pp_entry i =
    let dir = entries.(i) in
    let spinning =
      match Library.rescan_busy lib with
      | None -> false
      | Some path ->
        path = dir.path ||
        dir.view.folded && String.starts_with ~prefix: dir.path path
    in
    let spin = if not spinning then "" else " " ^ spin win
    and folded = if dir.children = [||] then None else Some dir.view.folded
    and c =
      if dir.path = File.(dir current_path // "")
      || dir.view.folded && String.starts_with ~prefix: dir.path current_path
      then `White
      else Ui.text_color lay.ui
    in dir.nest, folded, c, dir.name ^ spin
  in

  let dir = Library.selected_dir lib in
  (match Layout.browser_table lay browser pp_entry with
  | `None | `Scroll | `Move _ -> ()

  | `Select ->
    (* TODO: allow multiple selections *)
    State.focus_library browser st;
    if Library.selected_dir lib <> dir then
    (
      (match Library.selected_dir lib with
      | None -> Library.deselect_dir lib
      | Some i -> Library.select_dir lib i  (* do bureaucracy *)
      );
      Library.deselect_all lib;
      Library.refresh_artists_albums_tracks lib;
    );

  | `Fold i ->
    (* Click on triangle: fold/unfold entry *)
    let dir = browser.entries.(i) in
    Library.fold_dir st.library dir (not dir.view.folded)

  | `Click (Some i) ->
    if Api.Mouse.is_pressed `Left then State.focus_library browser st;
    if Library.selected_dir lib <> dir then
    (
      (* Click on different dir name: switch view *)
      Library.select_dir lib i;  (* do bureaucracy *)
      Library.deselect_all lib;
      Library.refresh_artists_albums_tracks lib;
      let dir' = entries.(i) in
      lay.left_width <- dir'.view.divider_width;
      lay.upper_height <- dir'.view.divider_height;
    );
    if Api.Mouse.is_pressed `Left && Api.Key.are_modifiers_down [`Shift] then
    (
      (* Shift-Click on dir name: rename *)
      rename st (Some i)
    );
    if Api.Mouse.is_doubleclick `Left then
    (
      (* Double-click on directory name: send track view to playlist *)
      let n_artists = Table.num_selected lib.artists in
      let n_albums = Table.num_selected lib.albums in
      if n_artists <> 0 && n_artists <> Table.length lib.artists
      || n_albums <> 0 && n_albums <> Table.length lib.albums then
      (
        Table.deselect_all lib.artists;   (* deactivate inner filters *)
        Table.deselect_all lib.albums;
        Library.refresh_albums_tracks_sync lib;  (* could be slow... *)
      );
      let tracks = lib.tracks.entries in
      if tracks <> [||] then
      (
        Playlist.replace_all pl (Array.copy tracks);
        State.focus_playlist st;
        Control.eject st.control;
        Control.switch st.control tracks.(0) true;
        Table.dirty st.library.tracks;  (* redraw for current track *)
        Table.dirty st.library.browser;
      )
    )

  | `Click None ->
    (* Click into empty space: deselect everything *)
    Library.deselect_dir lib;
    Library.deselect_all lib;
    Library.refresh_artists_albums_tracks lib;
    if Api.Mouse.is_pressed `Left then State.focus_library browser st;

  | `Drag (_, motion, _) ->
    (* Drag: adjust cursor *)
    if Api.Key.are_modifiers_down [] then
    (
      (* State.focus_library browser st; *)  (* don't steal after double-click! *)
      if lib.tracks.entries <> [||] then
      (
        if motion <> `Unmoved then Run_view.set_drop_cursor st;
        Run_view.drag_on_playlist st;
      );

      (* Intra-browser drag *)
      Option.iter (fun i ->
        if Library.selected_dir lib <> Some i then
        (
          Option.iter (fun j ->
            let dir = browser.entries.(j) in
            if
              i = Table.length browser && Data.is_root dir ||
              browser.entries.(i).parent = dir.parent
            then
            (
              (* Drag over sibling: reorder entry *)
              Api.Mouse.set_cursor (Ui.window lay.ui) `Point;
              Layout.browser_drag lay `Above browser;
            )
            else
            (
              (* Drag over other browser entry *)
              drag_on_browser st;
            )
          ) (Library.selected_dir lib)
        )
      ) (Layout.browser_mouse lay browser)
    )

  | `Drop ->
    (* Drop originating from browser *)
    if Api.Key.are_modifiers_down [] then
    (
      (* Drop onto playlist: send directory contents to playlist *)
      let tracks = lib.tracks.entries in
      Run_view.drop_on_playlist st tracks;

      (* Intra-browser drop *)
      Option.iter (fun i ->
        if Library.selected_dir lib <> Some i then
        (
          Option.iter (fun j ->
            let dir = browser.entries.(j) in
            if
              i = Table.length browser && Data.is_root dir ||
              browser.entries.(i).parent = dir.parent
            then
            (
              (* Drop on sibling: reorder entry *)
              let parent = Option.get (Library.find_parent lib dir) in
              let pos = Library.find_parent_pos lib dir in
              let pos' =
                if i = Table.length browser
                then Array.length lib.root.children
                else Library.find_parent_pos lib entries.(i)
              in
              Library.move_dir lib parent pos
                (if pos' > pos then pos' - 1 else pos');
            )
            else
            (
              (* Drop on other browser entry *)
              drop_on_browser st tracks;
            )
          ) (Library.selected_dir lib)
        )
      ) (Layout.browser_mouse lay browser)
    )

  | `Menu i_opt ->
    (* Right-click on browser: context menu *)
    State.focus_library browser st;
    let c = Ui.text_color lay.ui in
    let all, quant =
      if lib.current = None then true, " All" else false, "" in
    Run_menu.command_menu st [|
      `Entry (c, "Rescan" ^ quant ^ " Quick", Layout.key_rescan,
        if all then rescan_all_avail st else rescan_one_avail st),
        (fun () -> (if all then rescan_all else rescan_one) st `Quick);
      `Entry (c, "Rescan" ^ quant ^ " Thorough", Layout.key_rescan2,
        if all then rescan_all_avail st else rescan_one_avail st),
        (fun () -> (if all then rescan_all else rescan_one) st `Thorough);
      `Separator, ignore;
      `Entry (c, "Create Playlist...", Layout.key_newdir, create_playlist_avail st),
        (fun () -> create_playlist st);
      `Entry (c, "Create Viewlist...", Layout.key_viewdir, create_viewlist_avail st),
        (fun () -> create_playlist st);
      `Entry (c, "Rename", Layout.key_namedir, rename_avail st i_opt),
        (fun () -> rename st i_opt);
      `Entry (c, "Remove " ^
        (if Library.current_is_viewlist lib then "Viewlist" else "Playlist"),
        Layout.key_deldir, remove_list_avail st),
        (fun () -> remove_list st);
      `Separator, ignore;
      `Entry (c, "Add Root...", Layout.key_adddir, insert_avail st),
        (fun () -> insert st);
      `Entry (c, "Remove Root", Layout.key_deldir, remove_avail st),
        (fun () -> remove st);
      `Separator, ignore;
      `Entry (c, "Search...", Layout.key_search, lib.current <> None),
        (fun () -> State.focus_edit lib.search st);
    |]
  );

  let entries = browser.entries in  (* might have changed from un/folding *)

  (* Browser entry renaming *)
  let rename_had_focus = lib.rename.focus in
  Option.iter (fun i ->
    Ui.nonmodal lay.ui;
    let dir = entries.(i) in
    let folded = if dir.children = [||] then None else Some dir.view.folded in
    let area = Layout.rename_area lay browser i dir.nest folded in

    Layout.rename_box lay area;
    let _ = Layout.rename_edit lay area lib.rename in
    if Api.Key.is_released `Escape then
    (
      Library.end_rename lib false;
      Edit.defocus lib.rename;
      State.focus_table browser st;
    )
    else if Api.Key.(is_released `Return || is_released `Enter)
    || Api.Mouse.is_released `Left && not (Ui.mouse_inside lay.ui area) then
    (
      Library.end_rename lib (dir.name <> lib.rename.text);
      dir.name <- lib.rename.text;
      Edit.defocus lib.rename;
      State.focus_table browser st;
    )
    else
      Ui.modal lay.ui
  ) lib.renaming;

  (* Keys *)
  if Layout.rename_key lay (browser.focus && not rename_had_focus) then
  (
    (* Return or Enter key pressed: rename dir *)
    if not (Library.current_is_all lib) then
      rename st (Library.selected_dir lib)
  );

  if Layout.fold_key lay browser.focus then
  (
    (* Space key pressed: fold/unfold dir *)
    if not (Library.current_is_all lib) then
      Option.iter (fun (dir : dir) ->
        Library.fold_dir lib dir (not dir.view.folded)
      ) lib.current
  );

  (* Browser drag & drop *)
  let dropped = Api.Files.dropped win in
  if dropped <> [] then
  (
    Option.iter (fun i ->
      let pos =
        if i = Array.length entries
        then Array.length lib.root.children
        else Library.find_parent_pos lib entries.(i)
      in
      if not (Library.insert_roots lib dropped pos) then
        Layout.browser_error_box lay;  (* flash *)
    ) (Layout.browser_mouse lay browser)
  );

  (* Buttons *)
  let active_if avail = if avail st then Some false else None in
  let active_if2 avail1 avail2 =
    if avail1 st || avail2 st then Some false else None in

  if Layout.insert_button lay (active_if insert_avail) then
  (
    (* Click on Insert (Add) button: add directory or playlist *)
    insert st
  );

  if Layout.remove_button lay (active_if2 remove_avail remove_list_avail) then
  (
    (* Click on Remove (Del) button: remove directory or playlist *)
    let dir = Option.get (lib.current) in
    if Data.is_dir dir then
      remove st
    else if Data.is_playlist dir || Data.is_viewlist dir then
      remove_list st
  );

  if Layout.create_button lay (active_if create_playlist_avail) then
  (
    (* Click on Create (New) button: create new playlist *)
    create_playlist st
  );

  if Layout.view_button lay (active_if create_viewlist_avail) then
  (
    (* Click on View button: create new viewlist *)
    create_viewlist st
  );

  if Layout.rescan_button lay (active_if rescan_one_avail) then
  (
    (* Click on Rescan (Scan) button: rescan directory, view, or files *)
    Option.iter (fun (dir : dir) ->
      let mode =
        if Api.Key.is_modifier_down `Shift
        || dir.view.tracks.shown <> None && Table.has_selection lib.tracks
        || dir.view.albums.shown <> None && Table.has_selection lib.albums
        || dir.view.artists.shown <> None && Table.has_selection lib.artists
        then `Thorough else `Quick
      in
      if Table.has_selection lib.tracks then
        Library.rescan_tracks lib mode (Library.selected lib)
      else if
        Table.has_selection lib.artists || Table.has_selection lib.albums ||
        dir.view.search <> ""
      then
        Library.rescan_tracks lib mode lib.tracks.entries
      else
        Library.rescan_dirs lib mode [|dir|]
    ) lib.current
  );

  (* Scanning indicator *)
  Layout.scan_label lay;
  Layout.scan_indicator lay (Library.rescan_busy lib <> None);
  if Layout.scan_button lay then
  (
    let shift = Api.Key.is_modifier_down `Shift in
    if shift then
      (* Scanning indicator shift-clicked: purge cached covers *)
      Library.purge_covers lib;

    if Library.rescan_busy lib = None then
    (
      (* Inactive scanning indicator clicked: rescan *)
      let mode = if shift then `Thorough else `Quick in
      match Library.selected_dir lib with
      | None -> Library.rescan_root lib mode
      | Some i ->
        let dir = entries.(i) in
        if Data.is_dir dir then Library.rescan_dirs lib mode [|dir|]
    )
  );

  (* Browse modes *)
  let have_dir = lib.current <> None in
  let default =
    if Array.length entries > 0 then entries.(0) else
    Data.make_dir "" None 0 (Library.make_views "")
  in
  let dir = Option.value lib.current ~default in
  let view = dir.view in
  let cycle_shown = function
    | None -> Some `Table
    | Some `Table -> Some `Grid
    | Some `Grid -> None
  in

  let nothing_shown (view : Library.views) =
    view.artists.shown = None &&
    view.albums.shown = None &&
    view.tracks.shown = None
  in

  let artists = have_dir && view.artists.shown <> None in
  Layout.artists_label lay;
  Layout.artists_indicator lay artists;
  let artists' =
    Layout.artists_button lay (if have_dir then Some artists else None) in
  if have_dir && artists' <> artists then
  (
    (* Click on Artists button: toggle artist pane *)
    view.artists.shown <- if artists' then Some `Table else None;
    if nothing_shown view then
      view.tracks.shown <- Some `Table;  (* switch to tracks *)
    Library.save_dir lib dir;
  );

  let albums = have_dir && view.albums.shown <> None in
  Layout.albums_label lay;
  Layout.albums_indicator1 lay (view.albums.shown = Some `Table);
  Layout.albums_indicator2 lay (view.albums.shown = Some `Grid);
  let albums' =
    Layout.albums_button lay (if have_dir then Some albums else None) in
  if have_dir && albums' <> albums then
  (
    (* Click on Albums button: toggle artist pane *)
    view.albums.shown <- cycle_shown view.albums.shown;
    if nothing_shown view then
      view.albums.shown <- Some `Table;
    Library.save_dir lib dir;
  );

  let tracks = have_dir && view.tracks.shown <> None in
  Layout.tracks_label lay;
  Layout.tracks_indicator1 lay (view.tracks.shown = Some `Table);
  Layout.tracks_indicator2 lay (view.tracks.shown = Some `Grid);
  let tracks' =
    Layout.tracks_button lay (if have_dir then Some tracks else None) in
  if have_dir && tracks' <> tracks then
  (
    (* Click on Tracks button: toggle artist pane *)
    view.tracks.shown <- cycle_shown view.tracks.shown;
    if nothing_shown view then
      view.tracks.shown <- Some `Table;
    Library.save_dir lib dir;
  );

  let show_artists =
    have_dir && view.artists.shown <> None && lay.playlist_shown in
  let show_albums =
    have_dir && view.albums.shown <> None && lay.playlist_shown in
  let show_tracks =
    not have_dir || view.tracks.shown <> None || not lay.playlist_shown in
  lay.right_shown <- show_artists && show_albums;
  lay.lower_shown <- show_tracks && (show_artists || show_albums);

  (* Search *)
  Layout.search_label lay;
  Layout.search_box lay;
  if have_dir then
  (
    if Layout.search_key lay then
    (
      (* Search button pressed: focus search *)
      Library.focus_search lib;
    )
    else if Layout.search_button lay then
    (
      (* Click on Search label: clear and focus search *)
      if lib.search.text <> "" then
      (
        Edit.clear lib.search;
        Library.set_search lib "";
      );
      Library.focus_search lib;
    );

    let search = lib.search.text in
    let _ = Layout.search_edit lay lib.search in
    if lib.search.focus then
    (
      (* Have or gained focus: make sure it's consistent *)
      State.defocus_all st;
      Library.focus_search lib;
    );
    if lib.search.text <> search then
    (
      (* Changed search text: update search in dir *)
      Library.set_search lib lib.search.text;
    );

    if Layout.search_context lay then
    (
      let rec nub = function
        | [] -> []
        | x::xs -> x :: nub (List.filter ((<>) x) xs)
      in
      let c = Ui.text_color lay.ui in
      let history = Edit.history lib.search in
      let history' = nub history in
      Run_menu.command_menu st ([
        `Entry (c, "Clear Search", Layout.key_clear_search, lib.search.text <> ""),
          (fun () -> Edit.clear lib.search; Library.set_search lib "";
            State.focus_edit lib.search st);
        `Entry (c, "Clear Search History", Layout.key_clear_history, history <> []),
          (fun () ->
            Edit.clear_history lib.search;
            Data.iter_dir (fun (dir : Library.dir) ->
              if dir.view.search <> "" then
              (
                dir.view.search <- "";
                Library.save_dir lib dir;
              )
            ) lib.root;
            State.focus_edit lib.search st
          );
      ] @ (
        if history = [] then [] else [`Separator, ignore]
      ) @ List.map (fun s ->
        `Entry (c, "Search for " ^ s, Layout.nokey, true),
          (fun () -> Edit.set lib.search s; Library.set_search lib s;
            State.focus_edit lib.search st)
      ) history' |> Array.of_list)
    )
  );


  (* Info pane *)

  Layout.info_pane lay;

  Layout.msg_box lay;
  let now = Unix.gettimeofday () in
  if lib.error <> "" && now -. lib.error_time < 10.0 then
    Layout.msg_text lay (Ui.error_color lay.ui) `Regular true lib.error
  else
  (
    let tr = Table.length lib.tracks in
    let al = Table.length lib.albums in
    let ar = Table.length lib.artists in
    let trs = Table.num_selected lib.tracks in
    let als = Table.num_selected lib.albums in
    let ars = Table.num_selected lib.artists in
    let sel n = if n = 0 then "" else string_of_int n ^ "/" in
    let plu n = if n = 1 then "" else "s" in
(*
    let count name m n shown =
      if shown then Some (fmt "%s%d %s%s" (sel m) n name (plu n)) else None in
    let counts =
      [ count "artist" ars ar artists;
        count "album" als al albums;
        count "track" trs tr tracks;
      ]
    in
    Layout.msg_text lay (Ui.text_color lay.ui) `Regular true
      (String.concat ", " (List.filter_map Fun.id counts))
*)
    let count name m n = fmt "%s%d %s%s" (sel m) n name (plu n) in
    let counts = String.concat ", "
      [count "artist" ars ar; count "album" als al; count "track" trs tr] in
    Layout.msg_text lay (Ui.text_color lay.ui) `Regular true counts
  );


  (* Artists view *)

  if show_artists then
  (
    run_view st Layout.left_view 1
      lib.artists busy_artists lib.albums
      Library.refresh_artists_busy Library.refresh_albums_tracks
      Library.reorder_artists view.artists
      Data.artist_attr_string `Artist Data.artist_attrs
      (fun _ -> "") (fun _ -> "") (fun _ -> Ui.text_color lay.ui)
      (fun lib -> lib.tracks.entries) (fun lib _ -> lib.tracks.entries)
      false Run_view.artists_view;
  );

  (* Albums view *)

  if show_albums then
  (
    let text_of album =
      let artist = Data.album_attr_string album `AlbumArtist in
      let title = Data.album_attr_string album `AlbumTitle in
      let year = Data.album_attr_string album `Year in
      artist ^ " - " ^ title ^ (if year = "" then "" else " (" ^ year ^ ")")
    in

    run_view st
      Layout.(if lay.right_shown then right_view else left_view) lay.albums_grid
      lib.albums busy_albums busy_tracks
      Library.refresh_albums_busy Library.refresh_tracks
      Library.reorder_albums view.albums
      Data.album_attr_string `None Data.album_attrs
      (fun (album : Data.album) -> album.path) text_of
      (fun _ -> Ui.text_color lay.ui)
      (fun lib -> lib.tracks.entries) (fun lib _ -> lib.tracks.entries)
      false Run_view.albums_view;

    (* Divider *)
    if lay.right_shown then
    (
      let left_width' = Layout.right_divider lay lay.left_width
        (Layout.left_min lay) (Layout.left_max lay) in
      (* Possible drag of divider: update pane width *)
      lay.left_width <- left_width';
      dir.view.divider_width <- left_width';
    );
  );


  (* Tracks view *)

  if show_tracks then
  (
    let text_of track =
      let artist = Data.track_attr_string track `Artist in
      let title = Data.track_attr_string track `Title in
      let year = Data.track_attr_string track `Year in
      artist ^ " - " ^ title ^ (if year = "" then "" else " (" ^ year ^ ")")
    in

    let color_of (track : Data.track) =
      if (track.status = `Undet || track.status = `Predet)
      && Library.rescan_busy lib = None then
        Track.update track;
      match track.status with
      | _ when track.path = current_path -> `White
      | `Absent -> Ui.error_color lay.ui
      | `Invalid -> Ui.warn_color lay.ui
      | `Undet -> Ui.semilit_color (Ui.text_color lay.ui)
      | `Predet | `Det ->
        if track.pos = -1 || Data.is_separator track || Library.has_track lib track then
          Ui.text_color lay.ui
        else
          Ui.warn_color lay.ui
    in

    let prim_attr =
      if Library.current_is_playlist lib
      || Library.current_is_viewlist lib then `Pos else `FilePath
    in

    run_view st
      Layout.(if lay.lower_shown then lower_view else left_view) lay.tracks_grid
      lib.tracks busy_tracks busy_tracks
      Library.refresh_tracks_busy ignore
      Library.reorder_tracks view.tracks
      Data.track_attr_string prim_attr Data.track_attrs
      (fun (track : Data.track) -> track.path) text_of color_of
      Library.selected (fun lib i -> [|lib.tracks.entries.(i)|])
      (Library.current_is_plain_playlist lib) Run_view.tracks_view;

    (* Playlist file drag & drop *)
    Run_view.external_drop_on_tracks st;

    (* Divider *)
    if lay.lower_shown then
    (
      let upper_height' = Layout.lower_divider lay lay.upper_height
        (Layout.upper_min lay) (Layout.upper_max lay) in
      (* Possible drag of divider: update pane width *)
      lay.upper_height <- upper_height';
      dir.view.divider_height <- upper_height';
    );
  );

  (* Keys *)

  if
    Ui.key lay.ui Layout.key_copy
      ( (lib.browser.focus || lib.artists.focus || lib.albums.focus) &&
        lib.tracks.entries <> [||] )
  then
  (
    (* Press of Copy key: write selected tracks to clipboard *)
    let s = Track.to_m3u lib.tracks.entries in
    Api.Clipboard.write win s;
  );

  (* Pane divider *)

  let browser_width' = Layout.browser_divider lay lay.browser_width
    (Layout.browser_min lay) (Layout.browser_max lay) in
  (* Possible drag of divider: update pane width *)
  lay.browser_width <- browser_width'
