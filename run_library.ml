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
  st.library.search.text <> "" && st.library.tracks.entries <> [||]

let create_viewlist (st : state) =
  Option.iter (fun (dir : dir) ->
    let prefix =
      if Data.is_all dir || Data.is_viewlist dir then ""
      else "\"" ^ dir.path ^ "\" @ #filepath "
    in
    let query = prefix ^ st.library.search.text in
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


(* Runner *)

let spin_delay = 3
let spins = [|"|"; "/"; "-"; "\\"|]
let spin win = spins.(Api.Draw.frame win / spin_delay mod Array.length spins)

let convert_sorting columns sorting =
  let index attr = Array.find_index (fun (a, _) -> a = attr) columns in
  List.map (fun (attr, order) -> Option.get (index attr), order) sorting

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
    (* Click on dir name: switch view *)
    (* TODO: allow multiple selections *)
    if Api.Mouse.is_pressed `Left then State.focus_library browser st;
    if Library.selected_dir lib <> dir then
    (
      Library.select_dir lib i;  (* do bureaucracy *)
      Library.deselect_all lib;
      Library.refresh_artists_albums_tracks lib;
      let dir' = entries.(i) in
      lay.left_width <- dir'.view.divider_width;
      lay.upper_height <- dir'.view.divider_height;
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

  | `Drag (_, _, motion) ->
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

  | `Menu _ ->
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
      `Entry (c, "Add Root...", Layout.key_adddir, insert_avail st),
        (fun () -> insert st);
      `Entry (c, "Remove Root", Layout.key_deldir, remove_avail st),
        (fun () -> remove st);
      `Separator, ignore;
      `Entry (c, "Create Playlist...", Layout.key_newdir, create_playlist_avail st),
        (fun () -> create_playlist st);
      `Entry (c, "Create Viewlist...", Layout.key_viewdir, create_viewlist_avail st),
        (fun () -> create_playlist st);
      `Entry (c, "Remove " ^
        (if Library.current_is_viewlist lib then "Viewlist" else "Playlist"),
        Layout.key_deldir, remove_list_avail st),
        (fun () -> remove_list st);
      `Separator, ignore;
      `Entry (c, "Search...", Layout.key_search, lib.current <> None),
        (fun () -> State.focus_edit lib.search st);
    |]
  );

  let entries = browser.entries in  (* might have changed from un/folding *)

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
    let _ = Layout.search_text lay lib.search in
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
    let artists_pane, artists_area, artists_table, _grid, artists_spin =
      Layout.left_view in
    artists_pane lay;

    let busy = Library.refresh_artists_busy lib in
    let tab = if busy then busy_artists else lib.artists in
    let old_selected = tab.selected in
    let cols =
      Array.map (fun (attr, cw) -> cw, Library.attr_align attr)
        view.artists.columns
    and headings =
      Array.map (fun (attr, _) -> Library.attr_name attr) view.artists.columns
    in

    let entries = tab.entries in  (* could change concurrently *)
    let pp_row i =
      let artist = entries.(i) in
      Ui.text_color lay.ui,
      Array.map (fun (attr, _) -> `Text (Data.artist_attr_string artist attr))
        view.artists.columns
    in

    let sorting = convert_sorting view.artists.columns view.artists.sorting in
    (match artists_table lay cols (Some (headings, sorting)) tab pp_row with
    | `None | `Scroll | `Move _ -> ()

    | `Select ->
      (* New selection: grab focus, update filter *)
      State.focus_library tab st;
      Library.refresh_albums_tracks lib;

    | `Sort i ->
      (* Click on column header: reorder view accordingly *)
      let attr = fst view.artists.columns.(i) in
      let k =
        Bool.to_int (Api.Key.is_modifier_down `Shift) +
        Bool.to_int (Api.Key.is_modifier_down `Alt) * 2 +
        Bool.to_int (Api.Key.is_modifier_down `Command) * (-4)
      in
      view.artists.sorting <-
        Data.insert_sorting `Artist attr k 4 view.artists.sorting;
      Library.save_dir lib dir;
      Library.reorder_artists lib;

    | `Resize ws ->
      (* Column resizing: update column widths *)
      Array.mapi_inplace (fun i (a, _) -> a, ws.(i)) view.artists.columns;
      if have_dir then Library.save_dir lib dir;

    | `Reorder perm ->
      (* Column reordering: update columns *)
      Data.permute perm view.artists.columns;
      if have_dir then Library.save_dir lib dir;

    | `Click (Some _i) when Api.Mouse.is_doubleclick `Left ->
      (* Double-click on track: clear playlist and send tracks to it *)
      let n_albums = Table.num_selected lib.albums in
      if n_albums <> 0 && n_albums <> Table.length lib.albums then
      (
        Table.deselect_all lib.albums;    (* deactivate inner filter *)
        Library.refresh_tracks_sync lib;  (* could be slow... *)
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

    | `Click _ ->
      (* Single-click: grab focus, update filter *)
      if Api.Mouse.is_pressed `Left then State.focus_library tab st;
      if not (Table.IntSet.equal tab.selected old_selected) then
        Library.refresh_albums_tracks lib;

    | `Drag (_, _, motion) ->
      (* Drag: adjust cursor *)
      if Api.Key.are_modifiers_down [] then
      (
        (* State.focus_library tab st; *)  (* don't steal after double-click! *)
        if Table.num_selected lib.artists > 0 && lib.tracks.entries <> [||] then
        (
          if motion <> `Unmoved then Run_view.set_drop_cursor st;
          Run_view.drag_on_playlist st;
          drag_on_browser st;
        )
      );

    | `Drop ->
      if Api.Key.are_modifiers_down []
      && not (Ui.mouse_inside lay.ui (artists_area lay)) then
      (
        (* Drag & drop originating from artists view *)

        (* Drag & drop onto playlist or browser: send tracks to playlist *)
        let tracks = lib.tracks.entries in
        Run_view.drop_on_playlist st tracks;
        drop_on_browser st tracks;
      )

    | `Menu _ ->
      (* Right-click on artists: context menu *)
      State.focus_library tab st;
      let c = Ui.text_color lay.ui in
      let tracks = lib.tracks.entries in
      let quant = if Table.has_selection tab then "" else " All" in
      let view = Run_view.library_view st in
      Run_menu.command_menu st [|
        `Entry (c, "Tag" ^ quant, Layout.key_tag, Run_view.tag_avail st view),
          (fun () -> Run_view.tag st tracks false);
        `Entry (c, "Rescan" ^ quant, Layout.key_rescan, Run_view.rescan_avail st view),
          (fun () -> Run_view.rescan st tracks);
        `Separator, ignore;
        `Entry (c, "Select All", Layout.key_all,  Table.(num_selected tab < length tab)),
          (fun () -> Table.select_all tab; Library.refresh_albums_tracks lib);
        `Entry (c, "Select None", Layout.key_none, Table.(num_selected tab > 0)),
          (fun () -> Table.deselect_all tab; Library.refresh_albums_tracks lib);
        `Entry (c, "Invert Selection", Layout.key_invert, Table.(num_selected tab > 0)),
          (fun () -> Table.select_invert tab; Library.refresh_albums_tracks lib);
        `Separator, ignore;
        `Entry (c, "Search...", Layout.key_search, lib.current <> None),
          (fun () -> State.focus_edit lib.search st);
      |]

    | `HeadMenu i_opt ->
      (* Right-click on artists header: header menu *)
      State.focus_library tab st;
      let used_attrs = Array.to_list (Array.map fst view.artists.columns) in
      let unused_attrs = Data.diff_attrs Data.artist_attrs used_attrs in
      let i, current_attrs =
        match i_opt with
        | None -> Array.length view.artists.columns, []
        | Some i -> i, [fst view.artists.columns.(i)]
      in
      Run_menu.header_menu st dir.view.artists i current_attrs unused_attrs
    );

    if busy then
      artists_spin lay (spin win);
  );

  (* Albums view *)

  if show_albums then
  (
    let albums_pane, albums_area, albums_table, albums_grid, albums_spin =
      Layout.(if lay.right_shown then right_view else left_view) in
    albums_pane lay;

    let busy = Library.refresh_albums_busy lib in
    let tab = if busy then busy_albums else lib.albums in
    let old_selected = tab.selected in
    let cols =
      Array.map (fun (attr, cw) -> cw, Library.attr_align attr)
        view.albums.columns
    and headings =
      Array.map (fun (attr, _) -> Library.attr_name attr) view.albums.columns
    in

    if Option.get view.albums.shown = `Grid
    && Api.Draw.frame win mod refresh_delay = 2 then
      Table.dirty tab;  (* to capture cover updates *)

    let entries = tab.entries in  (* could change concurrently *)
    let pp_row i =
      let album = entries.(i) in
      Ui.text_color lay.ui,
      Array.map (fun (attr, _) ->
        if attr <> `Cover then
          `Text (Data.album_attr_string album attr)
        else if lib.cover then
          match Library.load_cover lib win album.path with
          | Some img -> `Image img
          | None -> `Text ""
        else `Text ""
      ) view.albums.columns
    in

    let pp_cell i =
      let album = entries.(i) in
      let img =
        match Library.load_cover lib win album.path with
        | Some img -> img
        | None -> Ui.nocover lay.ui
      and txt =
        Data.album_attr_string album `AlbumArtist ^ " - " ^
        Data.album_attr_string album `AlbumTitle ^ " (" ^
        Data.album_attr_string album `Year ^ ")"
      in img, Ui.text_color lay.ui, txt
    in

    let sorting = convert_sorting view.albums.columns view.albums.sorting in
    let header = Some (headings, sorting) in
    (match
      match Option.get view.albums.shown with
      | `Table -> albums_table lay cols header tab pp_row
      | `Grid -> albums_grid lay lay.albums_grid header tab pp_cell
    with
    | `None | `Scroll | `Move _ -> ()

    | `Select ->
      (* New selection: grab focus, update filter *)
      State.focus_library tab st;
      Library.refresh_tracks lib;

    | `Sort i ->
      (* Click on column header: reorder view accordingly *)
      let attr = fst view.albums.columns.(i) in
      let k =
        Bool.to_int (Api.Key.is_modifier_down `Shift) +
        Bool.to_int (Api.Key.is_modifier_down `Alt) * 2 +
        Bool.to_int (Api.Key.is_modifier_down `Command) * (-4)
      in
      view.albums.sorting <-
        Data.insert_sorting `None attr k 4 view.albums.sorting;
      Library.save_dir lib dir;
      Library.reorder_albums lib;

    | `Resize ws ->
      (* Column resizing: update column widths *)
      Array.mapi_inplace (fun i (a, _) -> a, ws.(i)) view.albums.columns;
      if have_dir then Library.save_dir lib dir;

    | `Reorder perm ->
      (* Column reordering: update columns *)
      Data.permute perm view.albums.columns;
      if have_dir then Library.save_dir lib dir;

    | `Click (Some _i) when Api.Mouse.is_doubleclick `Left ->
      (* Double-click on track: clear playlist and send tracks to it *)
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

    | `Click _ ->
      (* Single-click: grab focus, update filter *)
      if Api.Mouse.is_pressed `Left then State.focus_library tab st;
      if not (Table.IntSet.equal tab.selected old_selected) then
        Library.refresh_tracks lib;

    | `Drag (_, _, motion) ->
      (* Drag: adjust cursor *)
      if Api.Key.are_modifiers_down [] then
      (
        (* State.focus_library tab st; *)  (* don't steal after double-click! *)
        if Table.num_selected lib.albums > 0 && lib.tracks.entries <> [||] then
        (
          if motion <> `Unmoved then Run_view.set_drop_cursor st;
          Run_view.drag_on_playlist st;
          drag_on_browser st;
        )
      );

    | `Drop ->
      if Api.Key.are_modifiers_down []
      && not (Ui.mouse_inside lay.ui (albums_area lay)) then
      (
        (* Drag & drop originating from albums view *)

        (* Drag & drop onto playlist or browser: send tracks to playlist *)
        let tracks = lib.tracks.entries in
        Run_view.drop_on_playlist st tracks;
        drop_on_browser st tracks;
      )

    | `Menu _ ->
      (* Right-click on albums content: context menu *)
      State.focus_library tab st;
      let c = Ui.text_color lay.ui in
      let cmd = Api.Key.is_modifier_down `Command in
      let tracks = lib.tracks.entries in
      let quant = if Table.has_selection tab then "" else " All" in
      let view = Run_view.library_view st in
      Run_menu.command_menu st [|
        `Entry (c, "Tag" ^ quant, Layout.key_tag, Run_view.tag_avail st view),
          (fun () -> Run_view.tag st tracks cmd);
        `Entry (c, "Rescan" ^ quant, Layout.key_rescan, Run_view.rescan_avail st view),
          (fun () -> Run_view.rescan st tracks);
        `Separator, ignore;
        `Entry (c, "Select All", Layout.key_all, Table.(num_selected tab < length tab)),
          (fun () -> Table.select_all tab; Library.refresh_tracks lib);
        `Entry (c, "Select None", Layout.key_none, Table.(num_selected tab > 0)),
          (fun () -> Table.deselect_all tab; Library.refresh_tracks lib);
        `Entry (c, "Invert Selection", Layout.key_invert, Table.(num_selected tab > 0)),
          (fun () -> Table.select_invert tab; Library.refresh_tracks lib);
        `Separator, ignore;
        `Entry (c, "Search...", Layout.key_search, lib.current <> None),
          (fun () -> State.focus_edit lib.search st);
      |]

    | `HeadMenu i_opt ->
      (* Right-click on albums header: header menu *)
      State.focus_library tab st;
      let used_attrs = Array.to_list (Array.map fst view.albums.columns) in
      let unused_attrs = Data.diff_attrs Data.album_attrs used_attrs in
      let i, current_attrs =
        match i_opt with
        | None -> Array.length view.albums.columns, []
        | Some i -> i, [fst view.albums.columns.(i)]
      in
      Run_menu.header_menu st dir.view.albums i current_attrs unused_attrs
    );

    if busy then
      albums_spin lay (spin win);

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
    let tracks_pane, tracks_area, tracks_table, tracks_grid, tracks_spin =
      Layout.(if lay.lower_shown then lower_view else left_view) in
    tracks_pane lay;

    let busy = Library.refresh_tracks_busy lib in
    let tab = if busy then busy_tracks else lib.tracks in
    let cols =
      Array.map (fun (attr, cw) -> cw, Library.attr_align attr)
        view.tracks.columns
    and headings =
      Array.map (fun (attr, _) -> Library.attr_name attr) view.tracks.columns
    in

    if Option.get view.tracks.shown = `Grid
    && Api.Draw.frame win mod refresh_delay = 5 then
      Table.dirty tab;  (* to capture cover updates *)

    let track_color (track : Data.track) =
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

    let entries = tab.entries in  (* may update concurrently *)
    let pp_row i =
      let track = entries.(i) in
      track_color track,
      Array.map (fun (attr, _) ->
        if attr <> `Cover then
          `Text (Data.track_attr_string track attr)
        else if lib.cover then
          match Library.load_cover lib win track.path with
          | Some img -> `Image img
          | None -> `Text ""
        else `Text ""
      ) view.tracks.columns
    in

    let pp_cell i =
      let track = entries.(i) in
      let img =
        match Library.load_cover lib win track.path with
        | Some img -> img
        | None -> Ui.nocover lay.ui
      and txt =
        let artist = Data.track_attr_string track `Artist in
        let title = Data.track_attr_string track `Title in
        let year = Data.track_attr_string track `Year in
        artist ^ " - " ^ title ^ (if year = "" then "" else " (" ^ year ^ ")")
      in img, track_color track, txt
    in

    let sorting = convert_sorting view.tracks.columns view.tracks.sorting in
    let header = Some (headings, sorting) in
    (match
      match Option.get view.tracks.shown with
      | `Table -> tracks_table lay cols header tab pp_row
      | `Grid -> tracks_grid lay lay.tracks_grid header tab pp_cell
    with
    | `None | `Scroll -> ()

    | `Select ->
      State.focus_library tab st;

    | `Sort i ->
      (* Click on column header: reorder view accordingly *)
      let attr = fst view.tracks.columns.(i) in
      let k =
        Bool.to_int (Api.Key.is_modifier_down `Shift) +
        Bool.to_int (Api.Key.is_modifier_down `Alt) * 2 +
        Bool.to_int (Api.Key.is_modifier_down `Command) * (-4)
      in
      let primary =
        if Library.current_is_playlist lib
        || Library.current_is_viewlist lib then `Pos else `FilePath in
      view.tracks.sorting <-
        Data.insert_sorting primary attr k 4 view.tracks.sorting;
      Library.save_dir lib dir;
      Library.reorder_tracks lib;

    | `Resize ws ->
      (* Column resizing: update column widths *)
      Array.mapi_inplace (fun i (a, _) -> a, ws.(i)) view.tracks.columns;
      if have_dir then Library.save_dir lib dir;

    | `Reorder perm ->
      (* Column reordering: update columns *)
      Data.permute perm view.tracks.columns;
      if have_dir then Library.save_dir lib dir;

    | `Click (Some i) when Api.Mouse.is_doubleclick `Left ->
      (* Double-click on track: clear playlist and send tracks to it *)
      let tracks =
        if Api.Key.are_modifiers_down [`Command]
        then Library.selected lib
        else [|entries.(i)|]
      in
      if tracks <> [||] then
      (
        Playlist.replace_all pl tracks;
        State.focus_playlist st;
        Control.eject st.control;
        Control.switch st.control (Playlist.current pl) true;
        Table.dirty st.library.tracks;  (* redraw for current track *)
        Table.dirty st.library.browser;
      )

    | `Click _ ->
      (* Single-click: grab focus *)
      if Api.Mouse.is_pressed `Left then State.focus_library tab st;

    | `Move delta ->
      (* Cmd-cursor movement: move selection *)
      if Data.is_playlist dir then
        Library.move_selected lib delta;

    | `Drag (delta, way, motion) ->
      (* Drag: move selection if inside *)
      if Api.Key.are_modifiers_down [] then
      (
        (* State.focus_library tab st; *)  (* don't steal after double-click! *)
        if Library.num_selected lib > 0 then
        (
          if motion <> `Unmoved then Run_view.set_drop_cursor st;
          (match way with
          | `Inside | `Inward -> ()
          | `Outside | `Outward ->
            Run_view.drag_on_playlist st;
            drag_on_browser st;
          );

          if Library.current_is_plain_playlist lib then
          (
            (* Invariant as for playlist view *)
            if motion = `Moving then
            (
              (* Start of drag & drop: remember original configuration *)
              Table.push_undo lib.tracks;
            );
            (match way with
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
              match way with
              | `Inside | `Inward ->
                Library.move_selected lib delta;
                (* Erase intermediate new state *)
                Table.drop_undo lib.tracks;
              | `Outside | `Outward -> ()  (* ignore *)
            );
          )
        )
      )

    | `Drop ->
      if Api.Key.are_modifiers_down [] then
      (
        if Ui.mouse_inside lay.ui (tracks_area lay) then
        (
          (* Dropping inside tracks: drop aux undo if no change *)
          Table.clean_undo lib.tracks
        )
        else
        (
          (* Drag & drop originating from tracks *)

          (* Dropping outside tracks: drop aux redo for new state *)
          if Library.current_is_plain_playlist lib then
            Table.drop_redo lib.tracks;

          (* Drag & drop onto playlist or browser: send tracks to playlist *)
          let tracks = Library.selected lib in
          Run_view.drop_on_playlist st tracks;
          drop_on_browser st tracks;
        )
      )

    | `Menu i_opt ->
      (* Right-click on tracks content: context menu *)
      State.focus_library tab st;
      if Library.current_is_playlist lib then
        Run_view.(edit_menu st (library_view st) i_opt)
      else
        Run_view.(list_menu st (library_view st))

    | `HeadMenu i_opt ->
      (* Right-click on tracks header: header menu *)
      State.focus_library tab st;
      let used_attrs = Array.to_list (Array.map fst view.tracks.columns) in
      let unused_attrs = Data.diff_attrs Data.track_attrs used_attrs in
      let i, current_attrs =
        match i_opt with
        | None -> Array.length view.tracks.columns, []
        | Some i -> i, [fst view.tracks.columns.(i)]
      in
      Run_menu.header_menu st dir.view.tracks i current_attrs unused_attrs
    );

    if busy then
      tracks_spin lay (spin win);

    (* Playlist file drag & drop *)
    Run_view.external_drop_on_library st;

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
