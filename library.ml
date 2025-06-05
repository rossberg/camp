(* Library *)

open Audio_file
open Data

module Set = Set.Make(String)
module Map = Map.Make(String)


type time = float

type view =
{
  mutable search : string;
  mutable query : Query.query option;
  mutable folded : bool;
  mutable divider_width : int;
  mutable divider_height : int;
  mutable artists_shown : bool;
  mutable albums_shown : display option;
  mutable tracks_shown : display option;
  mutable artists_columns : artist_attr columns;
  mutable albums_columns : album_attr columns;
  mutable tracks_columns : track_attr columns;
  mutable artists_sorting : artist_attr sorting;
  mutable albums_sorting : album_attr sorting;
  mutable tracks_sorting : track_attr sorting;
}

type dir = view Data.dir

type scan =
{
  dir_queue : (bool * path * (unit -> bool)) Safe_queue.t;
  file_queue : (bool * path * (unit -> bool)) Safe_queue.t;
  cover_queue : (bool * path * (unit -> bool)) Safe_queue.t;
  dir_busy : string option Atomic.t;
  file_busy : string option Atomic.t;
  cover_busy : string option Atomic.t;
  completed : path option list Atomic.t;
  artists_refresh : (bool * (unit -> unit)) option Atomic.t;
  albums_refresh : (bool * (unit -> unit)) option Atomic.t;
  tracks_refresh : (bool * (unit -> unit)) option Atomic.t;
  artists_busy : bool Atomic.t;
  albums_busy : bool Atomic.t;
  tracks_busy : bool Atomic.t;
}

type cover =
  | NoCover
  | ScanCover
  | ScannedCover of Meta.picture
  | Cover of Api.image

type 'a t =
{
  scan : scan;
  mutable root : dir;
  mutable current : dir option;
  mutable browser : (dir, 'a) Table.t;
  mutable artists : (artist, 'a) Table.t;
  mutable albums : (album, 'a) Table.t;
  mutable tracks : (track, 'a) Table.t;
  mutable search : Edit.t;
  mutable error : string;
  mutable error_time : time;
  mutable refresh_time : time;
  mutable cover : bool;
  mutable covers : cover Map.t;
}


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]

let ok lib =
  Table.ok "browser" lib.browser @
  Table.ok "tracks" lib.tracks @
  check "browser nonempty" (Table.length lib.browser > 0) @
  check "artists pos unset" (lib.artists.pos = None || lib.artists.pos = Some 0) @
  check "albums pos unset" (lib.albums.pos = None || lib.albums.pos = Some 0) @
  check "browser selection singular" (Table.num_selected lib.browser <= 1) @
  check "browser selection consistent with current"
    ( Table.num_selected lib.browser = 0 ||
      Option.get lib.current == (Table.selected lib.browser).(0) ) @
  []


(* Constructor *)

let artists_columns : artist_attr columns =
[|
  `Artist, 150;
  `Albums, 20;
  `Tracks, 20;
|]

let albums_columns : album_attr columns =
[|
  `Cover, 30;
  `FileTime, 110;
  `Rating, 30;
  `AlbumArtist, 150;
  `AlbumTitle, 180;
  `Length, 30;
  `Tracks, 20;
  `Date, 60;
  `Country, 50;
  `Label, 50;
  `Codec, 30;
  `Rate, 50;
  `FileSize, 50;
  `FilePath, 400;
|]

let tracks_columns : track_attr columns =
[|
  `Pos, 20;
  `Cover, 30;
  `FileTime, 70;
  `Rating, 30;
  `Artist, 150;
  `Title, 180;
  `Length, 30;
  `AlbumArtist, 100;
  `AlbumTitle, 150;
  `DiscTrack, 20;
  `Date, 60;
  `Country, 50;
  `Label, 50;
  `Codec, 30;
  `Rate, 50;
  `FileSize, 50;
  `FilePath, 400;
|]

let make_view path : view =
  {
    search = "";
    query = None;
    folded = true;
    divider_width = 100;
    divider_height = 100;
    artists_shown = false;
    albums_shown = None;
    tracks_shown = Some `Table;
    artists_columns = artists_columns;
    albums_columns = albums_columns;
    tracks_columns = tracks_columns;
    artists_sorting = [`Artist, `Asc];
    albums_sorting = [`AlbumArtist, `Asc; `AlbumTitle, `Asc; `Codec, `Asc];
    tracks_sorting =
      if is_playlist_path path || is_viewlist_path path || Format.is_known_ext path
      then [`Pos, `Asc]
      else [`Artist, `Asc; `Title, `Asc; `Codec, `Asc];
  }


let rec complete scan path =
  let paths = Atomic.get scan.completed in
  if not (Atomic.compare_and_set scan.completed paths (path::paths)) then
    complete scan path

let try_refresh action busy =
  match Atomic.exchange action None with
  | None -> false
  | Some (_, f) ->
    (try f () with exn ->
      Storage.log_exn "internal" exn "refreshing view");
    Atomic.set busy false;  (* avoid race condition! *)
    Option.iter (fun (keep_busy, _) ->
      if keep_busy then Atomic.set busy true
    ) (Atomic.get action);
    true

let rec refresher scan () =
  if not (
    try_refresh scan.artists_refresh scan.artists_busy ||
    try_refresh scan.albums_refresh scan.albums_busy ||
    try_refresh scan.tracks_refresh scan.tracks_busy
  ) then
    Unix.sleepf 0.05;
  refresher scan ()

let rec scanner scan queue busy () =
  let local, path, f = Safe_queue.take queue in
  Atomic.set busy (Some path);
  let changed = f () in
  Atomic.set busy None;
  if changed then complete scan (if local then Some path else None);
  scanner scan queue busy ()

let make_scan () =
  let scan =
    {
      dir_queue = Safe_queue.create ();
      file_queue = Safe_queue.create ();
      cover_queue = Safe_queue.create ();
      dir_busy = Atomic.make None;
      file_busy = Atomic.make None;
      cover_busy = Atomic.make None;
      completed = Atomic.make [];
      artists_refresh = Atomic.make None;
      albums_refresh = Atomic.make None;
      tracks_refresh = Atomic.make None;
      artists_busy = Atomic.make false;
      albums_busy = Atomic.make false;
      tracks_busy = Atomic.make false;
    }
  in
  ignore (Domain.spawn (scanner scan scan.dir_queue scan.dir_busy));
  ignore (Domain.spawn (scanner scan scan.file_queue scan.file_busy));
  ignore (Domain.spawn (scanner scan scan.cover_queue scan.cover_busy));
  ignore (Domain.spawn (refresher scan));
  scan

let make () =
  let root = Data.make_dir "" None (-1) 0 (make_view "") in
  root.name <- "All";
  root.view.folded <- false;
  root.view.artists_shown <- true;
  root.view.albums_shown <- Some `Table;
  {
    scan = make_scan ();
    root;
    current = None;
    browser = Table.make 0;
    artists = Table.make 0;
    albums = Table.make 0;
    tracks = Table.make 100;
    search = Edit.make 100;
    error = "";
    error_time = 0.0;
    refresh_time = 0.0;
    cover = true;
    covers = Map.empty;
  }


(* Error Messages *)

let error lib msg =
  lib.error <- msg;
  lib.error_time <- Unix.gettimeofday ()


(* Attributes *)

let attr_prop = function
  | `Pos -> "#   ", `Right
  | `FilePath -> "File Path", `Left
  | `FileDir -> "File Location", `Left
  | `FileName -> "File Name", `Left
  | `FileExt -> "File Extension", `Left
  | `FileSize -> "File Size", `Right
  | `FileTime -> "File Date", `Left
  | `Codec -> "Format", `Left
  | `Channels -> "Channels", `Left
  | `Depth -> "Bit Depth", `Right
  | `SampleRate -> "Sample Rate", `Right
  | `BitRate -> "Bit Rate", `Right
  | `Rate -> "Rate", `Right
  | `Artist -> "Artist", `Left
  | `Title -> "Title", `Left
  | `Length -> "Length", `Right
  | `Rating -> "Rating", `Left
  | `AlbumArtist -> "Album Artist", `Left
  | `AlbumTitle -> "Album Title", `Left
  | `Track -> "Track", `Right
  | `Tracks -> "Tracks", `Right
  | `Disc -> "Disc", `Right
  | `Discs -> "Discs", `Right
  | `DiscTrack -> "Track", `Right
  | `Albums -> "Albums", `Right
  | `Date -> "Date", `Left
  | `Year -> "Year", `Left
  | `Label -> "Label", `Left
  | `Country -> "Country", `Left
  | `Cover -> "Cover", `Left
  | `True | `False | `Now | `Random | `None -> assert false

let attr_name attr = fst (attr_prop (attr :> any_attr))
let attr_align attr = snd (attr_prop (attr :> any_attr))


(* Lookup *)

let rec find_dir lib path = find_dir' path lib.root
and find_dir' path (dir : dir) = Array.find_map (find_dir'' path) dir.children
and find_dir'' path (dir : dir) =
  if String.starts_with path ~prefix: dir.path then
    if String.length path = String.length dir.path then
      Some dir
    else
      find_dir' path dir
  else
    None

let rec find_track lib path = find_track' path lib.root
and find_track' path (dir : dir) =
  Array.find_map (find_track'' path) dir.children
and find_track'' path (dir : dir) =
  if String.starts_with path ~prefix: dir.path then
    if File.(dir path // "") = dir.path then
      Array.find_opt (fun (track : track) -> track.path = path) dir.tracks
    else
      find_track' path dir
  else
    None

let find_item lib (item : M3u.item) =
  match find_track lib item.path with
  | None -> Track.of_m3u_item item
  | Some track -> assert (track.pos = -1); track


let has_track lib (track : track) =
  find_track lib track.path <> None


(* Data Persistence *)

let library_name = "library.bin"
let browser_name = "browser.bin"

let clear_track (track : track) = track.memo <- None

let rec clear_dir (dir : dir) =
  Array.iter clear_dir dir.children;
  Array.iter clear_track dir.tracks

let save_db lib =
  Storage.save library_name (fun oc ->
    clear_dir lib.root;
    Marshal.to_channel oc lib.root []
  )

let load_db lib =
  Storage.load library_name (fun ic ->
    lib.root <- (Marshal.from_channel ic : dir)
  )

let rec save_view oc (dir : dir) =
  Marshal.to_channel oc (dir.path, dir.view) [];
  Array.iter (save_view oc) dir.children

let save_browser lib =
  Storage.save browser_name (fun oc ->
    save_view oc lib.root
  )

let load_browser lib =
  Storage.load browser_name (fun ic ->
    try
      while true do
        let path, view = (Marshal.from_channel ic : path * view) in
        match find_dir lib path with
        | None -> ()
        | Some dir -> dir.view <- view
      done
    with End_of_file -> ()
  )


(* Scanning *)

type scan_mode = [`Quick | `Thorough]

let rescan_track' _lib mode (track : track) =
  let old = {track with memo = None} in
  try
    if not (File.exists track.path) then
    (
      track.status <- `Absent;
    )
    else if File.is_dir track.path then
    (
      track.status <- `Invalid;
    )
    else if not (Data.is_track_path track.path) then
    (
      let stats = File.stat track.path in
      track.file.size <- stats.st_size;
      track.file.time <- stats.st_mtime;
      track.status <- `Invalid;
    )
    else
    (
      let stats = File.stat track.path in
      track.file.size <- stats.st_size;
      track.file.time <- stats.st_mtime;
      if
        mode = `Thorough || track.status = `Undet ||
        track.file.size <> old.file.size || track.file.time <> old.file.time
      then
      (
        track.format <- Some (Format.read track.path);
        track.meta <- Some {(Meta.load track.path) with cover = None};
        track.status <- `Det;
      )
    );
    let time = Unix.gettimeofday () in
    let memo = track.memo in
    track.memo <- None;
    track.file.age <- time;
    old.file.age <- time;
    let changed = track <> old in
    if not changed then track.memo <- memo;
    changed
  with exn ->
    Storage.log_exn "file" exn ("scanning track " ^ track.path);
    false

let rescan_dir_tracks' lib mode (dir : dir) =
  try
    let new_tracks = Dynarray.create () in
    let old_tracks =
      Array.fold_left (fun map (track : track) ->
        Map.add track.path track map
      ) Map.empty dir.tracks
    in
    let updates = ref 0 in
    Array.iter (fun file ->
      let dir_path = dir.path in
      let path = File.(dir_path // file) in
      if not (File.is_dir path) && Data.is_track_path path then
      (
        let track =
          match Map.find_opt path old_tracks with
          | Some track -> track
          | None -> Data.make_track path
        in
        Dynarray.add_last new_tracks track;
        if rescan_track' lib mode track then incr updates
      )
    ) (File.read_dir dir.path);
    dir.tracks <- Dynarray.to_array new_tracks;
    !updates > 0 || Array.length dir.tracks <> Map.cardinal old_tracks
  with exn ->
    Storage.log_exn "file" exn ("scanning tracks in directory " ^ dir.path);
    true

let rescan_playlist' lib _mode (dir : dir) =
  try
    let s = File.load `Bin dir.path in
    let items = Array.of_list (M3u.parse_ext s) in
    let items' = Array.map (M3u.resolve (File.dir dir.path)) items in
    dir.tracks <-
      Array.mapi (fun pos item -> {(find_item lib item) with pos}) items';
    true
  with
  | Sys_error _ -> true
  | exn -> Storage.log_exn "file" exn ("scanning playlist " ^ dir.path); true

let rescan_viewlist' lib _mode (dir : dir) =
  try
    let s = File.load `Bin dir.path in
    (match Query.parse_query s with
    | Error msg ->
      dir.error <- msg ^ " in viewlist query " ^ dir.path;
      dir.tracks <- [||];
      error lib dir.error
    | Ok query ->
      let tracks =
        Query.exec query (fun (track : track) -> track.pos = -1) lib.root in
      dir.error <- "";
      dir.tracks <-
        Array.mapi (fun pos (track : track) -> {track with pos}) tracks;
    );
    true
  with
  | Sys_error _ -> true
  | exn -> Storage.log_exn "file" exn ("scanning viewlist " ^ dir.path); true


let rec rescan_dir' lib mode (origin : dir) =
  let rec scan_file (parent : dir) old_dirs file =
    let parent_path = parent.path in
    let path = File.(parent_path // file) in
    let subdir path' =
      match Map.find_opt path' old_dirs with
      | Some dir -> dir
      | None ->
        Data.make_dir path' (Some parent_path) (parent.nest + 1) 0
          (make_view path')
    in
    if File.is_dir path then
      scan_dir (subdir File.(path // ""))
    else if Data.is_playlist_path path then
      scan_playlist (subdir path)
    else if Data.is_viewlist_path path then
      scan_viewlist (subdir path)
    else if Data.is_track_path path then
      Some []  (* deferred to directory scan *)
    else
      None

  and scan_dir (dir : dir) =
    Domain.cpu_relax ();
    let old_dirs =
      Array.fold_left (fun map (dir : dir) ->
        Map.add dir.path dir map
      ) Map.empty dir.children
    in
    match
      let dirs_of = Option.value ~default: [] in
      Array.fold_left (fun r file ->
        match r, scan_file dir old_dirs file with
        | None, None -> None
        | dirs1, dirs2 -> Some (dirs_of dirs1 @ dirs_of dirs2)
      ) None (File.read_dir dir.path)
    with
    | None -> None
    | Some dirs ->
      if Data.is_track_path dir.name then
        dir.name <- File.remove_extension dir.name;
      dir.children <- Array.of_list dirs;
      Array.stable_sort Data.compare_dir dir.children;
      rescan_dir_tracks lib mode dir;
      Some [dir]

  and scan_playlist (dir : dir) =
    dir.name <- File.remove_extension dir.name;
    rescan_playlist lib mode dir;
    Some [dir]

  and scan_viewlist (dir : dir) =
    dir.name <- File.remove_extension dir.name;
    rescan_viewlist lib mode dir;
    Some [dir]
  in

  try
    if File.exists_dir origin.path then
      ignore (scan_dir origin);
    true
  with exn ->
    Storage.log_exn "file" exn ("scanning directory " ^ origin.path);
    true


and rescan_dir lib mode (dir : dir) =
  Safe_queue.add (false, dir.path, fun () -> rescan_dir' lib mode dir)
    lib.scan.dir_queue

and rescan_playlist lib mode (dir : dir) =
  Safe_queue.add (true, dir.path, fun () -> rescan_playlist' lib mode dir)
    lib.scan.file_queue

and rescan_viewlist lib mode (dir : dir) =
  Safe_queue.add (true, dir.path, fun () -> rescan_viewlist' lib mode dir)
    lib.scan.file_queue

and rescan_track lib mode (track : track) =
  Safe_queue.add (true, track.path, fun () -> rescan_track' lib mode track)
    lib.scan.file_queue

and rescan_dir_tracks lib mode (dir : dir) =
  Safe_queue.add (true, dir.path, fun () -> rescan_dir_tracks' lib mode dir)
    lib.scan.dir_queue


let rescan_dirs lib mode dirs = Array.iter (rescan_dir lib mode) dirs
let rescan_root lib mode = rescan_dirs lib mode lib.root.children
let rescan_tracks lib mode tracks = Array.iter (rescan_track lib mode) tracks

let rescan_done lib = Atomic.exchange lib.scan.completed []
let rescan_busy lib =
  match Atomic.get lib.scan.dir_busy with
  | None -> Atomic.get lib.scan.file_busy
  | some -> some


(* Browser *)

let length_browser lib = Table.length lib.browser

let defocus lib =
  Table.defocus lib.browser;
  Table.defocus lib.artists;
  Table.defocus lib.albums;
  Table.defocus lib.tracks;
  Edit.defocus lib.search

let focus_browser lib =
  defocus lib;
  Table.focus lib.browser

let focus_search lib =
  defocus lib;
  Edit.focus lib.search


let update_view_query lib (dir : dir) =
  dir.view.query <-
    if dir.view.search = "" then (lib.error <- ""; None) else
    match Query.parse_query dir.view.search with
    | Ok query -> lib.error <- ""; Some query
    | Error msg -> error lib (msg ^ " in search query"); Some Query.empty_query


let update_dir lib _dir =
  save_browser lib

let set_dir_opt lib dir_opt =
  Option.iter (fun (dir : dir) ->
    (* In case some input has been made but not yet sent *)
    if lib.search.text <> dir.view.search then
    (
      dir.view.search <- lib.search.text;
      update_dir lib dir;
    )
  ) lib.current;
  Table.deselect_all lib.browser;
  Table.clear_undo lib.tracks;
  lib.current <- dir_opt;
  Option.iter (fun (dir : dir) ->
    Edit.set lib.search dir.view.search;
    if dir.view.query = None then update_view_query lib dir;
  ) lib.current

let selected_dir lib =
  Table.first_selected lib.browser

let deselect_dir lib =
  set_dir_opt lib None

let select_dir lib i =
  let dir = lib.browser.entries.(i) in
  if lib.current <> Some dir then
  (
    set_dir_opt lib (Some dir);
    Table.select lib.browser i i;
    if Data.is_playlist_path dir.path then
      rescan_playlist lib `Thorough dir
    else if Data.is_viewlist_path dir.path then
      rescan_viewlist lib `Thorough dir
  )


let refresh_browser lib =
  let rec entries (dir : dir) acc =
    Option.iter (fun (cur : dir) ->
      if cur.path = dir.path then lib.current <- Some dir) lib.current;
    dir ::
    (if dir.view.folded then acc else Array.fold_right entries dir.children acc)
  in
  let selection = Table.save_selection lib.browser in
  Table.set lib.browser (Array.of_list (entries lib.root []));
  Table.restore_selection lib.browser selection (fun dir -> dir.path)


let fold_dir lib dir status =
  if status <> dir.view.folded then
  (
    dir.view.folded <- status;
    refresh_browser lib;
    if not status && lib.current <> None then
    (
      Option.iter (fun i -> Table.select lib.browser i i)
        (Array.find_index (fun dir -> lib.current = Some dir) lib.browser.entries)
    );
  )


let current_is_playlist lib =
  match lib.current with
  | None -> false
  | Some dir -> Data.is_playlist dir

let current_is_viewlist lib =
  match lib.current with
  | None -> false
  | Some dir -> Data.is_viewlist dir

let current_is_shown_playlist lib =
  match lib.current with
  | None -> false
  | Some dir -> dir.view.tracks_shown <> None && Data.is_playlist dir

let current_is_shown_viewlist lib =
  match lib.current with
  | None -> false
  | Some dir -> dir.view.tracks_shown <> None && Data.is_viewlist dir


(* Roots *)

let make_root lib path pos =
  if not (File.exists path) then
    failwith (path ^ " does not exist")
  else if not (File.is_dir path) then
    failwith (path ^ " is not a directory")
  else
  (
    let dirpath = File.(path // "") in
    match
      Array.find_opt (fun (dir : dir) ->
        path = dir.path ||
        String.starts_with ~prefix: dirpath dir.path ||
        String.starts_with ~prefix: dir.path dirpath
      ) lib.root.children
    with
    | Some dir ->
      failwith (dirpath ^ " overlaps with " ^ dir.name ^ " (" ^ dir.path ^ ")")
    | None -> Data.make_dir dirpath (Some "") 0 pos (make_view dirpath)
  )

let add_dirs lib paths pos =
  let paths = Array.of_list paths in
  lib.error <- "";
  try
    let roots = lib.root.children in
    let roots' = Array.mapi (fun i path -> make_root lib path (pos + i)) paths in
    let len = Array.length roots in
    let len' = Array.length roots' in
    lib.root.children <-
      Array.init (len + len') (fun i ->
        if i < pos then
          roots.(i)
        else if i < pos + len' then
          roots'.(i - pos)
        else
          let root = roots.(i - len') in
          root.pos <- i;
          root
      );
    Array.iter (fun (dir : dir) -> rescan_dir lib `Thorough dir) roots';
    refresh_browser lib;
    true
  with Failure msg ->
    error lib msg;
    false


let remove_dir lib path =
  let dirpath = File.(path // "") in
  let roots = lib.root.children in
  match Array.find_index (fun (root : dir) -> root.path = dirpath) roots with
  | None -> ()
  | Some pos ->
    lib.current <- None;
    lib.root.children <-
      Array.init (Array.length roots - 1) (fun i ->
        if i < pos then
          roots.(i)
        else
          let root = roots.(i + 1) in
          root.pos <- i; root
      );
    refresh_browser lib

let remove_dirs lib paths =
  List.iter (remove_dir lib) paths


(* Views *)

let focus_artists lib =
  defocus lib;
  Table.focus lib.artists

let focus_albums lib =
  defocus lib;
  Table.focus lib.albums

let focus_tracks lib =
  defocus lib;
  Table.focus lib.tracks


let has_selection lib = Table.has_selection lib.tracks
let num_selected lib = Table.num_selected lib.tracks
let first_selected lib = Table.first_selected lib.tracks
let last_selected lib = Table.last_selected lib.tracks
let is_selected lib i = Table.is_selected lib.tracks i
let selected lib = Table.selected lib.tracks

let select_all lib = Table.select_all lib.tracks
let deselect_all lib = Table.deselect_all lib.tracks
let select_invert lib = Table.select_invert lib.tracks
let select lib i j = Table.select lib.tracks i j
let deselect lib i j = Table.deselect lib.tracks i j


let artists_sorting view = view.artists_sorting
let albums_sorting view = view.albums_sorting
let tracks_sorting view = view.tracks_sorting

let artist_key (artist : artist) = artist.name
let album_key (album : album) = (*album.track*)  (* TODO *)
  match album.meta with
  | None -> "[unknown]"
  | Some meta -> meta.albumtitle

let track_key lib : track -> string =
  if current_is_playlist lib || current_is_viewlist lib then
    fun track -> string_of_int track.pos
  else
    fun track -> track.path


let refresh_delay = 5.0

let sort attr_string sorting entries =
  if sorting <> [] then
  (
    let entries' =
      Array.map (fun entry -> Data.key_entry attr_string sorting entry, entry)
        entries
    in
    Array.stable_sort compare entries';
    Array.iteri (fun i (_, entry) -> entries.(i) <- entry) entries';
  )

let refresh lib (tab : _ Table.t) attr_string sorting key exec =
  match lib.current with
  | None -> Table.remove_all tab
  | Some dir ->
    let entries = exec dir in
    sort attr_string (sorting dir.view) entries;
    Mutex.protect tab.mutex (fun () ->
      let selection = Table.save_selection tab in
      Table.set tab entries;
      Table.restore_selection tab selection key
    );
    if lib.refresh_time <> 0.0 then
      lib.refresh_time <- Unix.gettimeofday () +. refresh_delay

let refresh_tracks_sync lib =
  refresh lib lib.tracks track_attr_string tracks_sorting (track_key lib)
    (fun dir ->
      let artists =
        Array.fold_left (fun s (artist : artist) -> Set.add artist.name s)
          Set.empty (Table.selected lib.artists)
      and albums =
        Array.fold_left (fun s (album : album) ->
          Set.add (Data.album_attr_string album `AlbumTitle) s
        ) Set.empty (Table.selected lib.albums)
      in
      let filter (track : track) =
        ( artists = Set.empty ||
          Set.mem (Data.track_attr_string track `Artist) artists ||
          Set.mem (Data.track_attr_string track `AlbumArtist) artists )
        &&
        ( albums = Set.empty ||
          Set.mem (Data.track_attr_string track `AlbumTitle) albums )
      in
      let query =
        if dir.error <> "" then
          (error lib dir.error; Query.empty_query)
        else
          Option.value dir.view.query ~default: Query.full_query
      in
      Query.exec query filter dir
    )

module AlbumKey =
struct
  type t = string * string * string * string
  let compare : t -> t -> int = compare
end

module AlbumMap = Stdlib.Map.Make(AlbumKey)

let refresh_albums_sync lib =
  refresh lib lib.albums album_attr_string albums_sorting album_key
    (fun _dir ->
      let map =
        Array.fold_left (fun map (track : track) ->
          let key : AlbumKey.t =
            ( track_attr_string track `AlbumArtist,
              track_attr_string track `AlbumTitle,
              track_attr_string track `Codec,
              track_attr_string track `Label
            )
          in
          let meta = Option.value track.meta ~default: Meta.unknown in
          let album : album =
            { path = track.path;
              file = track.file;
              format = track.format;
              meta = Some {meta with tracks = 1};
              memo = None;
            }
          in
          let album' =
            match AlbumMap.find_opt key map with
            | None -> album
            | Some album' -> Data.accumulate_album album album'
          in
          AlbumMap.add key album' map
        ) AlbumMap.empty lib.tracks.entries
      in
      let a = Dynarray.create () in
      AlbumMap.iter (fun _ album -> Dynarray.add_last a album) map;
      Dynarray.to_array a
    )

let refresh_artists_sync lib =
  refresh lib lib.artists artist_attr_string artists_sorting artist_key
    (fun _dir ->
      let map =
        Array.fold_left (fun map (album : album) ->
          let key = album_attr_string album `AlbumArtist in
          let artist : artist =
            { name = key;
              albums = 1;
              tracks = (Option.get album.meta).tracks;
            }
          in
          let artist' =
            match Map.find_opt key map with
            | None -> artist
            | Some artist' -> Data.accumulate_artist artist artist'
          in
          Map.add key artist' map
        ) Map.empty lib.albums.entries
      in
      let a = Dynarray.create () in
      Map.iter (fun _ artist -> Dynarray.add_last a artist) map;
      Dynarray.to_array a
    )

let refresh_albums_tracks_sync lib =
  refresh_tracks_sync lib;
  refresh_albums_sync lib

let refresh_artists_albums_sync lib =
  refresh_albums_sync lib;
  refresh_artists_sync lib

let refresh_artists_albums_tracks_sync lib =
  refresh_tracks_sync lib;
  refresh_artists_albums_sync lib

let refresh_tracks ?(busy = true) lib =
  if busy then
  (
    Atomic.set lib.scan.tracks_busy true;
    Table.set lib.tracks [||];
  );
  Atomic.set lib.scan.tracks_refresh
    (Some (busy, fun () -> refresh_tracks_sync lib))

let refresh_albums_tracks ?(busy = true) lib =
  if busy then
  (
    Atomic.set lib.scan.albums_busy true;
    Table.set lib.tracks [||];
    Table.set lib.albums [||];
  );
  Atomic.set lib.scan.albums_refresh
    (Some (busy, fun () -> refresh_albums_tracks_sync lib))

let refresh_artists_albums_tracks ?(busy = true) lib =
  if busy then
  (
    Atomic.set lib.scan.artists_busy true;
    Table.set lib.tracks [||];
    Table.set lib.albums [||];
    Table.set lib.artists [||];
  );
  Atomic.set lib.scan.artists_refresh
    (Some (busy, fun () -> refresh_artists_albums_tracks_sync lib))

let refresh_artists_busy lib =
  Atomic.get lib.scan.artists_busy
let refresh_albums_busy lib =
  Atomic.get lib.scan.albums_busy || refresh_artists_busy lib
let refresh_tracks_busy lib =
  Atomic.get lib.scan.tracks_busy || refresh_albums_busy lib


(*
let rescan_affects_views lib path_opts =
  match lib.current with
  | None -> false
  | Some dir ->
    List.exists (function
      | None -> true
      | Some path ->
        String.starts_with ~prefix: dir.path path ||
        not (Data.is_dir dir) &&
        Array.exists (fun track ->
          String.starts_with ~prefix: path track.path) lib.tracks.entries
    ) path_opts

let refresh_after_rescan lib =
  let updated = rescan_done lib in
  if List.mem None updated then
  (
    refresh_browser lib;
    refresh_artists_albums_tracks lib ~busy: false;
  )
  else if rescan_affects_views lib updated then
    refresh_artists_albums_tracks lib ~busy: false
*)

let refresh_after_rescan lib =
  let updated = rescan_done lib in
  let now = Unix.gettimeofday () in
  if updated <> [] && lib.refresh_time = 0.0 then
    lib.refresh_time <- now +. refresh_delay
  else if now > lib.refresh_time && lib.refresh_time > 0.0
  || (current_is_playlist lib || current_is_viewlist lib)
     && Table.length lib.tracks = 0 then
  (
    lib.refresh_time <- 0.0;
    refresh_browser lib;
    refresh_artists_albums_tracks lib ~busy: false;
  )

let reorder lib tab sorting attr_string key =
  Option.iter (fun (dir : dir) ->
    let s = sorting dir.view in
    if s <> [] then
    (
      let selection = Table.save_selection tab in
      sort attr_string s tab.entries;
      Table.restore_selection tab selection key;
    )
  ) lib.current

let reorder_artists lib =
  reorder lib lib.artists artists_sorting artist_attr_string artist_key

let reorder_albums lib =
  reorder lib lib.albums albums_sorting album_attr_string album_key

let reorder_tracks lib =
  reorder lib lib.tracks tracks_sorting track_attr_string (track_key lib)


(* Search *)

let set_search lib search =
  Option.iter (fun (dir : dir) ->
    if dir.view.search <> search || dir.view.query = None then
    (
      dir.view.search <- search;
      update_view_query lib dir;
      update_dir lib dir;
      refresh_artists_albums_tracks lib;
    )
  ) lib.current


(* Playlist Editing *)

let length lib = Table.length lib.tracks
let tracks lib = lib.tracks.entries
let table lib = lib.tracks


let array_swap a i j =
  let temp = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- temp

let array_rev a =
  let len = Array.length a in
  for i = 0 to len / 2 - 1 do
    array_swap a i (len - i - 1)
  done


let save_playlist lib =
  assert (current_is_playlist lib);
  let dir = Option.get lib.current in
  let tracks = Array.map Fun.id lib.tracks.entries in
  Array.sort (fun (t1 : track) (t2 : track) -> compare t1.pos t2.pos) tracks;
  try
    let items = Array.to_list (Array.map (Track.to_m3u_item) tracks) in
    let s = M3u.make_ext items in
    File.store `Bin dir.path s;
  with exn ->
    Storage.log_exn "file" exn ("writing playlist " ^ dir.path)


(* Before editing, normalise playlist to pos-order to enable correct undo *)
let compare_pos (tr1 : track) (tr2 : track) = compare tr1.pos tr2.pos

let normalize_playlist lib =
  if Table.(has_selection lib.artists || has_selection lib.albums) then
  (
    Table.deselect_all lib.artists;
    Table.deselect_all lib.albums;
    refresh_artists_albums_tracks_sync lib;  (* could be slow... *)
  );
  let dir = Option.get lib.current in
  match dir.view.tracks_sorting with
  | (`Pos, `Asc)::_ -> `Asc
  | (`Pos, `Desc)::_ ->
    let selection = Table.save_selection lib.tracks in
    array_rev lib.tracks.entries;
    Table.restore_selection lib.tracks selection (track_key lib);
    `Desc
  | _ ->
    let selection = Table.save_selection lib.tracks in
    Array.sort compare_pos lib.tracks.entries;
    Table.restore_selection lib.tracks selection (track_key lib);
    `Other

let restore_playlist lib = function
  | `Asc -> ()
  | `Desc ->
    let selection = Table.save_selection lib.tracks in
    array_rev lib.tracks.entries;
    Table.restore_selection lib.tracks selection (track_key lib)
  | `Other ->
    let selection = Table.save_selection lib.tracks in
    reorder_tracks lib;
    Table.restore_selection lib.tracks selection (track_key lib)


let insert lib pos tracks =
  assert (current_is_playlist lib);
  if tracks <> [||] then
  (
    deselect_all lib;
    let pos' =
      if pos >= Table.length lib.tracks then max_int - 1 else
      lib.tracks.entries.(pos).pos
    in
    let order = normalize_playlist lib in  (* can change entries length! *)
    let len = Table.length lib.tracks in
    let len' = Array.length tracks in
    let pos'' = min (if order = `Desc then pos' + 1 else pos') len in
    let tracks' = Array.mapi
      (fun i (track : track) ->
        let pos = pos'' + (if order = `Desc then len' - i - 1 else i) in
        match find_track lib track.path with
        | Some track' -> {track' with pos}  (* clone to prevent aliasing! *)
        | None ->
          (* Abuse `Predet as an indication that the track isn't in lib *)
          let status =
            if track.status <> `Det || Data.is_separator track
            then track.status else `Predet
          in {track with pos; status}
      ) tracks
    in
    deselect_all lib;
    for i = pos'' to len - 1 do
      lib.tracks.entries.(i).pos <- lib.tracks.entries.(i).pos + len'
    done;
    Table.insert lib.tracks pos'' tracks';
    select lib pos'' (pos'' + len' - 1);
    restore_playlist lib order;
    save_playlist lib;
    refresh_artists_albums_tracks_sync lib;  (* could be slow... *)
  )

let remove_all lib =
  assert (current_is_playlist lib);
  if lib.tracks.entries <> [||] then
  (
    let order = normalize_playlist lib in
    Table.remove_all lib.tracks;
    restore_playlist lib order;
    save_playlist lib;
  )

let remove_if p lib n =
  assert (current_is_playlist lib);
  if n > 0 then
  (
    let order = normalize_playlist lib in
    let js = Table.remove_if p lib.tracks n in
    Array.iter (fun (track : track) -> track.pos <- js.(track.pos)) lib.tracks.entries;
    restore_playlist lib order;
    save_playlist lib;
    refresh_artists_albums_sync lib;  (* could be slow... *)
  )

let remove_selected lib =
  remove_if (Table.is_selected lib.tracks) lib (Table.num_selected lib.tracks)

let remove_unselected lib =
  remove_if (fun i -> not (Table.is_selected lib.tracks i)) lib
    (Table.length lib.tracks - Table.num_selected lib.tracks)

let num_invalid lib =
  Array.fold_left (fun n track ->
    n + Bool.to_int (Data.is_invalid track)
  ) 0 lib.tracks.entries

let remove_invalid lib =
  remove_if (fun i -> Data.is_invalid lib.tracks.entries.(i))
    lib (num_invalid lib)


let replace_all lib tracks =
  if lib.tracks.entries = [||] then
    insert lib 0 tracks
  else
  (
    remove_all lib;
    insert lib 0 tracks;
    Table.drop_undo lib.tracks;
  )


let move_selected lib d =
  assert (current_is_playlist lib);
  if Table.num_selected lib.tracks > 0 then
  (
    let order = normalize_playlist lib in
    let js = Table.move_selected lib.tracks d in
    Array.iter (fun (track : track) -> track.pos <- js.(track.pos))
      lib.tracks.entries;
    restore_playlist lib order;
    save_playlist lib;
  )


let undo lib =
  let order = normalize_playlist lib in
  Table.pop_undo lib.tracks;
  Array.iteri (fun i (track : track) -> track.pos <- i) lib.tracks.entries;
  restore_playlist lib order;
  save_playlist lib

let redo lib =
  let order = normalize_playlist lib in
  Table.pop_redo lib.tracks;
  Array.iteri (fun i (track : track) -> track.pos <- i) lib.tracks.entries;
  restore_playlist lib order;
  save_playlist lib


(* Covers *)

let rescan_cover' lib path =
  try
    let cover =
      try
        let meta = Meta.load path in
        match meta.cover with
        | None -> NoCover
        | Some pic -> ScannedCover pic
      with Sys_error _ -> NoCover
    in
    lib.covers <- Map.add path cover lib.covers;
    false
  with exn ->
    Storage.log_exn "file" exn ("scanning cover " ^ path);
    false

let rescan_cover lib path =
  lib.covers <- Map.add path ScanCover lib.covers;
  Safe_queue.add (false, path, fun () -> rescan_cover' lib path)
    lib.scan.cover_queue

let load_cover lib win path =
  if M3u.is_separator path then None else
  match Map.find_opt path lib.covers with
  | Some (NoCover | ScanCover) -> None
  | Some (Cover image) -> Some image
  | Some (ScannedCover pic) ->
    let cover, result =
      match Api.Image.load_from_memory win pic.mime pic.data with
      | exception _ -> NoCover, None
      | image -> Cover image, Some image
    in lib.covers <- Map.add path cover lib.covers; result
  | None -> rescan_cover lib path; None


let purge_covers lib =
  lib.covers <- Map.empty


(* Persistance *)

open Storage
let fmt = Printf.sprintf
let scan = Scanf.sscanf
let bool x = x <> 0
let num l h x = max l (min h x)

let to_map lib =
  Map.of_list
  [
    "browser_scroll", fmt "%d" lib.browser.vscroll;
    "browser_current",
      (Option.value lib.current
        ~default: (Data.make_dir "-" None 0 0 (make_view "-"))).path;
    "lib_cover", fmt "%d" (Bool.to_int lib.cover);
  ]

let to_map_extra lib =
  Map.of_list
  [
    "browser_pos", fmt "%d" (Option.value lib.browser.pos ~default: (-1));
    "browser_length", fmt "%d" (Array.length lib.browser.entries);
    "browser_selected",
      (Option.value (Option.map (fun i ->
          fmt "%d(%s)" i lib.browser.entries.(i).name
        ) (selected_dir lib)) ~default: "");
    "tracks_pos", fmt "%d" (Option.value lib.tracks.pos ~default: (-1));
    "tracks_vscroll", fmt "%d" lib.tracks.vscroll;
    "tracks_hscroll", fmt "%d" lib.tracks.hscroll;
    "tracks_length", fmt "%d" (Array.length lib.tracks.entries);
    "root_length", fmt "%d" (Array.length lib.root.children);
    "lib_error", fmt "%s" lib.error;
    "lib_error_time", fmt "%.1f" lib.error_time;
  ]

let of_map lib m =
  refresh_browser lib;
  read_map m "browser_scroll" (fun s ->
    Table.set_vscroll lib.browser
      (scan s "%d" (num 0 (max 0 (length_browser lib - 1)))) 4);
  read_map m "browser_current" (fun s ->
    Option.iter (fun i -> select_dir lib i)
      (Array.find_index (fun (dir : dir) -> dir.path = s) lib.browser.entries);
    if lib.current = None then lib.current <- find_dir lib s;
  );
  read_map m "lib_cover" (fun s -> lib.cover <- scan s "%d" bool);
  refresh_artists_albums_tracks lib
