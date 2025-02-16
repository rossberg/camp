(* Library *)

open Audio_file
open Data


type time = float
type db = Db.t

type scan =
{
  dir_queue : (bool * path * (unit -> unit)) Safe_queue.t;
  file_queue : (bool * path * (unit -> unit)) Safe_queue.t;
  dir_busy : string option Atomic.t;
  file_busy : string option Atomic.t;
  completed : path option list Atomic.t;
}

type t =
{
  db : db;
  scan : scan;
  mutable root : dir;
  mutable current : dir option;
  mutable browser : dir Table.t;
  mutable artists : artist Table.t;
  mutable albums : album Table.t;
  mutable tracks : track Table.t;
  mutable error : string;
  mutable error_time : time;
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

let rec complete scan path =
  let paths = Atomic.get scan.completed in
  if not (Atomic.compare_and_set scan.completed paths (path::paths)) then
    complete scan path

let rec scanner scan queue busy () =
  let local, path, f = Safe_queue.take queue in
  Atomic.set busy (Some path);
  f ();
  Atomic.set busy None;
  complete scan (if local then Some path else None);
  scanner scan queue busy ()

let make_scan () =
  let scan =
    {
      dir_queue = Safe_queue.create ();
      file_queue = Safe_queue.create ();
      dir_busy = Atomic.make None;
      file_busy = Atomic.make None;
      completed = Atomic.make [];
    }
  in
  ignore (Domain.spawn (scanner scan scan.dir_queue scan.dir_busy));
  ignore (Domain.spawn (scanner scan scan.file_queue scan.file_busy));
  scan

let make db =
  let root = Data.make_dir "" None (-1) 0 in
  root.name <- "All";
  root.artists_shown <- true;
  root.albums_shown <- true;
  {
    db;
    scan = make_scan ();
    root;
    current = None;
    browser = Table.make 0;
    artists = Table.make 0;
    albums = Table.make 0;
    tracks = Table.make 100;
    error = "";
    error_time = 0.0;
  }


(* Error Messages *)

let error lib msg =
  lib.error <- msg;
  lib.error_time <- Unix.gettimeofday ()


(* Attributes *)

let attr_prop = function
  | `Pos -> "Pos", `Right
  | `FilePath -> "File Path", `Left
  | `FileSize -> "File Size", `Right
  | `FileTime -> "File Date", `Left
  | `Codec -> "Format", `Left
  | `Channels -> "Channels", `Left
  | `Depth -> "Bit Depth", `Right
  | `SampleRate -> "Sample Rate", `Right
  | `Bitrate -> "Bit Rate", `Right
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
  | `Albums -> "Albums", `Right
  | `Date -> "Date", `Left
  | `Year -> "Year", `Left
  | `Label -> "Label", `Left
  | `Country -> "Country", `Left
  | `None -> assert false

let attr_name attr = fst (attr_prop (attr :> any_attr))
let attr_align attr = snd (attr_prop (attr :> any_attr))


let fmt = Printf.sprintf

let fmt_time t =
  let t' = int_of_float (Float.trunc t) in
  fmt "%d:%02d" (t' / 60) (t' mod  60)

let fmt_date t =
  let tm = Unix.localtime t in
  fmt "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

let _fmt_date_time t =
  let tm = Unix.localtime t in
  fmt "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let nonzero zero f x = if x = zero then "" else f x
let nonzero_int w x = nonzero 0 (fmt "%*d" w) x (* leading spaces for sorting *)
let nonempty f x attr = match x with None -> "" | Some x -> f x attr


let file_attr_string path (file : file) = function
  | `FilePath -> path
  | `FileSize -> nonzero 0.0 (fmt "%3.1f MB") (float file.size /. 2.0 ** 20.0)
  | `FileTime -> nonzero 0.0 fmt_date file.time

let rec format_attr_string (format : Format.t) = function
  | `Length -> nonzero 0.0 fmt_time format.time
  | `Codec -> format.codec
  | `Channels -> nonzero_int 2 format.channels
  | `Depth ->
    let depth = format.bitrate /. float format.rate /. float format.channels in
    let fmts : _ format =
      if float format.depth = Float.round depth then "%.0f" else "%.1f"
    in nonzero 0.0 (fmt fmts) depth
  | `SampleRate -> nonzero 0.0 (fmt "%3.1f KHz") (float format.rate /. 1000.0)
  | `Bitrate -> nonzero 0.0 (fmt "%4.0f kbps") (format.bitrate /. 1000.0)
  | `Rate ->
    let attr =
      match format.codec with
      | "MP3" | "OGG" | "OPUS" -> `Bitrate
      | _ -> `SampleRate
    in format_attr_string format attr

let meta_attr_string (meta : Meta.t) = function
  | `Artist -> meta.artist
  | `Title -> meta.title
  | `AlbumArtist -> meta.albumartist
  | `AlbumTitle -> meta.albumtitle
  | `Track -> nonzero_int 3 meta.track
  | `Tracks -> nonzero_int 3 meta.track (* TODO: set and use tracks *)
  | `Disc -> nonzero_int 2 meta.disc
  | `Discs -> nonzero_int 2 meta.disc (* TODO: set and use discs *)
  | `Date -> meta.date_txt
  | `Year -> nonzero_int 4 meta.year
  | `Label -> meta.label
  | `Country -> meta.country
  | `Length -> nonzero 0.0 fmt_time meta.length
  | `Rating ->
    let star = "*" in  (* TODO: "★" *)
    let len = String.length star in
    String.init (meta.rating * len) (fun i -> star.[i mod len])

let artist_attr_string' attr path meta =
  let s = nonempty meta_attr_string meta attr in
  if s <> "" then s else
  match Track.artist_title_of_path path with
  | Some (artist, _) -> artist
  | None -> "[unknown]"

let title_attr_string' attr path meta =
  let s = nonempty meta_attr_string meta attr in
  if s <> "" then s else
  match Track.artist_title_of_path path with
  | Some (_, title) -> title
  | None -> "[unknown]"

let length_attr_string' format meta =
  let s = nonempty format_attr_string format `Length in
  if s <> "" then s else nonempty meta_attr_string meta `Length

let artist_attr_string (artist : artist) = function
  | `Artist -> artist.name
  | `Tracks -> fmt "%4d" artist.tracks
  | `Albums -> fmt "%3d" artist.albums

let album_attr_string (album : album) = function
  | `AlbumArtist -> artist_attr_string' `AlbumArtist album.path album.meta
  | `AlbumTitle -> title_attr_string' `AlbumTitle album.path album.meta
  | `Length -> length_attr_string' album.format album.meta
  | #file_attr as attr -> file_attr_string album.path album.file attr
  | #format_attr as attr -> nonempty format_attr_string album.format attr
  | #meta_attr as attr -> nonempty meta_attr_string album.meta attr

let track_attr_string (track : track) = function
  | `Pos -> nonzero_int 3 (track.pos + 1)
  | `Artist -> artist_attr_string' `Artist track.path track.meta
  | `Title -> title_attr_string' `Title track.path track.meta
  | `Length -> length_attr_string' track.format track.meta
  | #file_attr as attr -> file_attr_string track.path track.file attr
  | #format_attr as attr -> nonempty format_attr_string track.format attr
  | #meta_attr as attr -> nonempty meta_attr_string track.meta attr


(* Scanning *)

type scan_mode = [`Fast | `Thorough]

let rescan_track lib mode track =
  let old = {track with id = track.id} in
  try
    if not (Sys.file_exists track.path) then
    (
      track.status <- `Absent;
    )
    else if Sys.is_directory track.path then
    (
      track.status <- `Invalid;
    )
    else if not (Data.is_track_path track.path) then
    (
      let stats = Unix.stat track.path in
      track.file.size <- stats.st_size;
      track.file.time <- stats.st_mtime;
      track.status <- `Invalid;
    )
    else
    (
      let stats = Unix.stat track.path in
      track.file.size <- stats.st_size;
      track.file.time <- stats.st_mtime;
      if
        mode = `Thorough || track.status = `Undet ||
        track.file.size <> old.file.size || track.file.time <> old.file.time
      then
      (
        track.format <- Some (Format.read track.path);
        track.meta <- Some (Meta.load ~with_cover: false track.path);
        track.status <- `Det;
      )
    );
    if track <> old then
    (
      track.file.age <- Unix.gettimeofday ();
      Db.insert_track lib.db track;
    )
  with exn ->
    Storage.log_exn "file" exn ("scanning track " ^ track.path)


let rescan_dir_tracks lib mode (dir : Data.dir) =
  try
    Array.iter (fun file ->
      let path = dir.path ^ file in
      if not (Sys.is_directory path) && Data.is_track_path path then
      (
        let track =
          match Db.find_track lib.db path with
          | Some track -> track
          | None -> Data.make_track path
        in
        (* Parent may have been deleted in the mean time... *)
        if Db.mem_dir lib.db dir.path then
          rescan_track lib mode track
      )
    ) (Sys.readdir dir.path)
  with exn ->
    Storage.log_exn "file" exn ("scanning tracks in directory " ^ dir.path)

let rescan_playlist lib _mode path =
  try
    let s = In_channel.(with_open_bin path input_all) in
    let items = M3u.parse_ext s in
    let items' = List.map (M3u.resolve (Filename.dirname path)) items in
    Db.delete_playlists lib.db path;
    Db.insert_playlists_bulk lib.db path items';
  with exn ->
    Storage.log_exn "file" exn ("scanning playlist " ^ path)


let queue_rescan_dir_tracks = ref (fun _ -> assert false)

let rescan_dir lib mode (origin : Data.dir) =
  (* TODO: bulk insert for performance *)
  let rec scan_path path nest =
    if Sys.is_directory path then
      if Data.is_track_path path then
        scan_album path nest
      else
        scan_dir path nest
    else
      if Data.is_track_path path then
        scan_track path
      else if Data.is_playlist_path path then
        scan_playlist path nest
      else
        None

  and scan_dir path nest' =
    let nest = nest' + 1 in
    let dirs = Option.value ~default: [] in
    match
      Array.fold_left (fun r file ->
        match r, scan_path (Filename.concat path file) nest with
        | None, None -> None
        | dirs1, dirs2 -> Some (dirs dirs1 @ dirs dirs2)
      ) None (Sys.readdir path)
    with
    | None -> None
    | Some dirs ->
      let dir =
        if nest = origin.nest then origin else
        let dirpath = Filename.concat path "" in
        match Db.find_dir lib.db dirpath with
        | Some dir -> dir
        | None ->
          let parent = Some (Data.parent_path path) in
          let dir = Data.make_dir dirpath parent nest 0 in  (* TODO: pos *)
          if Data.is_track_path path then
            dir.name <- Filename.remove_extension dir.name;
          dir.folded <- true;
          (* Root may have been deleted in the mean time... *)
          if Db.mem_dir lib.db origin.path then
            Db.insert_dir lib.db dir;
          dir
      in
      (* TODO: remove missing children from DB *)
      dir.children <- Array.of_list dirs;
      !queue_rescan_dir_tracks lib mode dir;
      Some [dir]

  and scan_album path nest =
    scan_dir path nest (* TODO *)

  and scan_track _path =
    Some []  (* deferred to directory scan *)

  and scan_playlist path nest =
    let dir =
      match Db.find_dir lib.db path with
      | Some dir -> dir
      | None ->
        let parent = Some (Data.parent_path path) in
        let dir = Data.make_dir path parent (nest + 1) 0 in  (* TODO: pos *)
        dir.name <- Filename.remove_extension dir.name;
        dir.folded <- true;
        (* Root may have been deleted in the mean time... *)
        if Db.mem_dir lib.db origin.path then
          Db.insert_dir lib.db dir;
        dir
    in
    Some [dir]
  in

  try
    ignore (scan_dir origin.path (origin.nest - 1))
  with exn ->
    Storage.log_exn "file" exn ("scanning directory " ^ origin.path)


let rescan_dir lib mode (dir : dir) =
  Safe_queue.add (false, dir.path, fun () -> rescan_dir lib mode dir)
    lib.scan.dir_queue

let rescan_playlist lib mode path =
  Safe_queue.add (true, path, fun () -> rescan_playlist lib mode path)
    lib.scan.file_queue

let rescan_track lib mode track =
  Safe_queue.add (true, track.path, fun () -> rescan_track lib mode track)
    lib.scan.file_queue

let rescan_dir_tracks lib mode (dir : dir) =
  Safe_queue.add (true, dir.path, fun () -> rescan_dir_tracks lib mode dir)
    lib.scan.dir_queue


let rescan_dirs lib mode dirs = Array.iter (rescan_dir lib mode) dirs
let rescan_root lib mode = rescan_dirs lib mode lib.root.children
let rescan_tracks lib mode tracks = Array.iter (rescan_track lib mode) tracks

let rescan_done lib = Atomic.exchange lib.scan.completed []
let rescan_busy lib =
  match Atomic.get lib.scan.dir_busy with
  | None -> Atomic.get lib.scan.file_busy
  | some -> some

let _ = queue_rescan_dir_tracks := rescan_dir_tracks


(* Browser *)

let length_browser lib = Table.length lib.browser

let has_track lib track = Db.mem_track lib.db track.path


let defocus lib =
  lib.browser.focus <- false;
  lib.artists.focus <- false;
  lib.albums.focus <- false;
  lib.tracks.focus <- false

let focus_browser lib =
  defocus lib;
  lib.browser.focus <- true


let selected_dir lib =
  Table.first_selected lib.browser

let deselect_dir lib =
  Table.deselect_all lib.browser;
  lib.current <- None;
  Table.clear_undo lib.tracks;
  Db.clear_playlists lib.db

let select_dir lib i =
  Table.deselect_all lib.browser;
  Table.select lib.browser i i;
  let dir = lib.browser.entries.(i) in
  if lib.current <> Some dir then
  (
    lib.current <- Some dir;
    Table.clear_undo lib.tracks;
    Db.clear_playlists lib.db;
    if Data.is_playlist_path dir.path then
      rescan_playlist lib false dir.path;
  )


let update_dir lib dir =
  Db.insert_dir lib.db dir

let refresh_browser lib =
  let rec entries (dir : dir) acc =
    Option.iter (fun (cur : dir) ->
      if cur.path = dir.path then lib.current <- Some dir) lib.current;
    dir ::
    (if dir.folded then acc else Array.fold_right entries dir.children acc)
  in
  let selection = Table.save_selection lib.browser in
  lib.browser.entries <- Array.of_list (entries lib.root []);
  Table.adjust_pos lib.browser;
  Table.restore_selection lib.browser selection (fun dir -> dir.path)


let fold_dir lib dir status =
  if status <> dir.folded then
  (
    dir.folded <- status;
    Db.insert_dir lib.db dir;
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

let current_is_shown_playlist lib =
  match lib.current with
  | None -> false
  | Some dir -> dir.tracks_shown && Data.is_playlist dir


(* Roots *)

let load_dirs lib =
  let dirs = ref [] in
  Db.iter_dirs lib.db (fun dir -> dirs := dir :: !dirs);
  if !dirs = [] then
  (
    (* Fresh library *)
    dirs := [lib.root];
    Db.insert_dir lib.db lib.root;
  );

  let rec treeify (parent : dir) children : dir list -> _ = function
    | dir::dirs when String.starts_with dir.path ~prefix: parent.path ->
      let dirs' = if Data.is_dir dir then treeify dir [] dirs else dirs in
      treeify parent (dir::children) dirs'
    | dirs ->
      parent.children <- Array.of_list children;
      let cmp (dir1 : dir) (dir2 : dir) =
        match compare dir1.pos dir2.pos with
        | 0 -> compare dir1.name dir2.name
        | i -> i
      in Array.stable_sort cmp parent.children; dirs
  in
  lib.root <- List.hd !dirs;
  ignore (treeify lib.root [] (List.tl !dirs));

  rescan_root lib `Fast


let make_root lib path pos =
  if not (Sys.file_exists path) then
    failwith (path ^ " does not exist")
  else if not (Sys.is_directory path) then
    failwith (path ^ " is not a directory")
  else
  (
    let dirpath = Filename.concat path "" in
    match
      Array.find_opt (fun (dir : dir) ->
        path = dir.path ||
        String.starts_with dir.path ~prefix: dirpath ||
        String.starts_with dirpath ~prefix: dir.path
      ) lib.root.children
    with
    | Some dir ->
      failwith (dirpath ^ " overlaps with " ^ dir.name ^ " (" ^ dir.path ^ ")")
    | None -> Data.make_dir dirpath (Some "") 0 pos
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
          Db.insert_dir lib.db root;  (* update position *)
          root
      );
    (* For some reason, this doesn't work with the parent condition
    Db.update_dirs_pos lib.db None pos (+len');
    *)
    Array.iter (Db.insert_dir lib.db) roots';
    Array.iter (fun (dir : dir) -> rescan_dir lib `Thorough dir) roots';
    refresh_browser lib;
    true
  with Failure msg ->
    error lib msg;
    false


let remove_dir lib path =
  let dirpath = Filename.concat path "" in
  let roots = lib.root.children in
  match Array.find_index (fun (r : Data.dir) -> r.path = dirpath) roots with
  | None -> ()
  | Some pos ->
    Db.delete_dirs lib.db dirpath;
    Db.delete_albums lib.db dirpath;
    Db.delete_tracks lib.db dirpath;
    Db.delete_playlists lib.db dirpath;
    lib.root.children <-
      Array.init (Array.length roots - 1) (fun i ->
        if i < pos then
          roots.(i)
        else
          let root = roots.(i + 1) in
          root.pos <- i; root
      );
    Db.update_dirs_pos lib.db None pos (-1);
    lib.current <- None;
    refresh_browser lib

let remove_dirs lib paths =
  List.iter (remove_dir lib) paths


(* Views *)

let focus_artists lib =
  defocus lib;
  lib.artists.focus <- true

let focus_albums lib =
  defocus lib;
  lib.albums.focus <- true

let focus_tracks lib =
  defocus lib;
  lib.tracks.focus <- true


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


let artists_sorting dir = dir.artists_sorting
let albums_sorting dir = dir.albums_sorting
let tracks_sorting dir = dir.tracks_sorting

let artist_key (artist : artist) = artist.name
let album_key (album : album) = (*album.track*)  (* TODO *)
  match album.meta with
  | None -> "[unknown]"
  | Some meta -> meta.albumtitle

let track_key lib =
  if current_is_playlist lib then
    fun track -> string_of_int track.pos
  else
    fun track -> track.path


let sort_entries entries sorting attr_string =
  let enriched =
    Array.map (fun entry ->
      List.map (fun (attr, _) -> attr_string entry attr) sorting, entry
    ) entries
  in
  let cmp e1 e2 = Data.compare_attrs sorting (fst e1) (fst e2) in
  Array.stable_sort cmp enriched;
  Array.map snd enriched

let refresh lib tab sorting attr_string key iter_db =
  let selection = Table.save_selection tab in
  let entries' = ref [||] in
  Option.iter (fun (dir : dir) ->
    let entries = ref [] in
    iter_db dir (fun e -> entries := e :: !entries);
    entries' := sort_entries (Array.of_list !entries) (sorting dir) attr_string;
  ) lib.current;
  tab.entries <- !entries';
  Table.adjust_pos tab;
  Table.restore_selection tab selection key

let refresh_tracks lib =
  refresh lib lib.tracks tracks_sorting track_attr_string (track_key lib)
    (fun dir f ->
      let artists =
        if Table.(num_selected lib.artists = length lib.artists) then [||] else
        Array.map artist_key (Table.selected lib.artists)
      and albums =
        if Table.(num_selected lib.albums = length lib.albums) then [||] else
        Array.map album_key (Table.selected lib.albums)
      in
      if dir.path = "" then
        Db.iter_tracks_for_path lib.db "%" artists albums f
      else if Data.is_dir dir then
        let path = Filename.concat dir.path "%" in
        Db.iter_tracks_for_path lib.db path artists albums f
      else if Data.is_playlist dir then
        Db.iter_playlist_tracks_for_path lib.db dir.path artists albums f
    )

let refresh_albums lib =
  refresh_tracks lib;
  refresh lib lib.albums albums_sorting album_attr_string album_key
    (fun dir f ->
      let artists =
        if Table.(num_selected lib.artists = length lib.artists) then [||] else
        Array.map artist_key (Table.selected lib.artists)
      in
      if dir.path = "" then
        Db.iter_tracks_for_path_as_albums lib.db "%" artists f
      else if Data.is_dir dir then
        let path = Filename.concat dir.path "%" in
        Db.iter_tracks_for_path_as_albums lib.db path artists f
      else if Data.is_playlist dir then
        Db.iter_playlist_tracks_for_path_as_albums lib.db dir.path artists f
    )

let refresh_artists lib =
  refresh_albums lib;
  refresh lib lib.artists artists_sorting artist_attr_string artist_key
    (fun dir f ->
      if dir.path = "" then
        Db.iter_tracks_for_path_as_artists lib.db "%" f
      else if Data.is_dir dir then
        let path = Filename.concat dir.path "%" in
        Db.iter_tracks_for_path_as_artists lib.db path f
      else if Data.is_playlist dir then
        Db.iter_playlist_tracks_for_path_as_artists lib.db dir.path f
    )

let refresh_views lib = refresh_artists lib


let rescan_affects_views lib path_opts =
  match lib.current with
  | None -> false
  | Some dir ->
    List.exists (function
      | None -> true
      | Some path ->
        String.starts_with path ~prefix: dir.path ||
        not (Data.is_dir dir) &&
        Array.exists (fun track ->
          String.starts_with track.path ~prefix: path) lib.tracks.entries
    ) path_opts

let refresh_after_rescan lib =
  let updated = rescan_done lib in
  if List.mem None updated then
  (
    refresh_browser lib;
    refresh_views lib;
  )
  else if rescan_affects_views lib updated then
    refresh_views lib


let reorder lib tab sorting attr_string key =
  Option.iter (fun (dir : dir) ->
    let selection = Table.save_selection tab in
    tab.entries <- sort_entries tab.entries (sorting dir) attr_string;
    Table.restore_selection tab selection key;
  ) lib.current

let reorder_artists lib =
  reorder lib lib.artists artists_sorting artist_attr_string artist_key

let reorder_albums lib =
  reorder lib lib.albums albums_sorting album_attr_string album_key

let reorder_tracks lib =
  reorder lib lib.tracks tracks_sorting track_attr_string (track_key lib)


(* Playlist Editing *)

let length lib = Table.length lib.tracks
let tracks lib = lib.tracks.entries
let table lib = lib.tracks

let adjust_scroll lib pos fit = Table.adjust_scroll lib.tracks pos fit


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
    Out_channel.with_open_bin dir.path (fun file -> output_string file s);
    Db.delete_playlists lib.db dir.path;
    if items <> [] then Db.insert_playlists_bulk lib.db dir.path items;
  with exn ->
    Storage.log_exn "file" exn ("writing playlist " ^ dir.path)


(* Before editing, normalise playlist to pos-order to enable correct undo *)
let compare_pos (tr1 : track) (tr2 : track) = compare tr1.pos tr2.pos

let normalize_playlist lib =
  if Table.(has_selection lib.artists || has_selection lib.albums) then
  (
    Table.deselect_all lib.artists;
    Table.deselect_all lib.albums;
    refresh_views lib;
  );
  let dir = Option.get lib.current in
  match dir.tracks_sorting with
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
        match Db.find_track lib.db track.path with
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
    refresh_views lib;
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
    Array.iter (fun track -> track.pos <- js.(track.pos)) lib.tracks.entries;
    restore_playlist lib order;
    save_playlist lib;
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
    Array.iter (fun track -> track.pos <- js.(track.pos)) lib.tracks.entries;
    restore_playlist lib order;
    save_playlist lib;
  )


let undo lib =
  let order = normalize_playlist lib in
  Table.pop_undo lib.tracks;
  Array.iteri (fun i track -> track.pos <- i) lib.tracks.entries;
  restore_playlist lib order;
  save_playlist lib

let redo lib =
  let order = normalize_playlist lib in
  Table.pop_redo lib.tracks;
  Array.iteri (fun i track -> track.pos <- i) lib.tracks.entries;
  restore_playlist lib order;
  save_playlist lib


(* Persistance *)

open Storage
let fmt = Printf.sprintf
let scan = Scanf.sscanf
let num l h x = max l (min h x)

let to_map lib =
  Map.of_list
  [
    "browser_scroll", fmt "%d" lib.browser.vscroll;
    "browser_current",
      (Option.value lib.current ~default: (Data.make_dir "-" None 0 0)).path;
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
    lib.browser.vscroll <- scan s "%d" (num 0 (max 0 (length_browser lib - 1))));
  read_map m "browser_current" (fun s ->
    lib.current <- Db.find_dir lib.db s;
    Option.iter (fun i ->
      let dir = lib.browser.entries.(i) in
      lib.current <- Some dir;
      select_dir lib i;
      if current_is_playlist lib then rescan_playlist lib `Fast dir.path;
    ) (Array.find_index (fun (dir : dir) -> dir.path = s) lib.browser.entries)
  );
  refresh_views lib
