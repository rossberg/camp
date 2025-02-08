(* Library *)

open Audio_file
open Data


type time = float
type db = Db.t

type scan =
{
  dir_queue : (path option * (unit -> unit)) Safe_queue.t;
  file_queue : (path option * (unit -> unit)) Safe_queue.t;
  dir_busy : bool Atomic.t;
  file_busy : bool Atomic.t;
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
  check "tracks pos unset" (lib.tracks.pos = None || lib.tracks.pos = Some 0) @
  check "browser consistent with roots"
    ( Array.length lib.browser.entries = 1 ||
      Array.length lib.browser.entries >= Array.length lib.root.children ) @
  check "browser selection singular" (Table.num_selected lib.browser <= 1) @
  check "browser selection consistent with current"
    ( Table.num_selected lib.browser = 0 ||
      lib.current = Some (Table.selected lib.browser).(0) ) @
  []


(* Constructor *)

let rec complete scan path =
  let paths = Atomic.get scan.completed in
  if not (Atomic.compare_and_set scan.completed paths (path::paths)) then
    complete scan path

let rec scanner scan queue busy () =
  Atomic.set busy false;
  let path, f = Safe_queue.take queue in
  Atomic.set busy true;
  f ();
  complete scan path;
  scanner scan queue busy ()

let make_scan () =
  let scan =
    {
      dir_queue = Safe_queue.create ();
      file_queue = Safe_queue.create ();
      dir_busy = Atomic.make false;
      file_busy = Atomic.make false;
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
    tracks = Table.make 0;
    error = "";
    error_time = 0.0;
  }


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
let nonzero_int x = nonzero 0 string_of_int x
let nonempty f x attr = match x with None -> "" | Some x -> f x attr


let file_attr_string path (file : file) = function
  | `FilePath -> path
  | `FileSize -> nonzero 0.0 (fmt "%.1f MB") (float file.size /. 2.0 ** 20.0)
  | `FileTime -> nonzero 0.0 fmt_date file.time

let rec format_attr_string (format : Format.t) = function
  | `Length -> nonzero 0.0 fmt_time format.time
  | `Codec -> format.codec
  | `Channels -> nonzero_int format.channels
  | `Depth ->
    let depth = format.bitrate /. float format.rate /. float format.channels in
    let fmts : _ format =
      if float format.depth = Float.round depth then "%.0f" else "%.1f"
    in nonzero 0.0 (fmt fmts) depth
  | `SampleRate -> nonzero 0.0 (fmt "%.1f KHz") (float format.rate /. 1000.0)
  | `Bitrate -> nonzero 0.0 (fmt "%.0f kbps") (format.bitrate /. 1000.0)
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
  | `Track -> nonzero_int meta.track
  | `Tracks -> nonzero_int meta.track (* TODO: set and use tracks *)
  | `Disc -> nonzero_int meta.disc
  | `Discs -> nonzero_int meta.disc (* TODO: set and use discs *)
  | `Date -> meta.date_txt
  | `Year -> nonzero_int meta.year
  | `Label -> meta.label
  | `Country -> meta.country
  | `Length -> nonzero 0.0 fmt_time meta.length
  | `Rating ->
    let star = "*" in  (* TODO: "★" *)
    let len = String.length star in
    String.init (meta.rating * len) (fun i -> star.[i mod len])

let length_attr_string format meta =
  let s = nonempty format_attr_string format `Length in
  if s <> "" then s else nonempty meta_attr_string meta `Length

let artist_attr_string (artist : artist) = function
  | `Artist -> artist.name
  | `Tracks -> string_of_int artist.tracks
  | `Albums -> string_of_int artist.albums

let album_attr_string (album : album) = function
  | `Length -> length_attr_string album.format album.meta
  | #file_attr as attr -> file_attr_string album.path album.file attr
  | #format_attr as attr -> nonempty format_attr_string album.format attr
  | #meta_attr as attr -> nonempty meta_attr_string album.meta attr

let track_attr_string (track : track) = function
  | `Pos -> nonzero 0 (fmt "%3d") track.pos
  | `Length -> length_attr_string track.format track.meta
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
    else if not (Format.is_known_ext track.path) then
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
  with exn -> Storage.log
    ("error scaning track " ^ track.path ^ ": " ^ Printexc.to_string exn)


let rescan_dir_tracks lib mode (dir : Data.dir) =
  try
    Array.iter (fun file ->
      let path = Filename.concat dir.path file in
      if
        Sys.file_exists path &&
        not (Sys.is_directory path) &&
        Format.is_known_ext path
      then
      (
        let track =
          match Db.find_track lib.db path with
          | Some track -> track
          | None -> Data.make_track path
        in
        (* Parent may have been deleted in the mean time... *)
        if Db.exists_dir lib.db dir.path then
          rescan_track lib mode track
      )
    ) (Sys.readdir dir.path)
  with exn -> Storage.log
    ("error scanning dir " ^ dir.path ^ ": " ^ Printexc.to_string exn)

let rescan_playlist lib _mode path =
  try
    let s = In_channel.(with_open_bin path input_all) in
    let items = M3u.parse_ext s in
    let items' = List.map (M3u.resolve (Filename.dirname path)) items in
    Db.delete_playlists lib.db path;
    Db.insert_playlists_bulk lib.db path items';
  with exn -> Storage.log
    ("error scanning playlist " ^ path ^ ": " ^ Printexc.to_string exn)


let queue_rescan_dir_tracks = ref (fun _ -> assert false)

let rescan_dir lib mode (origin : Data.dir) =
  let rec scan_path path nest =
    if Sys.is_directory path then
      if Format.is_known_ext path then
        scan_album path nest
      else
        scan_dir path nest
    else
      if Format.is_known_ext path then
        scan_track path
      else if M3u.is_known_ext path then
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
        match Db.find_dir lib.db path with
        | Some dir -> dir
        | None ->
          let parent = Some (Filename.dirname path) in
          let dir = Data.make_dir path parent nest 0 in  (* TODO: pos *)
          if Format.is_known_ext path then
            dir.name <- Filename.remove_extension dir.name;
          dir.folded <- true;
          (* Root may have been deleted in the mean time... *)
          if Db.exists_dir lib.db origin.path then
            Db.insert_dir lib.db dir;
          dir
      in
      (* TODO: remove missing children *)
      dir.children <- Array.of_list dirs;
      !queue_rescan_dir_tracks lib mode dir;
      Some [dir]

  and scan_album path nest =
    scan_dir path nest (* TODO *)

  and scan_track _path =
    Some []  (* deferred to directory scan *)

  and scan_playlist path nest =
    let parent = Some (Filename.dirname path) in
    let dir = Data.make_dir path parent (nest + 1) 0 in  (* TODO: pos *)
    if M3u.is_known_ext path then
      dir.name <- Filename.remove_extension dir.name;
    dir.folded <- true;
    (* Root may have been deleted in the mean time... *)
    if Db.exists_root lib.db origin.path then
      Db.insert_dir lib.db dir;
    Some [dir]
  in

  try
    ignore (scan_dir origin.path (origin.nest - 1))
  with exn -> Storage.log
    ("error scaning directory " ^ origin.path ^ ": " ^ Printexc.to_string exn)


let rescan_dir lib mode dir =
  Safe_queue.add (None, fun () -> rescan_dir lib mode dir)
    lib.scan.dir_queue

let rescan_playlist lib mode path =
  Safe_queue.add (Some path, fun () -> rescan_playlist lib mode path)
    lib.scan.file_queue

let rescan_track lib mode track =
  Safe_queue.add (Some track.path, fun () -> rescan_track lib mode track)
    lib.scan.file_queue

let rescan_dir_tracks lib mode (dir : dir) =
  Safe_queue.add (Some dir.path, fun () -> rescan_dir_tracks lib mode dir)
    lib.scan.dir_queue


let rescan_dirs lib mode dirs = Array.iter (rescan_dir lib mode) dirs
let rescan_roots lib mode = rescan_dirs lib mode lib.root.children
let rescan_tracks lib mode tracks = Array.iter (rescan_track lib mode) tracks

let rescan_busy lib = Atomic.(get lib.scan.dir_busy || get lib.scan.file_busy)
let rescan_done lib = Atomic.exchange lib.scan.completed []

let _ = queue_rescan_dir_tracks := rescan_dir_tracks


(* Browser *)

let length_browser lib = Table.length lib.browser


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
  Db.clear_playlists lib.db

let select_dir lib i =
  Table.deselect_all lib.browser;
  Table.select lib.browser i i;
  let dir = lib.browser.entries.(i) in
  lib.current <- Some dir;
  Db.clear_playlists lib.db;
  if M3u.is_known_ext dir.path then
    rescan_playlist lib false dir.path


let update_dir lib dir =
  Db.insert_dir lib.db dir

let update_browser lib =
  let rec entries dir acc =
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
    update_browser lib;
    if not status && lib.current <> None then
    (
      Option.iter (fun i -> Table.select lib.browser i i)
        (Array.find_index (fun dir -> lib.current = Some dir) lib.browser.entries)
    );
  )


(* Roots *)

let is_root _lib dir = dir.parent = Some ""

let load_roots lib =
  let dirs = ref [] in
  Db.iter_dirs lib.db (fun dir -> dirs := dir :: !dirs);
  if !dirs = [] then
  (
    (* Fresh library *)
    dirs := [lib.root];
    Db.insert_dir lib.db lib.root;
  );
  let dirs = Array.of_list !dirs in
  let dirpath path = Filename.concat path "" in
  let compare_dir (d1 : Data.dir) (d2 : Data.dir) =
    compare (dirpath d1.path) (dirpath d2.path) in
  Array.sort compare_dir dirs;

  let roots = ref [] in
  Db.iter_roots lib.db (fun root -> roots := root :: !roots);
  let roots =
    List.mapi (fun i path ->
      match Db.find_dir lib.db path with
      | Some dir -> dir
      | None ->
        Storage.log ("database contains root with no dir entry: " ^ path);
        Data.make_dir path (Some "") 0 i
    ) (List.rev !roots)
  in

  let len = Array.length dirs in
  let rec treeify i (parent : dir) children =
    if i < len && Filename.dirname dirs.(i).path = parent.path then
      treeify (i + 1) parent (dirs.(i) :: children)
    else
    (
      parent.children <- Array.of_list children;
      Array.stable_sort (fun (d1 : dir) (d2 : dir) -> compare d1.pos d2.pos)
        parent.children;
      if i < len then treeify (i + 1) dirs.(i) []
    )
  in
  lib.root <- dirs.(0);
  treeify 1 lib.root roots;

  rescan_roots lib `Fast


let make_root lib path pos =
  if not (Sys.file_exists path) then
    failwith (path ^ " does not exist")
  else if not (Sys.is_directory path) then
    failwith (path ^ " is not a directory")
  else
  (
    match
      Array.find_opt (fun (dir : dir) ->
        path = dir.path ||
        String.starts_with dir.path ~prefix: (Filename.concat path "") ||
        String.starts_with path ~prefix: (Filename.concat dir.path "")
      ) lib.root.children
    with
    | Some dir ->
      failwith (path ^ " overlaps with " ^ dir.name ^ " (" ^ dir.path ^ ")")
    | None -> Data.make_dir path (Some "") 0 pos
  )

let add_roots lib paths pos =
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
    Array.iter (Db.insert_root lib.db) paths;
    Array.iter (Db.insert_dir lib.db) roots';
    Array.iter (rescan_dir lib `Thorough) roots';
    update_browser lib;
    true
  with Failure msg ->
    lib.error <- msg;
    lib.error_time <- Unix.gettimeofday ();
    false


let remove_root lib path =
  let roots = lib.root.children in
  match Array.find_index (fun (r : Data.dir) -> r.path = path) roots with
  | None -> ()
  | Some pos ->
    let prefix = Filename.concat path "" in
    Db.delete_root lib.db path;
    Db.delete_dirs lib.db prefix;
    Db.delete_albums lib.db prefix;
    Db.delete_tracks lib.db prefix;
    Db.delete_playlists lib.db prefix;
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
    update_browser lib

let remove_roots lib paths =
  List.iter (remove_root lib) paths


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

let adjust_scroll lib pos fit = Table.adjust_scroll lib.tracks pos fit


let artists_sorting dir = dir.artists_sorting
let albums_sorting dir = dir.albums_sorting
let tracks_sorting dir = dir.tracks_sorting

let artist_key (artist : artist) = artist.name
let album_key (album : album) = (*album.track*)  (* TODO *)
  match album.meta with
  | None -> "[unknown]"
  | Some meta -> meta.albumtitle

let track_key (track : track) = track.path

let sort_entries entries (attr, order) attr_string =
  let enriched = Array.map (fun entry -> attr_string entry attr, entry) entries in
  let sign = if order = `Asc then +1 else - 1 in
  let cmp t1 t2 = sign * Data.compare_for attr (fst t1) (fst t2) in
  Array.stable_sort cmp enriched;
  Array.map snd enriched

let update lib tab sorting attr_string key iter_db =
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

let update_tracks lib =
  update lib lib.tracks tracks_sorting track_attr_string track_key
    (fun dir f ->
      (* TODO: filter by multiple artists and albums *)
      let artist =
        match Table.first_selected lib.artists with
        | Some i -> lib.artists.entries.(i).name
        | None -> ""
      and album =
        match Table.first_selected lib.albums with
        | Some i when lib.albums.entries.(i).meta <> None ->
          (Option.get lib.albums.entries.(i).meta).albumtitle
        | _ -> ""
      in
      if dir.path = "" then
        Db.iter_tracks_for_path lib.db "%" artist album f
      else if Sys.file_exists dir.path && Sys.is_directory dir.path then
        let path = Filename.concat dir.path "%" in
        Db.iter_tracks_for_path lib.db path artist album f
      else if M3u.is_known_ext dir.path then
        Db.iter_playlist_tracks_for_path lib.db dir.path artist album f
    )

let update_albums lib =
  update_tracks lib;
  update lib lib.albums albums_sorting album_attr_string album_key
    (fun dir f ->
      (* TODO: filter by multiple artists *)
      let artist =
        match Table.first_selected lib.artists with
        | Some i -> lib.artists.entries.(i).name
        | None -> ""
      in
      if dir.path = "" then
        Db.iter_tracks_for_path_as_albums lib.db "%" artist f
      else if Sys.file_exists dir.path && Sys.is_directory dir.path then
        let path = Filename.concat dir.path "%" in
        Db.iter_tracks_for_path_as_albums lib.db path artist f
      else if M3u.is_known_ext dir.path then
        Db.iter_playlist_tracks_for_path_as_albums lib.db dir.path artist f
    )

let update_artists lib =
  update_albums lib;
  update lib lib.artists artists_sorting artist_attr_string artist_key
    (fun dir f ->
      if dir.path = "" then
        Db.iter_tracks_for_path_as_artists lib.db "%" f
      else if Sys.file_exists dir.path && Sys.is_directory dir.path then
        let path = Filename.concat dir.path "%" in
        Db.iter_tracks_for_path_as_artists lib.db path f
      else if M3u.is_known_ext dir.path then
        Db.iter_playlist_tracks_for_path_as_artists lib.db dir.path f
    )

let update_views lib = update_artists lib


let rescan_affects_views lib path_opts =
  match lib.current with
  | None -> false
  | Some dir ->
    let is_dir = Sys.file_exists dir.path && Sys.is_directory dir.path in
    List.exists (function
      | None -> true
      | Some path ->
        String.starts_with path ~prefix: dir.path ||
        not is_dir && Format.is_known_ext path &&
        not (Sys.file_exists path && Sys.is_directory path) &&
        Array.exists (fun track -> track.path = path) lib.tracks.entries
    ) path_opts

let update_after_rescan lib =
  let updated = rescan_done lib in
  if List.mem None updated then
  (
    update_browser lib;
    update_views lib;
  )
  else if rescan_affects_views lib updated then
    update_views lib


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
  reorder lib lib.tracks tracks_sorting track_attr_string track_key


(* Persistance *)

open Storage
let fmt = Printf.sprintf
let scan = Scanf.sscanf
let num l h x = max l (min h x)

let to_map lib =
  Map.of_list
  [
    "browser_scroll", fmt "%d" lib.browser.vscroll;
  ]

let to_map_extra lib =
  Map.of_list
  [
    "browser_pos", fmt "%d" (Option.value lib.browser.pos ~default: (-1));
    "browser_length", fmt "%d" (Array.length lib.browser.entries);
    "browser_selected", fmt "%d"
      (Option.value (selected_dir lib) ~default: (-1));
    "browser_current",
      (Option.value lib.current ~default: (Data.make_dir "" None 0 0)).name;
    "tracks_pos", fmt "%d" (Option.value lib.tracks.pos ~default: (-1));
    "tracks_vscroll", fmt "%d" lib.tracks.vscroll;
    "tracks_hscroll", fmt "%d" lib.tracks.hscroll;
    "tracks_length", fmt "%d" (Array.length lib.tracks.entries);
    "root_length", fmt "%d" (Array.length lib.root.children);
    "lib_error", fmt "%s" lib.error;
    "lib_error_time", fmt "%.1f" lib.error_time;
  ]

let of_map lib m =
  update_browser lib;
  update_views lib;
  read_map m "browser_scroll" (fun s ->
    lib.browser.vscroll <- scan s "%d" (num 0 (max 0 (length_browser lib - 1))))
