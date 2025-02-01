(* Library *)

open Audio_file
open Data

module Set = Set.Make(String)


type time = float
type db = Db.t

type t =
{
  db : db;
  mutable roots : dir array;
  mutable current : dir option;
  mutable browser : dir Table.t;
  mutable artists : artist Table.t;
  mutable albums : album Table.t;
  mutable tracks : track Table.t;
  mutable error : string;
  mutable error_time : time;
}


(* Constructor *)

let make db =
  {
    db;
    roots = [||];
    current = None;
    browser = Table.make ();
    artists = Table.make ();
    albums = Table.make ();
    tracks = Table.make ();
    error = "";
    error_time = 0.0;
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
      Array.length lib.browser.entries >= Array.length lib.roots ) @
  check "browser selection singular" (Table.num_selected lib.browser <= 1) @
  check "browser selection consistent with current"
    ( Table.num_selected lib.browser = 0 ||
      lib.current = Some (Table.selected lib.browser).(0) ) @
  []


(* Attributes *)

let attr_prop = function
  | `FilePath -> "File Path", `Left
  | `FileSize -> "File Size", `Right
  | `FileTime -> "File Time", `Left
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
  | `Pos -> "Pos", `Right
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

let fmt_date_time t =
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
  | `FileTime -> nonzero 0.0 fmt_date_time file.time

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
  | `Tracks -> nonzero_int meta.tracks
  | `Disc -> nonzero_int meta.disc
  | `Discs -> nonzero_int meta.discs
  | `Date -> meta.date_txt
  | `Year -> nonzero_int meta.year
  | `Label -> meta.label
  | `Country -> meta.country
  | `Rating ->
    let star = "*" in  (* TODO: "★" *)
    let len = String.length star in
    String.init (meta.rating * len) (fun i -> star.[i mod len])


let _artist_attr_string artist = function
  | `Artist -> artist.name
  | `Tracks -> string_of_int artist.tracks
  | `Albums -> string_of_int artist.albums

let _album_attr_string album = function
  | #file_attr as attr -> file_attr_string album.path album.file attr
  | #format_attr as attr -> nonempty format_attr_string album.format attr
  | #meta_attr as attr -> nonempty meta_attr_string album.meta attr

let track_attr_string track = function
  | #file_attr as attr -> file_attr_string track.path track.file attr
  | #format_attr as attr -> nonempty format_attr_string track.format attr
  | #meta_attr as attr -> nonempty meta_attr_string track.meta attr
  | `Pos -> nonzero_int track.pos


(* Helpers *)

let array_swap a i j =
  let temp = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- temp

let array_of_list_rev l =
  let a = Array.of_list l in
  let len = Array.length a in
  for i = 0 to len / 2 - 1 do
    array_swap a i (len - i - 1)
  done;
  a


(* Scanning *)

let rescan_track lib track =
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
      track.format <- Some (Format.read track.path);
      track.meta <- Some (Meta.load ~with_cover: false track.path);
      track.status <- `Det;
    );
    if track <> old then
    (
      track.file.age <- Unix.gettimeofday ();
      Db.insert_track lib.db track;
    )
  with exn -> Storage.log
    ("error scaning track " ^ track.path ^ ": " ^ Printexc.to_string exn)


let rescan_dir lib (dir : Data.dir) =
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
          rescan_track lib track
      )
    ) (Sys.readdir dir.path)
  with exn -> Storage.log
    ("error scaning dir " ^ dir.path ^ ": " ^ Printexc.to_string exn)


let queue_rescan_dir = ref (fun _ -> assert false)

let rescan_root lib (root : Data.dir) =
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
        scan_playlist path
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
      let children = Array.map Data.link_val (Array.of_list dirs) in
      let dir =
        if nest = 0 then root else
        match Db.find_dir lib.db path with
        | Some dir -> dir
        | None ->
          let parent = Some (Filename.dirname path) in
          let dir = Data.make_dir path parent nest 0 in  (* TODO: pos *)
          dir.folded <- true;
          (* Root may have been deleted in the mean time... *)
          if Db.exists_root lib.db root.path then
            Db.insert_dir lib.db dir;
          dir
      in
      (* TODO: remove missing children *)
      dir.children <- children;
      !queue_rescan_dir lib dir;
      Some [dir]

  and scan_album path nest =
    scan_dir path nest (* TODO *)

  and scan_track _path =
    Some []  (* deferred to directory scan *)

  and scan_playlist path =
    if not (Db.exists_playlist lib.db path) then
      Db.insert_playlist lib.db (Data.make_playlist path);
    Some []
  in

  try
    ignore (scan_dir root.path (-1))
  with exn -> Storage.log
    ("error scaning root " ^ root.path ^ ": " ^ Printexc.to_string exn)


let dir_queue = Safe_queue.create ()
let track_queue = Safe_queue.create ()

let dir_busy = Atomic.make false
let track_busy = Atomic.make false

let completed = Atomic.make false

let rec scanner queue busy () =
  Atomic.set busy false;
  let f = Safe_queue.take queue in
  Atomic.set busy true;
  f ();
  Atomic.set completed true;
  scanner queue busy ()

let _ = Domain.spawn (scanner dir_queue dir_busy)
let _ = Domain.spawn (scanner track_queue track_busy)

let rescan_root lib root = Safe_queue.add (fun () -> rescan_root lib root) dir_queue
let rescan_roots lib = Array.iter (rescan_root lib) lib.roots
let rescan_dir lib dir = Safe_queue.add (fun () -> rescan_dir lib dir) track_queue
let rescan_dirs lib dirs = Array.iter (rescan_dir lib) dirs
let rescan_tracks lib tracks =
  Safe_queue.add (fun () -> Array.iter (rescan_track lib) tracks) track_queue

let rescan_busy _lib = Atomic.get dir_busy || Atomic.get track_busy
let rescan_done _lib = Atomic.exchange completed false

let _ = queue_rescan_dir := rescan_dir


(* Browser *)

let length_browser lib = Table.length lib.browser

let selected_dir lib = Table.first_selected lib.browser
let deselect_dir lib = Table.deselect_all lib.browser; lib.current <- None
let select_dir lib i =
  Table.deselect_all lib.browser;
  Table.select lib.browser i i;
  lib.current <- Some lib.browser.entries.(i)


let save_browser_selection lib =
  let selection =
    Option.map (fun i -> lib.browser.entries.(i).path)
      (Table.first_selected lib.browser)
  in
  Table.deselect_all lib.browser;
  selection

let restore_browser_selection lib selection =
  Array.iteri
    (fun i (d : Data.dir) ->
      if selection = Some d.path then Table.select lib.browser i i
    ) lib.browser.entries;
  Table.adjust_scroll lib.browser (Table.first_selected lib.browser) 4


let make_all lib =
  let dir = Data.make_dir "" None (-1) 0 in
  dir.name <- "All";
  dir.children <- Array.map Data.link_val lib.roots;
  dir

let update_dir lib dir =
  Db.insert_dir lib.db dir

let update_browser lib =
  let rec entries dir acc =
    dir :: ( if dir.folded then acc else
    Array.fold_right
      (fun link -> entries (Data.val_of_link link)) dir.children acc)
  in
  let selection = save_browser_selection lib in
  lib.browser.entries <- Array.of_list (entries (make_all lib) []);
  Table.adjust_pos lib.browser;
  restore_browser_selection lib selection


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

let load_roots lib =
  let roots = ref [] in
  Db.iter_roots lib.db (fun root -> roots := root :: !roots);
  lib.roots <-
    Array.mapi (fun i path ->
      match Db.find_dir lib.db path with
      | Some dir -> dir
      | None ->
        Storage.log ("database contains root with no dir entry: " ^ path);
        Data.make_dir path None 0 i
    ) (Array.of_list !roots);
  Array.sort (fun (d1 : dir) (d2 : dir) -> compare d1.pos d2.pos) lib.roots;

  let dirs = ref [] in
  Db.iter_dirs lib.db (fun dir -> dirs := dir :: !dirs);
  let dirs = Array.of_list !dirs in
  let dirpath path = Filename.concat path "" in
  let compare_dir (d1 : Data.dir) (d2 : Data.dir) =
    compare (dirpath d1.path) (dirpath d2.path) in
  Array.sort compare_dir dirs;

  let rec treeify i parent children =
    if i = Array.length dirs then
    (
      parent.children <- array_of_list_rev children
    )
    else
      let parent', children' =
        if Filename.dirname dirs.(i).path = parent.path then
          parent, Data.link_val dirs.(i) :: children
        else
        (
          parent.children <- array_of_list_rev children;
          dirs.(i), []
        )
      in treeify (i + 1) parent' children'
  in
  treeify 0 (make_all lib) [];

  rescan_roots lib


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
      ) lib.roots
    with
    | Some dir ->
      failwith (path ^ " overlaps with " ^ dir.name ^ " (" ^ dir.path ^ ")")
    | None -> Data.make_dir path None 0 pos
  )

let add_roots lib paths pos =
  let paths = Array.of_list paths in
  lib.error <- "";
  try
    let roots' = Array.mapi (fun i path -> make_root lib path (pos + i)) paths in
    let len = Array.length lib.roots in
    let len' = Array.length roots' in
    lib.roots <-
      Array.init (len + len') (fun i ->
        if i < pos then
          lib.roots.(i)
        else if i < pos + len' then
          roots'.(i - pos)
        else
          let root = lib.roots.(i - len') in
          root.pos <- i;
          Db.insert_dir lib.db root;  (* update position *)
          root
      );
    (* For some reason, this doesn't work with the parent condition
    Db.update_dirs_pos lib.db None pos (+len');
    *)
    Array.iter (Db.insert_root lib.db) paths;
    Array.iter (Db.insert_dir lib.db) roots';
    Array.iter (rescan_root lib) roots';
    update_browser lib;
    true
  with Failure msg ->
    lib.error <- msg;
    lib.error_time <- Unix.gettimeofday ();
    false


let remove_root lib path =
  match Array.find_index (fun (r : Data.dir) -> r.path = path) lib.roots with
  | None -> ()
  | Some pos ->
    let prefix = Filename.concat path "" in
    Db.delete_root lib.db path;
    Db.delete_dirs lib.db prefix;
    Db.delete_albums lib.db prefix;
    Db.delete_tracks lib.db prefix;
    Db.delete_playlists lib.db prefix;
    lib.roots <-
      Array.init (Array.length lib.roots - 1) (fun i ->
        if i < pos then
          lib.roots.(i)
        else
          let root = lib.roots.(i + 1) in
          root.pos <- i; root
      );
    Db.update_dirs_pos lib.db None pos (-1);
    lib.current <- None;
    update_browser lib

let remove_roots lib paths =
  List.iter (remove_root lib) paths


(* Tracks View *)

let focus lib b = lib.tracks.focus <- b

let has_selection lib = Table.has_selection lib.tracks
let num_selected lib = Table.num_selected lib.tracks
let first_selected lib = Table.first_selected lib.tracks
let last_selected lib = Table.last_selected lib.tracks
let is_selected lib i = Table.is_selected lib.tracks i
let selected lib = Table.selected lib.tracks

let select_all lib =
  Table.select_all lib.tracks

let deselect_all lib =
  Table.deselect_all lib.tracks

let select_invert lib =
  Table.select_invert lib.tracks

let select lib i j =
  Table.select lib.tracks i j

let deselect lib i j =
  Table.deselect lib.tracks i j


let adjust_scroll lib pos fit =
  Table.adjust_scroll lib.tracks pos fit


let save_tracks_selection lib =
  let selection = selected lib in
  deselect_all lib;
  selection

let restore_tracks_selection lib selection =
  let set = Array.fold_right (fun t -> Set.add t.path) selection Set.empty in
  Array.iteri (fun i t -> if Set.mem t.path set then select lib i i)
    lib.tracks.entries;
  Table.adjust_scroll lib.tracks (first_selected lib) 4


let sort_tracks tracks (attr, order) =
  let enriched_tracks =
    Array.map (fun track -> track_attr_string track attr, track) tracks in
  let sign = if order = `Asc then +1 else - 1 in
  let cmp t1 t2 = sign * compare (fst t1) (fst t2) in
  Array.stable_sort cmp enriched_tracks;
  Array.map snd enriched_tracks

let update_tracks lib =
  let selection = save_tracks_selection lib in
  let tracks' = ref [||] in
  Option.iter (fun (dir : dir) ->
    let path = Filename.concat dir.path "" in
    let tracks = ref [] in
    Db.iter_tracks_for lib.db path (fun tr -> tracks := tr :: !tracks);
    tracks' := sort_tracks (Array.of_list !tracks) dir.tracks_sorting;
  ) lib.current;
  lib.tracks.entries <- !tracks';
  Table.adjust_pos lib.tracks;
  restore_tracks_selection lib selection

let reorder_tracks lib =
  Option.iter (fun (dir : dir) ->
    let selection = save_tracks_selection lib in
    lib.tracks.entries <- sort_tracks lib.tracks.entries dir.tracks_sorting;
    restore_tracks_selection lib selection;
  ) lib.current


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
    "browser_length",  fmt "%d" (Array.length lib.browser.entries);
    "tracks_pos", fmt "%d" (Option.value lib.tracks.pos ~default: (-1));
    "tracks_vscroll", fmt "%d" lib.tracks.vscroll;
    "tracks_hscroll", fmt "%d" lib.tracks.hscroll;
    "tracks_length", fmt "%d" (Array.length lib.tracks.entries);
    "root_length", fmt "%d" (Array.length lib.roots);
    "lib_error", fmt "%s" lib.error;
    "lib_error_time", fmt "%.1f" lib.error_time;
  ]

let of_map lib m =
  update_browser lib;
  read_map m "browser_scroll" (fun s ->
    lib.browser.vscroll <- scan s "%d" (num 0 (max 0 (length_browser lib - 1))))
