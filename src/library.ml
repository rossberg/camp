(* Library *)

open Audio_file
open Data

module IntSet = Set.Make(Int)
module Set = Set.Make(String)
module Map = Map.Make(String)


(* Helpers *)

let (.$()) = Iarray.get

(* HACK since OCaml 5.4 lacks Dynarray.to_iarray *)
let dynarray_to_iarray a = Obj.magic (Dynarray.to_array a)


(* Representations *)

type time = float

type display = [`Table | `Grid]

type 'attr view =
{
  mutable shown : display option;
  mutable columns : 'attr columns;
  mutable sorting : 'attr sorting;
}

type views =
{
  mutable search : string;
  mutable query : Query.query option;
  mutable folded : bool;
  mutable custom : bool;
  mutable divider_width : int;
  mutable divider_height : int;
  artists : artist_attr view;
  albums : album_attr view;
  tracks : track_attr view;
}

type dir = views Data.dir

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
  changed : bool Atomic.t;
}

type cover =
  | NoCover
  | ScanCover
  | ScannedCover of Meta.picture
  | Cover of {image : Api.image; mutable last_use : int}

type 'cache t =
{
  mutable root : dir;
  mutable current : dir option;
  mutable error : string;
  mutable error_time : time;
  mutable refresh_time : time;
  mutable covers_shown : bool;
  mutable renaming : int option;
  mutable log : 'cache Log.t option;
  search : Edit.t;
  rename : Edit.t;
  browser : (dir, 'cache) Table.t;
  artists : (artist, 'cache) Table.t;
  albums : (album, 'cache) Table.t;
  tracks : (track, 'cache) Table.t;
  covers : cover Map.t Atomic.t;
  mutable age_covers : (path * cover) Seq.t;
  mutable views_dir_default : views;
  mutable views_album_default : views;
  mutable views_playlist_default : views;
  scan : scan;
}


let start_rename lib i = lib.renaming <- Some i
let end_rename lib changed =
  lib.renaming <- None;
  if changed then Atomic.set lib.scan.changed true

let start_log lib log = lib.log <- Some log
let end_log lib = lib.log <- None


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]
let check_opt opt f = Option.value (Option.map f opt) ~default: []

let ok lib =
  Table.ok "browser" lib.browser @
  Table.ok "tracks" lib.tracks @
  check_opt lib.log Log.ok @
  check "root folded" (not lib.root.view.folded) @
  check "browser nonempty" (Table.length lib.browser > 0) @
  check "artists pos unset" (lib.artists.pos = None || lib.artists.pos = Some 0) @
  check "albums pos unset" (lib.albums.pos = None || lib.albums.pos = Some 0) @
  check "browser selection singular" (Table.num_selected lib.browser <= 1) @
  check "browser selection consistent with current"
    ( Table.num_selected lib.browser = 0 ||
      Option.get lib.current == (Table.selected lib.browser).(0) ) @
  check "rename focus consistent" ((lib.renaming <> None) = lib.rename.focus) @
  []


(* Constructor *)

let artists_sorting = [`Artist, `Asc]
let artists_columns : artist_attr columns =
[|
  `Artist, 150;
  `Albums, 20;
  `Tracks, 20;
|]

let albums_sorting = [`AlbumArtist, `Asc; `AlbumTitle, `Asc; `Codec, `Asc]
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

let tracks_sorting = [`Artist, `Asc; `Title, `Asc; `Codec, `Asc]
let tracks_columns : track_attr columns =
[|
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

let playlist_sorting = [`Pos, `Asc]
let playlist_columns : track_attr columns =
  Iarray.append [|`Pos, 20|] tracks_columns

let album_sorting = [`DiscTrack, `Asc]
let album_columns : track_attr columns =
[|
  `Cover, 30;
  `DiscTrack, 20;
  `Rating, 30;
  `Artist, 150;
  `Title, 180;
  `Length, 30;
  `AlbumArtist, 100;
  `AlbumTitle, 150;
  `Date, 60;
  `Country, 50;
  `Label, 50;
  `Codec, 30;
  `Rate, 50;
  `FileSize, 50;
  `FileTime, 70;
  `FilePath, 400;
|]

let make_view shown columns sorting : _ view =
  { shown; columns; sorting }

let copy_view (view : _ view) =
  { view with shown = view.shown }

let artists_default_view = make_view None artists_columns artists_sorting
let albums_default_view = make_view None albums_columns albums_sorting
let tracks_default_view = make_view (Some `Table) tracks_columns tracks_sorting

let make_views () : views =
  {
    search = "";
    query = None;
    folded = true;
    custom = false;
    divider_width = 100;
    divider_height = 100;
    artists = copy_view artists_default_view;
    albums = copy_view albums_default_view;
    tracks = copy_view tracks_default_view;
  }

let root_default_views =
  { (make_views ()) with
    folded = false;
    artists = make_view (Some `Table) artists_columns artists_sorting;
    albums = make_view (Some `Table) albums_columns albums_sorting }

let dir_default_views = make_views ()

let album_default_views =
  { (make_views ()) with
    tracks = make_view (Some `Table) album_columns album_sorting }

let playlist_default_views =
  { (make_views ()) with
    tracks = make_view (Some `Table) playlist_columns playlist_sorting }

let copy_views (views : views) =
  {
    search = "";
    query = None;
    folded = true;
    custom = false;
    divider_width = views.divider_width;
    divider_height = views.divider_height;
    artists = copy_view views.artists;
    albums = copy_view views.albums;
    tracks = copy_view views.tracks;
  }

let make_views_for lib path : views =
  if path = "" then
    root_default_views
  else if is_playlist_path path || is_viewlist_path path then
    copy_views lib.views_playlist_default
  else if is_album_path path then
    copy_views lib.views_album_default
  else
    copy_views lib.views_dir_default


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
    try_refresh scan.tracks_refresh scan.tracks_busy ||
    try_refresh scan.albums_refresh scan.albums_busy ||
    try_refresh scan.artists_refresh scan.artists_busy
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
      changed = Atomic.make false;
    }
  in
  ignore (Domain.spawn (scanner scan scan.dir_queue scan.dir_busy));
  ignore (Domain.spawn (scanner scan scan.file_queue scan.file_busy));
  ignore (Domain.spawn (scanner scan scan.cover_queue scan.cover_busy));
  ignore (Domain.spawn (refresher scan));
  scan

type save = {mutable it : 'a. 'a t -> unit}
let save_db_fwd = {it = ignore}
let rec saver lib () =
  Unix.sleepf 30.0;
  save_db_fwd.it lib;
  saver lib ()

let make () =
  let root = Data.make_dir "" None (-1) root_default_views in
  root.name <- "All";
  let lib =
    {
      root;
      current = None;
      error = "";
      error_time = 0.0;
      refresh_time = infinity;
      covers_shown = true;
      renaming = None;
      log = None;
      search = Edit.make 100;
      rename = Edit.make 100;
      browser = Table.make 0;
      artists = Table.make 0;
      albums = Table.make 0;
      tracks = Table.make 100;
      covers = Atomic.make Map.empty;
      age_covers = Seq.empty;
      views_dir_default = dir_default_views;
      views_album_default = album_default_views;
      views_playlist_default = playlist_default_views;
      scan = make_scan ();
    }
  in
  ignore (Domain.spawn (saver lib));
  lib


(* Error Messages *)

let error lib msg =
  lib.error <- msg;
  lib.error_time <- Unix.gettimeofday ()


(* Attributes *)

let attr_prop = function
  | `Playlist -> "Playlist", `Left
  | `Pos -> "#   ", `Right
  | `FileExists -> "File Exists", `Left
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
and find_dir' path (dir : dir) = Iarray.find_map (find_dir'' path) dir.children
and find_dir'' path (dir : dir) =
  if String.starts_with path ~prefix: dir.path then
    if String.length path = String.length dir.path then
      Some dir
    else
      find_dir' path dir
  else
    None

let find_entry_dir lib (dir : _ Data.dir) =
  Array.find_index ((==) dir) lib.browser.entries

let find_parent lib (dir : _ Data.dir) =
  match dir.parent with
  | None -> None
  | Some "" -> Some lib.root
  | Some path -> find_dir lib path

let find_parent_pos lib (dir : _ Data.dir) =
  let parent = Option.get (find_parent lib dir) in
  let siblings = parent.children in
  let rec loop i = if siblings.$(i) == dir then i else loop (i + 1) in
  loop 0

let rec find_track lib path = find_track' path lib.root
and find_track' path (dir : dir) =
  Iarray.find_map (find_track'' path) dir.children
and find_track'' path (dir : dir) =
  if String.starts_with path ~prefix: dir.path then
    if File.(dir path // "") = dir.path then
      Iarray.find_opt (fun (track : track) -> track.path = path) dir.tracks
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
let browser_name = "browser.conf"
let browser_dir = "browser"
let view_ext = ".conf"

let save_db lib =
  if Atomic.exchange lib.scan.changed false then
  (
    Storage.save_string library_name (fun () ->
      Bin.encode (Data.Encode.dir ()) lib.root
    )
  )

let load_db lib =
  Storage.load_string library_name (fun s ->
    lib.root <- Bin.decode (Data.Decode.dir (make_views_for lib)) s;
  )

let _ = save_db_fwd.it <- save_db


module Print =
struct
  open Text.Print

  let attr_enum =
  [
    "PLL", `Playlist;
    "POS", `Pos;
    "EXS", `FileExists;
    "PTH", `FilePath;
    "DIR", `FileDir;
    "NAM", `FileName;
    "EXT", `FileExt;
    "SIZ", `FileSize;
    "TIM", `FileTime;
    "COD", `Codec;
    "CHA", `Channels;
    "DEP", `Depth;
    "KHZ", `SampleRate;
    "BPS", `BitRate;
    "RES", `Rate;
    "ART", `Artist;
    "TIT", `Title;
    "LEN", `Length;
    "RAT", `Rating;
    "ALA", `AlbumArtist;
    "ALB", `AlbumTitle;
    "TRK", `Track;
    "TRS", `Tracks;
    "DSC", `Disc;
    "DSS", `Discs;
    "DTR", `DiscTrack;
    "ALS", `Albums;
    "DAT", `Date;
    "YER", `Year;
    "LAB", `Label;
    "CTY", `Country;
    "COV", `Cover;
    "TRU", `True;
    "FLS", `False;
    "NOW", `Now;
    "RND", `Random;
  ]

  let any_attr = enum attr_enum
  let artist_attr (x : artist_attr) = any_attr (x :> any_attr)
  let album_attr (x : album_attr) = any_attr (x :> any_attr)
  let track_attr (x : track_attr) = any_attr (x :> any_attr)

  let display_enum = ["table", `Table; "grid", `Grid]
  let display = enum display_enum

  let order_enum = ["asc", `Asc; "desc", `Desc]
  let order = enum order_enum

  let sorting attr = list (pair attr order)
  let columns attr = iarray (pair attr nat)

  let view attr =
    record (fun (x : 'a view) -> [
      "shown", option display x.shown;
      "columns", columns attr x.columns;
      "sort", sorting attr x.sorting;
    ])

  let views full =
    record (fun (x : views) -> [
      "search", string x.search;
      (* query omitted and reconstructed from search *)
      "fold", bool x.folded;
      "custom", bool x.custom;
    ] @ if not (x.custom || full) then [] else [
      "div_w", int x.divider_width;
      "div_h", int x.divider_height;
      "artists", view artist_attr x.artists;
      "albums", view album_attr x.albums;
      "tracks", view track_attr x.tracks;
    ])

  let dir (x : dir) = views false x.view

  let browse (x : dir) =
    let rec tree (dir : dir) =
      (dir.path, dir.view.folded) ::
      List.concat_map tree (Iarray.to_list dir.children)
    in map bool (tree x)
end

module Parse =
struct
  open Text.Parse

  let any_attr = enum Print.attr_enum

  let artist_attr u =
    match any_attr u with
    | #artist_attr as x -> x
    | _ -> raise Text.Type_error

  let album_attr u =
    match any_attr u with
    | #album_attr as x -> x
    | _ -> raise Text.Type_error

  let track_attr u =
    match any_attr u with
    | #track_attr as x -> x
    | _ -> raise Text.Type_error

  let display = enum Print.display_enum
  let order = enum Print.order_enum

  let sorting attr = list (pair attr order)
  let columns attr = iarray (pair attr nat)

  let view attr : t -> 'a view =
    record (fun r -> {
      shown = option display (r $ "shown");
      columns = columns attr (r $ "columns");
      sorting = sorting attr (r $ "sort");
    })

  let views : t -> views =
    record (fun r -> {
      search = default "" string (r $? "search");
      query = None;
      folded = bool (r $ "fold");
      custom = default true bool (r $? "custom");
      divider_width = int (r $ "div_w");
      divider_height = int (r $ "div_h");
      artists = view artist_attr (r $ "artists");
      albums = view album_attr (r $ "albums");
      tracks = view track_attr (r $ "tracks");
    })

  let views_noncustom (view : views) =
    record (fun r ->
      apply (r $? "search") string
        (fun s -> view.search <- s; view.query <- None);
      apply (r $? "fold") bool (fun b -> view.folded <- b);
      apply (r $? "custom") bool (fun b -> view.custom <- b);
    )

  let dir u (dir : dir) =
    views_noncustom dir.view u;
    if dir.view.custom then dir.view <- views u

  let browse u =
    let map = Map.of_list (map bool u) in
    let rec tree (dir : dir) =
      Option.iter (fun b -> dir.view.folded <- b) (Map.find_opt dir.path map);
      Iarray.iter tree dir.children
    in tree
end

let sep_re = Str.(regexp (quote File.sep))
let colon_re = Str.regexp ":"
let percent_re = Str.regexp "%"

let dir_name dir =
  Storage.make_dir (Storage.path browser_dir);
  let file =
    if dir.path = "" then "%2f" else
    dir.path
      |> Str.global_replace percent_re "%%"
      |> Str.global_replace colon_re "%3a"
      |> Str.global_replace sep_re "%2f"
  in File.(browser_dir // file ^ view_ext)

let save_dir _lib dir =
  Storage.save_string (dir_name dir) (fun () ->
    Text.print (Print.dir dir)
  );
  if is_viewlist dir then File.save_safe `Bin dir.path dir.view.search

let load_dir lib dir =
  dir.view <- make_views_for lib dir.path;
  Storage.load_string_opt (dir_name dir) (fun s ->
    try
      Parse.dir (Text.parse s) dir
    with Text.Syntax_error _ | Text.Type_error as exn ->
      Storage.log_exn "parse" exn ("while loading view state for " ^ dir.path)
  );
  if is_viewlist dir then dir.view.search <- File.load `Bin dir.path

let delete_dir _lib dir =
  Storage.delete (dir_name dir)


let save_browser lib =
  Storage.save_string browser_name (fun () ->
    Text.print (Print.browse lib.root)
  )

let load_browser lib =
  Storage.load_string_opt browser_name (fun s ->
    try
      Parse.browse (Text.parse s) lib.root
    with Text.Syntax_error _ | Text.Type_error as exn ->
      Storage.log_exn "parse" exn "while loading browser state"
  )


(* Covers *)

let rec update_cover lib f =
  let covers = Atomic.get lib.covers in
  let covers' = f covers in
  if not (Atomic.compare_and_set lib.covers covers covers') then
    update_cover lib f

let rec collect_covers lib win =
  let now = Api.Draw.frame win in
  let rec progress n =
    if n > 0 && Safe_queue.length lib.scan.cover_queue = 0 then
      match Unix.sleepf 0.02; Seq.uncons lib.age_covers with
      | None -> lib.age_covers <- Map.to_seq (Atomic.get lib.covers)
      | Some ((path, cover), tail) ->
        lib.age_covers <- tail;
        (match cover with
        | Cover {last_use; _} when now - last_use > 3600 * 5 (* ~5 min *) ->
          update_cover lib (Map.remove path)
        | _ -> ()
        );
        progress (n - 1)
  in
  progress 20;
  collect_covers_cont lib win;
  false

and collect_covers_cont lib win =
  Safe_queue.add (false, "", fun () -> collect_covers lib win) lib.scan.cover_queue

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
    update_cover lib (Map.add path cover);
    (*Unix.sleepf 0.001;  (* avoid too much load *)*)
    false
  with exn ->
    Storage.log_exn "file" exn ("scanning cover " ^ path);
    false

let rescan_cover lib path =
  update_cover lib (Map.add path ScanCover);
  Safe_queue.add (false, path, fun () -> rescan_cover' lib path)
    lib.scan.cover_queue

let load_cover lib win path =
  if M3u.is_separator path then None else
  match Map.find_opt path (Atomic.get lib.covers) with
  | Some NoCover when Api.Draw.frame win mod 300 = 13 ->
    rescan_cover lib path; None  (* retry after about 5s *)
  | Some (NoCover | ScanCover) -> None
  | Some (Cover cover) -> cover.last_use <- Api.Draw.frame win; Some cover.image
  | Some (ScannedCover pic) ->
    let cover, result =
      match Api.Image.load_from_memory win pic.mime pic.data with
      | exception _ -> NoCover, None
      | image -> Cover {image; last_use = Api.Draw.frame win}, Some image
    in update_cover lib (Map.add path cover); result
  | None ->
    (* Start collector job if this is the first cover loaded. *)
    if Map.is_empty (Atomic.get lib.covers) then
      collect_covers_cont lib win;
    rescan_cover lib path; None

let purge_covers lib =
  Atomic.set lib.covers Map.empty

let activate_covers lib b =
  lib.covers_shown <- b;
  if not b then purge_covers lib


(* Scanning *)

type scan_mode = [`Quick | `Thorough]

let is_very_quick = function
  | `VeryQuick -> true
  | _ -> false

let rescan_track' lib mode (track : track) =
  let file = {track.file with time = track.file.time} in
  let old = {track with file; memo = None} in
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
        let meta = Meta.load track.path in
        track.format <- Some (Format.read track.path);
        track.meta <- Some {meta with cover = None};
        track.status <- `Det;
        Option.iter (fun cover ->
          update_cover lib (Map.add track.path (ScannedCover cover))
        ) meta.cover;
      )
      else if track.status = `Absent then
      (
        track.status <- `Undet;
        update_cover lib (Map.remove track.path);
      )
    );
    let time = Unix.gettimeofday () in
    let memo = track.memo in
    track.memo <- None;
    track.file.age <- time;
    old.file.age <- time;
    let changed = track <> old in
    if changed then
    (
      (* For playlist tracks, update possible copy in library *)
      if track.playlist <> "" then
      (
        Option.iter (fun track' ->
          track'.format <- track.format;
          track'.meta <- track.meta;
          track'.status <- track.status;
          track'.memo <- track.memo;
        ) (find_track lib track.path)
      );
      Atomic.set lib.scan.changed true;
    )
    else
    (
      track.memo <- memo;
    );
    changed
  with exn ->
    Storage.log_exn "file" exn ("scanning track " ^ track.path);
    false

let rescan_dir_tracks' lib mode (dir : dir) =
  try
    let new_tracks = Dynarray.create () in
    let old_tracks =
      Iarray.fold_left (fun map (track : track) ->
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
    dir.tracks <- dynarray_to_iarray new_tracks;
    let changed =
      !updates > 0 || Iarray.length dir.tracks <> Map.cardinal old_tracks in
    if changed then Atomic.set lib.scan.changed true;
    changed
  with exn ->
    Storage.log_exn "file" exn ("scanning tracks in directory " ^ dir.path);
    true

let rescan_playlist' lib _mode (dir : dir) =
  try
    let items = Iarray.of_list (M3u.load dir.path) in
    dir.tracks <-
      Iarray.mapi (fun pos item ->
        {(find_item lib item) with pos; playlist = dir.path}
      ) items;
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
      let filter (track : track) = false, false, (track.pos = -1) in
      let _, _, tracks = Query.exec query filter lib.root in
      dir.error <- "";
      dir.tracks <-
        Iarray.init (Array.length tracks) (fun i ->
          {tracks.(i) with pos = i; playlist = dir.path}
        );
    );
    true
  with
  | Sys_error _ -> true
  | exn -> Storage.log_exn "file" exn ("scanning viewlist " ^ dir.path); true


let rec rescan_dir' lib mode (origin : dir) =
  let rec scan_file (parent : dir) old_dirs file =
    let parent_path = parent.path in
    let path = File.(parent_path // file) in
    let subdir path' change_if_new =
      match Map.find_opt path' old_dirs with
      | Some dir -> dir
      | None ->
        if change_if_new then Atomic.set lib.scan.changed true;
        Data.make_dir path' (Some parent_path) (parent.nest + 1)
          (make_views_for lib path')
    in
    if File.is_dir path then
      let dir = subdir File.(path // "") false in
      if is_very_quick mode then
        if dir.children = [||] && dir.tracks = [||] then None else Some [dir]
      else
        scan_dir dir
    else if Data.is_playlist_path path then
      scan_playlist (subdir path true)
    else if Data.is_viewlist_path path then
      scan_viewlist (subdir path true)
    else if Data.is_track_path path then
      Some []  (* deferred to directory scan *)
    else
      None

  and scan_dir (dir : dir) =
    Domain.cpu_relax ();
    let old_dirs =
      Iarray.fold_left (fun map (dir : dir) ->
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
    | None ->
      if dir.children <> [||] then Atomic.set lib.scan.changed true;
      None
    | Some dirs ->
      if Data.is_track_path dir.name then
      (
        Atomic.set lib.scan.changed true;
        dir.name <- File.remove_extension dir.name;
      );
      let children' = Array.of_list dirs in
      Array.stable_sort Data.compare_dir children';
      (* Post-process to preserve possible custom order among old dirs. *)
      let new_dirs =
        Array.fold_left (fun (i, map) (dir : dir) ->
          i + 1, Map.add dir.path i map
        ) (0, Map.empty) children' |> snd |> ref
      in
      let i = ref 0 in
      let j = ref 0 in
      let changed = ref (Array.length children' <> Iarray.length dir.children) in
      while !i < Iarray.length dir.children do
        let dir = dir.children.$(!i) in
        match Map.find_opt dir.path !new_dirs with
        | None -> absent_dir dir; changed := true; incr i  (* removed dir *)
        | Some k when k = !j -> incr i; incr j  (* unchanged dir *)
        | Some k when Map.mem children'.(!j).path old_dirs ->
          (* Manually reordered dir; pull forward *)
          assert (k > !j);
          let pull = children'.(k) in
          for p = k - 1 downto !j do
            children'.(p + 1) <- children'.(p);
            new_dirs := Map.add children'.(p).path (p + 1) !new_dirs
          done;
          children'.(!j) <- pull;
          incr i; incr j
        | Some _ -> changed := true; incr j  (* added dir *)
      done;
      if !changed then Atomic.set lib.scan.changed true;
      dir.children <- Iarray.of_array children';
      rescan_dir_tracks lib mode dir;
      Some [dir]

  and scan_playlist (dir : dir) =
    if is_playlist_path dir.name then dir.name <- File.remove_extension dir.name;
    if not (is_very_quick mode) then rescan_playlist lib mode dir;
    Some [dir]

  and scan_viewlist (dir : dir) =
    if is_viewlist_path dir.name then dir.name <- File.remove_extension dir.name;
    if not (is_very_quick mode) then rescan_viewlist lib mode dir;
    Some [dir]

  and absent_dir (dir : dir) =
    Data.iter_dir (fun (dir : dir) ->
      if Data.is_dir dir then
        Iarray.iter (fun (track : track) -> track.status <- `Absent) dir.tracks
    ) dir
  in

  try
    if File.exists_dir origin.path then
      ignore (scan_dir origin)
    else
      absent_dir origin;
    true
  with exn ->
    Storage.log_exn "file" exn ("scanning directory " ^ origin.path);
    true


and rescan_dir lib mode (dir : dir) =
  Safe_queue.add (false, dir.path, fun () ->
    rescan_dir' lib (mode :> [scan_mode | `VeryQuick]) dir) lib.scan.dir_queue

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
let rescan_root lib mode = rescan_dirs lib mode (Iarray.to_array lib.root.children)
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


let set_dir_opt lib dir_opt =
  Option.iter (fun (dir : dir) ->
    (* In case some input has been made but not yet sent *)
    if lib.search.text <> dir.view.search then
    (
      dir.view.search <- lib.search.text;
      save_dir lib dir;
    )
  ) lib.current;
  Table.deselect_all lib.browser;
  Table.clear_undo lib.tracks;
  lib.current <- dir_opt;
  Option.iter (fun (dir : dir) ->
    load_dir lib dir;
    Edit.set lib.search dir.view.search;
    Edit.scroll lib.search 0;  (* reset *)
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
    if Data.is_playlist dir then
      rescan_playlist lib `Thorough dir
    else if Data.is_viewlist dir then
      rescan_viewlist lib `Thorough dir
  )


let refresh_browser lib =
  let rec entries (dir : dir) acc =
    Option.iter (fun (cur : dir) ->
      if cur.path = dir.path then lib.current <- Some dir) lib.current;
    dir ::
    (if dir.view.folded then acc else Iarray.fold_right entries dir.children acc)
  in
  let selection = Table.save_selection lib.browser in
  Table.set lib.browser (Array.of_list (entries lib.root []));
  Table.restore_selection lib.browser selection (fun dir -> dir.path)


let insert_dir lib path =
  match find_dir lib File.(dir path // "") with
  | None ->
    error lib "Cannot find directory in library";
    None
  | Some parent ->
    ignore (rescan_dir' lib `VeryQuick parent);
    refresh_browser lib;
    Iarray.find_opt (fun (dir : dir) -> dir.path = path) parent.children

let remove_dir lib path =
  match find_dir lib File.(dir path // "") with
  | None ->
    error lib "Cannot find directory in library anymore";
    false
  | Some parent ->
    ignore (rescan_dir' lib `VeryQuick parent);
    refresh_browser lib;
    not (Iarray.exists (fun (dir : dir) -> dir.path = path) parent.children)

let move_dir lib dir pos pos' =
  if pos <> pos' then
  (
    let children = dir.children in
    assert (pos < Iarray.length children);
    assert (pos' < Iarray.length children);
    let lo, hi = min pos pos', max pos pos' in
    dir.children <-  (* atomic update! *)
      Iarray.init (Iarray.length children) (fun i ->
        if i < lo || i > hi then
          children.$(i)
        else if i = pos' then
          children.$(pos)
        else if pos < pos' then
          children.$(i + 1)
        else
          children.$(i - 1)
      );
    Atomic.set lib.scan.changed true;
    refresh_browser lib
  )

let reverse_dir lib dir =
  let n = Iarray.length dir.children in
  dir.children <- Iarray.init n (fun i -> dir.children.$(n - i - 1));
  save_dir lib dir;
  refresh_browser lib


let current_is p lib =
  match lib.current with
  | None -> false
  | Some dir -> p dir

let current_is_all lib = current_is Data.is_all lib
let current_is_root lib = current_is Data.is_root lib
let current_is_dir lib = current_is Data.is_dir lib
let current_is_album lib = current_is Data.is_album lib
let current_is_playlist lib = current_is Data.is_playlist lib
let current_is_viewlist lib = current_is Data.is_viewlist lib

let current_is_shown_playlist lib =
  current_is (fun (dir : dir) ->
    dir.view.tracks.shown <> None && Data.is_playlist dir) lib

let current_is_shown_viewlist lib =
  current_is (fun (dir : dir) ->
    dir.view.tracks.shown <> None && Data.is_viewlist dir) lib

let current_is_plain_playlist lib =
  current_is (fun (dir : dir) ->
    dir.view.tracks.shown <> None && Data.is_playlist dir &&
    dir.view.search = "" &&
    dir.view.tracks.sorting <> [] &&
    List.hd dir.view.tracks.sorting = (`Pos, `Asc)
  ) lib



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


let artists_sorting (view : views) = view.artists.sorting
let albums_sorting (view : views) = view.albums.sorting
let tracks_sorting (view : views) = view.tracks.sorting

let artist_key (artist : artist) = artist.name
let album_key (album : album) = (*album.track*)  (* TODO *)
  match album.meta with
  | None -> "[unknown]"
  | Some meta -> meta.albumtitle

let track_key lib : track -> string =
  if current_is_playlist lib || current_is_viewlist lib then
    fun track -> string_of_int track.pos
  else
    fun track ->
      if track.playlist = "" then track.path
      else string_of_int track.pos ^ "\x00" ^ track.playlist


let refresh_delay = 5.0

let filter_artist lib =
  let artists =
    Array.fold_left (fun s (artist : artist) -> Set.add artist.name s)
      Set.empty (Table.selected lib.artists)
  in fun (track : track) ->
    artists = Set.empty ||
    let artist = Data.track_attr_string track `Artist in
    Set.mem artist artists ||
    let albumartist = Data.track_attr_string track `AlbumArtist in
    Set.mem albumartist artists ||
    List.exists (fun n -> Set.mem n artists) (Meta.artists_of_artist artist) ||
    List.exists (fun n -> Set.mem n artists) (Meta.artists_of_artist albumartist)

let filter_album lib =
  let albums =
    Array.fold_left (fun s (album : album) ->
      Query.AlbumSet.add (Query.album_key album) s
    ) Query.AlbumSet.empty (Table.selected lib.albums)
  in fun (track : track) ->
    albums = Query.AlbumSet.empty ||
    Query.AlbumSet.mem (Query.track_album_key track) albums

let filter lib with_artists with_albums with_tracks with_seps =
  let filter_artist = filter_artist lib in
  let filter_album = filter_album lib in
  fun (track : track) ->
    let is_sep = is_separator track in
    let has_artist = filter_artist track in
    let has_album = filter_album track in
    with_artists && not is_sep,
    with_albums && has_artist && not is_sep,
    with_tracks && has_artist && has_album && (not is_sep || with_seps)

let sort attr_string sorting entries =
  if sorting <> [] then
  Storage.log_time "sort" (fun () ->
    let entries' =
      Array.map (fun entry -> Data.key_entry attr_string sorting entry, entry)
        entries
    in
    Array.stable_sort compare entries';
    Array.iteri (fun i (_, entry) -> entries.(i) <- entry) entries';
  )

let refresh_view lib (tab : _ Table.t) attr_string sorting key f =
  match lib.current with
  | None -> Table.remove_all tab
  | Some dir ->
    let query =
      if dir.error <> "" then
        (error lib dir.error; Query.empty_query)
      else
        Option.value dir.view.query ~default: Query.full_query
    in
    let entries = f dir query (is_playlist dir) in
    sort attr_string (sorting dir.view) entries;
    Mutex.protect tab.mutex (fun () ->
      let selection = Table.save_selection tab in
      Table.set tab entries;
      Table.restore_selection tab selection key
    );
    lib.refresh_time <-
      min lib.refresh_time (Unix.gettimeofday () +. refresh_delay)

let refresh_tracks_view lib f =
  refresh_view lib lib.tracks track_attr_string tracks_sorting (track_key lib) f
let refresh_albums_view lib f =
  refresh_view lib lib.albums album_attr_string albums_sorting album_key f
let refresh_artists_view lib f =
  refresh_view lib lib.artists artist_attr_string artists_sorting artist_key f

let refresh_tracks_sync lib =
  refresh_tracks_view lib
    (fun dir query with_seps ->
      let _, _, tracks =
        Query.exec query (filter lib false false true with_seps) dir in
      tracks
    )

let refresh_albums_tracks_sync lib =
  refresh_tracks_view lib
    (fun dir query with_seps ->
      let _, albums, tracks =
        Query.exec query (filter lib false true true with_seps) dir in
      refresh_albums_view lib (fun _dir _query _with_seps -> albums);
      tracks
    )

let refresh_artists_albums_sync lib =
  refresh_albums_view lib
    (fun dir query with_seps ->
      let artists, albums, _ =
        Query.exec query (filter lib true true false with_seps) dir in
      refresh_artists_view lib (fun _dir _query _with_seps -> artists);
      albums
    )

let refresh_artists_albums_tracks_sync lib =
  refresh_tracks_view lib
    (fun dir query with_seps ->
      let artists, albums, tracks =
        Query.exec query (filter lib true true true with_seps) dir in
      refresh_artists_view lib (fun _dir _query _with_seps -> artists);
      refresh_albums_view lib (fun _dir _query _with_seps -> albums);
      tracks
    )


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
  if
    updated <> [] ||
    (current_is_playlist lib || current_is_viewlist lib)
      && Table.length lib.tracks = 0
  then
    lib.refresh_time <- min lib.refresh_time (now +. refresh_delay);
  if now > lib.refresh_time then
  (
    lib.refresh_time <- infinity;
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


let set_views_dir_default lib v = lib.views_dir_default <- copy_views v
let set_views_album_default lib v = lib.views_album_default <- copy_views v
let set_views_playlist_default lib v = lib.views_playlist_default <- copy_views v

let current_to_default_views lib =
  Option.iter (fun (dir : dir) ->
    (if is_playlist_path dir.path || is_viewlist_path dir.path then
      set_views_playlist_default
    else if is_album_path dir.path then
      set_views_album_default
    else
      set_views_dir_default
    ) lib dir.view;
    dir.view.custom <- false;
    save_dir lib dir;
  ) lib.current

let current_of_default_views lib =
  Option.iter (fun (dir : dir) ->
    let view = make_views_for lib dir.path in
    view.search <- dir.view.search;
    view.query <- dir.view.query;
    view.folded <- dir.view.folded;
    dir.view <- view;
    save_dir lib dir;
  ) lib.current


(* Folding directories *)

let rec fold_dir lib dir fold =
  if fold <> dir.view.folded then
  (
    dir.view.folded <- fold;
    save_dir lib dir;
    save_browser lib;
    refresh_browser lib;
    if lib.current <> None then
    (
      if fold then
      (
        (* If current dir was a sub directory, then deselect parent. *)
        Option.iter (fun (current : dir) ->
          if String.starts_with ~prefix: dir.path current.path then
          (
            Option.iter (select_dir lib)
              (Array.find_index ((==) dir) lib.browser.entries);
            refresh_artists_albums_tracks lib
          )
        ) lib.current;
      )
      else
      (
        Option.iter (fun parent ->
          if parent <> "" then
            fold_dir lib (Option.get (find_dir lib parent)) false
        ) dir.parent;
        (* If current dir was previously folded away, then reselect it. *)
        Option.iter (fun i -> Table.select lib.browser i i)
          (Array.find_index (fun dir -> lib.current = Some dir) lib.browser.entries)
      )
    );
  )


(* Roots *)

let make_root lib path =
  if not (File.exists path) then
    failwith (path ^ " does not exist")
  else if not (File.is_dir path) then
    failwith (path ^ " is not a directory")
  else
  (
    let dirpath = File.(path // "") in
    match
      Iarray.find_opt (fun (dir : dir) ->
        path = dir.path ||
        String.starts_with ~prefix: dirpath dir.path ||
        String.starts_with ~prefix: dir.path dirpath
      ) lib.root.children
    with
    | Some dir ->
      failwith (dirpath ^ " overlaps with " ^ dir.name ^ " (" ^ dir.path ^ ")")
    | None -> Data.make_dir dirpath (Some "") 0 (make_views_for lib dirpath)
  )

let insert_roots lib paths pos =
  let paths = Array.of_list paths in
  lib.error <- "";
  try
    let roots = lib.root.children in
    let roots' = Array.map (make_root lib) paths in
    let len = Iarray.length roots in
    let len' = Array.length roots' in
    lib.root.children <-
      Iarray.init (len + len') (fun i ->
        if i < pos then
          roots.$(i)
        else if i < pos + len' then
          roots'.(i - pos)
        else
          roots.$(i - len')
      );
    Atomic.set lib.scan.changed true;
    Array.iter (fun (dir : dir) -> rescan_dir lib `Thorough dir) roots';
    refresh_browser lib;
    refresh_artists_albums_tracks lib;
    true
  with Failure msg ->
    error lib msg;
    false

let remove_root lib path =
  let dirpath = File.(path // "") in
  let roots = lib.root.children in
  match Iarray.find_index (fun (root : dir) -> root.path = dirpath) roots with
  | None ->
    error lib "Directory is not a root directory";
    false
  | Some pos ->
    Data.iter_dir (delete_dir lib) roots.$(pos);
    lib.current <- None;
    lib.root.children <-
      Iarray.init (Iarray.length roots - 1) (fun i ->
        roots.$(if i < pos then i else i + 1)
      );
    Atomic.set lib.scan.changed true;
    refresh_browser lib;
    refresh_artists_albums_tracks lib;
    true

let remove_roots lib paths =
  List.for_all (remove_root lib) paths


(* Search *)

let set_search lib search =
  Option.iter (fun (dir : dir) ->
    if dir.view.search <> search || dir.view.query = None then
    (
      dir.view.search <- search;
      update_view_query lib dir;
      save_dir lib dir;
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
    File.save_safe `Bin dir.path s;
  with exn ->
    Storage.log_exn "file" exn ("writing playlist " ^ dir.path)

let make_viewlist (dir : dir) =
  let prefix =
    if Data.is_all dir || Data.is_viewlist dir then ""
    else "\"" ^ dir.path ^ "\" @ #filepath "
  in
  prefix ^ dir.view.search


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
  match dir.view.tracks.sorting with
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
    Option.iter (fun (dir : dir) ->
      dir.tracks <- Iarray.of_array lib.tracks.entries;
    ) lib.current;
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
    Option.iter (fun (dir : dir) -> dir.tracks <- [||]) lib.current;
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
    Option.iter (fun (dir : dir) ->
      dir.tracks <- Iarray.of_array lib.tracks.entries;
    ) lib.current;
    restore_playlist lib order;
    save_playlist lib;
    refresh_artists_albums_sync lib;  (* could be slow... *)
  )

let remove_selected lib =
  remove_if (Table.is_selected lib.tracks) lib (Table.num_selected lib.tracks)

let remove_unselected lib =
  remove_if (fun i -> not (Table.is_selected lib.tracks i)) lib
    (Table.length lib.tracks - Table.num_selected lib.tracks)

let num_invalid entries =
  Array.fold_left (fun n track ->
    n + Bool.to_int (Data.is_invalid track)
  ) 0 entries

let remove_invalid lib all =
  remove_if (fun i ->
    (all || is_selected lib i) && Data.is_invalid lib.tracks.entries.(i)
  ) lib (num_invalid (if all then lib.tracks.entries else selected lib))

let remove_duplicates lib all =
  let mems = ref Set.empty in
  let dups = ref IntSet.empty in
  Array.iteri (fun i (track : track) ->
    if all || is_selected lib i then
    (
      if Set.mem track.path !mems then
        dups := IntSet.add i !dups
      else
        mems := Set.add track.path !mems
    )
  ) lib.tracks.entries;
  remove_if (fun i -> IntSet.mem i !dups) lib (IntSet.cardinal !dups)


let replace_all lib tracks =
  if lib.tracks.entries = [||] then
    insert lib 0 tracks
  else
  (
    remove_all lib;
    insert lib 0 tracks;
    Table.drop_undo lib.tracks;
  )

let replace_map lib map all =
  Table.push_undo lib.tracks;
  let entries = lib.tracks.entries in
  Array.iteri (fun i (track : track) ->
    if all || is_selected lib i then
      Option.iter (fun track' ->
        entries.(i) <- {track' with pos = track.pos}
      ) (Map.find_opt track.path map)
  ) entries


let move_selected lib d =
  assert (current_is_playlist lib);
  if Table.num_selected lib.tracks > 0 then
  (
    let order = normalize_playlist lib in
    let _js = Table.move_selected lib.tracks d in
    Array.iteri (fun i (track : track) -> track.pos <- i) lib.tracks.entries;
    restore_playlist lib order;
    save_playlist lib;
  )

let reverse_selected lib =
  assert (current_is_playlist lib);
  if Table.num_selected lib.tracks > 1 then
  (
    let order = normalize_playlist lib in
    let _js = Table.reverse_selected lib.tracks in
    Array.iteri (fun i (track : track) -> track.pos <- i) lib.tracks.entries;
    restore_playlist lib order;
    save_playlist lib;
  )

let reverse_all lib =
  assert (current_is_playlist lib);
  let len = Table.length lib.tracks in
  if len > 1 then
  (
    let order = normalize_playlist lib in
    Table.reverse_all lib.tracks;
    Array.iteri (fun i (track : track) -> track.pos <- i) lib.tracks.entries;
    restore_playlist lib order;
    save_playlist lib;
  )

let reorder_all lib =
  assert (current_is_playlist lib);
  Array.iteri (fun i (track : track) -> track.pos <- i) lib.tracks.entries;
  save_playlist lib


(* Playlist Repair *)

type repair_map = track list Map.t

let key_subst = List.map (fun (re, s) -> Str.regexp re, s)
  [
    "  +", " ";
    "[?!]", "";
    "[´`]", "'";
    "[[{]", "(";
    "[]}]", ")";
    "\\u{2026}", "...";
    "\\(-\\|\u{2012}\\|\u{2013}\\|\u{2014}\\)+", "-";
  ]

let repair_key path =
  Unicode.search_key_utf_8 (File.name path) |>
  List.fold_right (fun (re, s) -> Str.global_replace re s) key_subst

let repair_map lib on_busy =
  let map = ref Map.empty in
  Data.iter_dir (fun dir ->
    on_busy dir;
    if Data.is_dir dir then
    (
      Iarray.iter (fun (track : Data.track) ->
        map := Map.add_to_list (repair_key track.path) track !map
      ) dir.tracks
    )
  ) lib.root;
  !map


(*
let path_distance path1 path2 =
  let rec dist names1 names2 =
    match names1, names2 with
    | name1::names1', name2::names2' when name1 = name2 -> dist names1' names2'
    | _, _ -> List.length names1 + List.length names2
  in dist (File.explode path1) (File.explode path2)
*)

let repair_path (map : repair_map) base_path path =
  let path' = M3u.resolve_path base_path path in
  if M3u.is_separator path
  || File.(exists path' && name (real path') = name path') then `Ok else
  match Map.find_opt (repair_key path') map with
  | None -> `Missing 
  | Some [track] -> `Replace track
  | Some _ -> `Ambiguous
(*
    let score (track : track) =
      let path_score = path_distance item.path track.path in
      let time_item =
        match item.info with Some info -> float info.time | None -> 0.0
      and track_time =
        match track.format with Some format -> format.time | None ->
        match track.meta with Some meta -> meta.length | None -> 0.0
      in
      let time_score = abs (int_of_float (track_time - item_time)) in
      path_score, time_score
    in
    let cmp track1 track2 = compare (score track1) (score track2) in
    let tracks' = List.sort cmp tracks in
    `Success (List.hd tracks').path
*)


(* Undo *)

let unredo lib f list =
  if !list <> [] then
  (
    let order = normalize_playlist lib in
    f lib.tracks;
    Array.iteri (fun i (track : track) -> track.pos <- i) lib.tracks.entries;
    restore_playlist lib order;
    save_playlist lib
  )

let undo lib = unredo lib Table.pop_undo lib.tracks.undos
let redo lib = unredo lib Table.pop_redo lib.tracks.redos


(* Persistence *)

let print_state lib =
  let open Text.Print in
  record (fun lib -> [
    "browser_scroll", int lib.browser.vscroll;
    "browser_current", option string (Option.map (fun dir -> dir.path) lib.current);
    "lib_cover", bool lib.covers_shown;
    "views_dir_default", Print.views true lib.views_dir_default;
    "views_album_default", Print.views true lib.views_album_default;
    "views_playlist_default", Print.views true lib.views_playlist_default;
  ]) lib

let print_intern lib =
  let open Text.Print in
  print_state lib @@@
  record (fun lib -> [
    "browser_pos", option int lib.browser.pos;
    "browser_length", int (Array.length lib.browser.entries);
    "browser_selected", option (pair int string)
      (Option.map (fun i -> i, lib.browser.entries.(i).name)
        (selected_dir lib));
    "tracks_pos", option int lib.tracks.pos;
    "tracks_vscroll", int lib.tracks.vscroll;
    "tracks_hscroll", int lib.tracks.hscroll;
    "tracks_length", int (Array.length lib.tracks.entries);
    "root_length", int (Iarray.length lib.root.children);
    "lib_error", string lib.error;
    "lib_error_time", float lib.error_time;
  ]) lib

let parse_state lib =
  let open Text.Parse in
  record (fun r ->
    refresh_browser lib;
    apply (r $? "browser_scroll") (num 0 (max 0 (length_browser lib - 1)))
      (fun i -> Table.set_vscroll lib.browser i 1 4);
    apply (r $? "views_dir_default") Parse.views
      (fun v -> lib.views_dir_default <- v);
    apply (r $? "views_album_default") Parse.views
      (fun v -> lib.views_album_default <- v);
    apply (r $? "views_playlist_default") Parse.views
      (fun v -> lib.views_playlist_default <- v);
    apply (r $? "browser_current") string
      (fun i ->
        lib.current <- None;
        Option.iter (select_dir lib)
          (Array.find_index (fun (dir : dir) -> dir.path = i)
            lib.browser.entries);
      );
    apply (r $? "lib_cover") bool
      (fun b -> lib.covers_shown <- b);
    refresh_artists_albums_tracks lib;
  )
