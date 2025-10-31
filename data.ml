(* Library Data Representation *)

open Audio_file

type path = string
type time = float
type date = float
type blob = string
type id = int64


(* View Attributes *)

type file_attr =
[
  | `FileExists | `FilePath | `FileDir | `FileName | `FileExt | `FileSize | `FileTime
]

type format_attr =
[
  | `Length | `Codec | `Channels | `Depth | `SampleRate | `BitRate | `Rate
]

type meta_attr =
[
  | `Artist | `Title | `Track | `Tracks | `Disc | `Discs | `DiscTrack | `Cover
  | `AlbumArtist | `AlbumTitle | `Date | `Year | `Country | `Label | `Rating
]

type artist_attr = [ `Artist | `Albums | `Tracks ]
type album_attr = [ file_attr | format_attr | meta_attr ]
type track_attr = [ file_attr | format_attr | meta_attr | `Pos ]
type query_attr = [ track_attr | `True | `False | `Now | `Random ]
type any_attr = [ artist_attr | album_attr | track_attr | query_attr | `None ]

type order = [`Asc | `Desc]
type 'attr sorting = ('attr * order) list
type 'attr columns = ('attr * int) iarray


let file_attrs =
  [ `FileExists; `FilePath; `FileDir; `FileName; `FileExt; `FileSize; `FileTime ]
let format_attrs =
  [ `Length; `Codec; `Channels; `Depth; `SampleRate; `BitRate; `Rate ]
let meta_attrs =
  [ `Artist; `Title; `Track; `Tracks; `Disc; `Discs; `DiscTrack; `Cover;
    `AlbumArtist; `AlbumTitle; `Date; `Year; `Country; `Label; `Rating ]

let artist_attrs = [ `Artist; `Albums; `Tracks ]
let album_attrs = file_attrs @ format_attrs @ meta_attrs
let track_attrs = file_attrs @ format_attrs @ meta_attrs @ [ `Pos ]

(* Can't use a set since the key cannot be polymorphic. *)
module AttrMap = Map.Make (struct type t = any_attr let compare = compare end)

let diff _ x y = if x <> None && y = None then x else None
let diff_attrs all_attrs used_attrs =
  let all_attrs' = List.map (fun a -> (a :> any_attr), a) all_attrs in
  let used_attrs' = List.map (fun a -> (a :> any_attr), a) used_attrs in
  List.map snd
    AttrMap.(to_list (merge diff (of_list all_attrs') (of_list used_attrs')))


(* Data *)

type file =
{
  mutable size : int;
  mutable time : date;
  mutable age : date;
}

type artist =
{
  name : string;  (* primary *)
  mutable albums : int;
  mutable tracks : int;
}

type memo =
{
  mutable file_size : string;
  mutable file_time : string;
  mutable artist : string;
  mutable title : string;
  mutable album_artist : string;
  mutable album_title : string;
  mutable length : string;
  mutable channels : string;
  mutable depth : string;
  mutable sample_rate : string;
  mutable bit_rate : string;
  mutable rate : string;
  mutable track : string;
  mutable tracks : string;
  mutable disc : string;
  mutable discs : string;
  mutable disc_track : string;
  mutable date : string;
  mutable year : string;
  mutable rating : string;
}

type album =
{
  path : path;  (* primary *)
  mutable file : file;
  mutable format : Format.t option;
  mutable meta : Meta.t option;
  mutable memo : memo option;
}

type track =
{
  path : path;  (* primary *)
  mutable file : file;
  mutable format : Format.t option;
  mutable meta : Meta.t option;
  mutable album : album option;
  mutable pos : int;  (* 0-based *)
  mutable status : [`Undet | `Predet | `Det | `Invalid | `Absent];
  mutable memo : memo option;
}

type 'view dir =
{
  path : path;  (* primary *)
  parent : path option;
  nest : int;
  mutable name : string;
  mutable children : 'view dir iarray;
  mutable tracks : track iarray;
  mutable error : string;  (* for view lists *)
  mutable view : 'view;
}


(* Properties *)

let is_known_view_ext path =
  String.lowercase_ascii (Filename.extension path) = ".m3v"

let parent_path path = File.(dir path // "")
let is_dir_path path = String.ends_with ~suffix: File.sep path
let is_playlist_path path = M3u.is_known_ext path
let is_viewlist_path path = is_known_view_ext path
let is_album_path path = Format.is_known_ext path (* assume album directory *)
let is_track_path path = Format.is_known_ext path || M3u.is_separator path

let is_all dir = dir.path = ""
let is_root dir = dir.parent = Some ""
let is_dir (dir : _ dir) = dir.path = "" || is_dir_path dir.path
let is_playlist (dir : _ dir) = is_playlist_path dir.path
let is_viewlist (dir : _ dir) = is_viewlist_path dir.path


let is_separator (track : track) = M3u.is_separator track.path

let is_invalid track =
  match track.status with
  | `Invalid | `Absent -> true
  | `Det | `Predet | `Undet -> false


let year_of_date t =
  let tm = File.local_time t in
  tm.tm_year + 1900

let date_of_year y =
  let tm =
    Unix.{
      tm_year = y - 1900;
      tm_mon = 0;
      tm_mday = 1;
      tm_hour = 0;
      tm_min = 0;
      tm_sec = 0;
      tm_yday = 0;
      tm_wday = 0;
      tm_isdst = false;
    }
  in
  File.make_time tm


(* Constructors *)

let make_dir path parent nest view : 'a dir =
  {
    path;
    parent;
    name = if path = "" then "" else File.name path;
    nest;
    children = [||];
    tracks = [||];
    error = "";
    view;
  }

let make_file () : file =
  {
    size = 0;
    time = 0.0;
    age = 0.0;
  }

let make_artist name : artist =
  {
    name;
    albums = 0;
    tracks = 0;
  }

let make_album path : album =
  {
    path;
    file = make_file ();
    format = None;
    meta = None;
    memo = None;
  }

let make_track path : track =
  {
    path;
    file = make_file ();
    format = None;
    meta = None;
    album = None;
    pos = -1;
    status = `Undet;
    memo = None;
  }

let make_memo () : memo =
  {
    file_size = "";
    file_time = "";
    artist = "";
    title = "";
    album_artist = "";
    album_title = "";
    length = "";
    channels = "";
    depth = "";
    sample_rate = "";
    bit_rate = "";
    rate = "";
    track = "";
    tracks = "";
    disc = "";
    discs = "";
    disc_track = "";
    date = "";
    year = "";
    rating = "";
  }

let make_separator () : track =
  let track = make_track M3u.separator in
  track.status <- `Det;
  track


let permute perm a =
  Iarray.init (Iarray.length a) (fun i -> Iarray.get a (Iarray.get perm i))


(* String Conversion *)

let fmt = Printf.sprintf

let string_of_time t =
  let t' = int_of_float (Float.trunc t) in
  if t < 3600.0 then
    fmt "%d:%02d" (t' / 60) (t' mod 60)
  else if t < 86400.0 then
    fmt "%d:%02d:%02d" (t' / 60 / 60) (t' / 60 mod 60) (t' mod  60)
  else
    fmt "%dd %02d:%02d:%02d"
      (t' / 60 / 60 / 24) (t' / 60 / 60 mod 24) (t' / 60 mod 60) (t' mod 60)

let string_of_date t =
  let tm = File.local_time t in
  fmt "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

let string_of_date_time t =
  let tm = File.local_time t in
  fmt "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

(*
let name_separator = String.make 80 '-'
*)
let name_separator = String.concat "" (List.init 80 (Fun.const "·"))

let fields_of_name name =
  let len = String.length name in
  let rec find i j =
    if i >= len then [] else
    if j >= len then [String.sub name i (len - i)] else
    match String.index_from_opt name j '-' with
    | Some j when j < len - 1 ->
      if name.[j - 1] <> ' ' || name.[j + 1] <> ' ' then
        find i (j + 2)
      else
        String.sub name i (j - i - 1) :: find (j + 2) (j + 4)
    | _ -> [String.sub name i (len - i)]
  in find 0 2

let fields_of_path path =
  if M3u.is_separator path then
    [name_separator; name_separator]
  else
    fields_of_name File.(remove_extension (name path))

let int_of_pos s =
  match String.index_opt s '.' with
  | None -> int_of_string_opt s
  | Some i -> int_of_string_opt (String.sub s (i + 1) (String.length s - i - 1))

let artist_title = function
  | artist :: title :: rest -> Some (artist, String.concat " - " (title::rest))
  | title :: [] -> Some ("[unknown]", title)
  | [] -> None

let pos_artist_title = function
  | [] -> None
  | ([_] | [_; _]) as fields ->
    Option.map (fun (artist, title) -> -1, artist, title) (artist_title fields)
  | pos :: rest ->
    match int_of_pos pos, artist_title rest with
    | Some pos, Some (artist, title) -> Some (pos - 1, artist, title)
    | _, _ ->
      Option.map (fun (artist, title) -> -1, artist, title) (artist_title rest)


let nonzero zero f x = if x = zero then "" else f x
let nonzero_int w x = nonzero 0 (fmt "%*d" w) x (* leading spaces for sorting *)
let nonempty f x attr = match x with None -> "" | Some x -> f x attr
let unknown f x attr = match nonempty f x attr with "" -> "[unknown]" | s -> s

let file_attr_string path (file : file) = function
  | `FileExists -> string_of_bool (M3u.is_separator path || File.exists path)
  | `FilePath -> path
  | `FileDir -> File.dir path
  | `FileName -> File.name path
  | `FileExt -> File.extension path
  | `FileSize -> nonzero 0.0 (fmt "%3.1f MB") (float file.size /. 2.0 ** 20.0)
  | `FileTime -> nonzero 0.0 string_of_date_time file.time

let rec format_attr_string (format : Format.t) = function
  | `Length -> nonzero 0.0 string_of_time format.time
  | `Codec -> format.codec
  | `Channels -> nonzero_int 2 format.channels
  | `Depth ->
    let depth = format.bitrate /. float format.rate /. float format.channels in
    let fmts : _ format =
      if float format.depth = Float.round depth then "%.0f" else "%.1f"
    in nonzero 0.0 (fmt fmts) depth
  | `SampleRate -> nonzero 0.0 (fmt "%3.1f KHz") (float format.rate /. 1000.0)
  | `BitRate -> nonzero 0.0 (fmt "%4.0f kbps") (format.bitrate /. 1000.0)
  | `Rate ->
    let attr =
      match format.codec with
      | "MP3" | "OGG" | "OPUS" -> `BitRate
      | _ -> `SampleRate
    in format_attr_string format attr

let rec meta_attr_string (meta : Meta.t) = function
  | `Artist -> meta.artist
  | `Title -> meta.title
  | `AlbumArtist -> meta.albumartist
  | `AlbumTitle -> meta.albumtitle
  | `Track -> nonzero_int 3 meta.track
  | `Tracks -> nonzero_int 3 meta.tracks
  | `Disc -> nonzero_int 2 meta.disc
  | `Discs -> nonzero_int 2 meta.discs
  | `DiscTrack ->
    if meta.disc = 0 then meta_attr_string meta `Track else
    meta_attr_string meta `Disc ^ "." ^ fmt "%02d" meta.track
  | `Date ->
    if meta.date_txt = "" then nonzero_int 4 meta.year else meta.date_txt
  | `Year -> nonzero_int 4 meta.year
  | `Label -> meta.label
  | `Country -> meta.country
  | `Length -> nonzero 0.0 string_of_time meta.length
  | `Rating ->
    let star = "*" in  (* "★" *)
    let len = String.length star in
    String.init (meta.rating * len) (fun i -> star.[i mod len])
  | `Cover -> nonempty (fun (pic : Meta.picture) () -> pic.data) meta.cover ()

let artist_attr_string' attr path meta =
  let s = nonempty meta_attr_string meta attr in
  if s <> "" then s else
  match pos_artist_title (fields_of_path path) with
  | Some (_, artist, _) -> artist
  | None -> "[unknown]"

let title_attr_string' attr path meta =
  let s = nonempty meta_attr_string meta attr in
  if s <> "" then s else
  match pos_artist_title (fields_of_path path) with
  | Some (_, _, title) -> title
  | None -> "[unknown]"

let length_attr_string' format meta =
  let s = nonempty format_attr_string format `Length in
  if s <> "" then s else nonempty meta_attr_string meta `Length

let artist_attr_string (artist : artist) = function
  | `Artist -> artist.name
  | `Tracks -> fmt "%4d" artist.tracks
  | `Albums -> fmt "%3d" artist.albums

let album_attr_string' (album : album) = function
  | `AlbumArtist -> unknown meta_attr_string album.meta `AlbumArtist
  | `AlbumTitle -> unknown meta_attr_string album.meta `AlbumTitle
  | `Length -> length_attr_string' album.format album.meta
  | #file_attr as attr -> file_attr_string album.path album.file attr
  | #format_attr as attr -> nonempty format_attr_string album.format attr
  | #meta_attr as attr -> nonempty meta_attr_string album.meta attr

let track_attr_string' (track : track) = function
  | `Pos -> nonzero_int 3 (track.pos + 1)
  | `Artist -> artist_attr_string' `Artist track.path track.meta
  | `Title -> title_attr_string' `Title track.path track.meta
  | `Length -> length_attr_string' track.format track.meta
  | `AlbumArtist when not (M3u.is_separator track.path) ->
    unknown meta_attr_string track.meta `AlbumArtist
  | `AlbumTitle when not (M3u.is_separator track.path) ->
    unknown meta_attr_string track.meta `AlbumTitle
  | #file_attr as attr -> file_attr_string track.path track.file attr
  | #format_attr as attr -> nonempty format_attr_string track.format attr
  | #meta_attr as attr -> nonempty meta_attr_string track.meta attr

let attr_string' get_memo set_memo f x attr =
  match (attr :> track_attr) with
  | `FileExists | `FilePath | `FileDir | `FileName | `FileExt
  | `Codec | `Label | `Country | `Cover | `Pos ->
    f x attr
  | _ ->
    let memo =
      match get_memo x with
      | Some memo -> memo
      | None -> let memo = make_memo () in set_memo x (Some memo); memo
    in
    match attr with
    | `FileSize ->
      if memo.file_size <> "" then memo.file_size else
      let s = f x attr in memo.file_size <- s; s
    | `FileTime ->
      if memo.file_time <> "" then memo.file_time else
      let s = f x attr in memo.file_time <- s; s
    | `Length ->
      if memo.length <> "" then memo.length else
      let s = f x attr in memo.length <- s; s
    | `Channels ->
      if memo.channels <> "" then memo.channels else
      let s = f x attr in memo.channels <- s; s
    | `Depth ->
      if memo.depth <> "" then memo.depth else
      let s = f x attr in memo.depth <- s; s
    | `SampleRate ->
      if memo.sample_rate <> "" then memo.sample_rate else
      let s = f x attr in memo.sample_rate <- s; s
    | `BitRate ->
      if memo.bit_rate <> "" then memo.bit_rate else
      let s = f x attr in memo.bit_rate <- s; s
    | `Rate ->
      if memo.rate <> "" then memo.rate else
      let s = f x attr in memo.rate <- s; s
    | `Artist ->
      if memo.artist <> "" then memo.artist else
      let s = f x attr in memo.artist <- s; s
    | `Title ->
      if memo.title <> "" then memo.title else
      let s = f x attr in memo.title <- s; s
    | `AlbumArtist ->
      if memo.album_artist <> "" then memo.album_artist else
      let s = f x attr in memo.album_artist <- s; s
    | `AlbumTitle ->
      if memo.album_title <> "" then memo.album_title else
      let s = f x attr in memo.album_title <- s; s
    | `Track ->
      if memo.track <> "" then memo.track else
      let s = f x attr in memo.track <- s; s
    | `Tracks ->
      if memo.tracks <> "" then memo.tracks else
      let s = f x attr in memo.tracks <- s; s
    | `Disc ->
      if memo.disc <> "" then memo.disc else
      let s = f x attr in memo.disc <- s; s
    | `Discs ->
      if memo.discs <> "" then memo.discs else
      let s = f x attr in memo.discs <- s; s
    | `DiscTrack ->
      if memo.disc_track <> "" then memo.disc_track else
      let s = f x attr in memo.disc_track <- s; s
    | `Date ->
      if memo.date <> "" then memo.date else
      let s = f x attr in memo.date <- s; s
    | `Year ->
      if memo.year <> "" then memo.year else
      let s = f x attr in memo.year <- s; s
    | `Rating ->
      if memo.rating <> "" then memo.rating else
      let s = f x attr in memo.rating <- s; s
    | `FileExists | `FilePath | `FileDir | `FileName | `FileExt
    | `Codec | `Label | `Country | `Cover | `Pos ->
      assert false

let get_album_memo (album : album) = album.memo
let get_track_memo (track : track) = track.memo
let set_album_memo (album : album) memo = album.memo <- memo
let set_track_memo (track : track) memo = track.memo <- memo

let album_attr_string (album : album) (attr : album_attr) =
  attr_string' get_album_memo set_album_memo album_attr_string' album attr

let track_attr_string (track : track) (attr : track_attr) =
  attr_string' get_track_memo set_track_memo track_attr_string' track attr

let query_attr_string (track : track) = function
  | #track_attr as attr -> track_attr_string track attr
  | `True -> "T"
  | `False -> "F"
  | `Now -> string_of_date_time (Unix.gettimeofday ())
  | `Random -> string_of_int (Random.int 0x1_0000_0000)
  | `None -> assert false


(* String Comparison *)

let compare_length s1 s2 =
  compare (String.length s1) (String.length s2)

let compare_dir (dir1 : _ dir) (dir2 : _ dir) =
  Unicode.compare_utf_8 dir1.name dir2.name


let attr_fold attr =
  match (attr :> _) with
  | `FileExists | `FileSize | `FileTime
  | `Length | `Channels | `Depth | `SampleRate | `BitRate | `Rate
  | `Track | `Tracks | `Disc | `Discs | `DiscTrack | `Date | `Year
  | `Rating | `Cover
  | `Pos
  | `Albums ->
    Fun.id
  | `FilePath | `FileDir | `FileName | `FileExt
  | `Codec
  | `Artist | `Title | `AlbumArtist | `AlbumTitle
  | `Label | `Country ->
    Unicode.sort_key

let key_entry' e attr_string (attr, order) =
  let s = attr_fold attr (attr_string e attr) in
  if order = `Asc then s else
  let n = String.length s in
  String.init (n + 1) Char.(fun i ->
    if i = n then '\xff' else chr (255 - code s.[i]))

let key_entry attr_string sorting e =
  List.map (key_entry' e attr_string) sorting


let rev_order = function
  | `Asc -> `Desc
  | `Desc -> `Asc

let rec insert_sorting primary attr i n = function
  | _ when n <= 0 -> []
  | [] ->
    if i >= 0 then
      [attr, `Asc]
    else
      []
  | (attr', order)::sorting' when attr = attr' ->
    (match compare i 0 with
    | +1 -> (attr', order)::sorting'
    | -1 -> sorting'
    | _ -> (attr, rev_order order)::sorting'
    )
  | (attr', order)::sorting' ->
    let sorting'' =
      (attr', order) :: insert_sorting primary attr (i - 1) (n - 1) sorting' in
    if i <> 0 then
      sorting''
    else if (attr :> any_attr) = (primary :> any_attr) then
      [attr, `Asc]
    else
      (attr, `Asc)::sorting''

let remove_sorting attr = insert_sorting `None attr (-1) max_int


(* Iteration *)

let rec iter_dir f (dir : _ dir) =
  f dir; Iarray.iter (iter_dir f) dir.children

let rec exists_dir f (dir : _ dir) =
  f dir || Iarray.exists (exists_dir f) dir.children

let rec for_all_dir f (dir : _ dir) =
  f dir && Iarray.for_all (for_all_dir f) dir.children


(* Persistence *)

module Print =
struct
  open Text.Print

  let status_enum =
    [
      "undet", `Undet;
      "predet", `Predet;
      "det", `Det;
      "invalid", `Invalid;
      "absent", `Absent;
    ]

  let status = enum status_enum

  let file =
    record (fun (x : file) -> [
      "size", int x.size;
      "time", float x.time;
      "age", float x.age;
    ])

  let format =
    record (fun (x : Format.t) -> [
      "codec", string x.codec;
      "channels", int x.channels;
      "depth", int x.depth;
      "rate", int x.rate;
      "bitrate", float x.bitrate;
      "time", float x.time;
      "size", int x.size;
    ])

  let meta =
    record (fun (x : Meta.t) -> [
      "loaded", bool x.loaded;
      "artist", string x.artist;
      "title", string x.title;
      "track", string x.track_txt;
      "disc", string x.disc_txt;
      "aartist", string x.albumartist;
      "album", string x.albumtitle;
      "year", nat x.year;
      "date", string x.date_txt;
      "label", string x.label;
      "country", string x.country;
      "length", float x.length;
      "rating", num 0 5 x.rating;
    ])

  let track =
    record (fun (x : track) -> assert (x.pos = -1); [
      "path", string x.path;
      "file", file x.file;
      "format", option format x.format;
      "meta", option meta x.meta;
      (* pos should be -1 *)
      "status", status x.status;
    ])

  let rec dir () =
    record (fun (x : _ dir) -> [
      "path", string x.path;
      "parent", option string x.parent;
      "nest", int x.nest;
      "name", string x.name;
      "children", iarray (dir ()) x.children;
      "tracks", iarray track (if is_dir x then x.tracks else [||]);
      "error", string x.error;
    ])
end

module Parse =
struct
  open Text.Parse

  let status = enum Print.status_enum

  let file : t -> file =
    record (fun r -> {
      size = int (r $ "size");
      time = float (r $ "time");
      age = float (r $ "age");
    })

  let format : t -> Format.t =
    record (fun r -> Format.{
      codec = string (r $ "codec");
      channels = int (r $ "channels");
      depth = int (r $ "depth");
      rate = int (r $ "rate");
      bitrate = float (r $ "bitrate");
      time = float (r $ "time");
      size = int (r $ "size");
    })

  let meta : t -> Meta.t =
    record (fun r ->
      let track_txt = string (r $ "track") in
      let disc_txt = string (r $ "disc") in
      let date_txt = string (r $ "date") in
      Meta.{
        loaded = bool (r $ "loaded");
        artist = string (r $ "artist");
        title = string (r $ "title");
        track = Meta.int_of_total_string track_txt;
        tracks = Meta.total_of_total_string track_txt;
        track_txt;
        disc = Meta.int_of_total_string disc_txt;
        discs = Meta.total_of_total_string disc_txt;
        disc_txt;
        albumartist = string (r $ "aartist");
        albumtitle = string (r $ "album");
        year = nat (r $ "year");
        date = Meta.date_of_string date_txt;
        date_txt;
        label = string (r $ "label");
        country = string (r $ "country");
        length = float (r $ "length");
        rating = num 1 5 (r $ "rating");
        cover = None;
      }
    )

  let track : t -> track =
    record (fun r -> {
      path = string (r $ "path");
      file = file (r $ "file");
      format = option format (r $ "format");
      meta = option meta (r $ "meta");
      album = None;
      pos = -1;
      status = status (r $ "status");
      memo = None;
    })

  let rec dir make_view =
    record (fun r -> {
      path = string (r $ "path");
      parent = option string (r $ "parent");
      nest = int (r $ "nest");
      name = string (r $ "name");
      children = iarray (dir make_view) (r $ "children");
      tracks = iarray track (r $ "tracks");
      error = string (r $ "error");
      view = make_view ();
    })
end

module Encode =
struct
  open Bin.Encode

  let status_enum =
    [
      0, `Undet;
      1, `Predet;
      2, `Det;
      3, `Invalid;
      4, `Absent;
    ]

  let status = enum status_enum

  let file =
    record (fun (x : file) -> [
      nat x.size;
      float x.time;
      float x.age;
    ])

  let format =
    record (fun (x : Format.t) -> [
      string x.codec;
      nat x.channels;
      nat x.depth;
      nat x.rate;
      float x.bitrate;
      float x.time;
      nat x.size;
    ])

  let meta =
    record (fun (x : Meta.t) -> [
      bool x.loaded;
      string x.artist;
      string x.title;
      string x.track_txt;
      string x.disc_txt;
      string x.albumartist;
      string x.albumtitle;
      nat x.year;
      string x.date_txt;
      string x.label;
      string x.country;
      float x.length;
      nat x.rating;
    ])

  let track =
    record (fun (x : track) -> assert (x.pos = -1); [
      string x.path;
      file x.file;
      option format x.format;
      option meta x.meta;
      (* pos should be -1 *)
      status x.status;
    ])

  let rec dir () =
    record (fun (x : _ dir) -> [
      string x.path;
      option string x.parent;
      option nat (if x.nest = -1 then None else Some x.nest);
      string x.name;
      iarray (dir ()) x.children;
      iarray track (if is_dir x then x.tracks else [||]);
      string x.error;
    ])
end

module Decode =
struct
  open Bin.Decode

  let status = enum Encode.status_enum

  let file =
    record (fun n buf ->
      if n <> 3 then error buf;
      let size = nat buf in
      let time = float buf in
      let age = float buf in
      {size; time; age}
    )

  let format =
    record (fun n buf ->
      if n <> 7 then error buf;
      let codec = string buf in
      let channels = nat buf in
      let depth = nat buf in
      let rate = nat buf in
      let bitrate = float buf in
      let time = float buf in
      let size = nat buf in
      Format.{codec; channels; depth; rate; bitrate; time; size}
    )

  let meta =
    record (fun n buf ->
      if n <> 13 then error buf;
      let loaded = bool buf in
      let artist = string buf in
      let title = string buf in
      let track_txt = string buf in
      let disc_txt = string buf in
      let albumartist = string buf in
      let albumtitle = string buf in
      let year = nat buf in
      let date_txt = string buf in
      let label = string buf in
      let country = string buf in
      let length = float buf in
      let rating = num 0 5 buf in
      Meta.{
        loaded; artist; title;
        track = Meta.int_of_total_string track_txt;
        tracks = Meta.total_of_total_string track_txt;
        track_txt;
        disc = Meta.int_of_total_string disc_txt;
        discs = Meta.total_of_total_string disc_txt;
        disc_txt;
        albumartist; albumtitle; year;
        date = Meta.date_of_string date_txt;
        date_txt;
        label; country; length; rating;
        cover = None;
      }
    )

  let track =
    record (fun n buf ->
      if n <> 5 then error buf;
      let path = string buf in
      let file = file buf in
      let format = option format buf in
      let meta = option meta buf in
      let status = status buf in
      {path; file; format; meta; album = None; pos = -1; status; memo = None}
    )

  let rec dir make_view =
    record (fun n buf ->
      if n <> 7 then error buf;
      let path = string buf in
      let parent = option string buf in
      let nest = Option.value (option nat buf) ~default: (-1) in
      let name = string buf in
      let children = iarray (dir make_view) buf in
      let tracks = iarray track buf in
      let error = string buf in
      let view = make_view () in
      {path; parent; nest; name; children; tracks; error; view}
    )
end
