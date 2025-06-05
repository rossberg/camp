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
  | `FilePath | `FileDir | `FileName | `FileExt | `FileSize | `FileTime
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
type 'attr columns = ('attr * int) array

type display = [`Table | `Grid]


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
  mutable pos : string;  (* 0-based *)
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
  file : file;
  mutable format : Format.t option;
  mutable meta : Meta.t option;
  mutable memo : memo option;
}

type track =
{
  path : path;  (* primary *)
  file : file;
  mutable format : Format.t option;
  mutable meta : Meta.t option;
  mutable album : album option;
  mutable pos : int;
  mutable status : [`Undet | `Predet | `Det | `Invalid | `Absent];
  mutable memo : memo option;
}

type 'view dir =
{
  path : path;  (* primary *)
  parent : path option;
  nest : int;
  mutable name : string;
  mutable pos : int;
  mutable children : 'view dir array;
  mutable tracks : track array;
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
let is_track_path path = Format.is_known_ext path || M3u.is_separator path

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

let make_dir path parent nest pos view : 'a dir =
  {
    path;
    parent;
    name = if path = "" then "" else File.name path;
    nest;
    pos;
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
    pos = "";
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


let accumulate_string s1 s2 =
  if s1 = s2 then s1 else ""

let accumulate_option accumulate opt1 opt2 =
  match opt1, opt2 with
  | None, _ -> opt2
  | _, None -> opt1
  | Some x1, Some x2 -> Some (accumulate x1 x2)

let accumulate_file (file1 : file) (file2 : file) =
  {
    size = file1.size + file2.size;
    time = max file1.time file2.time;
    age = max file1.age file2.age;
  }

let accumulate_format (format1 : Format.t) (format2 : Format.t) =
  Format.{
    codec = accumulate_string format1.codec format2.codec;
    channels = min format1.channels format2.channels;
    depth = min format1.depth format2.depth;
    rate = min format1.rate format2.rate;
    bitrate = min format1.bitrate format2.bitrate;
    time = format1.time +. format2.time;
    size = format1.size + format2.size;
  }

let accumulate_meta (meta1 : Meta.t) (meta2 : Meta.t) =
  { Meta.unknown with
    artist = accumulate_string meta1.artist meta2.artist;
    title = accumulate_string meta1.title meta2.title;
    tracks = meta1.tracks + meta2.tracks;
    albumartist = accumulate_string meta1.albumartist meta2.albumartist;
    albumtitle = accumulate_string meta1.albumtitle meta2.albumtitle;
    year = max meta1.year meta2.year;
    date = max meta1.date meta2.date;
    label = accumulate_string meta1.label meta2.label;
    country = accumulate_string meta1.country meta2.country;
    length = meta1.length +. meta2.length;
    rating = max meta1.rating meta2.rating;
  }

let accumulate_album (album1 : album) (album2 : album) =
  {
    path = album1.path;  (* arbitrary *)
    file = accumulate_file album1.file album2.file;
    format = accumulate_option accumulate_format album1.format album2.format;
    meta = accumulate_option accumulate_meta album1.meta album2.meta;
    memo = None;
  }

let accumulate_artist (artist1 : artist) (artist2 : artist) =
  assert (artist1.name = artist2.name);
  {
    name = artist1.name;
    albums = artist1.albums + artist2.albums;
    tracks = artist1.tracks + artist2.tracks;
  }


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


let name_separator = String.make 80 '-'

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
    let star = "*" in  (* TODO: "★" *)
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
  | `AlbumArtist -> artist_attr_string' `AlbumArtist album.path album.meta
  | `AlbumTitle -> title_attr_string' `AlbumTitle album.path album.meta
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
  | `FilePath | `FileDir | `FileName | `FileExt
  | `Codec | `Label | `Country | `Cover ->
    f x attr
  | _ ->
    let memo =
      match get_memo x with
      | Some memo -> memo
      | None -> let memo = make_memo () in set_memo x (Some memo); memo
    in
    match attr with
    | `Pos ->
      if memo.pos <> "" then memo.pos else
      let s = f x attr in memo.pos <- s; s
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
    | `FilePath | `FileDir | `FileName | `FileExt
    | `Codec | `Label | `Country | `Cover ->
      assert false

let get_album_memo (album : album) = album.memo
let get_track_memo (track : track) = track.memo
let set_album_memo (album : album) memo = album.memo <- memo
let set_track_memo (track : track) memo = track.memo <- memo

let album_attr_string (album : album) (attr : album_attr) =
  attr_string' get_album_memo set_album_memo album_attr_string' album attr

let track_attr_string (track : track) attr =
  attr_string' get_track_memo set_track_memo track_attr_string' track attr

let query_attr_string (track : track) = function
  | #track_attr as attr -> track_attr_string track attr
  | `True -> "T"
  | `False -> "F"
  | `Now -> string_of_date_time (Unix.gettimeofday ())
  | `Random -> string_of_int (Random.int 0x1_0000_0000)
  | `None -> assert false


let string_of_order = function
  | `Asc -> "+"
  | `Desc -> "-"

let order_of_char = function
  | '+' -> `Asc
  | '-' -> `Desc
  | _ -> failwith ""


let attr_str =
[
  `Pos, "POS";
  `FilePath, "PTH";
  `FileDir, "DIR";
  `FileName, "NAM";
  `FileExt, "EXT";
  `FileSize, "SIZ";
  `FileTime, "TIM";
  `Codec, "COD";
  `Channels, "CHA";
  `Depth, "DEP";
  `SampleRate, "KHZ";
  `BitRate, "BPS";
  `Rate, "RES";
  `Artist, "ART";
  `Title, "TIT";
  `Length, "LEN";
  `Rating, "RAT";
  `AlbumArtist, "ALA";
  `AlbumTitle, "ALB";
  `Track, "TRK";
  `Tracks, "TRS";
  `Disc, "DSC";
  `Discs, "DSS";
  `DiscTrack, "DTR";
  `Albums, "ALS";
  `Date, "DAT";
  `Year, "YER";
  `Label, "LAB";
  `Country, "CTY";
  `Cover, "COV";
  `True, "TRU";
  `False, "FLS";
  `Now, "NOW";
  `Random, "RND";
]

let string_of_attr attr = List.assoc (attr :> any_attr) attr_str
let attr_of_string s = fst (List.find (fun (_, s') -> s' = s) attr_str)

let rec string_of_sorting = function
  | [] -> ""
  | (attr, order)::sorting' ->
    string_of_order order ^ string_of_attr attr ^ string_of_sorting sorting'

let rec sorting_of_string' to_attr s i =
  if i = String.length s then [] else
  let order = order_of_char s.[i] in
  let attr = to_attr (attr_of_string (String.sub s (i + 1) 3)) in
  (attr, order) :: sorting_of_string' to_attr s (i + 4)

let sorting_of_string to_attr s =
  try
    sorting_of_string' to_attr s 0
  with exn ->
    Storage.log_exn "internal" exn ("malformed sorting format: " ^ s);
    raise exn

let string_of_column (attr, w) = string_of_attr attr ^ string_of_int w
let column_of_string to_attr s =
  to_attr (attr_of_string (String.sub s 0 3)),
  int_of_string (String.sub s 3 (String.length s - 3))

let string_of_columns cs =
  String.concat " " (Array.to_list (Array.map string_of_column cs))

let columns_of_string to_attr s =
  try
    Array.of_list (List.map (column_of_string to_attr)
      (List.filter ((<>) "") (String.split_on_char ' ' s)))
  with exn ->
    Storage.log_exn "internal" exn ("malformed columns format: " ^ s);
    raise exn

(* TODO: this is a temporary conversion hack for retro-introducing covers. *)
let columns_of_string_add_cover i to_attr s =
  let columns = columns_of_string to_attr s in
  let disctrack = function
    | `Track, w -> `DiscTrack, w
    | other -> other
  in
  if fst columns.(i) = `Cover then Array.map disctrack columns else
  Array.init (Array.length columns + 1) (fun j ->
    match compare j i with
    | -1 -> disctrack columns.(j)
    | +1 -> disctrack columns.(j - 1)
    | _ -> `Cover, 30
  )

let to_artist_attr = function
  | #artist_attr as attr -> attr
  | _ -> failwith "to_artist_attr"

let to_album_attr = function
  | #album_attr as attr -> attr
  | _ -> failwith "to_album_column"

let to_track_attr = function
  | #track_attr as attr -> attr
  | _ -> failwith "to_track_column"


let string_of_artist_sorting (s : artist_attr sorting) = string_of_sorting s
let string_of_album_sorting (s : album_attr sorting) = string_of_sorting s
let string_of_track_sorting (s : track_attr sorting) = string_of_sorting s

let string_of_artist_columns (cs : artist_attr columns) = string_of_columns cs
let string_of_album_columns (cs : album_attr columns) = string_of_columns cs
let string_of_track_columns (cs : track_attr columns) = string_of_columns cs

let artist_sorting_of_string s = sorting_of_string to_artist_attr s
let album_sorting_of_string s = sorting_of_string to_album_attr s
let track_sorting_of_string s = sorting_of_string to_track_attr s

let artist_columns_of_string s = columns_of_string to_artist_attr s
let album_columns_of_string s = columns_of_string_add_cover 0 to_album_attr s
let track_columns_of_string s = columns_of_string_add_cover 1 to_track_attr s


(* String Comparison *)

module UCol = Camomile.UCol.Make (Camomile.UTF8)

let compare_utf_8 s1 s2 = UCol.compare ~prec: `Primary s1 s2
let compare_length s1 s2 = compare (String.length s1) (String.length s2)

let compare_dir (dir1 : _ dir) (dir2 : _ dir) =
  match compare dir1.pos dir2.pos with
  | 0 -> compare_utf_8 dir1.name dir2.name
  | i -> i


let key_entry' e attr_string (attr, order) =
  let s = attr_string e attr in
  if order = `Asc then s else String.map Char.(fun c -> chr (255 - code c)) s

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
