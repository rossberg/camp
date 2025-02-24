(* Library Data Representation *)

open Audio_file

type path = string
type time = float
type blob = string
type id = int64


(* View Attributes *)

type file_attr =
[
  | `FilePath | `FileSize | `FileTime
]

type format_attr =
[
  | `Length | `Codec | `Channels | `Depth | `SampleRate | `Bitrate | `Rate
]

type meta_attr =
[
  | `Artist | `Title | `Track | `Tracks | `Disc | `Discs
  | `AlbumArtist | `AlbumTitle | `Date | `Year | `Country | `Label | `Rating
]

type artist_attr = [ `Artist | `Albums | `Tracks ]
type album_attr = [ file_attr | format_attr | meta_attr ]
type track_attr = [ file_attr | format_attr | meta_attr | `Pos ]
type any_attr = [ artist_attr | album_attr | track_attr | `None ]

type order = [`Asc | `Desc]
type 'attr sorting = ('attr * order) list
type 'attr columns = ('attr * int) array


(* Data *)

type search = string array

type dir =
{
  mutable id : id;
  path : path;  (* primary *)
  parent : path option;
  nest : int;
  mutable name : string;
  mutable pos : int;
  mutable children : dir array;
  mutable search : search;
  mutable folded : bool;
  mutable artists_shown : bool;
  mutable albums_shown : bool;
  mutable tracks_shown : bool;
  mutable divider_width : int;
  mutable divider_height : int;
  mutable artists_columns : artist_attr columns;
  mutable albums_columns : album_attr columns;
  mutable tracks_columns : track_attr columns;
  mutable artists_sorting : artist_attr sorting;
  mutable albums_sorting : album_attr sorting;
  mutable tracks_sorting : track_attr sorting;
}

type file =
{
  mutable size : int;
  mutable time : time;
  mutable age : time;
}

type artist =
{
  mutable id : id;
  name : string;  (* primary *)
  mutable albums : int;
  mutable tracks : int;
}

type album =
{
  mutable id : id;
  path : path;  (* primary *)
  file : file;
  mutable format : Format.t option;
  mutable meta : Meta.t option;
}

type track =
{
  mutable id : id;
  path : path;  (* primary *)
  file : file;
  mutable format : Format.t option;
  mutable meta : Meta.t option;
  mutable album : album option;
  mutable pos : int;
  mutable status : [`Undet | `Predet | `Det | `Invalid | `Absent];
}


(* Constructors *)

let artists_columns : artist_attr columns =
[|
  `Artist, 150;
  `Albums, 20;
  `Tracks, 20;
|]

let albums_columns : album_attr columns =
[|
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
  `FileTime, 70;
  `Rating, 30;
  `Artist, 150;
  `Title, 180;
  `Length, 30;
  `AlbumArtist, 100;
  `AlbumTitle, 150;
  `Track, 20;
  `Date, 60;
  `Country, 50;
  `Label, 50;
  `Codec, 30;
  `Rate, 50;
  `FileSize, 50;
  `FilePath, 400;
|]


let make_search () : search =
  [||]

let make_dir path parent nest pos : dir =
  {
    id = -1L;
    path;
    parent;
    name = if path = "" then path else File.name path;
    nest;
    pos;
    children = [||];
    search = make_search ();
    folded = false;
    artists_shown = false;
    albums_shown = false;
    tracks_shown = true;
    divider_width = 100;
    divider_height = 100;
    artists_columns = artists_columns;
    albums_columns = albums_columns;
    tracks_columns = tracks_columns;
    artists_sorting = [`Artist, `Asc];
    albums_sorting = [`AlbumArtist, `Asc; `AlbumTitle, `Asc; `Codec, `Asc];
    tracks_sorting =
      if M3u.is_known_ext path || Format.is_known_ext path
      then [`Pos, `Asc]
      else [`Artist, `Asc; `Title, `Asc; `Codec, `Asc];
  }

let make_file () : file =
  {
    size = 0;
    time = 0.0;
    age = 0.0;
  }

let make_artist name : artist =
  {
    id = -1L;
    name;
    albums = 0;
    tracks = 0;
  }

let make_album path : album =
  {
    id = -1L;
    path;
    file = make_file ();
    format = None;
    meta = None;
  }

let make_track path : track =
  {
    id = -1L;
    path;
    file = make_file ();
    format = None;
    meta = None;
    album = None;
    pos = -1;
    status = `Undet;
  }

let make_separator () : track =
  let track = make_track M3u.separator in
  track.status <- `Det;
  track


(* Properties *)

let parent_path path = File.(dir path // "")
let is_dir_path path = String.ends_with ~suffix: File.sep path
let is_playlist_path path = M3u.is_known_ext path
let is_track_path path = Format.is_known_ext path || M3u.is_separator path

let is_root dir = dir.parent = Some ""
let is_dir (dir : dir) = dir.path = "" || is_dir_path dir.path
let is_playlist (dir : dir) = is_playlist_path dir.path


let is_separator (track : track) = M3u.is_separator track.path

let is_invalid track =
  match track.status with
  | `Invalid | `Absent -> true
  | `Det | `Predet | `Undet -> false


(* String Conversion *)

let fmt = Printf.sprintf

let string_of_time t =
  let t' = int_of_float (Float.trunc t) in
  fmt "%d:%02d" (t' / 60) (t' mod  60)

let string_of_date t =
  let tm = Unix.localtime t in
  fmt "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

let string_of_date_time t =
  let tm = Unix.localtime t in
  fmt "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec


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
  `FileSize, "SIZ";
  `FileTime, "TIM";
  `Codec, "COD";
  `Channels, "CHA";
  `Depth, "DEP";
  `SampleRate, "KHZ";
  `Bitrate, "BPS";
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
  `Albums, "ALS";
  `Date, "DAT";
  `Year, "YER";
  `Label, "LAB";
  `Country, "CTY";
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
let album_columns_of_string s = columns_of_string to_album_attr s
let track_columns_of_string s = columns_of_string to_track_attr s


let string_of_search search =  String.concat " " (Array.to_list search)
let search_of_string s =
  Array.of_list (List.filter ((<>) "") (String.split_on_char ' ' s))


(* String Comparison *)

module UCol = Camomile.UCol.Make (Camomile.UTF8)

let compare_utf_8 s1 s2 = UCol.compare ~prec: `Primary s1 s2

let compare_attr : 'a. ([< any_attr] as 'a) -> _ = function
  | `Artist | `Title | `AlbumArtist | `AlbumTitle | `Country | `Label ->
    fun s1 s2 -> compare_utf_8 s1 s2
  | _ -> compare

let rec compare_attrs sorting ss1 ss2 =
  match sorting, ss1, ss2 with
  | (attr, order)::sorting', s1::ss1', s2::ss2' ->
    (match compare_attr attr s1 s2 with
    | 0 -> compare_attrs sorting' ss1' ss2'
    | r -> if order = `Asc then +r else -r
    )
  | _, _, _ -> 0


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
