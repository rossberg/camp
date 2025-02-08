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
type any_attr = [ artist_attr | album_attr | track_attr ]

type 'attr sorting = 'attr * [`Asc | `Desc]
type 'attr columns = ('attr * int) array


(* Data *)

type dir =
{
  mutable id : id;
  path : path;  (* primary *)
  parent : path option;
  nest : int;
  mutable name : string;
  mutable pos : int;
  mutable children : dir array;
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


let make_dir path parent nest pos : dir =
  {
    id = -1L;
    path;
    parent;
    name = Filename.basename path;
    nest;
    pos;
    children = [||];
    folded = false;
    artists_shown = false;
    albums_shown = false;
    tracks_shown = true;
    divider_width = 100;
    divider_height = 100;
    artists_columns = artists_columns;
    albums_columns = albums_columns;
    tracks_columns = tracks_columns;
    artists_sorting = `Artist, `Asc;
    albums_sorting = `AlbumArtist, `Asc;
    tracks_sorting =
      if M3u.is_known_ext path || Format.is_known_ext path
      then `Pos, `Asc
      else `Artist, `Asc;
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
    pos = 0;
    status = `Undet;
  }


(* String Conversion *)

let string_of_order = function
  | `Asc -> "+"
  | `Desc -> "-"

let order_of_char = function
  | '+' -> `Asc
  | '-' -> `Desc
  | _ -> failwith ""

let rev_order = function
  | `Asc -> `Desc
  | `Desc -> `Asc


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

let string_of_sorting (attr, order) =
  string_of_order order ^ string_of_attr attr

let sorting_of_string to_attr s =
  to_attr (attr_of_string (String.sub s 1 (String.length s - 1))),
  order_of_char s.[0]

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
    Storage.log ("malformed columns format: " ^ s);
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
