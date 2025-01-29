(* Library Data Representation *)

open Audio_file

type path = string
type time = float
type blob = string
type id = int64
type 'a link = [`Id of id | `Val of 'a] ref


(* Links *)
(* TODO: remove? *)

let link_id id = ref (`Id id)
let link_val x = ref (`Val x)

let id_of_link = function {contents = `Id id} -> id | _ -> assert false
let val_of_link = function {contents = `Val x} -> x | _ -> assert false


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
  mutable children : dir link array;
  mutable folded : bool;
  mutable artists_shown : bool;
  mutable albums_shown : bool;
  mutable tracks_shown : bool;
  mutable divider_width : int;
  mutable divider_height : int;
  mutable artists_columns : artist_attr columns;
  mutable albums_columns : album_attr columns;
  mutable tracks_columns : track_attr columns;
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

type playlist =
{
  mutable id : id;
  path : path;  (* primary *)
}

type track =
{
  mutable id : id;
  path : path;  (* primary *)
  file : file;
  mutable format : Format.t option;
  mutable meta : Meta.t option;
  mutable album : album link option;
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
  `Artist, 150;
  `Title, 180;
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
  `FileTime, 110;
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

let make_playlist path : playlist =
  {
    id = -1L;
    path;
  }


(* String Conversion *)

let attr_str =
[
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
  `Pos, "POS";
]

let string_of_attr attr = List.assoc (attr :> any_attr) attr_str
let attr_of_string s = fst (List.find (fun (_, s') -> s' = s) attr_str)

let string_of_column (attr, w) = string_of_attr attr ^ string_of_int w
let column_of_string s =
  attr_of_string (String.sub s 0 3),
  int_of_string (String.sub s 3 (String.length s - 3))

let string_of_columns cs =
  String.concat " " (Array.to_list (Array.map string_of_column cs))
let columns_of_string to_column s =
  Array.of_list (List.map (fun s -> to_column (column_of_string s))
    (List.filter ((<>) "") (String.split_on_char ' ' s)))

let to_artist_column = function
  | #artist_attr as attr, n -> attr, n
  | _ -> failwith "to_artist_column"

let to_album_column = function
  | #album_attr as attr, n -> attr, n
  | _ -> failwith "to_album_column"

let to_track_column = function
  | #track_attr as attr, n -> attr, n
  | _ -> failwith "to_track_column"

let string_of_artist_columns (cs : artist_attr columns) = string_of_columns cs
let string_of_album_columns (cs : album_attr columns) = string_of_columns cs
let string_of_track_columns (cs : track_attr columns) = string_of_columns cs

let artist_columns_of_string s = columns_of_string to_artist_column s
let album_columns_of_string s = columns_of_string to_album_column s
let track_columns_of_string s = columns_of_string to_track_column s
