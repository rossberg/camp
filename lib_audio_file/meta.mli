type path = string
type time = float

type tag =
  | Id3v2Tag of Id3v2.tag
  | VorbisTag of Vorbis.tag

type picture =
{
  mime : string;
  width : int;
  height : int;
  progressive : bool;
  data : string;
}

type meta =
{
  loaded : bool;
  artist  : string;
  title : string;
  track : int;
  tracks : int;
  trackfmt : int;
  disc : int;
  discs : int;
  discfmt : int;
  albumartist : string;
  album : string;
  year : int;
  date : time;
  label : string;
  country : string;
  length : time;
  rating : int;
  cover : picture option;
}

val warn_hook : (path -> string -> unit) ref
val progress_hook : (unit -> unit) ref

val date0 : time
val date : int -> int -> int -> time

val load_tag : path -> tag option
val load_meta : path -> meta

val meta : path -> tag option -> meta
