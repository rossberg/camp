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

type t =
{
  loaded : bool;
  artist : string;
  title : string;
  track : int;
  tracks : int;
  track_txt : string;
  disc : int;
  discs : int;
  disc_txt : string;
  albumartist : string;
  albumtitle : string;
  year : int;
  date : time;
  date_txt : string;
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
val date_of_string : string -> time  (* returns 0.0 on error *)
val year_of_string : string -> int   (* returns 0 on error *)

val int_of_total_string : string -> int    (* returns 0 on error *)
val total_of_total_string : string -> int  (* returns 0 on error *)

val artists_of_artist : string -> string list

val unknown : t

val meta : path -> tag option -> t

val load_tag : path -> tag option
val load : path -> t
