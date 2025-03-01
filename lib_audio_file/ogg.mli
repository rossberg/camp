type format =
{
  channels : int;
  rate : int;
  bitrate_maximum : int;
  bitrate_nominal : int;
  bitrate_minimum : int;
  samples : int;
  size : int;
}

type analysis =
{
  format : format;
  vorbis : Vorbis.tag option;
}

type error = int * string

val read_format : string -> format
val analyze : string -> analysis * error list
