type format =
{
  channels : int;
  rate : int;
  preskip : int;
  gain : float;
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
