type format =
{
  min_block_size : int; (* 16 to 65535 *)
  max_block_size : int; (* 16 to 65535 *)
  min_frame_size : int;
  max_frame_size : int;
  rate : int; (* 1 to 655350 Hz *)
  channels : int; (* 1 to 8 *)
  depth : int; (* 4 to 32 *)
  samples : int;
  md5 : string;
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
