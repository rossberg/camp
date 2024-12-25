type version = Mpeg1 | Mpeg2 | Mpeg2_5
type joint = {intensity : bool; ms : bool}
type channels = Stereo | Joint of joint | Dual | Mono
type encoding = CBR | VBR
type emphasis = Emph50 | CCIT | Reserved

type format =
{
  version : version;
  layer : int;
  bitrate : int; (* kb/s *)
  rate : int; (* Hz *)
  channels : channels;
  encoding : encoding;
  emphasis : emphasis option;
  protection : bool;
  privatebit : bool;
  copyright : bool;
  original : bool;
  frames : int;
  time : float; (* s *)
  size : int; (* B *)
  offset : int (* B *)
}

type analysis =
{
  format : format;
  id3v1 : Id3v1.tag option;
  id3v2 : Id3v2.tag option;
}

type error = int * string

val read_format : string -> format
val analyze : string -> analysis * error list
