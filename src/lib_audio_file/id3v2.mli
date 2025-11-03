type flags =
{
  unsynchronisation : bool;
  extended : bool;
  experimental : bool;
  footer : bool;
}

type header =
{
  offset : int;
  version : int;
  revision : int;
  flags : flags;
  size : int
}

type restrictions =
{
  tag_size : [`Max1MB | `Max128KB | `Max40KB | `Max4KB];
  text_encoding : [`ISO8859_1_or_UTF8] option;
  text_size : [`Max1024 | `Max128 | `Max30] option;
  image_encoding : [`JPEG_or_PNG] option;
  image_size : [`Max256 | `Max64 | `Eq64] option;
}

type extended_header =
{
  offset : int;
  size : int;
  update : bool;
  crc : Int32.t option;
  restrictions : restrictions option
}

type status_flags =
{
  tag_alter_discard : bool;
  file_alter_discard : bool;
  read_only : bool;
}

type format_flags =
{
  group : bool;
  compression : bool;
  encryption : bool;
  unsynchronisation : bool;
  data_length : bool;
}

type picture =
{
  picture_type : int;
  mime : string;
  description : string;
  data : string;
}

type frame =
{
  offset : int;
  id : string;
  size : int;
  status_flags : status_flags;
  format_flags : format_flags;
  group : int option;
  data_length : int option;
  contents : string list;
  picture : picture option;
}

type tag =
{
  header : header;
  extended : extended_header option;
  frames : frame list;
  padding : int
}

type error = int * string

val input_tag : in_channel -> tag option * error list
