type format =
{
  width : int;
  height : int;
  depth : int;
  color_type : int;
  compression : int;
  filter : int;
  interlace : int;
}

exception Format

val read_format : string -> format
