type comment =
{
  offset : int;
  key : string;
  value : string;
}

type picture =
{
  offset : int;
  picture_type : int;
  mime : string;
  width : int;
  height : int;
  depth : int;
  num_colors : int;
  description : string;
  data : string;
}

type tag =
{
  offset : int;
  vendor : string;
  comments : comment list;
  pictures : picture list;
}

type error = int * string

val input_tag : in_channel -> tag option * error list
