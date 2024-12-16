type tag =
{
  offset : int;
  title : string;
  artist : string;
  album : string;
  year : int option;
  comment : string;
  track : int option;
  genre : int;
}

type error = int * string

val input_tag : in_channel -> tag option * error list
