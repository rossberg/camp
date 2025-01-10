type path = string

type t =
{
  code : string;
  channels : int;
  depth : int;
  rate : int; (* Hz *)
  bitrate : float; (* b/s *)
  time : float; (* s *)
  size : int; (* B *)
}

val unknown : t
val read : path -> t

val is_known_ext : path -> bool
