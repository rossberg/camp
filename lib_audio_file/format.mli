type t =
{
  name : string;
  channels : int;
  depth : int;
  rate : int; (* Hz *)
  bitrate : int; (* kb/s *)
  time : float; (* s *)
  size : int; (* B *)
}

val unknown : t
val read : string -> t
