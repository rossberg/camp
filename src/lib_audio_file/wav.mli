type resolution = PCM | Float

type format =
{
  resolution : resolution;
  rate : int; (* 1 to 655350 Hz *)
  channels : int; (* 1 to 8 *)
  depth : int; (* 4 to 32 *)
  size : int;
}

val read_format : string -> format
