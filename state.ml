(* Program state *)

type t =
{
  win : Api.window;
  audio : Api.audio;
  mutable sound : Api.sound;
  mutable playing : int;
  mutable playlist : Song.t array;
  mutable volume : float;
}
