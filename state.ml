(* Program state *)

type t =
{
  win : Api.window;
  audio : Api.audio;
  mutable volume : float;
  mutable sound : Api.sound;
  mutable playing : Song.t option;
  mutable playpos : int;
  mutable playlist : Song.t array;
}

let make win audio =
  let sound = Api.Audio.silence audio in
  {win; audio; volume = 0.5; sound; playing = None; playpos = 0; playlist = [||]}


let switch_song st (song : Song.t) on =
  Api.Audio.free st.audio st.sound;
  st.sound <- Api.Audio.load st.audio song.path;
  st.playing <- Some song;
  if st.sound <> Api.Audio.silence st.audio then
  (
    Api.Audio.volume st.audio st.sound st.volume;
    Api.Audio.play st.audio st.sound;
    (if on then Api.Audio.resume else Api.Audio.pause) st.audio st.sound;
  )

let seek_song st percent =
  let length = Api.Audio.length st.audio st.sound in
  Api.Audio.seek st.audio st.sound (percent *. length)
