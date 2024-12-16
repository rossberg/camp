(* Program state *)

open Audio_file


type t =
{
  win : Api.window;
  audio : Api.audio;
  mutable volume : float;
  mutable sound : Api.sound;
  mutable current : Song.t option;
  mutable playpos : int;
  mutable playlist : Song.t array;
}

let make win audio =
  let sound = Api.Audio.silence audio in
  {
    win; audio; sound;
    volume = 0.5;
    current = None;
    playpos = 0;
    playlist = [||]
  }

let ok st =
  let played = Api.Audio.played st.audio st.sound in
  let playing = Api.Audio.is_playing st.audio st.sound in
  let paused = not playing && played > 0.0 in
  let stopped = not playing && not paused in
  let silence = st.sound = Api.Audio.silence st.audio in
(*
Printf.printf "[%s current=%b played=%.2f silence=%b playpos=%d len=%d]\n%!"
(if playing then "playing" else if paused then "paused" else "stopped")
(st.current <> None) played silence st.playpos (Array.length st.playlist);
*)
  assert (st.current <> None || silence);
  assert (st.current <> None || stopped);
  assert (st.current <> None || st.playlist = [||]);
  assert (st.playpos >= 0 && st.playpos <= max 0 (Array.length st.playlist - 1))
(*
  let length = Api.Audio.length st.audio st.sound in
  let played = Api.Audio.played st.audio st.sound in
  let playing = Api.Audio.is_playing st.audio st.sound in
  let paused = not playing && played > 0.0 in
  let stopped = not playing && not paused in
  let silence = st.sound = Api.Audio.silence st.audio in
Printf.printf "[%s current=%s played=%.2f silence=%b playpos=%d len=%d]\n%!"
(match st.status with Playing -> "playing" | Paused -> "paused" | Stopped -> "stopped")
(st.current <> None) played silence st.playpos (Array.length st.playlist);
  assert (st.current <> None || silence);
  assert (st.current <> None || st.status = Stopped);
  assert (st.current <> None || st.playlist = [||]);
  assert (st.status <> Playing || playing || stopped);  (* silence can be playing at 0.0 *)
  assert (st.status <> Paused || paused);
  assert (st.status <> Stopped || stopped);
  assert (not playing || st.status = Playing);
  assert (not paused || st.status = Paused);
  assert (not stopped || st.status = Stopped);
  assert (not silence || st.status <> Paused);
  assert (st.playpos >= 0 && st.playpos <= max 0 (Array.length st.playlist - 1))
*)


(* Play Control *)

let eject_song st =
  Api.Audio.stop st.audio st.sound;
  st.current <- None;
  if st.sound <> Api.Audio.silence st.audio then
  (
    Api.Audio.free st.audio st.sound;
    st.sound <- Api.Audio.silence st.audio;
  )

let switch_song st (song : Song.t) play =
  eject_song st;
  st.sound <- Api.Audio.load st.audio song.path;
  st.current <- Some song;
  Api.Audio.volume st.audio st.sound st.volume;
  Api.Audio.play st.audio st.sound;
  if not play then Api.Audio.pause st.audio st.sound

let seek_song st percent =
  if st.sound <> Api.Audio.silence st.audio then
  (
    let length = Api.Audio.length st.audio st.sound in
    Api.Audio.seek st.audio st.sound (percent *. length)
  )


(* Playlist Manipulation *)

let insert_song' songs path =
  let song = Song.make path in
  songs := song :: !songs

let insert_playlist' songs path =
  let s = In_channel.(with_open_bin path input_all) in
  List.iter (fun M3u.{path; info} ->
    let song =
      match info with
      | None -> Song.make path
      | Some {title; time} -> Song.{path; name = title; time = float time}
    in songs := song :: !songs
  ) (M3u.parse_ext s)

let rec insert_file' songs path =
  try
    match String.lowercase_ascii (Filename.extension path) with
    | _ when Sys.file_exists path && Sys.is_directory path ->
      Array.iter (fun file ->
        insert_file' songs (Filename.concat path file)
      ) (Sys.readdir path)
    | ".m3u" | ".m3u8" -> insert_playlist' songs path
    | _ -> insert_song' songs path
  with Sys_error _ -> insert_song' songs path

let insert_songs st pos paths =
  if paths <> [] then
  (
    let songs = ref [] in
    List.iter (insert_file' songs) paths;
    let playlist' = Array.of_list (List.rev !songs) in
    if st.playlist = [||] then
    (
      st.playlist <- playlist';
      if st.current = None then
        switch_song st st.playlist.(0) false;
    )
    else
    (
      let len = Array.length st.playlist in
      let len' = Array.length playlist' in
      st.playlist <-
        Array.init (len + len') (fun i ->
          if i < pos then st.playlist.(i) else
          if i < pos + len' then playlist'.(i - pos) else
          st.playlist.(i - len')
        );
      if pos < st.playpos then st.playpos <- st.playpos + len';
    )
  )
