(* Program state *)

open Audio_file


(* Songs *)

type path = string
type time = float

type song =
{
  path : path;
  mutable name : string;
  mutable time : time;
  mutable time_ok : bool;
}

let make_song path =
{
  path;
  name = Filename.(remove_extension (basename path));  (* TODO *)
  time = 0.0;  (* TODO *)
  time_ok = false;
}

let make_song_ext path name time =
  {path; name; time; time_ok = false}

let settle_song_time song time =
  song.time <- time;
  song.time_ok <- true

let is_loaded_song song =
  song.time_ok || song.time <> 0.0 ||
  song.name <> Filename.(remove_extension (basename song.path))


(* State *)

type t =
{
  win : Api.window;
  audio : Api.audio;
  mutable volume : float;
  mutable sound : Api.sound;
  mutable current : song option;
  mutable playpos : int;
  mutable playlist : song array;
  mutable undo : (int * song array) list;
  mutable undocount : int;
}

let make win audio =
  let sound = Api.Audio.silence audio in
  {
    win; audio; sound;
    volume = 0.5;
    current = None;
    playpos = 0;
    playlist = [||];
    undo = [];
    undocount = 0;
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


(* Song update queue *)

let queue = Safe_queue.create ()

let update_song song =
  Safe_queue.add song queue

let rec updater () =
  let song = Safe_queue.take queue in
  (try
    let meta = Meta.load_meta song.path in
    if not song.time_ok then song.time <- meta.length;
    song.name <-
      if meta.artist <> "" && meta.title <> "" then
        meta.artist ^ " - " ^ meta.title
      else if meta.title <> "" then meta.title
      else song.name
  with Sys_error _ -> ());
  updater ()

let _ = Domain.spawn updater


(* Play Control *)

let eject_song st =
  Api.Audio.stop st.audio st.sound;
  st.current <- None;
  if st.sound <> Api.Audio.silence st.audio then
  (
    Api.Audio.free st.audio st.sound;
    st.sound <- Api.Audio.silence st.audio;
  )

let switch_song st song play =
  eject_song st;
  st.sound <- Api.Audio.load st.audio song.path;
  st.current <- Some song;
  let time =
    if st.sound = Api.Audio.silence st.audio then 0.0
    else Api.Audio.length st.audio st.sound
  in
  settle_song_time song time;
  update_song song;
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

let undo_depth = 100

let push_undo st =
  if st.undocount < undo_depth then
    st.undocount <- st.undocount + 1
  else
    st.undo <- List.filteri (fun i _ -> i < undo_depth - 1) st.undo;
  st.undo <- (st.playpos, st.playlist) :: st.undo

let pop_undo st =
  match st.undo with
  | [] -> ()
  | (pos, list) :: undo' ->
    st.playpos <- pos;
    st.playlist <- list;
    st.undo <- undo';
    st.undocount <- st.undocount - 1;
    if st.current = None && list <> [||] then st.current <- Some list.(pos)


let clear_songs st =
  push_undo st;
  st.playlist <- [||];
  st.playpos <- 0


let insert_song' songs path =
  songs := make_song path :: !songs

let insert_playlist' songs path =
  let s = In_channel.(with_open_bin path input_all) in
  List.iter (fun M3u.{path; info} ->
    let song =
      match info with
      | None -> make_song path
      | Some {title; time} -> make_song_ext path title (float time)
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
    push_undo st;
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
