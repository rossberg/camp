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
  mutable status : [`Undet | `Predet | `Det | `Invalid | `Absent];
  mutable last_update : time;
}

let exts = [".mp3"; ".flac"; ".wav"; ".ogg"; ".mod"]

let known_ext path =
  List.mem (String.lowercase_ascii (Filename.extension path)) exts

let name_of_path path =
  let file = Filename.basename path in
  if known_ext file then Filename.remove_extension file else file

let make_song path =
  {
    path;
    name = name_of_path path;
    time = 0.0;
    status = `Undet;
    last_update = 0.0;
  }

let make_song_predet path name time =
  {path; name; time; status = `Predet; last_update = 0.0}


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
  mutable playscroll : int;
  mutable undo : (int * song array * int) list;
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
    playscroll = 0;
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

let update_song st song =
  if song.last_update >= 0.0 then
  (
    song.last_update <- -1.0;
    Safe_queue.add (st, song) queue;
  )

let rec updater () =
  let st, song = Safe_queue.take queue in
  if not (Sys.file_exists song.path) then
  (
    song.status <- `Absent;
    song.name <- name_of_path song.path
  )
  else if not (known_ext song.path) then
  (
    song.status <- `Invalid;
    song.name <- name_of_path song.path
  )
  else
  (
    try
      let meta = Meta.load_meta song.path in
      if meta.loaded then song.status <- `Det;
      if song.time = 0.0 then
      (
        if meta.length <> 0.0 then song.time <- meta.length else
        let sound = Api.Audio.load st.audio song.path in
        song.time <-
          if sound = Api.Audio.silence st.audio then 0.0 else
          (
            let t = Api.Audio.length st.audio sound in
            Api.Audio.free st.audio sound;
            t
          )
      );
      song.name <-
        if meta.artist <> "" && meta.title <> "" then meta.artist ^ " - " ^ meta.title else
        if meta.title <> "" then meta.title else name_of_path song.path
    with
    | Sys_error _ -> song.status <- `Invalid
    | exn ->
      Printf.fprintf stderr "uncaught exception in updater thread: %s\n%!"
        (Printexc.to_string exn)
  );
  song.last_update <- Unix.time ();
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
  song.time <-
    if st.sound = Api.Audio.silence st.audio then 0.0
    else Api.Audio.length st.audio st.sound;
  update_song st song;
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
  st.undo <- (st.playpos, st.playlist, st.playscroll) :: st.undo

let pop_undo st =
  match st.undo with
  | [] -> ()
  | (pos, list, scroll) :: undo' ->
    st.playpos <- pos;
    st.playlist <- list;
    st.playscroll <- scroll;
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
      | Some {title; time} -> make_song_predet path title (float time)
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
      if pos < st.playscroll then st.playscroll <- st.playscroll + len';
    )
  )
