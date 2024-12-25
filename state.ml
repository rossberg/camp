(* Program state *)

open Audio_file

module IntSet = Set.Make(Int)


(* Tracks *)

type path = string
type time = float

type track =
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

let make_track' path name time status =
  {path; name; time; status; last_update = 0.0}

let make_track path = make_track' path (name_of_path path) 0.0 `Undet
let make_track_predet path name time = make_track' path name time `Det


(* State *)

type t =
{
  win : Api.window;
  audio : Api.audio;
  mutable mute : bool;
  mutable volume : float;
  mutable sound : Api.sound;
  mutable current : track option;
  mutable timemode : [`Elapse | `Remain];
  mutable shuffle : bool;
  mutable repeat : [`None | `One | `All];
  mutable loop : [`None | `A of time | `AB of time * time];
  mutable playopen : bool;
  mutable playheight : int;
  mutable playscroll : int;
  mutable playpos : int;
  mutable playrange : int * int;
  mutable playlist : track array;
  mutable playselected : IntSet.t;
  mutable playsum : time * int;
  mutable playselsum : time * int;
  mutable undo : (int * track array * int * (time * int)) list ref;
  mutable redo : (int * track array * int * (time * int)) list ref;
}

let no_range = min_int, 0

let make win audio =
  let sound = Api.Audio.silence audio in
  {
    win; audio; sound;
    mute = false;
    volume = 0.5;
    current = None;
    timemode = `Elapse;
    shuffle = false;
    repeat = `None;
    loop = `None;
    playopen = false;
    playheight = 200;
    playscroll = 0;
    playpos = 0;
    playrange = no_range;
    playlist = [||];
    playselected = IntSet.empty;
    playsum = 0.0, 0;
    playselsum = 0.0, 0;
    undo = ref [];
    redo = ref [];
  }

let ok st =
  let played = Api.Audio.played st.audio st.sound in
  let playing = Api.Audio.is_playing st.audio st.sound in
  let paused = not playing && played > 0.0 in
  let stopped = not playing && not paused in
  let silence = st.sound = Api.Audio.silence st.audio in
  let len = Array.length st.playlist in
(*
Printf.printf "[%s current=%b played=%.2f silence=%b playpos=%d len=%d]\n%!"
(if playing then "playing" else if paused then "paused" else "stopped")
(st.current <> None) played silence st.playpos (Array.length st.playlist);
*)
  assert (st.current <> None || silence);
  assert (st.current <> None || stopped);
  assert (st.current <> None || st.playlist = [||]);
  assert (st.playpos = 0 && len = 0 || st.playpos >= 0 && st.playpos < len);
  assert (st.playscroll = 0 && len = 0 || st.playscroll >= 0 && st.playscroll < len);
  assert (IntSet.max_elt_opt st.playselected <= Some (len - 1));
  assert (fst st.playrange = min_int || fst st.playrange = max_int || fst st.playrange >= 0 && fst st.playrange < len);
  assert (snd st.playrange = 0 || snd st.playrange >= 0 && snd st.playrange < len);
  assert (match st.loop with `AB (t1, t2) -> t1 <= t2 | _ -> true);
  ()
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


(* Playlist Summary *)

let add_summary (t1, n1) (t2, n2) = (t1 +. t2, n1 + n2)
let sub_summary (t1, n1) (t2, n2) = (max 0.0 (t1 -. t2), max 0 (n1 - n2))

let track_summary track =
  match track.status with
  | `Undet | `Invalid | `Absent -> 0.0, 1
  | `Predet | `Det -> track.time, 0

let update_summary st =
  st.playsum <- 0.0, 0;
  st.playselsum <- 0.0, 0;
  for i = 0 to Array.length st.playlist - 1 do
    let sum = track_summary st.playlist.(i) in
    st.playsum <- add_summary st.playsum sum;
    if IntSet.mem i st.playselected then
      st.playselsum <- add_summary st.playselsum sum;
  done


(* Track update queue *)

let queue = Safe_queue.create ()

let update_track st track =
  if track.last_update >= 0.0 then
  (
    track.last_update <- -1.0;
    Safe_queue.add (st, track) queue;
  )

let rec updater () =
  let st, track = Safe_queue.take queue in
  if not (Sys.file_exists track.path) then
  (
    track.status <- `Absent;
    track.name <- name_of_path track.path
  )
  else if not (known_ext track.path) then
  (
    track.status <- `Invalid;
    track.name <- name_of_path track.path
  )
  else
  (
    try
      let meta = Meta.load_meta track.path in
      if meta.loaded then track.status <- `Det;
      if track.time = 0.0 then
      (
        if meta.length <> 0.0 then track.time <- meta.length else
        let sound = Api.Audio.load st.audio track.path in
        track.time <-
          if sound = Api.Audio.silence st.audio then 0.0 else
          (
            let t = Api.Audio.length st.audio sound in
            Api.Audio.free st.audio sound;
            t
          )
      );
      track.name <-
        if meta.artist <> "" && meta.title <> "" then meta.artist ^ " - " ^ meta.title else
        if meta.title <> "" then meta.title else name_of_path track.path
    with
    | Sys_error _ -> track.status <- `Invalid
    | exn ->
      Printf.fprintf stderr "uncaught exception in updater thread: %s\n%!"
        (Printexc.to_string exn)
  );
  track.last_update <- Unix.time ();
  updater ()

let _ = Domain.spawn updater


(* Play Control *)

let eject_track st =
  Api.Audio.stop st.audio st.sound;
  st.current <- None;
  st.loop <- `None;
  if st.sound <> Api.Audio.silence st.audio then
  (
    Api.Audio.free st.audio st.sound;
    st.sound <- Api.Audio.silence st.audio;
  )

let switch_track st track play =
  eject_track st;
  st.sound <- Api.Audio.load st.audio track.path;
  st.current <- Some track;
  track.time <-
    if st.sound = Api.Audio.silence st.audio then 0.0
    else Api.Audio.length st.audio st.sound;
  update_track st track;
  Api.Audio.volume st.audio st.sound (if st.mute then 0.0 else st.volume);
  Api.Audio.play st.audio st.sound;
  if not play then Api.Audio.pause st.audio st.sound

let seek_track st percent =
  if st.sound <> Api.Audio.silence st.audio then
  (
    let length = Api.Audio.length st.audio st.sound in
    Api.Audio.seek st.audio st.sound (percent *. length)
  )


(* Playlist Selection *)

let num_selected st = IntSet.cardinal st.playselected
let first_selected st = IntSet.min_elt_opt st.playselected
let last_selected st = IntSet.max_elt_opt st.playselected
let is_selected st i = IntSet.mem i st.playselected

let move_select st i j =
  assert (IntSet.mem i st.playselected);
  st.playselected <- IntSet.add j (IntSet.remove i st.playselected)

let select_all st =
  for i = 0 to Array.length st.playlist - 1 do
    st.playselected <- IntSet.add i st.playselected
  done;
  st.playselsum <- st.playsum

let deselect_all st =
  st.playselected <- IntSet.empty;
  st.playselsum <- 0.0, 0

let select st i j =
  let i, j = min i j, max i j in
  for k = i to j do
    if not (IntSet.mem k st.playselected) then
    (
      st.playselected <- IntSet.add k st.playselected;
      st.playselsum <- add_summary st.playselsum (track_summary st.playlist.(k))
    )
  done

let deselect st i j =
  let i, j = min i j, max i j in
  for k = i to j do
    if IntSet.mem k st.playselected then
    (
      st.playselected <- IntSet.remove k st.playselected;
      st.playselsum <- sub_summary st.playselsum (track_summary st.playlist.(k))
    )
  done


(* Playlist Undo *)

let undo_depth = 100

let push_undo st =
  if List.length !(st.undo) >= undo_depth then
    st.undo := List.filteri (fun i _ -> i < undo_depth - 1) !(st.undo);
  st.undo := (st.playpos, st.playlist, st.playscroll, st.playsum) :: !(st.undo);
  st.redo := []

let pop_unredo st undo redo =
  match !undo with
  | [] -> ()
  | (pos, list, scroll, sum) :: undo' ->
    redo := (st.playpos, st.playlist, st.playscroll, st.playsum) :: !redo;
    undo := undo';
    deselect_all st;
    st.playpos <- pos;
    st.playscroll <- scroll;
    st.playrange <- no_range;
    st.playlist <- list;
    st.playsum <- sum;
    if st.current = None && list <> [||] then st.current <- Some list.(pos)

let pop_undo st = pop_unredo st st.undo st.redo
let pop_redo st = pop_unredo st st.redo st.undo


(* Playlist Manipulation *)

let insert_track tracks path =
  tracks := make_track path :: !tracks

let insert_playlist tracks path =
  let s = In_channel.(with_open_bin path input_all) in
  List.iter (fun M3u.{path; info} ->
    let track =
      match info with
      | None -> make_track path
      | Some {title; time} -> make_track_predet path title (float time)
    in tracks := track :: !tracks
  ) (M3u.parse_ext s)

let rec insert_file tracks path =
  try
    match String.lowercase_ascii (Filename.extension path) with
    | _ when Sys.file_exists path && Sys.is_directory path ->
      Array.iter (fun file ->
        insert_file tracks (Filename.concat path file)
      ) (Sys.readdir path)
    | ".m3u" | ".m3u8" -> insert_playlist tracks path
    | _ -> insert_track tracks path
  with Sys_error _ -> insert_track tracks path

let insert st pos paths =
  if paths <> [] then
  (
    push_undo st;
    let tracks = ref [] in
    List.iter (insert_file tracks) paths;
    let playlist' = Array.of_list (List.rev !tracks) in
    if st.playlist = [||] then
    (
      st.playlist <- playlist';
      if st.current = None then
        switch_track st st.playlist.(0) false;
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
      if pos <= st.playpos then st.playpos <- st.playpos + len';
      if pos <= st.playscroll then st.playscroll <- st.playscroll + len';
      IntSet.iter (fun i -> if pos <= i then move_select st i (i + len'))
        st.playselected;
    );
    for i = 0 to Array.length playlist' - 1 do
      let sum = track_summary playlist'.(i) in
      st.playsum <- add_summary st.playsum sum
    done
  )

let remove_all st =
  if st.playlist <> [||] then
  (
    push_undo st;
    deselect_all st;
    st.playlist <- [||];
    st.playpos <- 0;
    st.playrange <- no_range;
    st.playsum <- 0.0, 0;
    st.playselsum <- 0.0, 0;
  )

let remove_selected st =
  let n = num_selected st in
  if n > 0 then
  (
    push_undo st;
    let off = ref 0 in
    st.playlist <-
      Array.init (Array.length st.playlist - n) (fun i ->
        let off' = !off in
        while is_selected st (i + !off) do incr off done;
        let adjust pos =
          if i + off' <= pos && pos <= i + !off then pos - off' else pos
        in
        st.playpos <- adjust st.playpos;
        st.playscroll <- adjust st.playscroll;
        st.playlist.(i + !off)
      );
    deselect_all st;
    st.playrange <- no_range;
    st.playsum <- sub_summary st.playsum st.playselsum;
    st.playsum <- 0.0, 0;
  )

let move_selected st d =
  if num_selected st > 0 then
  (
    let len = Array.length st.playlist in
    if d < 0 then
      for i = 0 to len - 1 do
        if i < -d then assert (not (is_selected st i)) else
        if is_selected st i then
        (
          let temp = st.playlist.(i) in
          for j = i - 1 downto i + d do
            st.playlist.(j + 1) <- st.playlist.(j)
          done;
          st.playlist.(i + d) <- temp;
          move_select st i (i + d);
          let adjust pos =
            if pos = i then i + d else
            if i + d <= pos && pos < i then pos + 1 else
            pos
          in
          st.playpos <- adjust st.playpos;
          st.playrange <- adjust (fst st.playrange), adjust (snd st.playrange);
        )
      done
    else
      for i = len - 1 downto 0 do
        if i >= len - d then assert (not (is_selected st i)) else
        if is_selected st i then
        (
          let temp = st.playlist.(i) in
          for j = i + 1 to i + d do
            st.playlist.(j - 1) <- st.playlist.(j)
          done;
          st.playlist.(i + d) <- temp;
          move_select st i (i + d);
          let adjust pos =
            if pos = i then i + d else
            if i < pos && pos <= i + d then pos - 1 else
            pos
          in
          st.playpos <- adjust st.playpos;
          st.playrange <- adjust (fst st.playrange), adjust (snd st.playrange);
        )
      done
  )
