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

let name_separator = String.make 80 '-'
let name_of_path path =
  let file = Filename.basename path in
  if known_ext file then Filename.remove_extension file else file

let is_separator track = M3u.is_separator track.path

let make_track' path name time status =
  {path; name; time; status; last_update = 0.0}

let make_separator () = make_track' "separator://" name_separator 0.0 `Det

let make_track_predet path name time = make_track' path name time `Predet

let make_track path =
  if M3u.is_separator path then make_separator () else
  make_track' path (name_of_path path) 0.0 `Undet


(* State *)

type undo =
{
  undo_playlist_pos : int;
  undo_playlist_scroll : int;
  undo_playlist : track array;
  undo_playlist_selected : IntSet.t;
  undo_playlist_sum : time * int;
  undo_playlist_sum_selected : time * int;
}

type t =
{
  ui : Ui.t;
  audio : Api.audio;
  mutable fps : bool;
  mutable mute : bool;
  mutable volume : float;
  mutable sound : Api.sound;
  mutable current : track option;
  mutable timemode : [`Elapse | `Remain];
  mutable shuffled : bool;
  mutable repeat : [`None | `One | `All];
  mutable loop : [`None | `A of time | `AB of time * time];
  mutable playlist : track array;
  mutable playlist_open : bool;
  mutable playlist_height : int;
  mutable playlist_rows : int;
  mutable playlist_scroll : int;
  mutable playlist_pos : int;
  mutable playlist_range : int * int;
  mutable playlist_selected : IntSet.t;
  mutable playlist_sum : time * int;
  mutable playlist_sum_selected : time * int;
  mutable shuffle : int array;
  mutable shuffle_pos : int;
  mutable shuffle_unobserved : int;
  mutable library_open : bool;
  mutable library_width : int;
  mutable library_side : Api.side;
  mutable undos : undo list ref;
  mutable redos : undo list ref;
  mutable exec_tag : string;
  mutable exec_tag_max_len : int;
}

let no_range = min_int, 0

let make ui audio =
  let sound = Api.Audio.silence audio in
  {
    ui; audio; sound;
    fps = false;
    mute = false;
    volume = 0.5;
    current = None;
    timemode = `Elapse;
    shuffled = false;
    repeat = `None;
    loop = `None;
    playlist = [||];
    playlist_open = false;
    playlist_height = 200;
    playlist_rows = 4;
    playlist_scroll = 0;
    playlist_pos = 0;
    playlist_range = no_range;
    playlist_selected = IntSet.empty;
    playlist_sum = 0.0, 0;
    playlist_sum_selected = 0.0, 0;
    shuffle = [||];
    shuffle_pos = 0;
    shuffle_unobserved = 0;
    library_open = false;
    library_width = 600;
    library_side = `Right;
    undos = ref [];
    redos = ref [];
    exec_tag = "";
    exec_tag_max_len = 0;
  }


(* Validation *)

let log_file = "error.log"
let dumped_before = ref None
let dump = ref (fun _ -> assert false)

let check st msg b =
  if not (b || !dumped_before = Some st) then
  (
    let tm = Unix.(localtime (time ())) in
    let s = Printf.sprintf
      "%04d-%02d-%02d %02d:%02d:%02d Invariant violated: %s\n%s"
      (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
      tm.tm_hour tm.tm_min tm.tm_sec
      msg (!dump st)
    in
    Out_channel.output_string stderr s;
    if !dumped_before = None then Storage.save log_file ignore;  (* clear log *)
    Storage.append log_file (fun file -> Out_channel.output_string file s);
    Out_channel.flush_all ();
    dumped_before := Some st;
  )

let ok st =
  let length = Api.Audio.length st.audio st.sound in
  let played = Api.Audio.played st.audio st.sound in
  let playing = Api.Audio.is_playing st.audio st.sound in
  let paused = not playing && played > 0.0 in
  let stopped = not playing && not paused in
  let silence = st.sound = Api.Audio.silence st.audio in
  let len = Array.length st.playlist in
  check st "volume in range" (st.volume >= 0.0 && st.volume <= 1.0);
  check st "silence when no current track" (st.current <> None || silence);
  check st "stopped when no current track" (st.current <> None || stopped);
  check st "playlist empty when no current track"
    (st.current <> None || st.playlist = [||]);
  check st "playlist position in range" (
    st.playlist_pos = 0 && len = 0 ||
    st.playlist_pos >= 0 && st.playlist_pos < len
  );
  check st "playlist row number in range" (st.playlist_rows >= 3);
  check st "playlist scroll in range" (
    st.playlist_scroll = 0 && len = 0 ||
    st.playlist_scroll >= 0 && st.playlist_scroll < len
  );
  check st "playlist window height positive" (st.playlist_height > 0);
  check st "selections in range"
    (IntSet.max_elt_opt st.playlist_selected <= Some (len - 1));
  check st "primary selection range position in range" (
    fst st.playlist_range = min_int ||
    fst st.playlist_range = max_int ||
    fst st.playlist_range >= 0 && fst st.playlist_range < len
  );
  check st "secondary selection range position in range" (
    snd st.playlist_range = 0 ||
    snd st.playlist_range >= 0 && snd st.playlist_range < len
  );
  check st "playlist summary in range"
    (fst st.playlist_sum >= 0.0 && snd st.playlist_sum <= len);
  check st "playlist selection summary in range" (
    fst st.playlist_sum_selected >= 0.0 &&
    fst st.playlist_sum_selected <= fst st.playlist_sum &&
    snd st.playlist_sum_selected <= snd st.playlist_sum
  );
  check st "no loop when no current track"
    (st.current <> None || st.loop = `None);
  check st "lower loop boundary in range"
    (match st.loop with `A t1 | `AB (t1, _) -> t1 >= 0.0 && t1 <= length | _ -> true);
  check st "upper loop boundary in range"
    (match st.loop with `AB (t1, t2) -> t1 <= t2 && t2 <= length | _ -> true);
  check st "no shuffle list when not shuffled"
    (st.shuffled || st.shuffle = [||]);
  check st "no shuffle position when not shuffled"
    (st.shuffled || st.shuffle_pos = 0);
  check st "no shuffle observation when not shuffled"
    (st.shuffled || st.shuffle_unobserved = 0);
  check st "shuffle list has consistent length"
    (not st.shuffled || Array.length st.shuffle = len);
  check st "shuffle position in range" (
    st.shuffle_pos = 0 && st.shuffle = [||] ||
    st.shuffle_pos >= 0 && st.shuffle_pos < len
  );
  check st "shuffle observation in range" (
    st.shuffle_unobserved = 0 && st.shuffle = [||] ||
    st.shuffle_unobserved > st.shuffle_pos && st.shuffle_unobserved <= len
  );
  ()


(* Scrolling *)

let scroll_to_view st pos =
  if pos < st.playlist_scroll
  || pos >= st.playlist_scroll + st.playlist_rows then
    st.playlist_scroll <- max 0 (min (Array.length st.playlist - st.playlist_rows)
      (pos - (st.playlist_rows - 2)/2))


(* Playlist Summary *)

let add_summary (t1, n1) (t2, n2) = (t1 +. t2, n1 + n2)
let sub_summary (t1, n1) (t2, n2) = (max 0.0 (t1 -. t2), max 0 (n1 - n2))

let track_summary track =
  match track.status with
  | `Undet | `Invalid | `Absent -> 0.0, 1
  | `Predet | `Det -> track.time, 0

let update_summary st =
  st.playlist_sum <- 0.0, 0;
  st.playlist_sum_selected <- 0.0, 0;
  for i = 0 to Array.length st.playlist - 1 do
    let sum = track_summary st.playlist.(i) in
    st.playlist_sum <- add_summary st.playlist_sum sum;
    if IntSet.mem i st.playlist_selected then
      st.playlist_sum_selected <- add_summary st.playlist_sum_selected sum;
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
  if M3u.is_separator track.path then
  (
    track.status <- `Det;
    track.time <- 0.0;
    track.name <- name_separator;
  )
  else if not (Sys.file_exists track.path) then
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
  st.loop <- `None;
  track.time <-
    if st.sound = Api.Audio.silence st.audio then 0.0
    else Api.Audio.length st.audio st.sound;
  update_track st track;
  Api.Audio.volume st.audio st.sound (if st.mute then 0.0 else st.volume);
  Api.Audio.play st.audio st.sound;
  if not play then Api.Audio.pause st.audio st.sound;
  scroll_to_view st st.playlist_pos

let seek_track st percent =
  if st.sound <> Api.Audio.silence st.audio then
  (
    let length = Api.Audio.length st.audio st.sound in
    Api.Audio.seek st.audio st.sound (percent *. length)
  )


(* Shuffle *)

let unshuffle st =
  st.shuffled <- false;
  st.shuffle <- [||];
  st.shuffle_pos <- 0;
  st.shuffle_unobserved <- 0

let swap a i j =
  let temp = a.(i) in a.(i) <- a.(j); a.(j) <- temp

let reshuffle st =
  let len = Array.length st.shuffle in
  for i = st.shuffle_unobserved to len - 2 do
    swap st.shuffle i (i + Random.int (len - i))
  done

let shuffle st i_opt =
  st.shuffled <- true;
  st.shuffle <- Array.init (Array.length st.playlist) Fun.id;
  st.shuffle_pos <- 0;
  st.shuffle_unobserved <-
    (match i_opt with
    | None -> 0
    | Some i -> swap st.shuffle 0 i; 1
    );
  reshuffle st


(* Playlist Selection *)

let num_selected st = IntSet.cardinal st.playlist_selected
let first_selected st = IntSet.min_elt_opt st.playlist_selected
let last_selected st = IntSet.max_elt_opt st.playlist_selected
let is_selected st i = IntSet.mem i st.playlist_selected

let select_all st =
  for i = 0 to Array.length st.playlist - 1 do
    st.playlist_selected <- IntSet.add i st.playlist_selected
  done;
  st.playlist_sum_selected <- st.playlist_sum

let deselect_all st =
  st.playlist_selected <- IntSet.empty;
  st.playlist_sum_selected <- 0.0, 0

let select_inv st =
  let s = st.playlist_selected in
  deselect_all st;
  for i = 0 to Array.length st.playlist - 1 do
    if not (IntSet.mem i s) then
    (
      st.playlist_selected <- IntSet.add i st.playlist_selected;
      st.playlist_sum_selected <-
        add_summary st.playlist_sum_selected (track_summary st.playlist.(i));
    )
  done

let select st i j =
  let i, j = min i j, max i j in
  for k = i to j do
    if not (IntSet.mem k st.playlist_selected) then
    (
      st.playlist_selected <- IntSet.add k st.playlist_selected;
      st.playlist_sum_selected <-
        add_summary st.playlist_sum_selected (track_summary st.playlist.(k))
    )
  done

let deselect st i j =
  let i, j = min i j, max i j in
  for k = i to j do
    if IntSet.mem k st.playlist_selected then
    (
      st.playlist_selected <- IntSet.remove k st.playlist_selected;
      st.playlist_sum_selected <-
        sub_summary st.playlist_sum_selected (track_summary st.playlist.(k))
    )
  done


(* Playlist Undo *)

let undo_depth = 100

let make_undo st =
  { undo_playlist_pos = st.playlist_pos;
    undo_playlist_scroll = st.playlist_scroll;
    undo_playlist = st.playlist;
    undo_playlist_selected = st.playlist_selected;
    undo_playlist_sum = st.playlist_sum;
    undo_playlist_sum_selected = st.playlist_sum_selected;
  }

let push_undo st =
  if List.length !(st.undos) >= undo_depth then
    st.undos := List.filteri (fun i _ -> i < undo_depth - 1) !(st.undos);
  st.undos := make_undo st :: !(st.undos);
  st.redos := []

let pop_unredo st undos redos =
  match !undos with
  | [] -> ()
  | undo :: undos' ->
    redos := make_undo st :: !redos;
    undos := undos';
    deselect_all st;
    st.playlist_pos <- undo.undo_playlist_pos;
    st.playlist_scroll <- undo.undo_playlist_scroll;
    st.playlist <- undo.undo_playlist;
    st.playlist_selected <- undo.undo_playlist_selected;
    st.playlist_sum <- undo.undo_playlist_sum;
    st.playlist_sum_selected <- undo.undo_playlist_sum_selected;
    st.playlist_range <- no_range;
    if st.current = None && st.playlist <> [||] then
      st.current <- Some st.playlist.(st.playlist_pos)

let pop_undo st = pop_unredo st st.undos st.redos
let pop_redo st = pop_unredo st st.redos st.undos


(* Playlist Manipulation *)

let move_pos st i j len =
  let j' = min j (len - 1) in
  if i <> j' then
  (
    if i = st.playlist_pos then
      st.playlist_pos <- j';
    if i = fst st.playlist_range then
      st.playlist_range <- j', snd st.playlist_range;
    if i = snd st.playlist_range then
      st.playlist_range <- fst st.playlist_range, j';
  );
  if i <> j then
  (
    assert (not (IntSet.mem j st.playlist_selected));
    if IntSet.mem i st.playlist_selected then
      st.playlist_selected <-
        IntSet.add j (IntSet.remove i st.playlist_selected)
    (* Do not update shuffle list individually, since that would result in
     * quadratic complexity. *)
  )

let copy_selected st =
  let d = ref 0 in
  Array.init (num_selected st) (fun i ->
    while not (is_selected st (i + !d)) do incr d done;
    st.playlist.(i + !d)
  )


let insert st pos tracks =
  if tracks <> [||] then
  (
    push_undo st;
    let len = Array.length st.playlist in
    let len' = Array.length tracks in
    if len = 0 then
    (
      st.playlist <- tracks;
      if st.shuffled then st.shuffle <- Array.init len' Fun.id;
      if st.current = None then
        switch_track st st.playlist.(0) false;
    )
    else
    (
      st.playlist <-
        Array.init (len + len') (fun i ->
          if i < pos then st.playlist.(i) else
          if i < pos + len' then tracks.(i - pos) else
          st.playlist.(i - len')
        );
      for i = len - 1 downto pos do
        move_pos st i (i + len') (len + len')
      done;
      if st.shuffled then
        st.shuffle <-
          Array.init (len + len') (fun i ->
            if i >= len then i - len + pos else
            let j = st.shuffle.(i) in
            if j < pos then j else j + len'
          )
    );
    for i = 0 to Array.length tracks - 1 do
      let sum = track_summary tracks.(i) in
      st.playlist_sum <- add_summary st.playlist_sum sum
    done;
    if st.shuffled then reshuffle st;
  )

let insert_paths st pos paths =
  let tracks = ref [] in
  let add_track track =
    tracks := track :: !tracks;
    if track.status = `Undet then update_track st track
  in
  let add_song path = add_track (make_track path) in
  let add_playlist path =
    let s = In_channel.(with_open_bin path input_all) in
    List.iter (fun M3u.{path; info} ->
      let track =
        match info with
        | None -> make_track path
        | Some {title; time} -> make_track_predet path title (float time)
      in add_track track
    ) (M3u.parse_ext s)
  in
  let rec add_path path =
    try
      match String.lowercase_ascii (Filename.extension path) with
      | _ when Sys.file_exists path && Sys.is_directory path ->
        Array.iter (fun file ->
          add_path (Filename.concat path file)
        ) (Sys.readdir path)
      | ".m3u" | ".m3u8" -> add_playlist path
      | _ -> add_song path
    with Sys_error _ -> add_song path
  in
  List.iter add_path paths;
  insert st pos (Array.of_list (List.rev !tracks))


let remove_all st =
  if st.playlist <> [||] then
  (
    push_undo st;
    deselect_all st;
    st.playlist <- [||];
    st.playlist_pos <- 0;
    st.playlist_scroll <- 0;
    st.playlist_range <- no_range;
    st.playlist_sum <- 0.0, 0;
    st.playlist_sum_selected <- 0.0, 0;
    st.shuffle <- [||];
    st.shuffle_pos <- 0;
    st.shuffle_unobserved <- 0;
    if st.current <> None && st.sound = Api.Audio.silence st.audio then
      eject_track st;
  )

let remove_if p st n =
  if n > 0 then
  (
    push_undo st;
    let len = Array.length st.playlist in
    let len' = len - n in
    let d = ref 0 in
    let js = Array.make len (-2) in
    let rec skip i =
      let j = i + !d in
      if j < len then
      (
        assert (js.(j) = -2);
        let b = p j in
        move_pos st j i len';  (* could affect (p j)! *)
        if b then
        (
          st.playlist_sum <-
            sub_summary st.playlist_sum (track_summary st.playlist.(j));
          deselect st i i;
          incr d;
          js.(j) <- -1;
          skip i;
        )
        else
        (
          js.(j) <- i;
        )
      )
    in
    let playlist' = Array.init len' (fun i -> skip i; st.playlist.(i + !d)) in
    skip len';
    assert (len' + !d = len);
    st.playlist <- playlist';
    st.playlist_scroll <- max 0 (min (len' - 1) st.playlist_scroll);
    if st.shuffled then
    (
      let d = ref 0 in
      let rec skip i =
        let j = js.(st.shuffle.(i + !d)) in
        if j = -1 then (incr d; skip i) else j
      in
      st.shuffle <- Array.init len' skip;
    );
    if st.current <> None && st.sound = Api.Audio.silence st.audio then
      if len' = 0 then
        eject_track st
      else 
        switch_track st st.playlist.(st.playlist_pos) false;
  )

let remove_selected st =
  remove_if (is_selected st) st (num_selected st);
  st.playlist_range <- no_range

let remove_unselected st =
  remove_if (fun i -> not (is_selected st i)) st
    (Array.length st.playlist - num_selected st);
  st.playlist_range <- 0, Array.length st.playlist - 1


let is_invalid track =
  match track.status with
  | `Invalid | `Absent -> true
  | `Det | `Predet | `Undet -> false

let num_invalid st =
  Array.fold_left (fun n track -> n + Bool.to_int (is_invalid track)) 0 st.playlist

let remove_invalid st =
  remove_if (fun i -> is_invalid st.playlist.(i)) st (num_invalid st);
  st.playlist_range <-
    Option.value (first_selected st) ~default: (fst no_range),
    Option.value (last_selected st) ~default: (snd no_range)


let move_selected st d =
  if num_selected st > 0 then
  (
    push_undo st;
    let len = Array.length st.playlist in
    let js = Array.init len Fun.id in
    if d < 0 then
      for i = 0 to len - 1 do
        if is_selected st i then
        (
          assert (i >= -d);
          let temp = st.playlist.(i) in
          move_pos st i (-1) len;  (* temp position *)
          for j = i - 1 downto i + d do
            st.playlist.(j + 1) <- st.playlist.(j);
            move_pos st j (j + 1) len;
            js.(j) <- j + 1;
          done;
          move_pos st (-1) (i + d) len;
          js.(i) <- i + d;
          st.playlist.(i + d) <- temp;
        )
      done
    else
      for i = len - 1 downto 0 do
        if is_selected st i then
        (
          assert (i < len - d);
          let temp = st.playlist.(i) in
          move_pos st i (-1) len;  (* temp position *)
          for j = i + 1 to i + d do
            st.playlist.(j - 1) <- st.playlist.(j);
            move_pos st j (j - 1) len;
            js.(j) <- j - 1;
          done;
          move_pos st (-1) (i + d) len;
          js.(i) <- i + d;
          st.playlist.(i + d) <- temp;
        )
      done;
    Array.map_inplace (fun i -> js.(i)) st.shuffle;
  )


(* Persist Playlists *)

let string_of_playlist tracks =
  List.init (Array.length tracks) (fun i ->
    let time = int_of_float tracks.(i).time in
    let info =
      if time = 0 then None else Some M3u.{time; title = tracks.(i).name} in
    M3u.{path = tracks.(i).path; info}
  ) |> M3u.make_ext

let playlist_of_string s =
  Array.map (fun (item : M3u.item) ->
    match item.info with
    | None -> make_track item.path
    | Some info -> make_track_predet item.path info.title (float info.time)
  ) (Array.of_list (M3u.parse_ext s))

let playlist_file = "playlist.m3u"

let save_playlist tracks =
  Storage.save playlist_file (fun file ->
    Out_channel.output_string file (string_of_playlist tracks)
  )

let load_playlist () =
  let tracks = ref [||] in
  Storage.load playlist_file (fun file ->
    tracks := playlist_of_string (In_channel.input_all file)
  );
  !tracks


(* Persist State *)

let min_w = 360  (* TODO: avoid duplication *)
let min_h = 160

let state_file = "state.conf"
let config_file = "config.conf"

let state_header = App.name ^ " state"
let config_header = App.name ^ " config"

let to_string st =
  let buf = Buffer.create 1024 in
  let output fmt  = Printf.bprintf buf fmt in
  output "[%s]\n" state_header;
  let x, y = Ui.window_pos st.ui in
  output "win_pos = %d, %d\n" x y;
  output "color_scheme = %d\n" (Ui.get_color_scheme st.ui);
  output "volume = %.2f\n" st.volume;
  output "mute = %d\n" (Bool.to_int st.mute);
  output "play = %s\n" (match st.current with Some s -> s.path | None -> "");
  let length = Api.Audio.length st.audio st.sound in
  let played = Api.Audio.played st.audio st.sound in
  output "seek = %.4f\n" (if length > 0.0 then played /. length else 0.0);
  output "timemode = %d\n" (Bool.to_int (st.timemode = `Remain));
  output "shuffle = %d\n" (Bool.to_int st.shuffled);
  output "repeat = %d\n"
    (match st.repeat with `None -> 0 | `One -> 1 | `All -> 2);
  let a, b =
    match st.loop with `None -> -1.0, -1.0 | `A t1 -> t1, -1.0 | `AB tt -> tt
  in
  output "loop = %.4f, %.4f\n" a b;
  output "play_pos = %d\n" st.playlist_pos;
  output "play_scroll = %d\n" st.playlist_scroll;
  output "play_open = %d\n" (Bool.to_int st.playlist_open);
  output "play_height = %d\n" st.playlist_height;
  output "lib_open = %d\n" (Bool.to_int st.library_open);
  output "lib_width = %d\n" st.library_width;
  output "lib_side = %d\n" (Bool.to_int (st.library_side = `Right));
  Buffer.contents buf

let _ = dump := fun st ->
  let buf = Buffer.create 1024 in
  let pr fmt = Printf.bprintf buf fmt in
  pr "%s" (to_string st);
  pr "play_rows = %d\n" st.playlist_rows;
  pr "play_length = %d\n" (Array.length st.playlist);
  pr "play_selected = %d" (IntSet.cardinal st.playlist_selected);
  if st.playlist_selected <> IntSet.empty then
    pr " (%d-%d)"
      (IntSet.min_elt st.playlist_selected)
      (IntSet.max_elt st.playlist_selected);
  pr "\n";
  pr "play_range = %d, %d\n" (fst st.playlist_range) (snd st.playlist_range);
  pr "play_sum = %.2f, %d\n" (fst st.playlist_sum) (snd st.playlist_sum);
  pr "play_sum_selected = %.2f, %d\n"
    (fst st.playlist_sum_selected) (snd st.playlist_sum_selected);
  pr "shuffle_length = %d\n" (Array.length st.shuffle);
  pr "shuffle_pos = %d\n" st.shuffle_pos;
  pr "shuffle_unobserved = %d\n" st.shuffle_unobserved;
  pr "undo_length = %d\n" (List.length !(st.undos));
  pr "redo_length = %d\n" (List.length !(st.redos));
  pr "%!";
  Buffer.contents buf


let save_state st =
  Storage.save state_file (fun file ->
    Out_channel.output_string file (to_string st)
  );
  save_playlist st.playlist


let value = Fun.id
let pair x y = x, y

let clamp l h x = max l (min h x)
let clamp_pair lx ly hx hy (x, y) = clamp lx hx x, clamp ly hy y

let fscanf file =
  match In_channel.input_line file with
  | None -> raise End_of_file
  | Some s -> Scanf.sscanf s

let load_state st =
  Storage.load state_file (fun file ->
    let win = Ui.window st.ui in
    let input fmt = fscanf file fmt in
    if input " [%s@]" value <> state_header then failwith "load_state";
    let sw, sh = Api.Window.screen_size win in
    let x, y = clamp_pair 0 0 (sw - 20) (sh - 20)
      (input " win_pos = %d , %d " pair) in
    Api.Window.set_pos win x y;

    st.playlist <- load_playlist ();

    Ui.set_color_scheme st.ui (clamp 0 (Ui.num_color_scheme st.ui - 1)
      (input " color_scheme = %d " value));
    st.volume <- clamp 0.0 1.0 (input " volume = %f " value);
    st.mute <- (0 <> input " mute = %d " value);
    let current = String.trim (input " play = %[\x20-\xff]" value) in
    st.current <- if current = "" then None else Some (make_track current);
    let seek = clamp 0.0 1.0 (input " seek = %f " value) in
    st.timemode <-
      if input " timemode = %d " value = 0 then `Elapse else `Remain;
    st.shuffled <- (0 <> input " shuffle = %d " value);
    if st.shuffled then shuffle st (Some st.playlist_pos);
    st.repeat <-
      (match input " repeat = %d " value with 1 -> `One | 2 -> `All | _ -> `None);
    st.loop <-
      (match input " loop = %f, %f " pair with
      | t1, _t2 when t1 < 0.0 -> `None
      | t1, t2 when t2 < 0.0 -> `A t1
      | t1, t2 -> `AB (t1, max t1 t2)
      );

    let len = Array.length st.playlist - 1 in
    st.playlist_pos <- clamp 0 (len - 1) (input " play_pos = %d " value);
    if st.current = None && len > 0 then
      st.current <- Some st.playlist.(st.playlist_pos);
    if st.current <> None then
      switch_track st (Option.get st.current) false;
    seek_track st seek;

    st.playlist_scroll <- clamp 0 (len - 1) (input " play_scroll = %d " value);
    st.playlist_open <- (0 <> input " play_open = %d " value);
    (* TODO: 83 = playlist_min; use constant *)
    st.playlist_height <- clamp 83 sh (input " play_height = %d " value);
    st.library_open <- (0 <> input " lib_open = %d " value);
    (* TODO: 400 = library_min; use constant *)
    st.library_width <- clamp 400 sw (input " lib_width = %d " value);
    st.library_side <-
      (if (input " lib_side = %d " value) = 0 then `Left else `Right);
  );
  update_summary st;
  Storage.load config_file (fun file ->
    let input fmt = fscanf file fmt in
    if input " [%s@]" value <> config_header then failwith "load_config";
    st.exec_tag <- String.trim (input " exec_tag = %[\x20-\xff]" value);
    st.exec_tag_max_len <- max 0 (input " exec_tag_max_len = %d " value);
  );
  ok st
