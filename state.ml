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


let is_invalid track =
  match track.status with
  | `Invalid | `Absent -> true
  | `Det | `Predet | `Undet -> false


(* Control *)

type control =
{
  audio : Api.audio;
  mutable mute : bool;
  mutable volume : float;
  mutable sound : Api.sound;
  mutable current : track option;
  mutable timemode : [`Elapse | `Remain];
  mutable repeat : [`None | `One | `All];
  mutable loop : [`None | `A of time | `AB of time * time];
  mutable fps : bool;
}

let make_control audio =
  {
    audio;
    mute = false;
    volume = 0.5;
    sound = Api.Audio.silence audio;
    current = None;
    timemode = `Elapse;
    repeat = `None;
    loop = `None;
    fps = false;
  }


(* Playlist *)

type undo =
{
  undo_pos : int;
  undo_scroll : int;
  undo_tracks : track array;
  undo_selected : IntSet.t;
  undo_total : time * int;
  undo_total_selected : time * int;
}

type playlist =
{
  mutable tracks : track array;  (* external *)
  mutable shown : bool;  (* external *)
  mutable height : int;  (* external *)
  mutable rows : int;  (* external *)
  mutable scroll : int;  (* external *)
  mutable pos : int;  (* external *)
  mutable range : int * int;  (* external *)
  mutable selected : IntSet.t;
  mutable total : time * int;  (* r external *)
  mutable total_selected : time * int;  (* r external *)
  mutable shuffle_on : bool;
  mutable shuffle_tracks : int array;
  mutable shuffle_pos : int;
  mutable shuffle_unobserved : int;
  mutable undos : undo list ref;
  mutable redos : undo list ref;
}

let no_range = min_int, 0

let make_playlist () : playlist =
  {
    tracks = [||];
    shown = false;
    height = 200;
    rows = 4;
    scroll = 0;
    pos = 0;
    range = no_range;
    selected = IntSet.empty;
    total = 0.0, 0;
    total_selected = 0.0, 0;
    shuffle_on = false;
    shuffle_tracks = [||];
    shuffle_pos = 0;
    shuffle_unobserved = 0;
    undos = ref [];
    redos = ref [];
  }


(* Library *)

type library =
{
  mutable shown : bool;     (* external *)
  mutable width : int;      (* external *)
  mutable side : Api.side;  (* external *)
}

let make_library () : library =
  {
    shown = false;
    width = 600;
    side = `Right;
  }


(* Configuration *)

type config =
{
  mutable exec_tag : string;
  mutable exec_tag_max_len : int;
}

let make_config () : config =
  {
    exec_tag = "";
    exec_tag_max_len = 0;
  }


(* Overall State *)

type t =
{
  ui : Ui.t;
  control : control;
  playlist : playlist;
  library : library;
  config : config;
}

let make ui audio =
  {
    ui;
    control = make_control audio;
    playlist = make_playlist ();
    library = make_library ();
    config = make_config ();
  }


(* Validation *)

let log_file = "error.log"
let dumped_before = ref None
let dump = ref (fun _ -> assert false)

let check msg b = if b then [] else [msg]

let ok_control ctl =
  let length = Api.Audio.length ctl.audio ctl.sound in
  let played = Api.Audio.played ctl.audio ctl.sound in
  let playing = Api.Audio.is_playing ctl.audio ctl.sound in
  let paused = not playing && played > 0.0 in
  let stopped = not playing && not paused in
  let silence = ctl.sound = Api.Audio.silence ctl.audio in
  check "volume in range" (ctl.volume >= 0.0 && ctl.volume <= 1.0) @
  check "silence when no current track" (ctl.current <> None || silence) @
  check "stopped when no current track" (ctl.current <> None || stopped) @
  check "no loop when no current track"
    (ctl.current <> None || ctl.loop = `None) @
  check "lower loop boundary in range"
    (match ctl.loop with
    | `A t1
    | `AB (t1, _) -> t1 >= 0.0 && t1 <= length
    | _ -> true
    ) @
  check "upper loop boundary in range"
    (match ctl.loop with
    | `AB (t1, t2) -> t1 <= t2 && t2 <= length
    | _ -> true
    )

let ok_playlist pl =
  let len = Array.length pl.tracks in
  let shuffle_len = Array.length pl.shuffle_tracks in
  check "playlist position in range" (
    pl.pos = 0 ||
    pl.pos >= 0 && pl.pos < len
  ) @
  check "playlist row number in range" (pl.rows >= 3) @
  check "playlist scroll in range" (
    pl.scroll = 0 ||
    pl.scroll >= 0 && pl.scroll < len
  ) @
  check "playlist window height positive" (pl.height > 0) @
  check "selections in range"
    (IntSet.max_elt_opt pl.selected <= Some (len - 1)) @
  check "primary selection range position in range" (
    fst pl.range = min_int ||
    fst pl.range = max_int ||
    fst pl.range >= 0 && fst pl.range < len
  ) @
  check "secondary selection range position in range" (
    snd pl.range = 0 ||
    snd pl.range >= 0 && snd pl.range < len
  ) @
  check "playlist total in range"
    (fst pl.total >= 0.0 && snd pl.total <= len) @
  check "playlist selection total in range" (
    fst pl.total_selected >= 0.0 &&
    fst pl.total_selected <= fst pl.total &&
    snd pl.total_selected <= snd pl.total
  ) @
  check "shuffle empty when not shuffled"
    (pl.shuffle_on || pl.shuffle_tracks = [||]) @
  check "shuffle list has consistent length"
    (shuffle_len = len || shuffle_len = 0) @
  check "shuffle position in range" (
    pl.shuffle_pos = 0 ||
    pl.shuffle_pos >= 0 && pl.shuffle_pos < shuffle_len
  ) @
  check "shuffle observation in range" (
    pl.shuffle_unobserved = 0 && shuffle_len = 0 ||
    pl.shuffle_unobserved > pl.shuffle_pos &&
      pl.shuffle_unobserved <= shuffle_len
  )

let ok_library _lib =
  []

let ok_config _cfg =
  []

let ok st =
  match
    ok_control st.control @
    ok_playlist st.playlist @
    ok_library st.library @
    ok_config st.config @
    check "playlist empty when no current track"
      (st.control.current <> None || st.playlist.tracks = [||]);
  with
  | errors when errors <> [] && !dumped_before <> Some st ->
    let tm = Unix.(localtime (time ())) in
    let pre = Printf.sprintf
      "%04d-%02d-%02d %02d:%02d:%02d Invariant violated: "
      (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
      tm.tm_hour tm.tm_min tm.tm_sec
    in
    let s = String.concat "\n" (List.map ((^) pre) errors) ^ "\n" ^ !dump st in
    Out_channel.output_string stderr s;
    if !dumped_before = None then Storage.save log_file ignore;  (* clear log *)
    Storage.append log_file (fun file -> Out_channel.output_string file s);
    Out_channel.flush_all ();
    dumped_before := Some st;
  | _ -> ()
  


(* Track update queue *)

let queue = Safe_queue.create ()

let update_track audio track =
  if track.last_update >= 0.0 then
  (
    track.last_update <- -1.0;
    Safe_queue.add (audio, track) queue;
  )

let rec updater () =
  let audio, track = Safe_queue.take queue in
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
        let sound = Api.Audio.load audio track.path in
        track.time <-
          if sound = Api.Audio.silence audio then 0.0 else
          (
            let t = Api.Audio.length audio sound in
            Api.Audio.free audio sound;
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


(* Control *)

let eject_track ctl =
  Api.Audio.stop ctl.audio ctl.sound;
  ctl.current <- None;
  ctl.loop <- `None;
  if ctl.sound <> Api.Audio.silence ctl.audio then
  (
    Api.Audio.free ctl.audio ctl.sound;
    ctl.sound <- Api.Audio.silence ctl.audio;
  )

let switch_track ctl track play =
  eject_track ctl;
  ctl.sound <- Api.Audio.load ctl.audio track.path;
  ctl.current <- Some track;
  ctl.loop <- `None;
  track.time <-
    if ctl.sound = Api.Audio.silence ctl.audio then 0.0
    else Api.Audio.length ctl.audio ctl.sound;
  update_track ctl.audio track;
  Api.Audio.volume ctl.audio ctl.sound (if ctl.mute then 0.0 else ctl.volume);
  Api.Audio.play ctl.audio ctl.sound;
  if not play then Api.Audio.pause ctl.audio ctl.sound

let seek_track ctl percent =
  if ctl.sound <> Api.Audio.silence ctl.audio then
  (
    let length = Api.Audio.length ctl.audio ctl.sound in
    Api.Audio.seek ctl.audio ctl.sound (percent *. length)
  )

let adjust_track ctl track_opt =
  match ctl.current, track_opt with
  | None, Some track -> switch_track ctl track false
  | _, _ -> ()


(* Playlist Navigation *)

let modulo n m = let k = n mod m in if k < 0 then k + m else k

let current_opt pl = if pl.tracks = [||] then None else Some pl.tracks.(pl.pos)
let current pl = Option.get (current_opt pl)

let skip pl delta repeat =
  let len = Array.length pl.tracks in
  let up pos = if repeat then modulo (pos + delta) len else pos + delta in
  if not pl.shuffle_on then
    let pos = up pl.pos in
    let valid = pos >= 0 && pos < len in
    if valid then pl.pos <- pos;
    valid
  else
    let pos = up pl.shuffle_pos in
    let valid = pos >= 0 && pos < len in
    if valid then
    (
      pl.shuffle_pos <- pos;
      pl.shuffle_unobserved <- max pl.shuffle_unobserved (pos + 1);
      pl.pos <- pl.shuffle_tracks.(pos);
    );
    valid

let adjust_scroll pl pos =
  if pos < pl.scroll || pos >= pl.scroll + pl.rows then
    pl.scroll <- max 0 (min (Array.length pl.tracks - pl.rows)
      (pos - (pl.rows - 2)/2))


(* Playlist Total *)

let add_total (t1, n1) (t2, n2) = (t1 +. t2, n1 + n2)
let sub_total (t1, n1) (t2, n2) = (max 0.0 (t1 -. t2), max 0 (n1 - n2))

let track_total track =
  match track.status with
  | `Undet | `Invalid | `Absent -> 0.0, 1
  | `Predet | `Det -> track.time, 0

let update_total pl =
  pl.total <- 0.0, 0;
  pl.total_selected <- 0.0, 0;
  for i = 0 to Array.length pl.tracks - 1 do
    let total = track_total pl.tracks.(i) in
    pl.total <- add_total pl.total total;
    if IntSet.mem i pl.selected then
      pl.total_selected <- add_total pl.total_selected total;
  done


(* Playlist Shuffle *)

let unshuffle pl =
  pl.shuffle_on <- false;
  pl.shuffle_tracks <- [||];
  pl.shuffle_pos <- 0;
  pl.shuffle_unobserved <- 0

let swap a i j =
  let temp = a.(i) in a.(i) <- a.(j); a.(j) <- temp

let reshuffle pl =
  let len = Array.length pl.shuffle_tracks in
  for i = pl.shuffle_unobserved to len - 2 do
    swap pl.shuffle_tracks i (i + Random.int (len - i))
  done

let shuffle pl i_opt =
  let len = Array.length pl.tracks in
  pl.shuffle_on <- true;
  pl.shuffle_tracks <- Array.init len Fun.id;
  pl.shuffle_pos <- 0;
  pl.shuffle_unobserved <-
    (match i_opt with
    | Some i -> swap pl.shuffle_tracks 0 i; 1
    | None -> Bool.to_int (len > 0)
    );
  reshuffle pl;
  if len > 0 then pl.pos <- pl.shuffle_tracks.(0)


(* Playlist Selection *)

let num_selected pl = IntSet.cardinal pl.selected
let first_selected pl = IntSet.min_elt_opt pl.selected
let last_selected pl = IntSet.max_elt_opt pl.selected
let is_selected pl i = IntSet.mem i pl.selected

let select_all pl =
  for i = 0 to Array.length pl.tracks - 1 do
    pl.selected <- IntSet.add i pl.selected
  done;
  pl.total_selected <- pl.total

let deselect_all pl =
  pl.selected <- IntSet.empty;
  pl.total_selected <- 0.0, 0

let select_inv pl =
  let selected = pl.selected in
  deselect_all pl;
  for i = 0 to Array.length pl.tracks - 1 do
    if not (IntSet.mem i selected) then
    (
      pl.selected <- IntSet.add i pl.selected;
      pl.total_selected <-
        add_total pl.total_selected (track_total pl.tracks.(i));
    )
  done

let select pl i j =
  let i, j = min i j, max i j in
  for k = i to j do
    if not (IntSet.mem k pl.selected) then
    (
      pl.selected <- IntSet.add k pl.selected;
      pl.total_selected <-
        add_total pl.total_selected (track_total pl.tracks.(k))
    )
  done

let deselect pl i j =
  let i, j = min i j, max i j in
  for k = i to j do
    if IntSet.mem k pl.selected then
    (
      pl.selected <- IntSet.remove k pl.selected;
      pl.total_selected <-
        sub_total pl.total_selected (track_total pl.tracks.(k))
    )
  done


(* Playlist Undo *)

let undo_depth = 100

let make_undo pl =
  { undo_pos = pl.pos;
    undo_scroll = pl.scroll;
    undo_tracks = pl.tracks;
    undo_selected = pl.selected;
    undo_total = pl.total;
    undo_total_selected = pl.total_selected;
  }

let push_undo pl =
  if List.length !(pl.undos) >= undo_depth then
    pl.undos := List.filteri (fun i _ -> i < undo_depth - 1) !(pl.undos);
  pl.undos := make_undo pl :: !(pl.undos);
  pl.redos := []

let pop_unredo pl undos redos =
  match !undos with
  | [] -> ()
  | undo :: undos' ->
    redos := make_undo pl :: !redos;
    undos := undos';
    deselect_all pl;
    pl.pos <- undo.undo_pos;
    pl.scroll <- undo.undo_scroll;
    pl.tracks <- undo.undo_tracks;
    pl.selected <- undo.undo_selected;
    pl.total <- undo.undo_total;
    pl.total_selected <- undo.undo_total_selected;
    pl.range <- no_range

let pop_undo pl = pop_unredo pl pl.undos pl.redos
let pop_redo pl = pop_unredo pl pl.redos pl.undos


(* Playlist Manipulation *)

let move_pos pl i j len =
  let j' = min j (len - 1) in
  if i <> j' then
  (
    if i = pl.pos then
      pl.pos <- j';
    if i = fst pl.range then
      pl.range <- j', snd pl.range;
    if i = snd pl.range then
      pl.range <- fst pl.range, j';
  );
  if i <> j then
  (
    assert (not (IntSet.mem j pl.selected));
    if IntSet.mem i pl.selected then
      pl.selected <-
        IntSet.add j (IntSet.remove i pl.selected)
    (* Do not update shuffle list individually, since that would result in
     * quadratic complexity. *)
  )

let copy_selected pl =
  let d = ref 0 in
  Array.init (num_selected pl) (fun i ->
    while not (is_selected pl (i + !d)) do incr d done;
    pl.tracks.(i + !d)
  )


let insert pl pos tracks =
  if tracks <> [||] then
  (
    push_undo pl;
    let len = Array.length pl.tracks in
    let len' = Array.length tracks in
    if len = 0 then
    (
      pl.tracks <- tracks;
      if pl.shuffle_on then pl.shuffle_tracks <- Array.init len' Fun.id;
    )
    else
    (
      pl.tracks <-
        Array.init (len + len') (fun i ->
          if i < pos then pl.tracks.(i) else
          if i < pos + len' then tracks.(i - pos) else
          pl.tracks.(i - len')
        );
      for i = len - 1 downto pos do
        move_pos pl i (i + len') (len + len')
      done;
      if pl.shuffle_on then
        pl.shuffle_tracks <-
          Array.init (len + len') (fun i ->
            if i >= len then i - len + pos else
            let j = pl.shuffle_tracks.(i) in
            if j < pos then j else j + len'
          )
    );
    Array.iter (fun track ->
      pl.total <- add_total pl.total (track_total track)
    ) tracks;
    if pl.shuffle_on then reshuffle pl;
  )

let insert_paths pl pos paths audio =
  let tracks = ref [] in
  let add_track track =
    tracks := track :: !tracks;
    if track.status = `Undet then update_track audio track
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
  insert pl pos (Array.of_list (List.rev !tracks))


let remove_all pl =
  if pl.tracks <> [||] then
  (
    push_undo pl;
    deselect_all pl;
    pl.tracks <- [||];
    pl.pos <- 0;
    pl.scroll <- 0;
    pl.range <- no_range;
    pl.total <- 0.0, 0;
    pl.total_selected <- 0.0, 0;
    pl.shuffle_tracks <- [||];
    pl.shuffle_pos <- 0;
    pl.shuffle_unobserved <- 0;
  )

let remove_if p pl n =
  if n > 0 then
  (
    push_undo pl;
    let len = Array.length pl.tracks in
    let len' = len - n in
    let d = ref 0 in
    let js = Array.make len (-2) in
    let rec skip i =
      let j = i + !d in
      if j < len then
      (
        assert (js.(j) = -2);
        let b = p j in
        move_pos pl j i len';  (* could affect (p j)! *)
        if b then
        (
          pl.total <- sub_total pl.total (track_total pl.tracks.(j));
          deselect pl i i;
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
    let tracks' = Array.init len' (fun i -> skip i; pl.tracks.(i + !d)) in
    skip len';
    assert (len' + !d = len);
    pl.tracks <- tracks';
    pl.scroll <- max 0 (min (len' - 1) pl.scroll);
    if pl.shuffle_on then
    (
      let d = ref 0 in
      let rec skip i =
        if i + !d = pl.shuffle_pos then pl.shuffle_pos <- min i (len' - 1);
        if i + !d = pl.shuffle_unobserved then pl.shuffle_unobserved <- max i (pl.shuffle_pos + 1);
        if i + !d < len then
        (
          let j = js.(pl.shuffle_tracks.(i + !d)) in
          if j = -1 then (incr d; skip i) else j
        )
        else 0
      in
      let shuffle_tracks' = Array.init len' skip in
      ignore (skip len');
      pl.shuffle_tracks <- shuffle_tracks';
    );
  )

let remove_selected pl =
  remove_if (is_selected pl) pl (num_selected pl);
  pl.range <- no_range

let remove_unselected pl =
  remove_if (fun i -> not (is_selected pl i)) pl
    (Array.length pl.tracks - num_selected pl);
  pl.range <- 0, Array.length pl.tracks - 1


let num_invalid pl =
  Array.fold_left (fun n track -> n + Bool.to_int (is_invalid track)) 0 pl.tracks

let remove_invalid pl =
  remove_if (fun i -> is_invalid pl.tracks.(i)) pl (num_invalid pl);
  pl.range <-
    Option.value (first_selected pl) ~default: (fst no_range),
    Option.value (last_selected pl) ~default: (snd no_range)


let move_selected pl d =
  if num_selected pl > 0 then
  (
    push_undo pl;
    let len = Array.length pl.tracks in
    let js = Array.init len Fun.id in
    if d < 0 then
      for i = 0 to len - 1 do
        if is_selected pl i then
        (
          assert (i >= -d);
          let temp = pl.tracks.(i) in
          move_pos pl i (-1) len;  (* temp position *)
          for j = i - 1 downto i + d do
            pl.tracks.(j + 1) <- pl.tracks.(j);
            move_pos pl j (j + 1) len;
            js.(j) <- j + 1;
          done;
          move_pos pl (-1) (i + d) len;
          js.(i) <- i + d;
          pl.tracks.(i + d) <- temp;
        )
      done
    else
      for i = len - 1 downto 0 do
        if is_selected pl i then
        (
          assert (i < len - d);
          let temp = pl.tracks.(i) in
          move_pos pl i (-1) len;  (* temp position *)
          for j = i + 1 to i + d do
            pl.tracks.(j - 1) <- pl.tracks.(j);
            move_pos pl j (j - 1) len;
            js.(j) <- j - 1;
          done;
          move_pos pl (-1) (i + d) len;
          js.(i) <- i + d;
          pl.tracks.(i + d) <- temp;
        )
      done;
    Array.map_inplace (fun i -> js.(i)) pl.shuffle_tracks;
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
  output "volume = %.2f\n" st.control.volume;
  output "mute = %d\n" (Bool.to_int st.control.mute);
  output "play = %s\n" (match st.control.current with Some s -> s.path | None -> "");
  let length = Api.Audio.length st.control.audio st.control.sound in
  let played = Api.Audio.played st.control.audio st.control.sound in
  output "seek = %.4f\n" (if length > 0.0 then played /. length else 0.0);
  output "timemode = %d\n" (Bool.to_int (st.control.timemode = `Remain));
  output "shuffle = %d\n" (Bool.to_int st.playlist.shuffle_on);
  output "repeat = %d\n"
    (match st.control.repeat with
    | `None -> 0
    | `One -> 1
    | `All -> 2
    );
  let a, b =
    match st.control.loop with
    | `None -> -1.0, -1.0
    | `A t1 -> t1, -1.0
    | `AB tt -> tt
  in
  output "loop = %.4f, %.4f\n" a b;
  output "play_pos = %d\n" st.playlist.pos;
  output "play_scroll = %d\n" st.playlist.scroll;
  output "play_open = %d\n" (Bool.to_int st.playlist.shown);
  output "play_height = %d\n" st.playlist.height;
  output "lib_open = %d\n" (Bool.to_int st.library.shown);
  output "lib_width = %d\n" st.library.width;
  output "lib_side = %d\n" (Bool.to_int (st.library.side = `Right));
  Buffer.contents buf

let _ = dump := fun st ->
  let buf = Buffer.create 1024 in
  let pr fmt = Printf.bprintf buf fmt in
  pr "%s" (to_string st);
  pr "play_rows = %d\n" st.playlist.rows;
  pr "play_length = %d\n" (Array.length st.playlist.tracks);
  pr "play_selected = %d" (IntSet.cardinal st.playlist.selected);
  if st.playlist.selected <> IntSet.empty then
    pr " (%d-%d)"
      (IntSet.min_elt st.playlist.selected)
      (IntSet.max_elt st.playlist.selected);
  pr "\n";
  pr "play_range = %d, %d\n" (fst st.playlist.range) (snd st.playlist.range);
  pr "play_total = %.2f, %d\n" (fst st.playlist.total) (snd st.playlist.total);
  pr "play_total_selected = %.2f, %d\n"
    (fst st.playlist.total_selected) (snd st.playlist.total_selected);
  pr "play_shuffle_length = %d\n" (Array.length st.playlist.shuffle_tracks);
  pr "play_shuffle_pos = %d\n" st.playlist.shuffle_pos;
  pr "play_shuffle_unobserved = %d\n" st.playlist.shuffle_unobserved;
  pr "play_undo_length = %d\n" (List.length !(st.playlist.undos));
  pr "play_redo_length = %d\n" (List.length !(st.playlist.redos));
  pr "%!";
  Buffer.contents buf


let save_state st =
  Storage.save state_file (fun file ->
    Out_channel.output_string file (to_string st)
  );
  save_playlist st.playlist.tracks


let value = Fun.id
let bool x = x <> 0
let num l h x = max l (min h x)
let pair x y = x, y
let num_pair lx ly hx hy x y = num lx hx x, num ly hy y

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
    let x, y = input " win_pos = %d , %d " (num_pair 0 0 (sw - 20) (sh - 20)) in
    Api.Window.set_pos win x y;

    st.playlist.tracks <- load_playlist ();

    Ui.set_color_scheme st.ui
      (input " color_scheme = %d " (num 0 (Ui.num_color_scheme st.ui - 1)));
    st.control.volume <- input " volume = %f " (num 0.0 1.0);
    st.control.mute <- input " mute = %d " bool;
    let current = input " play = %[\x20-\xff]" String.trim in
    st.control.current <-
      if current = "" then None else Some (make_track current);
    let seek = input " seek = %f " (num 0.0 1.0) in
    st.control.timemode <-
      if input " timemode = %d " bool then `Remain else `Elapse;
    st.playlist.shuffle_on <- input " shuffle = %d " bool;
    if st.playlist.shuffle_on then shuffle st.playlist (Some st.playlist.pos);
    st.control.repeat <-
      (match input " repeat = %d " value with
      | 1 -> `One
      | 2 -> `All
      | _ -> `None
      );
    st.control.loop <-
      (match input " loop = %f, %f " pair with
      | t1, _t2 when t1 < 0.0 -> `None
      | t1, t2 when t2 < 0.0 -> `A t1
      | t1, t2 -> `AB (t1, max t1 t2)
      );

    let len = Array.length st.playlist.tracks - 1 in
    st.playlist.pos <- input " play_pos = %d " (num 0 (len - 1));
    if st.control.current = None && len > 0 then
      st.control.current <- Some st.playlist.tracks.(st.playlist.pos);
    if st.control.current <> None then
      switch_track st.control (Option.get st.control.current) false;
    seek_track st.control seek;

    st.playlist.scroll <- input " play_scroll = %d " (num 0 (len - 1));
    st.playlist.shown <- input " play_open = %d " bool;
    (* TODO: 83 = playlist_min; use constant *)
    st.playlist.height <- input " play_height = %d " (num 83 sh);
    st.library.shown <- input " lib_open = %d " bool;
    (* TODO: 400 = library_min; use constant *)
    st.library.width <- input " lib_width = %d " (num 400 sw);
    st.library.side <- if input " lib_side = %d " bool then `Right else `Left;
  );
  update_total st.playlist;
  Storage.load config_file (fun file ->
    let input fmt = fscanf file fmt in
    if input " [%s@]" value <> config_header then failwith "load_config";
    st.config.exec_tag <- input " exec_tag = %[\x20-\xff]" String.trim;
    st.config.exec_tag_max_len <- input " exec_tag_max_len = %d " (num 0 max_int);
  );
  ok st
