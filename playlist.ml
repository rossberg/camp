(* Playlist *)

open Audio_file

type path = Data.path
type time = Data.time
type track = Data.track

type shuffle =
{
  mutable tracks : int array;
  mutable pos : int option;
  mutable unobserved : int;
}

type t =
{
  table : track Table.t;
  mutable total : time * int;
  mutable total_selected : time * int;
  mutable shuffle : shuffle option;
}


(* Constructor *)

let make () =
  {
    table = Table.make 100;
    total = 0.0, 0;
    total_selected = 0.0, 0;
    shuffle = None;
  }


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]

let ok pl =
  let len = Array.length pl.table.entries in
  Table.ok "playlist" pl.table @
  check "playlist total in range"
    (fst pl.total >= 0.0 && snd pl.total <= len) @
  check "playlist selection total in range"
    (fst pl.total_selected >= 0.0 && snd pl.total_selected <= len) @
  (match pl.shuffle with
  | None -> []
  | Some shuffle ->
    let shuffle_len = Array.length shuffle.tracks in
    check "shuffle list has consistent length" (shuffle_len = len) @
    check "shuffle position in range" (
      shuffle.pos = None && shuffle_len = 0 ||
      Option.get shuffle.pos >= 0 && Option.get shuffle.pos < shuffle_len
    ) @
    check "shuffle observation in range" (
      shuffle.unobserved = 0 && shuffle_len = 0 ||
      shuffle.unobserved > Option.get shuffle.pos &&
        shuffle.unobserved <= shuffle_len
    )
  )


(* Accessors *)

let length pl = Table.length pl.table
let current pl = Table.current pl.table
let current_opt pl = Table.current_opt pl.table

let focus pl = pl.table.focus <- true
let defocus pl = pl.table.focus <- false
let adjust_scroll pl fit = Table.adjust_scroll pl.table pl.table.pos fit


(* Total *)

let add_total (t1, n1) (t2, n2) = (t1 +. t2, n1 + n2)
let sub_total (t1, n1) (t2, n2) = (max 0.0 (t1 -. t2), max 0 (n1 - n2))

let track_total (track : track) =
  match track.status with
  | `Undet | `Invalid | `Absent -> 0.0, 1
  | `Predet | `Det ->
    match track.format with
    | Some format when format.time <> 0.0 -> format.time, 0
    | _ -> 0.0, 1

let range_total pl i j =
  let total = ref (0.0, 0) in
  let total_selected = ref (0.0, 0) in
  for i = i to j do
    let track_total = track_total pl.table.entries.(i) in
    total := add_total !total track_total;
    if Table.IntSet.mem i pl.table.selected then
      total_selected := add_total !total_selected track_total;
  done;
  !total, !total_selected

let update_total pl =
  let total, total_sel = range_total pl 0 (Array.length pl.table.entries - 1) in
  pl.total <- total;
  pl.total_selected <- total_sel

let update_total_selected pl =
  pl.total_selected <-
    Table.IntSet.fold (fun i total ->
      add_total total (track_total pl.table.entries.(i))
    ) pl.table.selected (0.0, 0)


(* Navigation *)

let modulo n m = let k = n mod m in if k < 0 then k + m else k

let skip pl delta repeat =
  let len = Array.length pl.table.entries in
  len > 0 &&  (* implies pl.pos <> None *)
  let up pos = if repeat then modulo (pos + delta) len else pos + delta in
  match pl.shuffle with
  | None ->
    let pos = up (Option.get pl.table.pos) in
    let valid = pos >= 0 && pos < len in
    if valid then pl.table.pos <- Some pos;
    valid
  | Some shuffle ->
    let pos = up (Option.get shuffle.pos) in
    let valid = pos >= 0 && pos < len in
    if valid then
    (
      shuffle.pos <- Some pos;
      shuffle.unobserved <- max shuffle.unobserved (pos + 1);
      pl.table.pos <- Some shuffle.tracks.(pos);
    );
    valid


(* Shuffle *)

let swap a i j =
  let temp = a.(i) in a.(i) <- a.(j); a.(j) <- temp

let unshuffle pl =
  pl.shuffle <- None

let reshuffle pl =
  let sh = Option.get pl.shuffle in
  let len = Array.length sh.tracks in
  for i = sh.unobserved to len - 2 do
    swap sh.tracks i (i + Random.int (len - i))
  done

let shuffle pl i_opt =
  let len = Array.length pl.table.entries in
  let tracks = Array.init len Fun.id in
  let shuffle =
    { tracks;
      pos = if len > 0 then Some 0 else None;
      unobserved =
        match i_opt with
        | Some i -> swap tracks 0 i; 1
        | None -> 0
    }
  in
  pl.shuffle <- Some shuffle;
  reshuffle pl;
  if len > 0 then
  (
    pl.table.pos <- Some tracks.(0);
    shuffle.unobserved <- 1;
  )

let shuffle_next pl i =
  let sh = Option.get pl.shuffle in
  let pos = Option.get (Array.find_index ((=) i) sh.tracks) in
  if pos < sh.unobserved then
    sh.pos <- Some pos
  else
  (
    (* Minimise new observation to one *)
    swap sh.tracks pos sh.unobserved;
    sh.pos <- Some sh.unobserved;
    sh.unobserved <- sh.unobserved + 1;
  )


(* Selection *)

let has_selection pl = Table.has_selection pl.table
let num_selected pl = Table.num_selected pl.table
let first_selected pl = Table.first_selected pl.table
let last_selected pl = Table.last_selected pl.table
let is_selected pl i = Table.is_selected pl.table i
let selected pl = Table.selected pl.table

let select_all pl =
  Table.select_all pl.table;
  pl.total_selected <- pl.total

let deselect_all pl =
  Table.deselect_all pl.table;
  pl.total_selected <- 0.0, 0

let select_invert pl =
  Table.select_invert pl.table;
  update_total_selected pl

let select pl i0 j0 =
  let i, j = min i0 j0, max i0 j0 in
  let _, prev = range_total pl i j in
  Table.select pl.table i0 j0;
  let _, current = range_total pl i j in
  pl.total_selected <- add_total (sub_total pl.total_selected prev) current

let deselect pl i0 j0 =
  let i, j = min i0 j0, max i0 j0 in
  let _, prev = range_total pl i j in
  Table.deselect pl.table i0 j0;
  let _, current = range_total pl i j in
  pl.total_selected <- add_total (sub_total pl.total_selected prev) current


(* Undo *)

let pop_unredo pl f list =
  if !list <> [] then
  (
    let shuffled = pl.shuffle <> None in
    unshuffle pl;  (* prevent nastiness *)
    f pl.table;
    if shuffled then shuffle pl pl.table.pos;
    update_total pl;
  )

let pop_undo pl = pop_unredo pl Table.pop_undo pl.table.undos
let pop_redo pl = pop_unredo pl Table.pop_redo pl.table.redos


(* Editing *)

let insert pl pos tracks =
  if tracks <> [||] then
  (
    let len = Table.length pl.table in
    let len' = Array.length tracks in
    Table.insert pl.table pos tracks;
    pl.total <- add_total pl.total (fst (range_total pl pos (pos + len' - 1)));
    Option.iter (fun shuffle ->
      shuffle.tracks <-
        Array.init (len + len') (fun i ->
          if i >= len then i - len + pos else
          let j = shuffle.tracks.(i) in
          if j < pos then j else j + len'
        );
      reshuffle pl;
      if shuffle.pos = None then shuffle.pos <- Some 0;
      if len = 0 then shuffle.unobserved <- 1;
    ) pl.shuffle;
  )

let insert_paths pl pos paths =
  let tracks = ref [] in
  let add_track (track : track) =
    tracks := track :: !tracks;
    if track.status = `Undet then Track.update track
  in
  let add_playlist path =
    let s = In_channel.(with_open_bin path input_all) in
    List.iter (fun item -> add_track (Track.of_m3u_item item)) (M3u.parse_ext s)
  in
  let rec add_path path =
    try
      if Sys.file_exists path && Sys.is_directory path then
        Array.iter (fun file ->
          add_path (Filename.concat path file)
        ) (Sys.readdir path)
      else if Data.is_playlist_path path then
        add_playlist path
      else if Data.is_track_path path then
        add_track (Data.make_track path)
    with Sys_error _ -> ()
  in
  List.iter add_path paths;
  insert pl pos (Array.of_list (List.rev !tracks))


let remove_all pl =
  if pl.table.entries <> [||] then
  (
    Table.remove_all pl.table;
    pl.total <- 0.0, 0;
    pl.total_selected <- 0.0, 0;
    Option.iter (fun shuffle ->
      shuffle.unobserved <- 0;
      shuffle.pos <- None;
      shuffle.tracks <- [||];
    ) pl.shuffle
  )

let remove_if p pl n =
  if n > 0 then
  (
    let len = Table.length pl.table in
    let len' = len - n in
    let js = Table.remove_if p pl.table n in
    update_total pl;
    Option.iter (fun shuffle ->
      let d = ref 0 in
      let rec skip i =
        if Some (i + !d) = shuffle.pos then
          shuffle.pos <- Some (min i (len' - 1));
        if i + !d = shuffle.unobserved then
          shuffle.unobserved <- max i (Option.get shuffle.pos + 1);
        if i + !d < len then
        (
          let j = js.(shuffle.tracks.(i + !d)) in
          if j = -1 then (incr d; skip i) else j
        )
        else 0
      in
      let shuffle_tracks' = Array.init len' skip in
      ignore (skip len');
      shuffle.tracks <- shuffle_tracks';
    ) pl.shuffle
  )

let remove_selected pl =
  remove_if (Table.is_selected pl.table) pl (Table.num_selected pl.table)

let remove_unselected pl =
  remove_if (fun i -> not (Table.is_selected pl.table i)) pl
    (Table.length pl.table - Table.num_selected pl.table)

let num_invalid pl =
  Array.fold_left (fun n track ->
    n + Bool.to_int (Data.is_invalid track)
  ) 0 pl.table.entries

let remove_invalid pl =
  remove_if (fun i -> Data.is_invalid pl.table.entries.(i)) pl (num_invalid pl)


let replace_all pl tracks =
  if pl.table.entries = [||] then
    insert pl 0 tracks
  else
  (
    remove_all pl;
    insert pl 0 tracks;
    Table.drop_undo pl.table;
  )


let move_selected pl d =
  if Table.num_selected pl.table > 0 then
  (
    let js = Table.move_selected pl.table d in
    Option.iter (fun shuffle ->
      Array.map_inplace (fun i -> js.(i)) shuffle.tracks
    ) pl.shuffle
  )


(* Persistance *)

let playlist_file = "playlist.m3u"


let save_playlist pl =
  Storage.save playlist_file (fun file ->
    Out_channel.output_string file (Track.to_m3u pl.table.entries)
  )

let load_playlist pl =
  Storage.load playlist_file (fun file ->
    pl.table.entries <- Track.of_m3u (In_channel.input_all file)
  )


open Storage
let fmt = Printf.sprintf
let scan = Scanf.sscanf
let bool x = x <> 0
let num l h x = max l (min h x)
let num_opt l h x = if x < 0 then None else Some (num l h x)

let opt f = function
  | None -> ""
  | Some i -> f i

let opt_int_pair = opt (fun (i, j) -> string_of_int i ^ ", " ^ string_of_int j)

let to_map pl =
  Map.of_list
  [
    "shuffle", fmt "%d" (Bool.to_int (pl.shuffle <> None));
    "play_pos", fmt "%d" (Option.value pl.table.pos ~default: (-1));
    "play_scroll", fmt "%d" pl.table.vscroll;
  ]

let to_map_extra pl =
  Map.of_list
  [
    "play_length", fmt "%d" (Table.length pl.table);
    "play_selected", fmt "%d" (Table.IntSet.cardinal pl.table.selected) ^
      ( if pl.table.selected = Table.IntSet.empty then "" else
        Table.IntSet.(fmt "(%d-%d)"
          (min_elt pl.table.selected) (max_elt pl.table.selected)) );
    "play_sel_range", fmt "%s" (opt_int_pair pl.table.sel_range);
    "play_total", fmt "%.2f, %d" (fst pl.total) (snd pl.total);
    "play_total_selected", fmt "%.2f, %d"
    (fst pl.total_selected) (snd pl.total_selected);
    "play_undo_length", fmt "%d" (List.length !(pl.table.undos));
    "play_redo_length", fmt "%d" (List.length !(pl.table.redos));
  ]

let of_map pl m =
  let len = Table.length pl.table in
  update_total pl;
  read_map m "play_pos"
    (fun s -> pl.table.pos <- scan s "%d" (num_opt 0 (len - 1)));
  Table.adjust_pos pl.table;
  read_map m "play_scroll"
    (fun s -> pl.table.vscroll <- scan s "%d" (num 0 (len - 1)));
  adjust_scroll pl 4;
  read_map m "shuffle"
    (fun s -> if scan s "%d" bool then shuffle pl pl.table.pos)
