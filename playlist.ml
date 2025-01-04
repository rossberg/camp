(* Playlist *)

open Audio_file

module IntSet = Set.Make(Int)

type time = Track.time
type track = Track.t

type undo =
{
  undo_pos : int;
  undo_scroll : int;
  undo_tracks : track array;
  undo_selected : IntSet.t;
  undo_total : time * int;
  undo_total_selected : time * int;
}

type t =
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


(* Constructor *)

let no_range = min_int, 0

let make () =
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


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]

let ok pl =
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


(* Total *)

let add_total (t1, n1) (t2, n2) = (t1 +. t2, n1 + n2)
let sub_total (t1, n1) (t2, n2) = (max 0.0 (t1 -. t2), max 0 (n1 - n2))

let track_total (track : track) =
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


(* Navigation *)

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


(* Shuffle *)

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


(* Selection *)

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

let select_invert pl =
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


(* Undo *)

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


(* Editing *)

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
  let add_track (track : track) =
    tracks := track :: !tracks;
    if track.status = `Undet then Track.update audio track
  in
  let add_song path = add_track (Track.make path) in
  let add_playlist path =
    let s = In_channel.(with_open_bin path input_all) in
    List.iter (fun M3u.{path; info} ->
      let track =
        match info with
        | None -> Track.make path
        | Some {title; time} -> Track.make_predet path title (float time)
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
  Array.fold_left (fun n track ->
    n + Bool.to_int (Track.is_invalid track)
  ) 0 pl.tracks

let remove_invalid pl =
  remove_if (fun i -> Track.is_invalid pl.tracks.(i)) pl (num_invalid pl);
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
