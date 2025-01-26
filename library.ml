(* Library *)

open Audio_file
open Data

module Set = Set.Make(String)


type time = float
type db = Db.t

type attr =
[
  | `FilePath | `FileSize | `FileTime
  | `Codec | `Channels | `Depth | `SampleRate | `Bitrate | `Rate
  | `Artist | `Title | `Length | `Rating
  | `AlbumArtist | `AlbumTitle | `Track | `Disc
  | `Date | `Year | `Label | `Country
]

type t =
{
  db : db;
  mutable shown : bool;
  mutable side : Api.side;
  mutable width : int;
  mutable browser_width : int;
  mutable browser : dir Table.t;
  mutable view : track Table.t;
  mutable error : string;
  mutable error_time : time;
  mutable roots : dir array;
  mutable columns : (attr * int) array;
}


(* Constructor *)

let columns =
[|
  `FileTime, 110;
  `Rating, 30;
  `Artist, 150;
  `Title, 180;
  `Length, 30;
  `AlbumArtist, 100;
  `AlbumTitle, 150;
  `Track, 20;
  `Date, 60;
  `Country, 50;
  `Label, 50;
  `Codec, 30;
  `Rate, 50;
  `FileSize, 50;
  `FilePath, 400;
|]

let make db =
  {
    db;
    shown = false;
    side = `Right;
    width = 600;
    browser_width = 100;
    browser = Table.make ();
    view = Table.make ();
    error = "";
    error_time = 0.0;
    roots = [||];
    columns;
  }


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]

let ok lib =
  check "browser width in range" (lib.browser_width <= lib.width - 40) @
  Table.ok "browser" lib.browser @
  Table.ok "view" lib.view @
  check "view pos unset" (lib.view.pos = None || lib.view.pos = Some 0) @
  check "browser consistent with roots"
    ( Array.length lib.browser.entries = 1 ||
      Array.length lib.browser.entries > Array.length lib.roots ) @
  []


(* Attributes *)

let attr_prop = function
  | `FilePath -> "File Path", `Left
  | `FileSize -> "File Size", `Right
  | `FileTime -> "File Time", `Left
  | `Codec -> "Format", `Left
  | `Channels -> "Channels", `Left
  | `Depth -> "Bit Depth", `Right
  | `SampleRate -> "Sample Rate", `Right
  | `Bitrate -> "Bit Rate", `Right
  | `Rate -> "Rate", `Right
  | `Artist -> "Artist", `Left
  | `Title -> "Title", `Left
  | `Length -> "Length", `Right
  | `Rating -> "Rating", `Left
  | `AlbumArtist -> "Album Artist", `Left
  | `AlbumTitle -> "Album Title", `Left
  | `Track -> "Track", `Right
  | `Disc -> "Disc", `Right
  | `Date -> "Date", `Left
  | `Year -> "Year", `Left
  | `Label -> "Label", `Left
  | `Country -> "Country", `Left

let attr_name attr = fst (attr_prop attr)
let attr_align attr = snd (attr_prop attr)

let rate_for_codec = function
  | "MP3" | "OGG" | "OPUS" -> `Bitrate
  | _ -> `SampleRate

let fmt = Printf.sprintf

let fmt_time t =
  let t' = int_of_float (Float.trunc t) in
  fmt "%d:%02d" (t' / 60) (t' mod  60)

let fmt_date_time t =
  let tm = Unix.localtime t in
  fmt "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let star = "*"  (* TODO: "★" *)
let fmt_rating n =
  let len = String.length star in
  String.init (n * len) (fun i -> star.[i mod len])

let nonzero zero f x = if x = zero then "" else f x
let nonzero_int x = nonzero 0 string_of_int x
let nonempty opt f =
  match opt with
  | None -> ""
  | Some x -> f x

let rec attr_string track = function
  | `FilePath -> track.path
  | `FileSize ->
    nonzero 0.0 (fmt "%.1f MB") (float track.filesize /. 2.0 ** 20.0)
  | `FileTime -> nonzero 0.0 fmt_date_time track.filetime
  | `Codec -> nonempty track.format (fun format -> format.codec)
  | `Channels ->
    nonempty track.format (fun format -> nonzero_int format.channels)
  | `Depth ->
    nonempty track.format (fun format ->
      let depth = format.bitrate /. float format.rate /. float format.channels in
      let fmts : _ format =
        if float format.depth = Float.round depth then "%.0f" else "%.1f"
      in nonzero 0.0 (fmt fmts) depth
    )
  | `SampleRate ->
    nonempty track.format (fun format ->
      nonzero 0.0 (fmt "%.1f KHz") (float format.rate /. 1000.0))
  | `Bitrate ->
    nonempty track.format (fun format ->
      nonzero 0.0 (fmt "%.0f kbps") (format.bitrate /. 1000.0))
  | `Rate ->
    nonempty track.format (fun format ->
      attr_string track (rate_for_codec format.codec))
  | `Artist -> nonempty track.meta (fun meta -> meta.artist)
  | `Title -> nonempty track.meta (fun meta -> meta.title)
  | `Length ->
    nonempty track.format (fun format -> nonzero 0.0 fmt_time format.time)
  | `Rating -> nonempty track.meta (fun meta -> fmt_rating meta.rating)
  | `AlbumArtist -> nonempty track.meta (fun meta -> meta.albumartist)
  | `AlbumTitle -> nonempty track.meta (fun meta -> meta.albumtitle)
  | `Track -> nonempty track.meta (fun meta -> nonzero_int meta.track)
  | `Disc -> nonempty track.meta (fun meta -> nonzero_int meta.disc)
  | `Date -> nonempty track.meta (fun meta -> meta.date_txt)
  | `Year -> nonempty track.meta (fun meta -> nonzero_int meta.year)
  | `Label -> nonempty track.meta (fun meta -> meta.label)
  | `Country -> nonempty track.meta (fun meta -> meta.country)


(* Helpers *)

let array_swap a i j =
  let temp = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- temp

let array_rev a =
  let len = Array.length a in
  for i = 0 to len / 2 - 1 do
    array_swap a i (len - i - 1)
  done


(* Scanning *)

let queue = Safe_queue.create ()
let completed = Atomic.make false

let rescan_root lib root = Safe_queue.add (lib, root) queue
let rescan_roots lib = Array.iter (rescan_root lib) lib.roots
let rescan_roots_done _lib = Atomic.exchange completed false

let dirs = Option.value ~default: []

let rec scanner () =
  let lib, root = Safe_queue.take queue in

  let rec scan_path path nest =
    try
      if Sys.file_exists path then
        if Sys.is_directory path then
          if Format.is_known_ext path then
            scan_album path nest
          else
            scan_dir path nest
        else
          if Format.is_known_ext path then
            scan_track path
          else if M3u.is_known_ext path then
            scan_playlist path
          else
            None
      else None
    with Sys_error _ -> None

  and scan_dir path nest' =
    let nest = nest' + 1 in
    match
      Array.fold_left (fun r file ->
        match r, scan_path (Filename.concat path file) nest with
        | None, None -> None
        | dirs1, dirs2 -> Some (dirs dirs1 @ dirs dirs2)
      ) None (Sys.readdir path)
    with
    | None -> None
    | Some dirs ->
      let children = Array.map Data.link_val (Array.of_list dirs) in
      let dir =
        match Db.find_dir lib.db path with
        | Some dir ->
          (* TODO: remove missing children *)
          {dir with children}
        | None ->
          let dir : Data.dir =
            {
              id = -1L;
              path;
              name = Filename.basename path;
              children;
              pos = 0;
              nest;
              folded = true;
            }
          in Db.insert_dir lib.db dir;
          dir
      in
      Some [dir]

  and scan_album path nest =
    scan_dir path nest (* TODO *)

  and scan_track path =
    let stats = Unix.stat path in
    let track : Data.track =
      {
        id = -1L;
        path;
        album = None;
        filesize = stats.st_size;
        filetime = stats.st_mtime;
        fileage = Unix.gettimeofday ();
        status = `Det;
        format = Some (Format.read path);
        meta = Some (Meta.load path);
      }
    in
    if not (Db.exist_track lib.db path) then
      Db.insert_track lib.db track;
    Some []

  and scan_playlist path =
    let playlist : Data.playlist =
      {
        id = -1L;
        path;
      }
    in
    if not (Db.exist_playlist lib.db path) then
      Db.insert_playlist lib.db playlist;
    Some []
  in

  Option.iter (fun dirs ->
    assert (List.length dirs = 1);
    root.children <- (List.hd dirs).children;
  ) (scan_dir root.path (-1));

  Atomic.set completed true;
  scanner ()

let _ = Domain.spawn scanner


(* Browser *)

let selected_dir lib = Table.first_selected lib.browser
let deselect_dir lib = Table.deselect_all lib.browser
let select_dir lib i = deselect_dir lib; Table.select lib.browser i i


let save_browser_selection lib =
  let selection =
    Option.map (fun i -> lib.browser.entries.(i).path) (selected_dir lib) in
  deselect_dir lib;
  selection

let restore_browser_selection lib selection =
  Array.iteri
    (fun i (d : Data.dir) -> if selection = Some d.path then select_dir lib i)
    lib.browser.entries;
  Table.adjust_scroll lib.browser (Table.first_selected lib.browser)


let make_all lib =
  {
    id = -1L;
    path = "";
    name = "All";
    children = Array.map Data.link_val lib.roots;
    pos = 0;
    nest = -1;
    folded = false;
  }

let update_browser lib =
  let rec entries dir acc =
    dir ::
    Array.fold_right
      (fun link -> entries (Data.val_of_link link)) dir.children acc
  in
  let selection = save_browser_selection lib in
  lib.browser.entries <- Array.of_list (entries (make_all lib) []);
  Table.adjust_pos lib.browser;
  restore_browser_selection lib selection


(* Roots *)

let count_roots lib = Db.count_roots lib.db
let iter_roots lib f = Db.iter_roots lib.db f

let load_roots lib =
  let roots = ref [] in
  Db.iter_roots lib.db (fun root -> roots := root :: !roots);
  lib.roots <- Array.of_list !roots;
  Array.sort (fun r1 r2 -> compare r1.pos r2.pos) lib.roots;
  rescan_roots lib


let make_root lib path pos =
  if not (Sys.file_exists path) then
    failwith (path ^ " does not exist")
  else if not (Sys.is_directory path) then
    failwith (path ^ " is not a directory")
  else
  (
    match
      Array.find_opt (fun (root : dir) ->
        path = root.path ||
        String.starts_with root.path ~prefix: (Filename.concat path "") ||
        String.starts_with path ~prefix: (Filename.concat root.path "")
      ) lib.roots
    with
    | Some root ->
      failwith
        (path ^ " overlaps with " ^ root.name ^ " (" ^ root.path ^ ")")
    | None ->
      Data.{
        id = 0L;
        path;
        name = Filename.basename path;
        children = [||];
        pos;
        nest = 0;
        folded = false;
      }
  )

let add_roots lib paths pos =
  let paths = Array.of_list paths in
  lib.error <- "";
  try
    let roots' = Array.mapi (fun i path -> make_root lib path (pos + i)) paths in
    let len = Array.length lib.roots in
    let len' = Array.length roots' in
    lib.roots <-
      Array.init (len + len') (fun i ->
        if i < pos then
          lib.roots.(i)
        else if i < pos + len' then
          roots'.(i - pos)
        else
          let root = lib.roots.(i - len') in
          root.pos <- i; root
      );
    update_browser lib;
    Db.update_roots_pos lib.db pos (+len');
    Array.iter (Db.insert_root lib.db) roots';
    Array.iter (rescan_root lib) roots';
    true
  with Failure msg ->
    lib.error <- msg;
    lib.error_time <- Unix.gettimeofday ();
    false


(* View *)

let has_selection lib = Table.has_selection lib.view
let num_selected lib = Table.num_selected lib.view
let first_selected lib = Table.first_selected lib.view
let last_selected lib = Table.last_selected lib.view
let is_selected lib i = Table.is_selected lib.view i
let selected lib = Table.selected lib.view

let select_all lib =
  Table.select_all lib.view

let deselect_all lib =
  Table.deselect_all lib.view

let select_invert lib =
  Table.select_invert lib.view

let select lib i j =
  Table.select lib.view i j

let deselect lib i j =
  Table.deselect lib.view i j


let adjust_scroll lib pos =
  Table.adjust_scroll lib.view pos


let save_view_selection lib =
  let selection = selected lib in
  deselect_all lib;
  selection

let restore_view_selection lib selection =
  let set = Array.fold_right (fun t -> Set.add t.path) selection Set.empty in
  Array.iteri (fun i t -> if Set.mem t.path set then select lib i i)
    lib.view.entries;
  Table.adjust_scroll lib.view (first_selected lib)


let update_view lib =
  let selection = save_view_selection lib in
  let tracks = ref [] in
  Option.iter (fun i ->
    let dir = lib.browser.entries.(i) in
    let path = Filename.concat dir.path "" in
    Db.iter_tracks_for lib.db path (fun tr -> tracks := tr :: !tracks);
  ) (Table.first_selected lib.browser);
  lib.view.entries <- Array.of_list !tracks;
  array_rev lib.view.entries;
  Table.adjust_pos lib.view;
  restore_view_selection lib selection


let array_is_sorted cmp a =
  let rec loop i =
    i >= Array.length a || cmp a.(i - 1) a.(i) <= 0 && loop (i + 1)
  in loop 1

let reorder_view lib attr =
  let selection = save_view_selection lib in
  let entries' =
    Array.map (fun track -> attr_string track attr, track) lib.view.entries in
  let cmp_asc t1 t2 = compare (fst t1) (fst t2) in
  let cmp_desc t1 t2 = - cmp_asc t1 t2 in
  let cmp = if array_is_sorted cmp_asc entries' then cmp_desc else cmp_asc in
  Array.stable_sort cmp entries';
  lib.view.entries <- Array.map snd entries';
  restore_view_selection lib selection


(* Persistance *)

let attr_str =
[
  `FilePath, "PTH";
  `FileSize, "SIZ";
  `FileTime, "TIM";
  `Codec, "COD";
  `Channels, "CHA";
  `Depth, "DEP";
  `SampleRate, "KHZ";
  `Bitrate, "BPS";
  `Rate, "RES";
  `Artist, "ART";
  `Title, "TIT";
  `Length, "LEN";
  `Rating, "RAT";
  `AlbumArtist, "ALA";
  `AlbumTitle, "ALB";
  `Track, "TRK";
  `Disc, "DIS";
  `Date, "DAT";
  `Year, "YER";
  `Label, "LAB";
  `Country, "CTY";
]

let string_of_attr attr = List.assoc attr attr_str
let attr_of_string s = fst (List.find (fun (_, s') -> s' = s) attr_str)

let string_of_col (attr, w) = string_of_attr attr ^ string_of_int w
let col_of_string s =
  attr_of_string (String.sub s 0 3),
  int_of_string (String.sub s 3 (String.length s - 3))


let to_string' lib =
  let buf = Buffer.create 1024 in
  let output fmt  = Printf.bprintf buf fmt in
  output "lib_open = %d\n" (Bool.to_int lib.shown);
  output "lib_side = %d\n" (Bool.to_int (lib.side = `Right));
  output "lib_width = %d\n" lib.width;
  output "lib_browser_width = %d\n" lib.browser_width;
  output "lib_browser_scroll = %d\n" lib.browser.scroll_v;
  output "lib_view_columns = %s\n"
    (String.concat " " (Array.to_list (Array.map string_of_col lib.columns)));
  Buffer.contents buf

let to_string lib =
  to_string' lib ^
  let buf = Buffer.create 1024 in
  let output fmt = Printf.bprintf buf fmt in
  output "lib_browser_fit = %d\n" lib.browser.fit;
  output "lib_browser_pos = %d\n" (Option.value lib.browser.pos ~default: (-1));
  output "lib_browser_length = %d\n" (Array.length lib.browser.entries);
  output "lib_view_fit = %d\n" lib.view.fit;
  output "lib_view_pos = %d\n" (Option.value lib.view.pos ~default: (-1));
  output "lib_view_scroll_v = %d\n" lib.view.scroll_v;
  output "lib_view_scroll_h = %d\n" lib.view.scroll_h;
  output "lib_view_length = %d\n" (Array.length lib.view.entries);
  output "lib_root_length = %d\n" (Array.length lib.roots);
  output "lib_error = %s\n" lib.error;
  output "lib_error_time = %.1f\n" lib.error_time;
  Buffer.contents buf

let save lib file =
  Out_channel.output_string file (to_string' lib)


let fscanf file =
  match In_channel.input_line file with
  | None -> raise End_of_file
  | Some s -> Scanf.sscanf s

let bool x = x <> 0
let num l h x = max l (min h x)

let load lib file =
  let input fmt = fscanf file fmt in
  lib.shown <- input " lib_open = %d " bool;
  lib.side <- if input " lib_side = %d " bool then `Right else `Left;
  (* TODO: 400 = library_min, 360 = control_w; use constants *)
  lib.width <- input " lib_width = %d " (num 120 max_int);  (* clamped later *)
  (* TODO: 40 = browser_min, 60 = browser_min + 2*margin; use constants *)
  lib.browser_width <- input " lib_browser_width = %d "
    (num 40 (lib.width - 60));
  update_browser lib;
  lib.browser.scroll_v <- input " lib_browser_scroll = %d "
    (num 0 (max 0 (Array.length lib.roots - 1)));
  let cols = input " lib_view_columns = %[\x20-\xff]" String.trim in
  lib.columns <-
    Array.of_list (List.map col_of_string
      (List.filter ((<>) "") (String.split_on_char ' ' cols)))
