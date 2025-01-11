(* Library *)

open Audio_file
open Data

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
  mutable shown : bool;         (* external *)
  mutable side : Api.side;      (* external *)
  mutable width : int;          (* external *)
  mutable browser_width : int;  (* external *)
  mutable browser_rows : int;   (* external *)
  mutable browser_scroll : int; (* external *)
  mutable view_rows : int;      (* external *)
  mutable view_scroll : int;    (* external *)
  mutable view_scroll_h : int;  (* external *)
  mutable error : string;       (* external *)
  mutable error_time : time;    (* external *)
  mutable roots : dir array;    (* external *)
  mutable tracks : track array; (* external *)
  mutable columns : (attr * int) array; (* external *)
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
    browser_rows = 4;
    browser_scroll = 0;
    view_rows = 4;
    view_scroll = 0;
    view_scroll_h = 0;
    error = "";
    error_time = 0.0;
    roots = [||];
    tracks = [||];
    columns;
  }


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

let rec attr_string (track : Data.track) = function
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

let scan_roots lib roots =
  let rec scan_path path =
    try
      if Sys.file_exists path then
        if Sys.is_directory path then
          if Format.is_known_ext path then
            scan_album path
          else
            scan_dir path
        else
          if Format.is_known_ext path then
            scan_track path
          else if M3u.is_known_ext path then
            scan_playlist path
          else
            false
      else false
    with Sys_error _ -> false

  and scan_dir path =
    if
      Array.fold_left (fun b file ->
        scan_path (Filename.concat path file) || b
      ) false (Sys.readdir path)
    then
    (
      let dir : Data.dir =
        {
          id = -1L;
          path;
          name = Filename.basename path;
          children = [||];
          pos = 0;
          folded = true;
        }
      in Db.insert_dir lib.db dir;
      true
    )
    else false

  and scan_album path =
    scan_dir path  (* TODO *)

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
    Db.insert_track lib.db track;
    true

  and scan_playlist path =
    let playlist : Data.playlist =
      {
        id = -1L;
        path;
      }
    in
    Db.insert_playlist lib.db playlist;
    true
  in
  Array.iter (fun (root : dir) -> ignore (scan_dir root.path)) roots


(* Roots *)

let count_roots lib = Db.count_roots lib.db
let iter_roots lib f = Db.iter_roots lib.db f

let load_roots lib =
  let roots = ref [] in
  iter_roots lib (fun root -> roots := root :: !roots);
  lib.roots <- Array.of_list !roots;
  Array.sort (fun r1 r2 -> compare r1.pos r2.pos) lib.roots

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
        folded = true;
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
    Db.update_roots_pos lib.db pos (+len');
    Array.iter (Db.insert_root lib.db) roots';
    scan_roots lib roots';
    true
  with Failure msg ->
    lib.error <- msg;
    lib.error_time <- Unix.gettimeofday ();
    false


(* View *)

let update_view lib =
  let tracks = ref [] in
  Db.iter_tracks lib.db (fun track -> tracks := track :: !tracks);
  lib.tracks <- Array.of_list !tracks;
  array_rev lib.tracks


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]

let ok lib =
  check "browser width in range" (lib.browser_width <= lib.width - 40) @
  []
