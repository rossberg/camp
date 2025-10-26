open Audio_file
open Meta
open Data


(* Values *)

type key = Data.query_attr

type value =
  | BoolV of bool
  | IntV of int * string option
  | TimeV of time * string option
  | DateV of date * string option
  | TextV of string


let file_value path (file : file) = function
  | `FileExists -> BoolV (M3u.is_separator path || File.exists path)
  | `FilePath -> TextV path
  | `FileDir -> TextV (File.dir path)
  | `FileName -> TextV (File.name path)
  | `FileExt -> TextV (File.extension path)
  | `FileSize -> IntV (file.size, None)
  | `FileTime -> DateV (file.time, None)

let format_value (format_opt : Format.t option) =
  let format = Option.value format_opt ~default: Format.unknown in
  function
  | `Length -> TimeV (format.time, None)
  | `Codec -> TextV format.codec
  | `Channels -> IntV (format.channels, None)
  | `Depth ->
    let d = format.bitrate /. float format.rate /. float format.channels in
    IntV (Float.to_int d, None)
  | `SampleRate -> IntV (format.rate, None)
  | `BitRate -> IntV (Float.to_int format.bitrate, None)
  | `Rate -> assert false

let meta_value (meta_opt : Meta.t option) =
  let meta = Option.value meta_opt ~default: Meta.unknown in
  function
  | `Artist -> TextV meta.artist
  | `Title -> TextV meta.title
  | `AlbumArtist -> TextV meta.albumartist
  | `AlbumTitle -> TextV meta.albumtitle
  | `Track -> IntV (meta.track, Some meta.track_txt)
  | `Tracks -> IntV (meta.tracks, None)
  | `Disc -> IntV (meta.disc, Some meta.disc_txt)
  | `Discs -> IntV (meta.discs, None)
  | `DiscTrack when meta.disc = 0 -> TextV (Printf.sprintf "%3d" meta.track)
  | `DiscTrack -> TextV (Printf.sprintf "%d.%02d" meta.disc meta.track)
  | `Date -> DateV (meta.date, Some meta.date_txt)
  | `Year -> IntV (meta.year, None)
  | `Label -> TextV meta.label
  | `Country -> TextV meta.country
  | `Length -> TimeV (meta.length, None)
  | `Rating -> IntV (meta.rating, None)
  | `Cover -> BoolV (meta.cover <> None)

let value key (track : track) =
  match key with
  | `True -> BoolV true
  | `False -> BoolV false
  | `Now -> TimeV (Unix.gettimeofday (), None)
  | `Random -> IntV (Random.int 0x1_0000_0000, None)
  | `Pos -> IntV (track.pos + 1, None)
  | `Length ->
    (match format_value track.format `Length with
    | TimeV (0.0, _) -> meta_value track.meta `Length
    | v -> v
    )
  | `Year ->
    (match meta_value track.meta `Year with
    | IntV (0, _) as v ->
      (match meta_value track.meta `Date with
      | DateV (0.0, _) -> v
      | DateV (t, _) -> IntV (year_of_date t, None)
      | _ -> assert false
      )
    | v -> v
    )
  | `Date ->
    (match meta_value track.meta `Date with
    | DateV (0.0, _) as v ->
      (match meta_value track.meta `Year with
      | IntV (0, _) -> v
      | IntV (n, _) -> DateV (date_of_year n, None)
      | _ -> assert false
      )
    | v -> v
    )
  | #file_attr as attr -> file_value track.path track.file attr
  | #format_attr as attr -> format_value track.format attr
  | #meta_attr as attr -> meta_value track.meta attr
  | `Albums | `None -> assert false

let string_of_value = function
  | BoolV b -> string_of_bool b
  | IntV (i, _) -> string_of_int i
  | TimeV (t, _) -> Data.string_of_time t
  | DateV (t, _) -> Data.string_of_date_time t
  | TextV s ->
    let buf = Buffer.create (String.length s) in
    String.iter (fun c ->
      if c >= '\x20' then
        Buffer.add_char buf c
      else
        Buffer.add_string buf (Printf.sprintf "\\%02x" (Char.code c))
    ) s;
    "\"" ^ Buffer.contents buf ^ "\""


(* Queries *)

type track = Data.track
type album = Data.album
type artist = Data.artist
type order = Data.order
type sorting = Data.track_attr Data.sorting

type unop = Not | Neg
type binop =
  | And | Or | EQ | NE | LT | GT | LE | GE | IN | NI | Add | Sub | Mul | Cat
type expr =
  | Text of string
  | Int of int * string
  | Time of time * string
  | Date of date * string
  | Key of key
  | Un of unop * expr
  | Bin of binop * expr * expr

type query = {expr : expr; sort : sorting}

type type_ =
  | BoolT
  | TextT
  | IntT
  | TimeT
  | DateT

let keys =
  [
    "true", `True; "false", `False;
    "now", `Now; "random", `Random;
    "fileexists", `FileExists;
    "filetime", `FileTime; "filesize", `FileSize;
    "filepath", `FilePath; "filedir", `FileDir;
    "filename", `FileName; "fileext", `FileExt;
    "title", `Title; "artist", `Artist;
    "disc", `Disc; "track", `Track; "discs", `Discs; "tracks", `Tracks;
    "disctrack", `DiscTrack;
    "album", `AlbumTitle; "albumartist", `AlbumArtist;
    "year", `Year; "date", `Date;
    "label", `Label; "country", `Country;
    "length", `Length; "rating", `Rating;
    "codec", `Codec; "channels", `Channels; "depth", `Depth;
    "samplerate", `SampleRate; "bitrate", `BitRate;
    "cover", `Cover;
    "pos", `Pos;
  ]


let empty_query = {expr = Key `False; sort = []}
let full_query = {expr = Key `True; sort = []}


let string_of_key k =
  fst (List.find (fun (_, x) -> x = (k :> key)) keys)

let string_of_order = function
  | `Asc -> ""
  | `Desc -> "-"

let string_of_unop = function
  | Not -> "-"
  | Neg -> "~"

let string_of_binop = function
  | And -> "&"
  | Or -> "|"
  | EQ -> "="
  | NE -> "<>"
  | LT -> "<"
  | GT -> ">"
  | LE -> "<="
  | GE -> ">="
  | IN -> "@"
  | NI -> "-@"
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Cat -> "#"

let rec string_of_expr = function
  | Int (_, s) -> s
  | Time (_, s) -> s
  | Date (_, s) -> s
  | Text s -> "\"" ^ s ^ "\""
  | Key k -> "#" ^ string_of_key k
  | Un (op, e) -> "(" ^ string_of_unop op ^ " " ^ string_of_expr e ^ ")"
  | Bin (op, e1, e2) ->
    "(" ^ string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2 ^ ")"

let string_of_query {expr; sort} =
  string_of_expr expr ^ " ^ " ^
  String.concat " "
    (List.map (fun (k, o) -> string_of_order o ^ string_of_key k) sort)


(* Validation *)

exception TypeError

let rec validate q =
  match q with
  | Key (`FileExists | `True | `False | `Cover) -> BoolT
  | Int _
  | Key (`Random | `FileSize | `Disc | `Track | `Discs | `Tracks | `Pos | `Year)
  | Key (`Rating | `Channels | `Depth | `SampleRate | `BitRate | `Rate) -> IntT
  | Time _ | Key `Length -> TimeT
  | Date _ | Key (`Now | `Date | `FileTime) -> DateT
  | Text _
  | Key (`FilePath | `FileDir | `FileName | `FileExt)
  | Key (`Artist | `Title | `AlbumArtist | `AlbumTitle)
  | Key (`Label | `Country | `Codec | `DiscTrack) -> TextT
  | Un (op, q1) ->
    (match op, validate q1 with
    | Not, BoolT -> BoolT
    | Neg, IntT -> IntT
    | Neg, TimeT -> TimeT
    | _ -> raise TypeError
    )
  | Bin (op, q1, q2) ->
    (match op, validate q1, validate q2 with
    | (And | Or), BoolT, BoolT -> BoolT
    | (EQ | NE | LT | GT | LE | GE), t1, t2 when t1 = t2 -> BoolT
    | (IN | NI), TextT, (TextT | IntT | TimeT | DateT) -> BoolT
    | (Add | Sub | Mul), IntT, IntT -> IntT
    | (Add | Sub), TimeT, TimeT -> TimeT
    | (Add | Sub), DateT, TimeT -> DateT
    | Sub, DateT, DateT -> TimeT
    | Mul, TimeT, IntT -> TimeT
    | Mul, IntT, TimeT -> TimeT
    | Cat, TextT, TextT -> TextT
    | _ -> raise TypeError
    )


let rec string_contains_at' s i s' j =
  j = String.length s' ||
  s.[i + j] = s'.[j] && string_contains_at' s i s' (j + 1)

let string_contains_at s i s' =
  String.length s - i >= String.length s' &&
  string_contains_at' s i s' 0

let rec index_sub_from_opt s i s' =
  if s' = "" then Some i else
  match String.index_from_opt s i s'.[0] with
  | None -> None
  | Some j ->
    if j + String.length s' > String.length s then
      None
    else if string_contains_at s j s' then
      Some j
    else if j + String.length s' >= String.length s then
      None
    else
      index_sub_from_opt s (j + 1) s'

let string_contains ~inner s = index_sub_from_opt s 0 inner <> None

(* TODO: use Data.contains_utf_8 (and remove UCase), once Camomile bug
 * https://github.com/ocaml-community/Camomile/issues/10 is fixed. *)
let string_contains_caseless ~inner s =
  string_contains ~inner: (Data.UCase.casefolding inner) (Data.UCase.casefolding s)


(* Evaluation *)

let lit = function
  | IntV (i, Some _) -> IntV (i, None)
  | TimeV (t, Some _) -> TimeV (t, None)
  | DateV (t, Some _) -> DateV (t, None)
  | v -> v

let text = function
  | BoolV b -> string_of_bool b
  | IntV (_, Some s) | TimeV (_, Some s) | DateV (_, Some s) | TextV s -> s
  | IntV (i, None) -> string_of_int i
  | TimeV (t, None) -> Data.string_of_time t
  | DateV (t, None) -> Data.string_of_date_time t

let rec eval q track =
  match q with
  | Text s -> TextV s
  | Int (i, s) -> IntV (i, Some s)
  | Time (t, s) -> TimeV (t, Some s)
  | Date (t, s) -> DateV (t, Some s)
  | Key key -> value key track
  | Un (Not, q1) -> BoolV (not (check q1 track))
  | Bin (And, q1, q2) -> BoolV (check q1 track && check q2 track)
  | Bin (Or, q1, q2) -> BoolV (check q1 track || check q2 track)
  | Un (op, q1) ->
    (match op, eval q1 track with
    | Neg, IntV (i, _) -> IntV (- i, None)
    | Neg, TimeV (t, _) -> TimeV (-. t, None)
    | _ -> assert false
    )
  | Bin (op, q1, q2) ->
    (match op, eval q1 track, eval q2 track with
    | EQ, v1, v2 -> BoolV (lit v1 = lit v2)
    | NE, v1, v2 -> BoolV (lit v1 <> lit v2)
    | LT, v1, v2 -> BoolV (lit v1 < lit v2)
    | GT, v1, v2 -> BoolV (lit v1 > lit v2)
    | LE, v1, v2 -> BoolV (lit v1 <= lit v2)
    | GE, v1, v2 -> BoolV (lit v1 >= lit v2)
    | IN, v1, v2 ->
      BoolV (string_contains_caseless ~inner: (text v1) (text v2))
    | NI, v1, v2 ->
      BoolV (not (string_contains_caseless ~inner: (text v1) (text v2)))
    | Add, IntV (i1, _), IntV (i2, _) -> IntV (i1 + i2, None)
    | Add, TimeV (t1, _), TimeV (t2, _) -> TimeV (t1 +. t2, None)
    | Add, DateV (t1, _), TimeV (t2, _) -> DateV (t1 +. t2, None)
    | Sub, IntV (i1, _), IntV (i2, _) -> IntV (i1 - i2, None)
    | Sub, TimeV (t1, _), TimeV (t2, _) -> TimeV (t1 -. t2, None)
    | Sub, DateV (t1, _), DateV (t2, _) -> TimeV (t1 -. t2, None)
    | Sub, DateV (t1, _), TimeV (t2, _) -> DateV (t1 -. t2, None)
    | Mul, IntV (i1, _), IntV (i2, _) -> IntV (i1 * i2, None)
    | Mul, TimeV (t1, _), IntV (i2, _) -> TimeV (t1 *. float_of_int i2, None)
    | Mul, IntV (i1, _), TimeV (t2, _) -> TimeV (float_of_int i1 *. t2, None)
    | Cat, v1, v2 -> TextV (text v1 ^ text v2)
    | _ -> assert false
    )

and check q track =
  match eval q track with
  | BoolV b -> b
  | _ -> assert false


(* Execution *)

module AlbumKey =
struct
  type t = string * string * string * string
  let compare : t -> t -> int = compare
end
module AlbumSet = Set.Make(AlbumKey)
module AlbumMap = Map.Make(AlbumKey)
module ArtistMap = Map.Make(String)

let album_key' attr_string x : AlbumKey.t =
  ( attr_string x `AlbumArtist,
    attr_string x `AlbumTitle,
    attr_string x `Codec,
    attr_string x `Label
  )

let album_key album = album_key' album_attr_string album
let track_album_key track = album_key' track_attr_string track

let new_album_of_track (track : track) : album =
  let meta = Option.value track.meta ~default: Meta.unknown in
  { path = track.path;
    file = track.file;
    format = track.format;
    meta = Some {meta with tracks = 1};
    memo = None;
  }

let new_artist_of_track (track : track) : artist =
  { name = Data.track_attr_string track `AlbumArtist;
    albums = 1;
    tracks = 1;
  }

let accumulate_string s1 s2 =
  if s1 = s2 then s1 else ""

let accumulate_option accumulate opt1 opt2 =
  match opt1, opt2 with
  | None, _ -> opt2
  | _, None -> opt1
  | Some x1, Some x2 -> Some (accumulate x1 x2)

let accumulate_file (file1 : file) (file2 : file) =
  {
    size = file1.size + file2.size;
    time = max file1.time file2.time;
    age = max file1.age file2.age;
  }

let accumulate_format (format1 : Format.t) (format2 : Format.t) =
  Format.{
    codec = accumulate_string format1.codec format2.codec;
    channels = min format1.channels format2.channels;
    depth = min format1.depth format2.depth;
    rate = min format1.rate format2.rate;
    bitrate = min format1.bitrate format2.bitrate;
    time = format1.time +. format2.time;
    size = format1.size + format2.size;
  }

let accumulate_meta (meta1 : Meta.t) (meta2 : Meta.t) =
  { Meta.unknown with
    artist = accumulate_string meta1.artist meta2.artist;
    title = accumulate_string meta1.title meta2.title;
    tracks = meta1.tracks + meta2.tracks;
    albumartist = accumulate_string meta1.albumartist meta2.albumartist;
    albumtitle = accumulate_string meta1.albumtitle meta2.albumtitle;
    year = max meta1.year meta2.year;
    date = max meta1.date meta2.date;
    label = accumulate_string meta1.label meta2.label;
    country = accumulate_string meta1.country meta2.country;
    length = meta1.length +. meta2.length;
    rating = max meta1.rating meta2.rating;
  }

let rec iter_dir f (dir : _ dir) =
  Iarray.iter (iter_dir f) dir.children;
  Iarray.iter f dir.tracks

let sort s tracks =
  if s <> [] then
  (
    let tracks' =
      Array.map (fun tr -> Data.key_entry track_attr_string s tr, tr) tracks in
    Array.stable_sort compare tracks';
    Array.iteri (fun i (_, tr) -> tracks.(i) <- tr) tracks';
  )

let exec q p dir =
  let t_start = Unix.gettimeofday () in
  let t_check = ref 0.0 in
  let tracks = Dynarray.create () in
  let albums = Dynarray.create () in
  let artists = Dynarray.create () in
  let album_map = ref AlbumMap.empty in
  let artist_map = ref ArtistMap.empty in
  iter_dir (fun track ->
    let t1 = Unix.gettimeofday () in
    let b = check q.expr track in
    let t2 = Unix.gettimeofday () in
    t_check := !t_check +. t2 -. t1;
    if b then
    (
      let to_artists, to_albums, to_tracks = p track in
      if to_tracks then Dynarray.add_last tracks track;
      if to_albums || to_artists then
      (
        let album = new_album_of_track track in
        let album_key = album_key album in
        let is_new_album =
          match AlbumMap.find_opt album_key !album_map with
          | None ->
            album_map := AlbumMap.add album_key album !album_map;
            if to_albums then Dynarray.add_last albums album;
            true
          | Some album' ->
            album'.file <- accumulate_file album'.file album.file;
            album'.format <-
              accumulate_option accumulate_format album'.format album.format;
            album'.meta <-
              accumulate_option accumulate_meta album'.meta album.meta;
            false
        in
        if to_artists then
        (
          let artist = new_artist_of_track track in
          match ArtistMap.find_opt artist.name !artist_map with
          | None ->
            artist_map := ArtistMap.add artist.name artist !artist_map;
            Dynarray.add_last artists artist
          | Some artist' ->
            artist'.tracks <- artist'.tracks + artist.albums;
            if is_new_album then
              artist'.albums <- artist'.albums + artist.albums;
        )
      )
    )
  ) dir;
  Dynarray.to_array artists,
  Dynarray.to_array albums,
  let tracks = Dynarray.to_array tracks in
  let t_sort = Unix.gettimeofday () in
  sort q.sort tracks;
  let t_finish = Unix.gettimeofday () in
  if !App.debug_perf then
    Printf.printf "    [exec %s] %.3f s = %.3f search (%.3f check), %.3f sort\n%!"
    (let s = string_of_expr q.expr in String.(sub s 0 (min 30 (length s)) ^ "..."))
    (t_finish -. t_start) (t_sort -. t_start) !t_check (t_finish -. t_sort);
  tracks


(* Parsing *)

exception SyntaxError of int

type token =
  | TextToken of string
  | IntToken of int * string
  | TimeToken of time * string
  | DateToken of time * string
  | KeyToken of key
  | UnopToken of unop
  | BinopToken of binop
  | LParToken
  | RParToken
  | SortToken
  | EndToken

(*
let string_of_token = function
  | TextToken s -> "\"" ^ s ^ "\""
  | IntToken (_, s) -> s
  | TimeToken (_, s) -> s
  | DateToken t -> Date.string_of_date t
  | KeyToken x -> "#" ^ string_of_key x
  | UnopToken op -> string_of_unop op
  | BinopToken op -> string_of_binop op
  | LParToken -> "("
  | RParToken -> ")"
  | SortToken -> "^"
  | EndToken -> "(end of string)"
*)

let is c s i = i < String.length s && s.[i] = c
let is_letter = function
  | '0'..'9' | 'A'..'Z' | 'a'..'z' | '_' | '.' | '!' | '?' | '-' -> true
  | c -> c >= '\x80'

let scan_word s i =
  let j = ref i in
  while !j < String.length s && is_letter s.[!j] do
    incr j
  done;
  String.sub s i (!j - i), !j

let scan lo hi s i =
  let j = ref i in
  while !j < String.length s && lo <= s.[!j] && s.[!j] <= hi do
    incr j
  done;
  String.sub s i (!j - i), !j

let scan_name = scan 'a' 'z'
let scan_num = scan '0' '9'

let scan_date s i =
  let y, j = scan_num s i in
  if y = "" || not (is '/' s j || is '-' s j) then raise (SyntaxError j) else
  let m, k = scan_num s (j + 1) in
  if m = "" || not (is s.[j] s k) then raise (SyntaxError k) else
  let d, l = scan_num s (k + 1) in
  if d = "" then raise (SyntaxError l) else
  let post, n = scan_word s l in
  let s' = String.sub s (i + 1) (n - i) in
  if post = "" then
    let t = date (int_of_string y) (int_of_string m) (int_of_string d) in
    DateToken (t, s'), n
  else
    TextToken s', n

let rec scan_time s i t =
  let n, j = scan_num s i in
  if n = "" then Some (t, i) else
  let z = float_of_string n in
  match scan_name s j with
  | "s", k -> scan_time s k (t +. z)
  | "m", k -> scan_time s k (t +. 60.0 *. z)
  | "h", k -> scan_time s k (t +. 60.0 *. 60.0 *. z)
  | "d", k -> scan_time s k (t +. 24.0 *. 60.0 *. 60.0 *. z)
  | "y", k -> scan_time s k (t +. 365.0 *. 24.0 *. 60.0 *. 60.0 *. z)
  | _ -> None

let suffix = function
  | "" -> Some 1
  | "K" -> Some 1_000
  | "M" -> Some 1_000_000
  | "G" -> Some 1_000_000_000
  | "T" -> Some 1_000_000_000_000
  | "P" -> Some 1_000_000_000_000_000
  | "E" -> Some 1_000_000_000_000_000_000
  | "Ki" -> Some (2 lsl 10)
  | "Mi" -> Some (2 lsl 20)
  | "Gi" -> Some (2 lsl 30)
  | "Ti" -> Some (2 lsl 40)
  | "Pi" -> Some (2 lsl 50)
  | "Ei" -> Some (2 lsl 60)
  | _ -> None

let rec token s i =
  if i = String.length s then EndToken, i else
  match s.[i] with
  | ' ' | '\t' | '\r' | '\n' -> token s (i + 1)
  | '^' -> SortToken, i + 1
  | '(' -> LParToken, i + 1
  | ')' -> RParToken, i + 1
  | '&' -> BinopToken And, i + 1
  | '|' -> BinopToken Or, i + 1
  | '+' when is '+' s (i + 1) -> BinopToken Cat, i + 2
  | '+' -> BinopToken Add, i + 1
  | '-' -> BinopToken Sub, i + 1
  | '*' -> BinopToken Mul, i + 1
  | '@' -> BinopToken IN, i + 1
  | '=' -> BinopToken EQ, i + 1
  | '<' when is '>' s (i + 1) -> BinopToken NE, i + 2
  | '<' when is '=' s (i + 1) -> BinopToken LE, i + 2
  | '<' -> BinopToken LT, i + 1
  | '>' when is '=' s (i + 1) -> BinopToken GE, i + 2
  | '>' -> BinopToken GT, i + 1
  | '~' when is '@' s (i + 1) -> BinopToken NI, i + 2
  | '~' -> UnopToken Not, i + 1
  | '0'..'9' ->
    let n, j = scan_num s i in
    if is '/' s j then scan_date s i else
    let suf, k = scan_word s j in
    (match suffix suf with
    | Some m -> IntToken (int_of_string n * m, String.sub s i (k - i)), k
    | None ->
      match scan_time s i 0.0 with
      | None ->
        let s', k = scan_word s i in
        TextToken s', k
      | Some (t, j) ->
        let post, k = scan_word s j in
        let s' = String.sub s (i + 1) (k - i) in
        if post = "" then
          TimeToken (t, s'), k
        else
          TextToken s', k
    )
  | 'a'..'z' | 'A'..'Z' | '_' | '.' | '!' | '?' ->
    let s', j = scan_word s i in
    TextToken s', j
  | '\"' ->
    (match String.index_from_opt s (i + 1) '\"' with
    | Some j -> TextToken (String.sub s (i + 1) (j - i - 1)), j + 1
    | None -> raise (SyntaxError i)
    )
  | '#' ->
    let x, j = scan_word s (i + 1) in
    (match List.assoc_opt x keys with
    | Some key -> KeyToken key, j
    | None -> raise (SyntaxError i)
    )
  | c when c >= '\x80' ->
    let s', j = scan_word s i in
    TextToken s', j
  | _ -> raise (SyntaxError i)


let search_keys =
  [`Artist; `Title; `AlbumArtist; `AlbumTitle; `Label; `Country; `Date]

let rec coerce_bool = function
  | Key (`FileExists | `True | `False | `Cover) as q -> q
  | Text _ | Bin (Cat, _, _) | Key _ as q ->
    (* Treat text literal in Boolean position as search term *)
    List.fold_right (fun key q' ->
      Bin (Or, Bin (IN, q, Key key), q')
    ) search_keys (Key `False)
  | Int (_, s) | Time (_, s) | Date (_, s) ->
    (* Treat other literals in Boolean position as search terms as well *)
    coerce_bool (Text s)
  | Un (Neg, q) ->
    (* Treat negation in Boolean position as logical negation *)
    Un (Not, coerce_bool q)
  | Bin (Sub, q1, q2) ->
    (* Treat subtraction in Boolean position as logical negation of r.h.s. *)
    Bin (And, coerce_bool q1, Un (Not, coerce_bool q2))
  | q -> q


let rec parse_prim s i =
  match token s i with
  | LParToken, j ->
    let q, k = parse_disj s j in
    let rpar, l = token s k in
    if rpar <> RParToken then raise (SyntaxError k) else
    q, l
  | TextToken s', j -> Text s', j
  | IntToken (n, s'), j -> Int (n, s'), j
  | TimeToken (t, s'), j -> Time (t, s'), j
  | DateToken (t, s'), j -> Date (t, s'), j
  | KeyToken key, j -> Key key, j
  | _ -> raise (SyntaxError i)

and parse_mul s i =
  let q, j = parse_prim s i in
  parse_mul_rest q s j

and parse_mul_rest q1 s i =
  match token s i with
  | BinopToken ((Mul) as op), j ->
    let q2, k = parse_prim s j in
    parse_mul_rest (Bin (op, q1, q2)) s k
  | _ -> q1, i

and parse_add s i =
  match token s i with
  | BinopToken Sub, j ->
    let q, k = parse_mul s j in
    parse_add_rest (Un (Neg, q)) s k
  | _ ->
    let q, j = parse_mul s i in
    parse_add_rest q s j

and parse_add_rest q1 s i =
  match token s i with
  | BinopToken ((Add | Sub | Cat) as op), j ->
    let q2, k = parse_mul s j in
    parse_add_rest (Bin (op, q1, q2)) s k
  | _ -> q1, i

and parse_rel s i =
  let q1, j = parse_add s i in
  match token s j with
  | BinopToken ((EQ | NE | LT | GT | LE | GE | IN | NI) as op), k ->
    let q2, l = parse_add s k in
    Bin (op, q1, q2), l
  | _ -> q1, j

and parse_neg s i =
  match token s i with
  | UnopToken Not, j ->
    let q, k = parse_rel s j in
    Un (Not, coerce_bool q), k
  | _ ->
    parse_rel s i

and parse_conj s i =
  let tok, _ = token s i in
  match tok with
  | EndToken | SortToken | RParToken | BinopToken Or ->
    Key `True, i  (* empty conjunction *)
  | _ ->
    let q, j = parse_neg s i in
    parse_conj_rest q s j

and parse_conj_rest q1 s i =
  let tok, j = token s i in
  match tok with
  | EndToken | SortToken | RParToken | BinopToken Or ->
    q1, i
  | BinopToken And ->
    let q2, k = parse_neg s j in
    parse_conj_rest (Bin (And, coerce_bool q1, coerce_bool q2)) s k
  | _ ->
    let q2, k = parse_neg s i in
    parse_conj_rest (Bin (And, coerce_bool q1, coerce_bool q2)) s k

and parse_disj s i =
  let q, j = parse_conj s i in
  parse_disj_rest q s j

and parse_disj_rest q1 s i =
  match token s i with
  | BinopToken Or, j ->
    let q2, k = parse_conj s j in
    parse_disj_rest (Bin (Or, coerce_bool q1, coerce_bool q2)) s k
  | _ -> q1, i

let rec parse_sort s i =
  match token s i with
  | EndToken, _ -> []
  | KeyToken (#track_attr as key), j -> (key, `Asc) :: parse_sort s j
  | BinopToken Sub, j ->
    (match token s j with
    | KeyToken (#track_attr as key), k -> (key, `Desc) :: parse_sort s k
    | _ -> raise (SyntaxError j)
    )
  | _ -> raise (SyntaxError i)


let parse_query s : (query, string) result =
  try
    let q, j = parse_disj s 0 in
    let q' = coerce_bool q in
    if validate q' <> BoolT then raise TypeError;
    let ks =
      match token s j with
      | EndToken, _ -> []
      | SortToken, k -> parse_sort s k
      | _ -> raise (SyntaxError j)
    in
    Ok {expr = q'; sort = ks}
  with
  | SyntaxError _i -> Error "Syntax error"
  | TypeError -> Error "Type error"

let parse_expr s : (expr, string) result =
  match parse_query s with
  | Ok {expr; sort = []} -> Ok expr
  | Ok _ -> Error "Syntax error"
  | Error s -> Error s


let quote_re = Str.regexp "\""

let quote s =
  if s <> "" && s.[0] <> '-' && String.for_all is_letter s then s else
  "\"" ^ Str.global_replace quote_re "\\\"" s ^ "\""
