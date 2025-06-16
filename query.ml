open Audio_file
open Meta
open Data


(* Values *)

type key = Data.query_attr

type value =
  | BoolV of bool
  | IntV of int
  | TimeV of time
  | DateV of date
  | TextV of string


let file_value path (file : file) = function
  | `FilePath -> TextV path
  | `FileDir -> TextV (File.dir path)
  | `FileName -> TextV (File.name path)
  | `FileExt -> TextV (File.extension path)
  | `FileSize -> IntV file.size
  | `FileTime -> DateV file.time

let format_value (format_opt : Format.t option) =
  let format = Option.value format_opt ~default: Format.unknown in
  function
  | `Length -> TimeV format.time
  | `Codec -> TextV format.codec
  | `Channels -> IntV format.channels
  | `Depth ->
    let d = format.bitrate /. float format.rate /. float format.channels in
    IntV (Float.to_int d)
  | `SampleRate -> IntV format.rate
  | `BitRate -> IntV (Float.to_int format.bitrate)
  | `Rate -> assert false

let meta_value (meta_opt : Meta.t option) =
  let meta = Option.value meta_opt ~default: Meta.unknown in
  function
  | `Artist -> TextV meta.artist
  | `Title -> TextV meta.title
  | `AlbumArtist -> TextV meta.albumartist
  | `AlbumTitle -> TextV meta.albumtitle
  | `Track -> IntV meta.track
  | `Tracks -> IntV meta.tracks
  | `Disc -> IntV meta.disc
  | `Discs -> IntV meta.discs
  | `DiscTrack when meta.disc = 0 -> TextV (Printf.sprintf "%3d" meta.track)
  | `DiscTrack -> TextV (Printf.sprintf "%d.%02d" meta.disc meta.track)
  | `Date -> DateV meta.date
  | `Year -> IntV meta.year
  | `Label -> TextV meta.label
  | `Country -> TextV meta.country
  | `Length -> TimeV meta.length
  | `Rating -> IntV meta.rating
  | `Cover -> BoolV (meta.cover <> None)

let value key (track : track) =
  match key with
  | `True -> BoolV true
  | `False -> BoolV false
  | `Now -> TimeV (Unix.gettimeofday ())
  | `Random -> IntV (Random.int 0x1_0000_0000)
  | `Pos -> IntV (track.pos + 1)
  | `Length ->
    let v = format_value track.format `Length in
    if v <> TimeV 0.0 then v else meta_value track.meta `Length
  | `Year ->
    let v = meta_value track.meta `Year in
    if v <> IntV 0 then v else
    (match meta_value track.meta `Date with
    | DateV 0.0 -> v
    | DateV t -> IntV (year_of_date t)
    | _ -> assert false
    )
  | `Date ->
    let v = meta_value track.meta `Date in
    if v <> DateV 0.0 then v else
    (match meta_value track.meta `Year with
    | IntV 0 -> v
    | IntV n -> DateV (date_of_year n)
    | _ -> assert false
    )
  | #file_attr as attr -> file_value track.path track.file attr
  | #format_attr as attr -> format_value track.format attr
  | #meta_attr as attr -> meta_value track.meta attr
  | `Albums | `None -> assert false

let string_of_value = function
  | BoolV b -> string_of_bool b
  | IntV i -> string_of_int i
  | TimeV t -> Data.string_of_time t
  | DateV t -> Data.string_of_date_time t
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
  | Date of date
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
  | Date t -> Data.string_of_date_time t
  | Text s -> "\"" ^ s ^ "\""
  | Key k -> "$" ^ string_of_key k
  | Un (op, e) -> "(" ^ string_of_unop op ^ " " ^ string_of_expr e ^ ")"
  | Bin (op, e1, e2) ->
    "(" ^ string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2 ^ ")"

let string_of_query {expr; sort} =
  string_of_expr expr ^ " ^ " ^
  String.concat " "
    (List.map (fun (k, o) -> string_of_order o ^ string_of_key k) sort)


exception TypeError

let rec validate q =
  match q with
  | Key (`True | `False | `Cover) -> BoolT
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
    | (IN | NI), TextT, TextT -> BoolT
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

let string_contains_caseless ~inner s =
  string_contains ~inner: (Data.UCase.casefolding inner) (Data.UCase.casefolding s)


let rec eval q track =
  match q with
  | Text s -> TextV s
  | Int (i, _) -> IntV i
  | Time (t, _) -> TimeV t
  | Date t -> DateV t
  | Key key -> value key track
  | Un (Not, q1) -> BoolV (not (check q1 track))
  | Bin (And, q1, q2) -> BoolV (check q1 track && check q2 track)
  | Bin (Or, q1, q2) -> BoolV (check q1 track || check q2 track)
  | Un (op, q1) ->
    (match op, eval q1 track with
    | Neg, IntV i -> IntV (- i)
    | Neg, TimeV t -> TimeV (-. t)
    | _ -> assert false
    )
  | Bin (op, q1, q2) ->
    (match op, eval q1 track, eval q2 track with
    | EQ, v1, v2 -> BoolV (v1 = v2)
    | NE, v1, v2 -> BoolV (v1 <> v2)
    | LT, v1, v2 -> BoolV (v1 < v2)
    | GT, v1, v2 -> BoolV (v1 > v2)
    | LE, v1, v2 -> BoolV (v1 <= v2)
    | GE, v1, v2 -> BoolV (v1 >= v2)
    | IN, TextV t1, TextV t2 -> BoolV (string_contains_caseless ~inner: t1 t2)
    | NI, TextV t1, TextV t2 -> BoolV (not (string_contains_caseless ~inner: t1 t2))
    | Add, IntV i1, IntV i2 -> IntV (i1 + i2)
    | Add, TimeV t1, TimeV t2 -> TimeV (t1 +. t2)
    | Add, DateV t1, TimeV t2 -> DateV (t1 +. t2)
    | Sub, IntV i1, IntV i2 -> IntV (i1 - i2)
    | Sub, TimeV t1, TimeV t2 -> TimeV (t1 -. t2)
    | Sub, DateV t1, DateV t2 -> TimeV (t1 -. t2)
    | Sub, DateV t1, TimeV t2 -> DateV (t1 -. t2)
    | Mul, IntV i1, IntV i2 -> IntV (i1 * i2)
    | Mul, TimeV t1, IntV i2 -> TimeV (t1 *. float_of_int i2)
    | Mul, IntV i1, TimeV t2 -> TimeV (float_of_int i1 *. t2)
    | Cat, TextV s1, TextV s2 -> TextV (s1 ^ s2)
    | _ -> assert false
    )

and check q track =
  match eval q track with
  | BoolV b -> b
  | _ -> assert false


module AlbumKey =
struct
  type t = string * string * string * string
  let compare : t -> t -> int = compare
end
module AlbumMap = Map.Make(AlbumKey)
module ArtistMap = Map.Make(String)

let album_key (track : track) : AlbumKey.t =
  ( track_attr_string track `AlbumArtist,
    track_attr_string track `AlbumTitle,
    track_attr_string track `Codec,
    track_attr_string track `Label
  )

let array_of_map iter map =
  let a = Dynarray.create () in
  iter (fun _ x -> Dynarray.add_last a x) map;
  Dynarray.to_array a

let rec iter_dir f (dir : _ dir) =
  Array.iter (iter_dir f) dir.children;
  Array.iter f dir.tracks

let sort s tracks =
  if s <> [] then
  (
    let tracks' =
      Array.map (fun tr -> Data.key_entry track_attr_string s tr, tr) tracks in
    Array.stable_sort compare tracks';
    Array.iteri (fun i (_, tr) -> tracks.(i) <- tr) tracks';
  )

let exec with_artists with_albums with_tracks q p dir =
  let tracks = Dynarray.create () in
  let album_map = ref AlbumMap.empty in
  let artist_map = ref ArtistMap.empty in
  iter_dir (fun track ->
    if not (M3u.is_separator track.path) && check q.expr track && p track then
    (
      if with_tracks then Dynarray.add_last tracks track;
      if with_albums || with_artists then
      (
        let album_key = album_key track in
        let album = Data.album_of_track track in
        let album' =
          match AlbumMap.find_opt album_key !album_map with
          | None -> album
          | Some album' -> Data.accumulate_album album album'
        in
        album_map := AlbumMap.add album_key album' !album_map;
        if with_artists then
        (
          let artist = Data.artist_of_album album' in
          artist_map := ArtistMap.add artist.name artist !artist_map;
        )
      )
    )
  ) dir;
  (if with_artists then array_of_map ArtistMap.iter !artist_map else [||]),
  (if with_albums then array_of_map AlbumMap.iter !album_map else [||]),
  ( let tracks = if with_tracks then Dynarray.to_array tracks else [||] in
    sort q.sort tracks;
    tracks
  )

let exec_tracks q p dir = let _, _, z = exec false false true q p dir in z
let exec_albums q p dir = let _, y, _ = exec false true false q p dir in y
let exec_artists q p dir = let x, _, _ = exec true false false q p dir in x
let exec_albums_tracks q p dir = let _, y, z = exec false true true q p dir in y, z
let exec_artists_albums q p dir = let x, y, _ = exec true true false q p dir in x, y
let exec_artists_albums_tracks q p dir = exec true true true q p dir


(* Parsing *)

exception SyntaxError of int

type token =
  | TextToken of string
  | IntToken of int * string
  | TimeToken of time * string
  | DateToken of time
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
  | KeyToken x -> "$" ^ string_of_key x
  | UnopToken op -> string_of_unop op
  | BinopToken op -> string_of_binop op
  | LParToken -> "("
  | RParToken -> ")"
  | SortToken -> "^"
  | EndToken -> "(end of string)"
*)

let is c s i = i < String.length s && s.[i] = c
let is_letter = function
  | '0'..'9' | 'A'..'Z' | 'a'..'z' | '_' | '.' -> true
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
    DateToken t, n
  else
    TextToken s', n

let rec scan_time s i t =
  let n, j = scan_num s i in
  if n = "" then t, i else
  let z = float_of_string n in
  match scan_name s j with
  | "s", k -> scan_time s k (t +. z)
  | "m", k -> scan_time s k (t +. 60.0 *. z)
  | "h", k -> scan_time s k (t +. 60.0 *. 60.0 *. z)
  | "d", k -> scan_time s k (t +. 24.0 *. 60.0 *. 60.0 *. z)
  | "y", k -> scan_time s k (t +. 365.0 *. 24.0 *. 60.0 *. 60.0 *. z)
  | _ -> raise (SyntaxError j)

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
  | '+' -> BinopToken Add, i + 1
  | '-' when is '@' s (i + 1) -> BinopToken NI, i + 2
  | '-' -> BinopToken Sub, i + 1
  | '*' -> BinopToken Mul, i + 1
  | '@' -> BinopToken IN, i + 1
  | '=' -> BinopToken EQ, i + 1
  | '#' -> BinopToken Cat, i + 1
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
      let t, j = scan_time s i 0.0 in
      let post, k = scan_word s j in
      let s' = String.sub s (i + 1) (k - i) in
      if post = "" then
        TimeToken (t, s'), k
      else
        TextToken s', k
    )
  | 'a'..'z' | 'A'..'Z' | '_' | '.' ->
    let s', j = scan_word s i in
    TextToken s', j
  | '\"' ->
    (match String.index_from_opt s (i + 1) '\"' with
    | Some j -> TextToken (String.sub s (i + 1) (j - i - 1)), j + 1
    | None -> raise (SyntaxError i)
    )
  | '$' ->
    let x, j = scan_word s (i + 1) in
    (match List.assoc_opt x keys with
    | Some key -> KeyToken key, j
    | None -> raise (SyntaxError i)
    )
  | c when c >= '\x80' ->
    let s', j = scan_word s i in
    TextToken s', j
  | _ -> raise (SyntaxError i)


let search_keys = [`Artist; `Title; `AlbumArtist; `AlbumTitle; `Label; `Country]

let rec coerce_bool = function
  | Key (`True | `False | `Cover) as q -> q
  | Text _ | Bin (Cat, _, _) | Key _ as q ->
    (* Treat text literal in Boolean position as search term *)
    List.fold_right (fun key q' ->
      Bin (Or, Bin (IN, q, Key key), q')
    ) search_keys (Key `False)
  | Int (_, s) | Time (_, s) ->
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
  | DateToken t, j -> Date t, j
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
    let q'= coerce_bool q in
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
