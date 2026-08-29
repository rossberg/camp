open Audio_file
open Meta
open Data


(* Values *)

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
  | `Date when meta.date > 0.0 -> DateV (meta.date, Some meta.date_txt)
  | `Date -> DateV (date_of_year meta.year, None)
  | `Year when meta.year > 0 -> IntV (meta.year, None)
  | `Year -> IntV (year_of_date meta.date, None)
  | `Label -> TextV meta.label
  | `Country -> TextV meta.country
  | `Length -> TimeV (meta.length, None)
  | `Rating -> IntV (meta.rating, None)
  | `Cover -> BoolV (meta.cover <> None)

let artist_value attr (artist : artist) =
  match attr with
  | `Artist -> TextV artist.name
  | `Tracks -> IntV (artist.tracks, None)
  | `Albums -> IntV (artist.albums, None)

let album_value attr (album : album) =
  match attr with
  | `AlbumName as attr' -> TextV (Data.album_attr_string album attr')
  | #file_attr as attr' -> file_value album.path album.file attr'
  | #format_attr as attr' -> format_value album.format attr'
  | #meta_attr as attr' -> meta_value album.meta attr'

let track_value attr (track : track) =
  match attr with
  | `Playlist -> TextV track.playlist
  | `Pos -> IntV (track.pos + 1, None)
  | `Name | `AlbumName as attr' -> TextV (Data.track_attr_string track attr')
  | #file_attr as attr' -> file_value track.path track.file attr'
  | #format_attr as attr' -> format_value track.format attr'
  | #meta_attr as attr' -> meta_value track.meta attr'

let value (type x a) (k : (x, a) Data.kind) (attr : a) (x : x) : value =
  match k with
  | Artist -> artist_value attr x
  | Album -> album_value attr x
  | Track -> track_value attr x


let string_of_value = function
  | BoolV true -> "yes"
  | BoolV false -> "no"
  | IntV (i, _) -> string_of_int i
  | TimeV (t, _) -> Data.string_of_time t
  | DateV (t, _) -> Data.string_of_date_time t
  | TextV s -> s
(*
  | TextV s ->
    let buf = Buffer.create (String.length s) in
    String.iter (fun c ->
      if c >= '\x20' then
        Buffer.add_char buf c
      else
        Buffer.add_string buf (Printf.sprintf "\\%02x" (Char.code c))
    ) s;
    "\"" ^ Buffer.contents buf ^ "\""
*)


(* Queries *)

type key = Data.any_attr
type track = Data.track
type album = Data.album
type artist = Data.artist
type order = Data.order
type 'a sorting = 'a Data.sorting

type fnop = True | False | Now | Random | Min | Max | Avg | If | Id
type unop = Not | Neg
type binop =
  | And | Or | EQ | NE | LT | GT | LE | GE | IN | NI | Add | Sub | Mul | Cat

type expr =
  | Text of string
  | Int of int * string
  | Time of time * string
  | Date of date * string
  | Key of key
  | Fn of fnop * expr list
  | Un of unop * expr
  | Bin of binop * expr * expr

type query = {expr : expr; sort : Data.track_attr sorting}

type type_ =
  | BoolT
  | TextT
  | IntT
  | TimeT
  | DateT

type keys = (string * key) list

let artist_keys : keys =
  [
    "artist", `Artist;
    "albums", `Albums; "tracks", `Tracks;
  ]

let album_keys : keys =
  [
    "fileexists", `FileExists;
    "filetime", `FileTime; "filesize", `FileSize;
    "filepath", `FilePath; "filedir", `FileDir;
    "filename", `FileName; "fileext", `FileExt;
    "title", `Title; "artist", `Artist;
    "disc", `Disc; "track", `Track; "discs", `Discs; "tracks", `Tracks;
    "disctrack", `DiscTrack;
    "album", `AlbumTitle; "albumartist", `AlbumArtist; "albumname", `AlbumName;
    "year", `Year; "date", `Date;
    "label", `Label; "country", `Country;
    "length", `Length; "rating", `Rating;
    "codec", `Codec; "channels", `Channels; "depth", `Depth;
    "samplerate", `SampleRate; "bitrate", `BitRate;
    "cover", `Cover;
  ]

let track_keys : keys =
  [
    "name", `Name; "playlist", `Playlist; "pos", `Pos;
  ] @ album_keys

let any_keys : keys =
  List.sort_uniq compare (artist_keys @ album_keys @ track_keys)

let keys (type x a) (k : (x, a) Data.kind) : keys =
  match k with
  | Artist -> artist_keys
  | Album -> album_keys
  | Track -> track_keys

let fns =
  [
    "true", True; "false", False;
    "random", Random; "now", Now;
    "min", Min; "max", Max; "avg", Avg;
    "if", If;
    "", Id;
  ]


let empty_query = {expr = Fn (False, []); sort = []}
let full_query = {expr = Fn (True, []); sort = []}


let string_of_key k =
  fst (List.find (fun (_, x) -> x = k) any_keys)

let string_of_fn f =
  fst (List.find (fun (_, x) -> x = f) fns)

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
  | NI -> "`~@"
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Cat -> "++"

let rec string_of_expr = function
  | Int (_, s) -> s
  | Time (_, s) -> s
  | Date (_, s) -> s
  | Text s -> "\"" ^ s ^ "\""
  | Key k -> "#" ^ string_of_key k
  | Fn (f, []) -> "#" ^ string_of_fn f
  | Fn (f, es) ->
    "#" ^ string_of_fn f ^ "(" ^ String.concat ", " (List.map string_of_expr es) ^ ")"
  | Un (op, e) -> "(" ^ string_of_unop op ^ " " ^ string_of_expr e ^ ")"
  | Bin (op, e1, e2) ->
    "(" ^ string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2 ^ ")"

let string_of_query {expr; sort} =
  string_of_expr expr ^ " ^ " ^
  String.concat " "
    (List.map (fun (k, o) -> string_of_order o ^ string_of_key (k :> key)) sort)


(* Validation *)

exception TypeError of expr

let attr_type = function
  | `FileExists | `Cover -> BoolT
  | `FileSize | `Disc | `Track | `Discs | `Tracks | `Albums | `Pos | `Year
  | `Rating | `Channels | `Depth | `SampleRate | `BitRate | `Rate -> IntT
  | `Length -> TimeT
  | `Date | `FileTime -> DateT
  | `FilePath | `FileDir | `FileName | `FileExt
  | `Artist | `Title | `Name | `AlbumArtist | `AlbumTitle | `AlbumName
  | `Label | `Country | `Codec | `DiscTrack
  | `Playlist -> TextT

let rec validate : 'x 'a. ('x, 'a) Data.kind -> expr -> type_ =
  fun (type x a) (k : (x, a) Data.kind) q ->
  match q with
  | Int _ -> IntT
  | Time _ -> TimeT
  | Date _ -> DateT
  | Text _ -> TextT
  | Key attr ->
    (match k, attr with
    | Artist, #artist_attr -> ()
    | Album, #album_attr -> ()
    | Track, #track_attr -> ()
    | _, _ -> raise (TypeError q)
    );
    attr_type attr
  | Fn (fn, qs) ->
    let ts = List.map (validate k) qs in
    (match fn, ts with
    | (True | False), [] -> BoolT
    | Random, ts ->
      if List.exists ((<>) IntT) ts || List.length ts > 2 then
        raise (TypeError q);
      IntT
    | Now, [] -> DateT
    | (Min | Max), t::ts' ->
      if List.exists ((<>) t) ts' then raise (TypeError q);
      t
    | Avg, (IntT | TimeT | DateT as t)::ts' ->
      if List.exists ((<>) t) ts' then raise (TypeError q);
      t
    | If, [t1; t2; t3] ->
      if t1 <> BoolT || t2 <> t3 then raise (TypeError q);
      t2
    | Id, [t] ->
      t
    | _ -> raise (TypeError q)
    )
  | Un (op, q1) ->
    (match op, validate k q1 with
    | Not, BoolT -> BoolT
    | Neg, IntT -> IntT
    | Neg, TimeT -> TimeT
    | _ -> raise (TypeError q)
    )
  | Bin (op, q1, q2) ->
    (match op, validate k q1, validate k q2 with
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
    | _ -> raise (TypeError q)
    )


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

let rec eval : 'x 'a. ('x, 'a) Data.kind -> expr -> 'x -> value =
  fun (type x a) (k : (x, a) Data.kind) q (x : x) ->
  match q with
  | Text s -> TextV s
  | Int (i, s) -> IntV (i, Some s)
  | Time (t, s) -> TimeV (t, Some s)
  | Date (t, s) -> DateV (t, Some s)
  | Key key ->
    (match k, key with
    | Artist, (#Data.artist_attr as attr) -> artist_value attr x
    | Album, (#Data.album_attr as attr) -> album_value attr x
    | Track, (#Data.track_attr as attr) -> track_value attr x
    | _, _ -> assert false
    )
  | Fn (fn, qs) ->
    let vs = List.map (fun q -> eval k q x) qs in
    (match fn, vs with
    | True, [] -> BoolV true
    | False, [] -> BoolV false
    | Random, [] -> IntV (Random.int 0x1_0000_0000, None)
    | Random, [v1] ->
      (match v1 with
      | IntV (i, _) -> IntV ((if i < 1 then 0 else Random.int i), None)
      | _ -> assert false
      )
    | Random, [v1; v2] ->
      (match v1, v2 with
      | IntV (i1, _), IntV (i2, _) ->
        IntV ((if i1 >= i2 || i2 < 1 then 0 else Random.int (i2 - i1) + i1), None)
      | _ -> assert false
      )
    | Now, [] -> DateV (Unix.gettimeofday (), None)
    | Min, v1::vs' -> List.fold_left min v1 vs'
    | Max, v1::vs' -> List.fold_left max v1 vs'
    | Avg, v1::vs' ->
      let n = float (List.length vs) in
      (match
        List.fold_left (fun v1 v2 ->
          match v1, v2 with
          | IntV (i1, _), IntV (i2, _) -> IntV (i1 + i2, None)
          | TimeV (t1, _), TimeV (t2, _) -> TimeV (t1 +. t2, None)
          | DateV (t1, _), TimeV (t2, _) -> DateV (t1 +. t2, None)
          | _ -> assert false
        ) v1 vs'
      with
      | IntV (i, _) -> IntV (int_of_float (Float.round (float i /. n)), None)
      | TimeV (t, _) -> TimeV (t /. n, None)
      | DateV (t, _) -> DateV (t /. n, None)
      | _ -> assert false
      )
    | If, [v1; v2; v3] ->
      (match v1 with
      | BoolV true -> v2
      | BoolV false -> v3
      | _ -> assert false
      )
    | Id, [v] ->
      v
    | _ -> assert false
    )
  | Un (Not, q1) -> BoolV (not (check k q1 x))
  | Bin (And, q1, q2) -> BoolV (check k q1 x && check k q2 x)
  | Bin (Or, q1, q2) -> BoolV (check k q1 x || check k q2 x)
  | Un (op, q1) ->
    (match op, eval k q1 x with
    | Neg, IntV (i, _) -> IntV (- i, None)
    | Neg, TimeV (t, _) -> TimeV (-. t, None)
    | _ -> assert false
    )
  | Bin (op, q1, q2) ->
    (match op, eval k q1 x, eval k q2 x with
    | EQ, v1, v2 -> BoolV (lit v1 = lit v2)
    | NE, v1, v2 -> BoolV (lit v1 <> lit v2)
    | LT, v1, v2 -> BoolV (lit v1 < lit v2)
    | GT, v1, v2 -> BoolV (lit v1 > lit v2)
    | LE, v1, v2 -> BoolV (lit v1 <= lit v2)
    | GE, v1, v2 -> BoolV (lit v1 >= lit v2)
    | IN, v1, v2 ->
      BoolV (Unicode.includes_utf_8_diacriticless ~affix: (text v1) (text v2))
    | NI, v1, v2 ->
      BoolV (not (Unicode.includes_utf_8_diacriticless ~affix: (text v1) (text v2)))
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

and check : 'x 'a. ('x, 'a) Data.kind -> expr -> 'x -> bool = fun k q x ->
  match eval k q x with
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
module ArtistSet = Set.Make(String)
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

let new_artist name : artist =
  { name;
    albums = 0;
    tracks = 0;
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

let sort (type x a) (k : (x, a) Data.kind) (s : a sorting) (xs : x array) =
  if s <> [] then
  (
    let xs' : (string list * x) array =
      match k with
      | Artist -> 
        Array.map (fun x -> Data.key_entry artist_attr_string s x, x) xs
      | Album ->
        Array.map (fun x -> Data.key_entry album_attr_string s x, x) xs
      | Track ->
        Array.map (fun x -> Data.key_entry track_attr_string s x, x) xs
    in
    Array.stable_sort compare xs';
    Array.iteri (fun i (_, x) -> xs.(i) <- x) xs';
  )

let exec q p dir =
  let t_start = Unix.gettimeofday () in
  let t_check = ref 0.0 in
  let tracks = Dynarray.create () in
  let albums = Dynarray.create () in
  let artists = Dynarray.create () in
  let album_set = ref AlbumSet.empty in  (* all albums encountered *)
  let album_map = ref AlbumMap.empty in  (* albums already in albums array *)
  let artist_map = ref ArtistMap.empty in
  iter_dir (fun track ->
    let t1 = Unix.gettimeofday () in
    let b = check Track q.expr track in
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
        if to_albums then
        (
          match AlbumMap.find_opt album_key !album_map with
          | None ->
            album_map := AlbumMap.add album_key album !album_map;
            Dynarray.add_last albums album;
          | Some album' ->
            album'.file <- accumulate_file album'.file album.file;
            album'.format <-
              accumulate_option accumulate_format album'.format album.format;
            album'.meta <-
              accumulate_option accumulate_meta album'.meta album.meta;
        );
        if to_artists then
        (
          let find_artist name =
            match ArtistMap.find_opt name !artist_map with
            | None ->
              let artist = new_artist name in
              artist_map := ArtistMap.add artist.name artist !artist_map;
              Dynarray.add_last artists artist;
              artist
            | Some artist -> artist
          in
          let tname = Data.track_attr_string track `Artist in
          ArtistSet.iter (fun name ->
            let artist = find_artist name in
            artist.tracks <- artist.tracks + 1;
          ) (ArtistSet.of_list (tname :: Meta.artists_of_artist tname));
          if not (AlbumSet.mem album_key !album_set) then
          (
            album_set := AlbumSet.add album_key !album_set;
            let aname = Data.track_attr_string track `AlbumArtist in
            ArtistSet.iter (fun name ->
              let artist = find_artist name in
              artist.albums <- artist.albums + 1;
            ) (ArtistSet.of_list (aname :: Meta.artists_of_artist aname))
          )
        )
      )
    )
  ) dir;
  Dynarray.to_array artists,
  Dynarray.to_array albums,
  let tracks = Dynarray.to_array tracks in
  let t_sort = Unix.gettimeofday () in
  sort Data.Track q.sort tracks;
  let t_finish = Unix.gettimeofday () in
  if !App.debug_perf then
    Printf.eprintf
      "    [exec %s...] %.3f s = %.3f search (%.3f check), %.3f sort\n%!"
      (let s = string_of_expr q.expr in String.(sub s 0 (min 30 (length s))))
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
  | FnToken of fnop
  | UnopToken of unop
  | BinopToken of binop
  | LParToken
  | RParToken
  | CommaToken
  | SortToken
  | EndToken

(*
let string_of_token = function
  | TextToken s -> "\"" ^ s ^ "\""
  | IntToken (_, s) -> s
  | TimeToken (_, s) -> s
  | DateToken t -> Date.string_of_date t
  | KeyToken x -> "#" ^ string_of_key x
  | FnToken x -> "#" ^ string_of_fn x
  | UnopToken op -> string_of_unop op
  | BinopToken op -> string_of_binop op
  | LParToken -> "("
  | RParToken -> ")"
  | CommaToken -> ","
  | SortToken -> "^"
  | EndToken -> "(end of string)"
*)

let is c s i = i < String.length s && s.[i] = c
let is_letter = function
  | '0'..'9' | 'A'..'Z' | 'a'..'z' | '_' -> true
  | c -> false
let is_letter_ex = function
  | '0'..'9' | 'A'..'Z' | 'a'..'z' | '_' | '.' | '!' | '?' | '-' -> true
  | c -> c >= '\x80'

let quote_re = Str.regexp "\""

let quote s =
  if s <> "" && s.[0] <> '-' && String.for_all is_letter s then s else
  "\"" ^ Str.global_replace quote_re "\\\"" s ^ "\""


let scan_word s i =
  let j = ref i in
  while !j < String.length s && is_letter s.[!j] do
    incr j
  done;
  String.sub s i (!j - i), !j

let scan_word_ex s i =
  let j = ref i in
  while !j < String.length s && is_letter_ex s.[!j] do
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
  let int = IntToken (int_of_string y, y) in
  if y = "" || not (is '/' s j || is '-' s j) then int, j else
  let m, k = scan_num s (j + 1) in
  if m = "" || not (is s.[j] s k) then int, j else
  let d, l = scan_num s (k + 1) in
  if d = "" then int, j else
  let post, n = scan_word_ex s l in
  let s' = String.sub s i (n - i) in
  if post = "" then
    let t = date (int_of_string y) (int_of_string m) (int_of_string d) in
    DateToken (t, s'), n
  else if is '-' s j then
    int, j
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
  | ',' -> CommaToken, i + 1
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
    if is '/' s j || is '-' s j then scan_date s i else
    let suf, k = scan_word_ex s j in
    (match suffix suf with
    | Some m -> IntToken (int_of_string n * m, String.sub s i (k - i)), k
    | None ->
      match scan_time s i 0.0 with
      | None ->
        let s', k = scan_word_ex s i in
        TextToken s', k
      | Some (t, j) ->
        let post, k = scan_word_ex s j in
        let s' = String.sub s i (k - i) in
        if post = "" then
          TimeToken (t, s'), k
        else
          TextToken s', k
    )
  | 'a'..'z' | 'A'..'Z' | '_' | '.' | '!' | '?' ->
    let s', j = scan_word_ex s i in
    TextToken s', j
  | '\"' ->
    (match String.index_from_opt s (i + 1) '\"' with
    | Some j -> TextToken (String.sub s (i + 1) (j - i - 1)), j + 1
    | None -> raise (SyntaxError i)
    )
  | '#' ->
    let x, j = scan_word s (i + 1) in
    if x = "" && not (is '(' s (i + 1)) then raise (SyntaxError i) else
    (match List.assoc_opt x fns with
    | Some fn -> FnToken fn, j
    | None ->
      match List.assoc_opt x any_keys with
      | Some key -> KeyToken key, j
      | None -> raise (SyntaxError i)
    )
  | c when c >= '\x80' ->
    let s', j = scan_word_ex s i in
    TextToken s', j
  | _ -> raise (SyntaxError i)


let search_keys =
  [`Artist; `Title; `AlbumArtist; `AlbumTitle; `Label; `Country; `Date]

let rec coerce_bool = function
  | Text _ | Bin (Cat, _, _) as q ->
    (* Treat text literal in Boolean position as search term *)
    List.fold_right (fun key q' ->
      Bin (Or, Bin (IN, q, Key key), q')
    ) search_keys (Fn (False, []))
  | Int (_, s) | Time (_, s) | Date (_, s) ->
    (* Treat other literals in Boolean position as search terms as well *)
    coerce_bool (Text s)
  | Un (Neg, q1) ->
    (* Treat negation in Boolean position as logical negation *)
    Un (Not, coerce_bool q1)
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
  | FnToken fn, j ->
    let lpar, k = token s j in
    if lpar <> LParToken then Fn (fn, []), j else
    let qs, l = parse_list s k [] in
    let rpar, m = token s l in
    if rpar <> RParToken then raise (SyntaxError l) else
    (match fn, qs with
    | Id, [] -> Text "", m  (* special case: #() treated as "" *)
    | If, q1::qs' -> Fn (fn, coerce_bool q1 :: qs'), m
    | _, _ -> Fn (fn, qs), m
    )
  | _ -> raise (SyntaxError i)

and parse_mul s i =
  let q, j = parse_prim s i in
  parse_mul_rest s j q

and parse_mul_rest s i q1 =
  match token s i with
  | BinopToken ((Mul) as op), j ->
    let q2, k = parse_prim s j in
    parse_mul_rest s k (Bin (op, q1, q2))
  | _ -> q1, i

and parse_add s i =
  match token s i with
  | BinopToken Sub, j ->
    let q, k = parse_mul s j in
    parse_add_rest s k (Un (Neg, q))
  | _ ->
    let q, j = parse_mul s i in
    parse_add_rest s j q

and parse_add_rest s i q1 =
  match token s i with
  | BinopToken ((Add | Sub | Cat) as op), j ->
    let q2, k = parse_mul s j in
    parse_add_rest s k (Bin (op, q1, q2))
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
  | EndToken | SortToken | RParToken | CommaToken | BinopToken Or ->
    Fn (True, []), i  (* empty conjunction *)
  | _ ->
    let q, j = parse_neg s i in
    parse_conj_rest s j q

and parse_conj_rest s i q1 =
  let tok, j = token s i in
  match tok with
  | EndToken | SortToken | RParToken | CommaToken | BinopToken Or ->
    q1, i
  | BinopToken And ->
    let q2, k = parse_neg s j in
    parse_conj_rest s k (Bin (And, coerce_bool q1, coerce_bool q2))
  | _ ->
    let q2, k = parse_neg s i in
    parse_conj_rest s k (Bin (And, coerce_bool q1, coerce_bool q2))

and parse_disj s i =
  let q, j = parse_conj s i in
  parse_disj_rest s j q

and parse_disj_rest s i q1 =
  match token s i with
  | BinopToken Or, j ->
    let q2, k = parse_conj s j in
    parse_disj_rest s k (Bin (Or, coerce_bool q1, coerce_bool q2))
  | _ -> q1, i

and parse_list s i qs =
  if fst (token s i) = RParToken then List.rev qs, i else
  let q, j = parse_disj s i in
  match token s j with
  | CommaToken, k -> parse_list s k (q::qs)
  | _ -> List.rev (q::qs), j

let rec parse_sort s i =
  match token s i with
  | EndToken, _ -> []
  | KeyToken key, j -> (key, `Asc) :: parse_sort s j
  | BinopToken Sub, j ->
    (match token s j with
    | KeyToken key, k -> (key, `Desc) :: parse_sort s k
    | _ -> raise (SyntaxError j)
    )
  | _ -> raise (SyntaxError i)

let rec parse_text s i qs =
  let tok, j = token s i in
  match tok with
  | EndToken -> List.rev qs
  | _ ->
    let q, j = parse_neg s i in
    parse_text s j (q::qs)

(*
let rec parse_text s i i0 =
  if i = String.length s then
    if i = i0 then [] else [Text (String.sub s i0 (i - i0))]
  else if s.[i] <> '#' then
    parse_text s (i + 1) i0
  else
    let qs1 = if i = i0 then [] else [Text (String.sub s i0 (i - i0))] in
    let q, j =
      if i + 1 < String.length s && s.[i + 1] = '#' then
        Text "#", i + 2
      else
        parse_prim s i
    in
    let qs2 = parse_text s j j in
    qs1 @ [q] @ qs2
*)


let try_parse s f =
  try Ok (f ()) with
  | SyntaxError i ->
    Error ("Syntax error at " ^ 
      if i = String.length s then "end" else "\"" ^ String.drop_first i s ^ "\"")
  | TypeError q -> Error ("Type error for " ^ string_of_expr q)

let parse_expr k s : (expr * type_, string) result =
  try_parse s (fun () ->
    let q, j = parse_disj s 0 in
    let t = validate k q in
    match token s j with
    | EndToken, _ -> q, t
    | _ -> raise (SyntaxError j)
  )

let parse_query s : (query, string) result =
  try_parse s (fun () ->
    let q, j = parse_disj s 0 in
    let q' = coerce_bool q in
    if validate Track q' <> BoolT then raise (TypeError q);
    let sort =
      match token s j with
      | EndToken, _ -> []
      | SortToken, l ->
        let keys = parse_sort s l in
        List.map (function
          | #Data.track_attr as key', order -> key', order
          | key, _ -> raise (TypeError (Key key))
        ) keys
      | _ -> raise (SyntaxError j)
    in {expr = q'; sort}
  )

let parse_custom k s : (expr list * type_, string) result =
  try_parse s (fun () ->
    let qs = parse_text s 0 [] in
    let ts = List.map (validate k) qs in
    let qts = List.combine qs ts in
    match List.filter (function (Text _, _) -> false | _ -> true) qts with
    | [(_, t)] -> qs, t
    | _ -> qs, TextT
  )


(* Stringification *)

type Data.custom += Set of (expr list * type_, string) result

let custom k s r =
  match !r with
  | Set res -> res
  | Unset ->
    let res = parse_custom k s in
    r := Set res;
    res
  | _ -> assert false

let custom_string k s r x =
  match custom k s r with
  | Ok (qs, _t) ->
    String.concat "" (List.map (fun q -> string_of_value (eval k q x)) qs)
  | Error _ -> ""

let artist_attr_ex_string artist = function
  | `Custom (_, s, r) -> custom_string Artist s r artist
  | #artist_attr as attr -> Data.artist_attr_string artist attr

let album_attr_ex_string album = function
  | `Custom (l, s, r) -> custom_string Album s r album
  | #album_attr as attr -> Data.album_attr_string album attr

let track_attr_ex_string track = function
  | `Custom (l, s, r) -> custom_string Track s r track
  | #track_attr as attr -> Data.track_attr_string track attr

let any_attr_ex_type = function
  | `Custom (_, _, r) ->
    (match !r with
    | Set (Ok (_qs, t)) -> Some t
    | _ -> None
    )
  | #any_attr as attr -> Some (attr_type attr)
