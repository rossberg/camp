(* Music Database *)

open Audio_file
open Data

type t = Sqlite3.db


(* TODO: use STRICT tables once Sqlite 3.37 is available on Cygwin. *)


(* Errors *)

exception Db_error = Sqlite3.SqliteError

let (let&) db f = try f () with Db_error _ -> raise (Db_error (Sqlite3.errmsg db))
let (let*) rc f =
  match rc with
  | Sqlite3.Rc.(OK | DONE | ROW) -> f ()
  | _ -> Sqlite3.Rc.check rc; f ()


(* Statement Preparation *)

let stmt sql : Sqlite3.stmt option ref * string = (ref None, sql)

let prepare db (r, sql) = Sqlite3.prepare_or_reset db r sql

let bind_bool = Sqlite3.bind_bool
let bind_int = Sqlite3.bind_int
let bind_float = Sqlite3.bind_double
let bind_text = Sqlite3.bind_text
let bind_id = Sqlite3.bind_int64

let bind_null stmt i = Sqlite3.bind stmt i Sqlite3.Data.NULL

let bind_opt bind_x stmt i = function
  | None -> bind_null stmt i
  | Some x -> bind_x stmt i x

let bind_bool_opt stmt i = bind_opt bind_bool stmt i
let bind_int_opt stmt i = bind_opt bind_int stmt i
let bind_float_opt stmt i = bind_opt bind_float stmt i
let bind_text_opt stmt i = bind_opt bind_text stmt i
let bind_id_opt stmt i = bind_opt bind_id stmt i

let bind_default bind_x def stmt i v =
  if v = def then bind_null stmt i else bind_x stmt i v

let bind_int_default stmt i = bind_default bind_int 0 stmt i
let bind_float_default stmt i = bind_default bind_float 0.0 stmt i
let bind_text_default stmt i = bind_default bind_text "" stmt i

let of_bool b = Sqlite3.Data.INT (if b then 1L else 0L)
let of_int i = Sqlite3.Data.INT (Int64.of_int i)
let of_float z = Sqlite3.Data.FLOAT z
let of_text s = Sqlite3.Data.TEXT s
let of_id i = Sqlite3.Data.INT i

let to_bool i data = Sqlite3.Data.to_bool_exn data.(i)
let to_int i data = Sqlite3.Data.to_int_exn data.(i)
let to_float i data = Sqlite3.Data.to_float_exn data.(i)
let to_text i data = Sqlite3.Data.to_string_exn data.(i)
let to_id i data = Sqlite3.Data.to_int64_exn data.(i)
let to_link i data = ref (`Id (to_id i data))

let to_bool_opt i data = Sqlite3.Data.to_bool data.(i)
let to_int_opt i data = Sqlite3.Data.to_int data.(i)
let to_float_opt i data = Sqlite3.Data.to_float data.(i)
let to_text_opt i data = Sqlite3.Data.to_string data.(i)
let to_id_opt i data = Sqlite3.Data.to_int64 data.(i)
let to_link_opt i data = Option.map link_id (to_id_opt i data)

let to_default to_x def i data =
  if data.(i) = Sqlite3.Data.NULL then def else to_x i data

let to_bool_default i data = to_default to_bool false i data
let to_int_default i data = to_default to_int 0 i data
let to_float_default i data = to_default to_float 0.0 i data
let to_text_default i data = to_default to_text "" i data

let to_pair to_x i to_y j data = (to_x i data, to_y j data)

let dir_id_of_link link =
  match !link with
  | `Id id -> id
  | `Val (dir : dir) -> dir.id

let album_id_of_link link =
  match !link with
  | `Id id -> id
  | `Val (album : album) -> album.id


(* Generic statements *)

let create_table sql db =
  let& () = db in
  let* () = Sqlite3.exec db sql in
  ()

let count_table stmt db =
  let& () = db in
  let stmt = prepare db stmt in
  let* () = Sqlite3.step stmt in
  Sqlite3.column_int stmt 0

let exist_in_table' n stmt db path =
  let& () = db in
  let stmt = prepare db stmt in
  for i = 1 to n do
    let* () = bind_text stmt i path in ()
  done;
  let* () = Sqlite3.step stmt in
  Sqlite3.column_int stmt 0 > 0

let exist_in_table = exist_in_table' 1

let find_in_table of_data stmt db path =
  let& () = db in
  let stmt = prepare db stmt in
  let* () = bind_text stmt 1 path in
  let result = ref None in
  let f data = result := Some (of_data data) in
  let* () = Sqlite3.iter stmt ~f in
  !result

let iter_table binds of_data stmt db f =
  let& () = db in
  let stmt = prepare db stmt in
  ignore (Array.mapi (fun i data -> Sqlite3.bind stmt (i + 1) data) binds);
  let f data = f (of_data data) in
  let* () = Sqlite3.iter stmt ~f in
  ()


(* Roots *)

let to_root data : dir =
  let open Sqlite3.Data in
  {
    id = to_int64_exn data.(0);
    path = to_string_exn data.(1);
    name = to_string_exn data.(2);
    children = [||];
    pos = to_int_exn data.(3);
    nest = 0;
    folded = to_bool_exn data.(4);
  }


let create_roots = create_table
{|
  CREATE TABLE IF NOT EXISTS Roots
  (
    path TEXT NOT NULL PRIMARY KEY,
    name TEXT NOT NULL,
    pos INT NOT NULL,
    folded INT NOT NULL
  );
|}

let count_roots = count_table @@ stmt
{|
  SELECT COUNT(*) FROM Roots;
|}

let exist_root = exist_in_table @@ stmt
{|
  SELECT COUNT(*) FROM Roots WHERE path = ?;
|}

let iter_roots = iter_table [||] to_root @@ stmt
{|
  SELECT rowid, * FROM Roots;
|}


let stmt_insert_root = stmt
{|
  INSERT INTO Roots VALUES (?, ?, ?, ?);
|}

let insert_root db (root : dir) =
  let& () = db in
  let stmt = prepare db stmt_insert_root in
  let* () = bind_text stmt 1 root.path in
  let* () = bind_text stmt 2 root.name in
  let* () = bind_int stmt 3 root.pos in
  let* () = bind_bool stmt 4 root.folded in
  let* () = Sqlite3.step stmt in
  root.id <- Sqlite3.last_insert_rowid db


let stmt_update_roots_pos = stmt
{|
  UPDATE Roots SET pos = pos + ? WHERE pos >= ?;
|}

let update_roots_pos db first delta =
  let& () = db in
  let stmt = prepare db stmt_update_roots_pos in
  let* () = bind_int stmt 1 delta in
  let* () = bind_int stmt 2 first in
  let* () = Sqlite3.step stmt in
  ()


(* Dirs *)

let to_dir data : dir =
  {
    id = to_id 0 data;
    path = to_text 1 data;
    name = to_text 2 data;
    children = [||];
    pos = to_int 3 data;
    nest = to_int 4 data;
    folded = to_bool 5 data;
  }


let create_dirs = create_table
{|
  CREATE TABLE IF NOT EXISTS Dirs
  (
    path TEXT NOT NULL PRIMARY KEY,
    name TEXT NOT NULL,
    pos INT NOT NULL,
    nest INT NOT NULL,
    folded INT NOT NULL
  );
|}

let count_dirs = count_table @@ stmt
{|
  SELECT COUNT(*) FROM Dirs;
|}

let exist_dir = exist_in_table @@ stmt
{|
  SELECT COUNT(*) FROM Dirs WHERE path = ?;
|}

let find_dir = find_in_table to_dir @@ stmt
{|
  SELECT rowid, * FROM Dirs WHERE path = ?;
|}

let iter_dir db id = db |> iter_table [|of_id id|] to_dir @@ stmt
{|
  SELECT rowid, * FROM Dirs WHERE parent = ?;
|}


let stmt_insert_dir = stmt
{|
  INSERT INTO Dirs VALUES (?, ?, ?, ?, ?);
|}

let insert_dir db (dir : dir) =
  let& () = db in
  let stmt = prepare db stmt_insert_dir in
  let* () = bind_text stmt 1 dir.path in
  let* () = bind_text stmt 2 dir.name in
  let* () = bind_int stmt 3 dir.pos in
  let* () = bind_int stmt 4 dir.nest in
  let* () = bind_bool stmt 5 dir.folded in
  let* () = Sqlite3.step stmt in
  dir.id <- Sqlite3.last_insert_rowid db


(* Albums *)

let create_albums = create_table
{|
  CREATE TABLE IF NOT EXISTS Albums
  (
    path TEXT NOT NULL PRIMARY KEY,
    artist TEXT,
    title TEXT,
    tracks INT,
    discs INT,
    date TEXT,
    label TEXT,
    country TEXT,
    cover BLOB
  );
|}

let count_albums = count_table @@ stmt
{|
  SELECT COUNT(*) FROM Albums;
|}

let exist_album = exist_in_table @@ stmt
{|
  SELECT COUNT(*) FROM Albums WHERE path = ?;
|}


let stmt_insert_album = stmt
{|
  INSERT INTO Albums VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);
|}

let insert_album db (album : album) =
  let& () = db in
  let stmt = prepare db stmt_insert_album in
  let* () = bind_text stmt 1 album.path in
  let* () = bind_text_opt stmt 2 album.artist in
  let* () = bind_text_opt stmt 3 album.title in
  let* () = bind_int_opt stmt 4 album.tracks in
  let* () = bind_int_opt stmt 5 album.discs in
  let* () = bind_text_opt stmt 6 album.date in
  let* () = bind_text_opt stmt 7 album.label in
  let* () = bind_text_opt stmt 8 album.country in
  let* () = bind_null stmt 9 in
  let* () = Sqlite3.step stmt in
  album.id <- Sqlite3.last_insert_rowid db


(* Tracks *)

let all_null is data =
  Array.for_all (fun i -> data.(i) = Sqlite3.Data.NULL) is

let format_fields = [|6; 7; 8; 9; 10; 11; 12|]
let meta_fields = [|13; 14; 15; 16; 17; 18; 19; 20; 21; 22; 23|]

let of_status = function
  | `Undet -> 0
  | `Predet -> 1
  | `Det -> 2
  | `Invalid -> -1
  | `Absent -> -2

let to_status = function
  | 1 -> `Predet
  | 2 -> `Det
  | -1 -> `Invalid
  | -2 -> `Absent
  | _ -> `Undet

let to_track data : track =
  {
    id = to_id 0 data;
    path = to_text 1 data;
    album = to_link_opt 2 data;
    filesize = to_int_default 3 data;
    filetime = to_float_default 4 data;
    fileage = to_float_default 5 data;
    status = to_status (to_int 25 data);
    format = if all_null format_fields data then None else
      Some Format.
      {
        codec = to_text_default 6 data;
        channels = to_int_default 7 data;
        depth = to_int_default 8 data;
        rate = to_int_default 9 data;
        bitrate = to_float_default 10 data;
        size = to_int_default 11 data;
        time = to_float_default 12 data;
      };
    meta = if all_null meta_fields data then None else
      let track = to_int_default 15 data in
      let disc = to_int_default 16 data in
      let date_txt = to_text_default 19 data in
      Some Meta.
      {
        loaded = true;
        artist = to_text_default 13 data;
        title = to_text_default 14 data;
        track;
        track_txt = if track = 0 then "" else string_of_int track;
        tracks = 0;
        disc;
        disc_txt = if disc = 0 then "" else string_of_int disc;
        discs = 0;
        albumartist = to_text_default 17 data;
        albumtitle = to_text_default 18 data;
        date_txt;
        date = Meta.date_of_string date_txt;
        year = Meta.year_of_string date_txt;
        label = to_text_default 20 data;
        country = to_text_default 21 data;
        length = to_float_default 22 data;
        rating = to_int_default 23 data;
        cover = None;
      };
  }


let create_tracks = create_table
{|
  CREATE TABLE IF NOT EXISTS Tracks
  (
    path TEXT NOT NULL PRIMARY KEY,
    album_id INT,
    filesize INT,
    filetime REAL,
    fileage REAL,
    codec TEXT,
    channels INT,
    depth INT,
    rate INT,
    bitrate REAL,
    size INT,
    time REAL,
    artist TEXT,
    title TEXT,
    track INT,
    disc INT,
    albumartist TEXT,
    albumtitle TEXT,
    date TEXT,
    label TEXT,
    country TEXT,
    length REAL,
    rating INT,
    cover BLOB,
    status INT NOT NULL
  );
|}

let count_tracks = count_table @@ stmt
{|
  SELECT COUNT(*) FROM Tracks;
|}

let exist_track = exist_in_table @@ stmt
{|
  SELECT COUNT(*) FROM Tracks WHERE path = ?;
|}

let find_track = find_in_table to_track @@ stmt
{|
  SELECT rowid, * FROM Tracks WHERE path = ?;
|}

let iter_tracks = iter_table [||] to_track @@ stmt
{|
  SELECT rowid, * FROM Tracks;
|}

let iter_tracks_for db path = db |> iter_table [|of_text (path ^ "%")|] to_track @@ stmt
{|
  SELECT rowid, * FROM Tracks WHERE path LIKE ?;
|}


let stmt_insert_track = stmt
{|
  INSERT INTO Tracks
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
|}

let insert_track db (track : track) =
  assert (track.fileage > 0.0);
  let& () = db in
  let stmt = prepare db stmt_insert_track in
  let* () = bind_text stmt 1 track.path in
  let* () = bind_id_opt stmt 2 (Option.map album_id_of_link track.album) in
  let* () = bind_int_default stmt 3 track.filesize in
  let* () = bind_float_default stmt 4 track.filetime in
  let* () = bind_float_default stmt 5 track.fileage in
  let* () = bind_int stmt 25 (of_status track.status) in
  Option.iter (fun (format : Format.t) ->
    let* () = bind_text_default stmt 6 format.codec in
    let* () = bind_int_default stmt 7 format.channels in
    let* () = bind_int_default stmt 8 format.depth in
    let* () = bind_int_default stmt 9 format.rate in
    let* () = bind_float_default stmt 10 format.bitrate in
    let* () = bind_int_default stmt 11 format.size in
    let* () = bind_float_default stmt 12 format.time in
    ()
  ) track.format;
  Option.iter (fun (meta : Meta.t) ->
    let* () = bind_text_default stmt 13 meta.artist in
    let* () = bind_text_default stmt 14 meta.title in
    let* () = bind_int_default stmt 15 meta.track in
    let* () = bind_int_default stmt 16 meta.disc in
    let* () = bind_text_default stmt 17 meta.albumartist in
    let* () = bind_text_default stmt 18 meta.albumtitle in
    let* () = bind_text_default stmt 19 meta.date_txt in
    let* () = bind_text_default stmt 20 meta.label in
    let* () = bind_text_default stmt 21 meta.country in
    let* () = bind_float_default stmt 22 meta.length in
    let* () = bind_int_default stmt 23 meta.rating in
    let* () = bind_null stmt 24 in
    ()
  ) track.meta;
  let* () = Sqlite3.step stmt in
  track.id <- Sqlite3.last_insert_rowid db


(* Playlists *)

let create_playlists = create_table
{|
  CREATE TABLE IF NOT EXISTS Playlists
  (
    path TEXT NOT NULL PRIMARY KEY
  );
|}

let count_playlists = count_table @@ stmt
{|
  SELECT COUNT(*) FROM Playlists;
|}

let exist_playlist = exist_in_table @@ stmt
{|
  SELECT COUNT(*) FROM Playlists WHERE path = ?;
|}


let stmt_insert_playlist = stmt
{|
  INSERT INTO Playlists VALUES (?);
|}

let insert_playlist db (playlist : playlist) =
  let& () = db in
  let stmt = prepare db stmt_insert_playlist in
  let* () = bind_text stmt 1 playlist.path in
  let* () = Sqlite3.step stmt in
  playlist.id <- Sqlite3.last_insert_rowid db


(* Initialization *)

let filename = "library.db"

let init () =
  let db = Sqlite3.db_open (Storage.path filename) in
  create_roots db;
  create_dirs db;
  create_albums db;
  create_tracks db;
  create_playlists db;
  db

let exit db =
  ignore (Sqlite3.db_close db)
