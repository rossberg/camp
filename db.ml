(* Music Database *)

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

let bind_none stmt i = Sqlite3.bind stmt i Sqlite3.Data.NULL

let bind_opt bind_x stmt i = function
  | None -> bind_none stmt i
  | Some x -> bind_x stmt i x

let bind_bool_opt stmt i = bind_opt bind_bool stmt i
let bind_int_opt stmt i = bind_opt bind_int stmt i
let bind_float_opt stmt i = bind_opt bind_float stmt i
let bind_text_opt stmt i = bind_opt bind_text stmt i
let bind_id_opt stmt i = bind_opt bind_id stmt i

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
let to_link_opt i data = Option.map link (to_id_opt i data)

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
    parent = None;
    pos = to_int_exn data.(3);
    folded = to_bool_exn data.(4);
  }


let create_roots = create_table
{|
  CREATE TABLE IF NOT EXISTS Roots
  (
  	path TEXT NOT NULL UNIQUE,
    name TEXT NOT NULL,
    pos INT NOT NULL UNIQUE,
    folded INT NOT NULL
  );
|}

let count_roots = count_table @@ stmt
{|
  SELECT COUNT(*) FROM Roots;
|}

let exist_root = exist_in_table @@ stmt
{|
  SELECT COUNT(*) FROM Roots
  WHERE path = ?;
|}

let iter_similar_roots db p =
  db |> iter_table [|of_text p; of_text p; of_text p|]
    (to_pair to_text 0 to_text 1) @@ stmt
{|
  SELECT path, name FROM Roots
  WHERE (path LIKE ?) OR (path LIKE ?||'\%') OR (? LIKE path||'\%');
|}

let iter_roots = iter_table [||] to_root @@ stmt
{|
  SELECT rowid, * FROM Roots;
|}

let iter_root_paths = iter_table [||] (to_text 1) @@ stmt
{|
  SELECT path FROM Roots;
|}


let stmt_insert_root = stmt
{|
  INSERT OR REPLACE INTO Roots
  VALUES (?, ?, ?, ?);
|}

let insert_root db (root : dir) =
  assert (root.parent = None);
	let& () = db in
  let stmt = prepare db stmt_insert_root in
  let* () = bind_text stmt 1 root.path in
  let* () = bind_text stmt 2 root.name in
  let* () = bind_int stmt 3 root.pos in
  let* () = bind_bool stmt 4 root.folded in
  let* () = Sqlite3.step stmt in
  root.id <- Sqlite3.last_insert_rowid db


(* Dirs *)

let to_dir data : dir =
  {
    id = to_id 0 data;
    path = to_text 1 data;
    name = to_text 2 data;
    parent = to_link_opt 3 data;
    pos = to_int 4 data;
    folded = to_bool 5 data;
  }


let create_dirs = create_table
{|
  CREATE TABLE IF NOT EXISTS Dirs
  (
  	path TEXT NOT NULL UNIQUE,
    name TEXT NOT NULL,
    parent_id INT,
    pos INT NOT NULL UNIQUE,
    folded INT NOT NULL
  );
|}

let count_dirs = count_table @@ stmt
{|
  SELECT COUNT(*) FROM Dirs;
|}

let exist_dir = exist_in_table @@ stmt
{|
  SELECT COUNT(*) FROM Dirs
  WHERE path = ?;
|}

let iter_dir db id = db |> iter_table [|of_id id|] to_dir @@ stmt
{|
  SELECT rowid, * FROM Dirs
  WHERE parent = ?;
|}


let stmt_insert_dir = stmt
{|
  INSERT OR REPLACE INTO Dirs
  VALUES (?, ?, ?, ?, ?);
|}

let insert_dir db (dir : dir) =
  assert (dir.parent <> None);
	let& () = db in
  let stmt = prepare db stmt_insert_dir in
  let* () = bind_text stmt 1 dir.path in
  let* () = bind_text stmt 2 dir.name in
  let* () = bind_id_opt stmt 3 (Option.map dir_id_of_link dir.parent) in
  let* () = bind_int stmt 4 dir.pos in
  let* () = bind_bool stmt 5 dir.folded in
  let* () = Sqlite3.step stmt in
  dir.id <- Sqlite3.last_insert_rowid db


(* Albums *)

let create_albums = create_table
{|
  CREATE TABLE IF NOT EXISTS Albums
  (
  	path TEXT NOT NULL UNIQUE,
  	dir_id INT NOT NULL,
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
  SELECT COUNT(*) FROM Albums
  WHERE path = ?;
|}


let stmt_insert_album = stmt
{|
  INSERT OR REPLACE INTO Albums
  VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
|}

let insert_album db (album : album) =
	let& () = db in
  let stmt = prepare db stmt_insert_album in
  let* () = bind_text stmt 1 album.path in
  let* () = bind_id stmt 2 (dir_id_of_link album.dir) in
  let* () = bind_text_opt stmt 3 album.artist in
  let* () = bind_text_opt stmt 4 album.title in
  let* () = bind_int_opt stmt 5 album.tracks in
  let* () = bind_int_opt stmt 6 album.discs in
  let* () = bind_text_opt stmt 7 album.date in
  let* () = bind_text_opt stmt 8 album.label in
  let* () = bind_text_opt stmt 9 album.country in
  let* () = bind_none stmt 10 in
  let* () = Sqlite3.step stmt in
  album.id <- Sqlite3.last_insert_rowid db


(* Songs *)

let create_songs = create_table
{|
  CREATE TABLE IF NOT EXISTS Songs
  (
  	path TEXT NOT NULL UNIQUE,
  	dir_id INT NOT NULL,
  	album_id INT,
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
  	channels INT,
  	depth INT,
  	rate INT,
  	bitrate REAL
  );
|}

let count_songs = count_table @@ stmt
{|
  SELECT COUNT(*) FROM Songs;
|}

let exist_song = exist_in_table @@ stmt
{|
  SELECT COUNT(*) FROM Songs
  WHERE path = ?;
|}


let stmt_insert_song = stmt
{|
  INSERT OR REPLACE INTO Songs
  VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
|}

let insert_song db (song : song) =
	let& () = db in
  let stmt = prepare db stmt_insert_album in
  let* () = bind_text stmt 1 song.path in
  let* () = bind_id stmt 2 (dir_id_of_link song.dir) in
  let* () = bind_id_opt stmt 3 (Option.map album_id_of_link song.album) in
  let* () = bind_int_opt stmt 4 song.size in
  let* () = bind_float_opt stmt 5 song.time in
  let* () = bind_text_opt stmt 6 song.artist in
  let* () = bind_text_opt stmt 7 song.title in
  let* () = bind_int_opt stmt 8 song.track in
  let* () = bind_int_opt stmt 9 song.disc in
  let* () = bind_text_opt stmt 10 song.albumartist in
  let* () = bind_text_opt stmt 11 song.albumtitle in
  let* () = bind_text_opt stmt 12 song.date in
  let* () = bind_text_opt stmt 13 song.label in
  let* () = bind_text_opt stmt 14 song.country in
  let* () = bind_float_opt stmt 15 song.length in
  let* () = bind_int_opt stmt 16 song.rating in
  let* () = bind_none stmt 17 in
  let* () = bind_int_opt stmt 18 song.channels in
  let* () = bind_int_opt stmt 19 song.depth in
  let* () = bind_int_opt stmt 20 song.rate in
  let* () = bind_float_opt stmt 21 song.bitrate in
  let* () = Sqlite3.step stmt in
  song.id <- Sqlite3.last_insert_rowid db


(* Playlists *)

let create_playlists = create_table
{|
  CREATE TABLE IF NOT EXISTS Playlists
  (
  	path TEXT NOT NULL UNIQUE,
  	dir_id INT NOT NULL
  );
|}

let count_playlists = count_table @@ stmt
{|
  SELECT COUNT(*) FROM Playlists;
|}

let exist_playlist = exist_in_table @@ stmt
{|
  SELECT COUNT(*) FROM Playlists
  WHERE path = ?;
|}


let stmt_insert_playlist = stmt
{|
  INSERT INTO Playlists
  VALUES (?, ?);
|}

let insert_playlist db (playlist : playlist) =
	let& () = db in
  let stmt = prepare db stmt_insert_playlist in
  let* () = bind_text stmt 1 playlist.path in
  let* () = bind_id stmt 2 (dir_id_of_link playlist.dir) in
  let* () = Sqlite3.step stmt in
  playlist.id <- Sqlite3.last_insert_rowid db


(* Playlist Entries *)

let create_entries = create_table
{|
  CREATE TABLE IF NOT EXISTS Entries
  (
  	playlist_id INT NOT NULL,
  	song_id TEXT NOT NULL
  );
|}


let stmt_insert_entry = stmt
{|
  INSERT INTO Entries
  VALUES (?, ?);
|}

let insert_entry db playlist_id song_id =
	let& () = db in
  let stmt = prepare db stmt_insert_entry in
  let* () = bind_id stmt 1 playlist_id in
  let* () = bind_id stmt 2 song_id in
  let* () = Sqlite3.step stmt in
  ()


(* Initialization *)

let filename = "library.db"

let init () =
  let db = Sqlite3.db_open (Storage.path filename) in
  create_roots db;
  create_dirs db;
  create_albums db;
  create_songs db;
  create_playlists db;
  create_entries db;
  db

let exit db =
	ignore (Sqlite3.db_close db)


(* Scanning *)
