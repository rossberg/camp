(* Music Database *)

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

let bind_opt bind_x stmt i = function
  | None -> Sqlite3.bind stmt i Sqlite3.Data.NULL
  | Some x -> bind_x stmt i x


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

let exist_in_table stmt db path =
	let& () = db in
  let stmt = prepare db stmt in
  let* () = Sqlite3.bind_text stmt 1 path in
  let* () = Sqlite3.step stmt in
  Sqlite3.column_int stmt 0 > 0


(* Roots *)

let create_roots = create_table
{|
  CREATE TABLE IF NOT EXISTS Roots
  (
  	path TEXT NOT NULL UNIQUE
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


let stmt_insert_root = stmt
{|
  INSERT OR REPLACE INTO Roots
  VALUES (?);
|}

let insert_root db path =
	let& () = db in
  let stmt = prepare db stmt_insert_root in
  let* () = Sqlite3.bind_text stmt 1 path in
  let* () = Sqlite3.step stmt in
  ()


(* Dirs *)

let create_dirs = create_table
{|
  CREATE TABLE IF NOT EXISTS Dirs
  (
  	path TEXT NOT NULL UNIQUE,
  	root_id INT NOT NULL
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


let stmt_insert_dir = stmt
{|
  INSERT OR REPLACE INTO Dirs
  VALUES (?, ?);
|}

let insert_dir db path root_id =
	let& () = db in
  let stmt = prepare db stmt_insert_dir in
  let* () = Sqlite3.bind_text stmt 1 path in
  let* () = Sqlite3.bind_int stmt 2 root_id in
  let* () = Sqlite3.step stmt in
  ()


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
  SELECT COUNT(*) FROM Albums WHERE path = ?;
|}


let stmt_insert_album = stmt
{|
  INSERT OR REPLACE INTO Albums
  VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
|}

let insert_album db path dir_id artist title tracks discs date label country =
	let& () = db in
  let stmt = prepare db stmt_insert_album in
  let* () = Sqlite3.bind_text stmt 1 path in
  let* () = Sqlite3.bind_int stmt 2 dir_id in
  let* () = Sqlite3.bind_text stmt 3 artist in
  let* () = Sqlite3.bind_text stmt 4 title in
  let* () = Sqlite3.bind_int stmt 5 tracks in
  let* () = Sqlite3.bind_int stmt 6 discs in
  let* () = Sqlite3.bind_text stmt 7 date in
  let* () = Sqlite3.bind_text stmt 8 label in
  let* () = Sqlite3.bind_text stmt 9 country in
  let* () = Sqlite3.bind stmt 10 Sqlite3.Data.NULL in
  let* () = Sqlite3.step stmt in
  ()


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
  	album TEXT,
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
  SELECT COUNT(*) FROM Songs WHERE path = ?;
|}


let stmt_insert_song = stmt
{|
  INSERT OR REPLACE INTO Songs
  VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
|}

let insert_song db path dir_id album_id_opt size time
  artist title track disc albumartist album date label country
  length rating channels depth rate bitrate =
	let& () = db in
  let stmt = prepare db stmt_insert_album in
  let* () = Sqlite3.bind_text stmt 1 path in
  let* () = Sqlite3.bind_int stmt 2 dir_id in
  let* () = bind_opt Sqlite3.bind_int stmt 3 album_id_opt in
  let* () = Sqlite3.bind_int stmt 4 size in
  let* () = Sqlite3.bind_double stmt 5 time in
  let* () = Sqlite3.bind_text stmt 6 artist in
  let* () = Sqlite3.bind_text stmt 7 title in
  let* () = Sqlite3.bind_int stmt 8 track in
  let* () = Sqlite3.bind_int stmt 9 disc in
  let* () = Sqlite3.bind_text stmt 10 albumartist in
  let* () = Sqlite3.bind_text stmt 11 album in
  let* () = Sqlite3.bind_text stmt 12 date in
  let* () = Sqlite3.bind_text stmt 13 label in
  let* () = Sqlite3.bind_text stmt 14 country in
  let* () = Sqlite3.bind_double stmt 15 length in
  let* () = Sqlite3.bind_int stmt 16 rating in
  let* () = Sqlite3.bind stmt 17 Sqlite3.Data.NULL in
  let* () = Sqlite3.bind_int stmt 18 channels in
  let* () = Sqlite3.bind_int stmt 19 depth in
  let* () = Sqlite3.bind_int stmt 20 rate in
  let* () = Sqlite3.bind_double stmt 21 bitrate in
  let* () = Sqlite3.step stmt in
  ()


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
  SELECT COUNT(*) FROM Playlists WHERE path = ?;
|}


let stmt_insert_playlist = stmt
{|
  INSERT INTO Playlists
  VALUES (?, ?);
|}

let insert_playlist db path dir_id =
	let& () = db in
  let stmt = prepare db stmt_insert_album in
  let* () = Sqlite3.bind_text stmt 1 path in
  let* () = Sqlite3.bind_int stmt 2 dir_id in
  let* () = Sqlite3.step stmt in
  ()


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
  let* () = Sqlite3.bind_int stmt 1 playlist_id in
  let* () = Sqlite3.bind_int stmt 2 song_id in
  let* () = Sqlite3.step stmt in
  ()


(* Initialization *)

let filename = "music.db"

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
