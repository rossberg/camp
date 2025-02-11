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

let return = Sqlite3.Rc.OK


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

let to_bool_opt i data = Sqlite3.Data.to_bool data.(i)
let to_int_opt i data = Sqlite3.Data.to_int data.(i)
let to_float_opt i data = Sqlite3.Data.to_float data.(i)
let to_text_opt i data = Sqlite3.Data.to_string data.(i)
let to_id_opt i data = Sqlite3.Data.to_int64 data.(i)

let to_default to_x def i data =
  if data.(i) = Sqlite3.Data.NULL then def else to_x i data

let to_bool_default i data = to_default to_bool false i data
let to_int_default i data = to_default to_int 0 i data
let to_float_default i data = to_default to_float 0.0 i data
let to_text_default i data = to_default to_text "" i data
let to_id_default i data = to_default to_id (-1L) i data

let to_pair to_x i to_y j data = (to_x i data, to_y j data)


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


let file_cols = 3
let format_cols = 7
let meta_cols = 12

let bind_file stmt i (file : file) =
  assert (file.age > 0.0);
  let* () = bind_int_default stmt (i + 0) file.size in
  let* () = bind_float_default stmt (i + 1) file.time in
  let* () = bind_float_default stmt (i + 2) file.age in
  assert (3 = file_cols);
  return

let bind_format stmt i (format : Format.t) =
  let* () = bind_text_default stmt (i + 0) format.codec in
  let* () = bind_int_default stmt (i + 1) format.channels in
  let* () = bind_int_default stmt (i + 2) format.depth in
  let* () = bind_int_default stmt (i + 3) format.rate in
  let* () = bind_float_default stmt (i + 4) format.bitrate in
  let* () = bind_int_default stmt (i + 5) format.size in
  let* () = bind_float_default stmt (i + 6) format.time in
  assert (7 = format_cols);
  return

let bind_meta stmt i (meta : Meta.t) =
  let* () = bind_text_default stmt (i + 0) meta.artist in
  let* () = bind_text_default stmt (i + 1) meta.title in
  let* () = bind_int_default stmt (i + 2) meta.track in
  let* () = bind_int_default stmt (i + 3) meta.disc in
  let* () = bind_text_default stmt (i + 4) meta.albumartist in
  let* () = bind_text_default stmt (i + 5) meta.albumtitle in
  let* () = bind_text_default stmt (i + 6) meta.date_txt in
  let* () = bind_text_default stmt (i + 7) meta.label in
  let* () = bind_text_default stmt (i + 8) meta.country in
  let* () = bind_float_default stmt (i + 9) meta.length in
  let* () = bind_int_default stmt (i + 10) meta.rating in
  let* () = bind_null stmt (i + 11) in  (* cover *)
  assert (12 = meta_cols);
  return


let rec all_null i n data =
  n = 0 || data.(i) = Sqlite3.Data.NULL && all_null (i + 1) (n - 1) data

let to_file i data : file =
  {
    size = to_int_default (i + 0) data;
    time = to_float_default (i + 1) data;
    age = to_float_default (i + 2) data;
  }

let to_format i data : Format.t option =
  if all_null i format_cols data then None else
  Some Format.
  {
    codec = to_text_default (i + 0) data;
    channels = to_int_default (i + 1) data;
    depth = to_int_default (i + 2) data;
    rate = to_int_default (i + 3) data;
    bitrate = to_float_default (i + 4) data;
    size = to_int_default (i + 5) data;
    time = to_float_default (i + 6) data;
  }

let to_meta i data : Meta.t option =
  if all_null i meta_cols data then None else
  let track = to_int_default (i + 2) data in
  let disc = to_int_default (i + 3) data in
  let date_txt = to_text_default (i + 6) data in
  Some Meta.
  {
    loaded = true;
    artist = to_text_default (i + 0) data;
    title = to_text_default (i + 1) data;
    track;
    track_txt = if track = 0 then "" else Printf.sprintf "%02d" track;
    tracks = 0;
    disc;
    disc_txt = if disc = 0 then "" else string_of_int disc;
    discs = 0;
    albumartist = to_text_default (i + 4) data;
    albumtitle = to_text_default (i + 5) data;
    date_txt;
    date = Meta.date_of_string date_txt;
    year = Meta.year_of_string date_txt;
    label = to_text_default (i + 7) data;
    country = to_text_default (i + 8) data;
    length = to_float_default (i + 9) data;
    rating = to_int_default (i + 10) data;
    cover = None;
  }


(* Generic statements *)

let create_table sql db =
  let& () = db in
  let* () = Sqlite3.exec db sql in
  ()

let clear_table stmt db =
  let& () = db in
  let stmt = prepare db stmt in
  let* () = Sqlite3.step stmt in
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

let insert_into_table bind_x f stmt db x =
  let& () = db in
  let stmt = prepare db stmt in
  let* () = bind_x stmt 1 x in
  let* () = Sqlite3.step stmt in
  f x (Sqlite3.last_insert_rowid db)

let delete_from_table stmt db path =
  let& () = db in
  let stmt = prepare db stmt in
  let* () = bind_text stmt 1 path in
  let* () = Sqlite3.step stmt in
  ()

let delete_from_table_prefix stmt db path =
  let& () = db in
  let stmt = prepare db stmt in
  let* () = bind_text stmt 1 (path ^ "%") in
  let* () = Sqlite3.step stmt in
  ()

let iter_table binds of_data stmt db f =
  let& () = db in
  let stmt = prepare db stmt in
  ignore (Array.mapi (fun i data -> Sqlite3.bind stmt (i + 1) data) binds);
  let f data = f (of_data data) in
  let* () = Sqlite3.iter stmt ~f in
  ()


(* Dirs *)

let create_dirs = create_table
{|
  CREATE TABLE IF NOT EXISTS Dirs
  (
    path TEXT NOT NULL PRIMARY KEY,
    parent TEXT,
    name TEXT NOT NULL,
    pos INT NOT NULL,
    nest INT NOT NULL,
    folded INT NOT NULL,
    shown INT NOT NULL,
    divider_w INT NOT NULL,
    divider_h INT NOT NULL,
    artists_col TEXT NOT NULL,
    albums_col TEXT NOT NULL,
    tracks_col TEXT NOT NULL,
    artists_sort TEXT NOT NULL,
    albums_sort TEXT NOT NULL,
    tracks_sort TEXT NOT NULL
  );
|}

let to_dir i data : dir =
  {
    id = to_id i data;
    path = to_text (i + 1) data;
    parent = to_text_opt (i + 2) data;
    name = to_text (i + 3) data;
    children = [||];
    pos = to_int (i + 4) data;
    nest = to_int (i + 5) data;
    folded = to_bool (i + 6) data;
    artists_shown = to_int (i + 7) data land 1 <> 0;
    albums_shown = to_int (i + 7) data land 2 <> 0;
    tracks_shown = to_int (i + 7) data land 4 <> 0;
    divider_width = to_int (i + 8) data;
    divider_height = to_int (i + 9) data;
    artists_columns = artist_columns_of_string (to_text (i + 10) data);
    albums_columns = album_columns_of_string (to_text (i + 11) data);
    tracks_columns = track_columns_of_string (to_text (i + 12) data);
    artists_sorting = artist_sorting_of_string (to_text (i + 13) data);
    albums_sorting = album_sorting_of_string (to_text (i + 14) data);
    tracks_sorting = track_sorting_of_string (to_text (i + 15) data);
  }

let bind_dir stmt i (dir : dir) =
  assert ((dir.nest = -1) = (dir.parent = None));
  assert ((dir.nest = 0) = (dir.parent = Some ""));
  let* () = bind_text stmt (i + 0) dir.path in
  let* () = bind_text_opt stmt (i + 1) dir.parent in
  let* () = bind_text stmt (i + 2) dir.name in
  let* () = bind_int stmt (i + 3) dir.pos in
  let* () = bind_int stmt (i + 4) dir.nest in
  let* () = bind_bool stmt (i + 5) dir.folded in
  let* () = bind_int stmt (i + 6)
    Bool.(
      to_int dir.artists_shown +
      to_int dir.albums_shown lsl 1 +
      to_int dir.tracks_shown lsl 2
    )
  in
  let* () = bind_int stmt (i + 7) dir.divider_width in
  let* () = bind_int stmt (i + 8) dir.divider_height in
  let* () = bind_text stmt (i + 9) (string_of_artist_columns dir.artists_columns) in
  let* () = bind_text stmt (i + 10) (string_of_album_columns dir.albums_columns) in
  let* () = bind_text stmt (i + 11) (string_of_track_columns dir.tracks_columns) in
  let* () = bind_text stmt (i + 12) (string_of_artist_sorting dir.artists_sorting) in
  let* () = bind_text stmt (i + 13) (string_of_album_sorting dir.albums_sorting) in
  let* () = bind_text stmt (i + 14) (string_of_track_sorting dir.tracks_sorting) in
  return


let count_dirs = count_table @@ stmt
{|
  SELECT COUNT(*) FROM Dirs;
|}

let exists_dir = exist_in_table @@ stmt
{|
  SELECT COUNT(*) FROM Dirs WHERE path = ?;
|}

let find_dir = find_in_table (to_dir 0) @@ stmt
{|
  SELECT rowid, * FROM Dirs WHERE path = ?;
|}

let iter_dirs = iter_table [||] (to_dir 0) @@ stmt
{|
  SELECT rowid, * FROM Dirs ORDER BY path DESC;
|}

let insert_dir = insert_into_table bind_dir (fun d id -> d.id <-id) @@ stmt
{|
  INSERT OR REPLACE INTO Dirs VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
|}

let delete_dirs = delete_from_table_prefix @@ stmt
{|
  DELETE FROM Dirs WHERE path LIKE ?;
|}


let stmt_update_dirs_pos = stmt
{|
  UPDATE Dirs SET pos = pos + (?) WHERE parent = ? AND pos >= ?;
|}

let update_dirs_pos db parent first delta =
  let& () = db in
  let stmt = prepare db stmt_update_dirs_pos in
  let* () = bind_int stmt 1 delta in
  let* () = bind_text_opt stmt 2 parent in
  let* () = bind_int stmt 3 first in
  let* () = Sqlite3.step stmt in
  ()


(* Artists *)

let to_artist i data : artist =
  {
    id = to_id_default i data;
    name = to_text (i + 1) data;
    albums = to_int (i + 2) data;
    tracks = to_int (i + 3) data;
  }


(* Albums *)

let create_albums = create_table
{|
  CREATE TABLE IF NOT EXISTS Albums
  (
    path TEXT NOT NULL PRIMARY KEY,
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
    cover BLOB
  );
|}

let to_album i data : album =
  {
    id = to_id_default i data;
    path = to_text_default (i + 1) data;
    file = to_file (i + 2) data;
    format = to_format (i + 2 + file_cols) data;
    meta = to_meta (i + 2 + file_cols + format_cols) data;
  }

let bind_album stmt i (album : album) =
  let* () = bind_text stmt (i + 0) album.path in
  let* () = bind_file stmt (i + 1) album.file in
  let* () = bind_opt bind_format stmt (i + 1 + file_cols) album.format in
  let* () = bind_opt bind_meta stmt (i + 1 + file_cols + format_cols) album.meta in
  return


let count_albums = count_table @@ stmt
{|
  SELECT COUNT(*) FROM Albums;
|}

let exist_album = exist_in_table @@ stmt
{|
  SELECT COUNT(*) FROM Albums WHERE path = ?;
|}

let find_album = find_in_table (to_album 0) @@ stmt
{|
  SELECT rowid, * FROM Tracks WHERE path = ?;
|}

let insert_album = insert_into_table bind_album (fun a id -> a.id <- id) @@ stmt
{|
  INSERT OR REPLACE INTO Albums
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
|}

let delete_albums = delete_from_table_prefix @@ stmt
{|
  DELETE FROM Albums WHERE path LIKE ?;
|}


(* Tracks *)

let create_tracks = create_table
{|
  CREATE TABLE IF NOT EXISTS Tracks
  (
    path TEXT NOT NULL PRIMARY KEY,
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

let track_cols = 24

let to_track i data : track =
  {
    id = to_id_default i data;
    path = to_text_default (i + 1) data;
    file = to_file (i + 2) data;
    format = to_format (i + 2 + file_cols) data;
    meta = to_meta (i + 2 + file_cols + format_cols) data;
    album = None;
    pos = 0;
    status = to_status (to_int_default (i + 2 + file_cols + format_cols + meta_cols) data);
  }

let bind_track stmt i (track : track) =
  let* () = bind_text stmt (i + 0) track.path in
  let* () = bind_file stmt (i + 1) track.file in
  let* () = bind_opt bind_format stmt (i + 1 + file_cols) track.format in
  let* () = bind_opt bind_meta stmt (i + 1 + file_cols + format_cols) track.meta in
  let* () = bind_int stmt (i + 1 + file_cols + format_cols + meta_cols) (of_status track.status) in
  assert (2 + file_cols + format_cols + meta_cols = track_cols);
  return

let count_tracks = count_table @@ stmt
{|
  SELECT COUNT(*) FROM Tracks;
|}

let exists_track = exist_in_table @@ stmt
{|
  SELECT COUNT(*) FROM Tracks WHERE path = ?;
|}

let find_track = find_in_table (to_track 0) @@ stmt
{|
  SELECT rowid, * FROM Tracks WHERE path = ?;
|}

let iter_tracks = iter_table [||] (to_track 0) @@ stmt
{|
  SELECT rowid, * FROM Tracks;
|}

let iter_tracks_for_path db path artist album = db |>
  iter_table [|
    of_text path; of_text artist; of_text album;
  |] (to_track 0) @@ stmt
{|
  SELECT rowid, *
  FROM Tracks
  WHERE
    (path LIKE ?1) AND
    (?2 = '' OR artist = ?2 OR albumartist = ?2) AND
    (?3 = '' OR albumtitle = ?3);
|}

let iter_tracks_for_path_as_artists db path = db |>
  iter_table [|of_text path|] (to_artist 0) @@ stmt
{|
  SELECT NULL, artist, SUM(albums), SUM(tracks)
  FROM (
    SELECT artist, COUNT(DISTINCT albumtitle) AS albums, COUNT(*) AS tracks
    FROM Tracks
    WHERE path LIKE ?1
    GROUP BY artist
  UNION
    SELECT albumartist AS artist, COUNT(DISTINCT albumtitle) AS albums, COUNT(*) AS tracks
    FROM Tracks
    WHERE path LIKE ?1 AND artist <> albumartist
    GROUP BY albumartist
  )
  GROUP BY artist;
|}

let iter_tracks_for_path_as_albums db path artist = db |>
  iter_table [|of_text path; of_text artist|] (to_album 0) @@ stmt
{|
  SELECT
    NULL,
    path,
    SUM(filesize),
    MAX(filetime),
    MAX(fileage),
    codec,
    MIN(channels),
    MIN(depth),
    MIN(rate),
    AVG(bitrate),
    SUM(size),
    SUM(time),
    albumartist,
    albumtitle,
    COUNT(track),
    COUNT(DISTINCT disc),
    albumartist,
    albumtitle,
    MAX(date),
    label,
    country,
    SUM(length),
    MAX(rating),
    cover,
    MAX(status)
  FROM Tracks
  WHERE path LIKE ?1 AND (?2 = '' OR artist = ?2 OR albumartist = ?2)
  GROUP BY albumartist, albumtitle, codec, label;
|}


let insert_track = insert_into_table bind_track (fun t id -> t.id <- id) @@ stmt
{|
  INSERT OR REPLACE INTO Tracks
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
|}

let delete_tracks = delete_from_table_prefix @@ stmt
{|
  DELETE FROM Tracks WHERE path LIKE ?;
|}


(* Playlists *)

let create_playlists = create_table
{|
  CREATE TABLE IF NOT EXISTS Playlists
  (
    path TEXT NONT NULL,
    pos INT NOT NULL,
    track TEXT NOT NULL,
    name TEXT,
    artist TEXT,
    title TEXT,
    time REAL
  );
|}

let to_playlist_entry i data : path * int * M3u.item =
  to_text (i + 1) data,
  to_int (i + 2) data,
  let path = to_text (i + 3) data in
  let name = to_text_opt (i + 4) data in
  let time = to_float_opt (i + 5) data in
  let info : M3u.info option =
    if name = None && time = None then None else Some
    {
      title = Option.value name ~default: "";
      time = int_of_float (Option.value time ~default: 0.0);
    }
  in
  {path; info}

let to_playlist_track i data : track =
  let track = to_track i data in
  track.pos <- to_int_default (i + track_cols + 1) data;
  if track.path <> "" then
    track
  else
  (
    let path = to_text (i + track_cols + 2) data in
    let artist = to_text_default (i + track_cols + 3) data in
    let title = to_text_default (i + track_cols + 4) data in
    let length = to_float_default (i + track_cols + 5) data in
    track.meta <-
      Some {(Meta.meta track.path None) with artist; title; length};
    {track with path}
  )


let bind_playlist path pos stmt i (item : M3u.item) =
  let* () = bind_text stmt (i + 0) path in
  let* () = bind_int stmt (i + 1) pos in
  let* () = bind_text stmt (i + 2) item.path in
  Option.iter (fun (info : M3u.info) ->
    let* () = bind_text stmt (i + 3) info.title in
    let* () = bind_float stmt (i + 6) (float info.time) in
    match Track.artist_title_of_name info.title with
    | Some (artist, title) ->
      let* () = bind_text stmt (i + 4) artist in
      let* () = bind_text stmt (i + 5) title in
      ()
    | None ->
      match Track.artist_title_of_path path with
      | Some (artist, title) ->
        let* () = bind_text stmt (i + 4) artist in
        let* () = bind_text stmt (i + 5) title in
        ()
      | None ->
        let* () = bind_text stmt (i + 4) "[unknown]" in
        let* () = bind_text stmt (i + 5) info.title in
        ()
  ) item.info;
  return


let count_playlists = count_table @@ stmt
{|
  SELECT COUNT(DISTINCT path) FROM Playlists;
|}

let exists_playlists = exist_in_table @@ stmt
{|
  SELECT COUNT(*) FROM Playlists WHERE path = ?;
|}

let insert_playlists db path pos = db |> insert_into_table (bind_playlist path pos) (fun _ _ -> ()) @@ stmt
{|
  INSERT OR REPLACE INTO Playlists VALUES (?, ?, ?, ?, ?, ?, ?);
|}

let insert_playlists_bulk db path items =
  if items <> [] then
  let tuples = String.concat ", "
    (List.map (fun _ -> "(?, ?, ?, ?, ?, ?, ?)") items) in
  let stmt = stmt @@ "INSERT OR REPLACE INTO Playlists VALUES " ^ tuples ^ ";" in
  let& () = db in
  let stmt = prepare db stmt in
  List.iteri (fun i item ->
    let* () = bind_playlist path (i + 1) stmt (1 + i * 7) item in ()
  ) items;
  let* () = Sqlite3.step stmt in
  ()

let delete_playlists = delete_from_table_prefix @@ stmt
{|
  DELETE FROM Playlists WHERE path LIKE ?;
|}

let clear_playlists = clear_table @@ stmt
{|
  DELETE FROM Playlists;
|}


let iter_playlist_tracks_for_path_as_artists db path = db |>
  iter_table [|of_text path|] (to_artist 0) @@ stmt
{|
  SELECT NULL, aartist, SUM(albums), SUM(tracks)
  FROM (
    SELECT
      CASE
        WHEN albumartist NOT NULL THEN albumartist
        WHEN Tracks.artist NOT NULL THEN Tracks.artist
        WHEN Playlists.artist NOT NULL THEN Playlists.artist
        ELSE "[unknown]"
      END AS aartist,
      COUNT(DISTINCT
        CASE
          WHEN albumtitle NOT NULL THEN albumtitle
          ELSE "[unknown]"
        END
      ) AS albums,
      COUNT(*) AS tracks
    FROM Playlists LEFT JOIN Tracks on Tracks.path = Playlists.track
    WHERE Playlists.path = ?1
    GROUP BY aartist
  UNION
    SELECT
      CASE
        WHEN albumartist NOT NULL THEN albumartist
        ELSE "[unknown]"
      END AS aartist,
      COUNT(DISTINCT
        CASE
          WHEN albumtitle NOT NULL THEN albumtitle
          ELSE "[unknown]"
        END
      ) AS albums,
      COUNT(*) AS tracks
    FROM Playlists LEFT JOIN Tracks on Tracks.path = Playlists.track
    WHERE Playlists.path = ?1 AND Tracks.artist <> albumartist
    GROUP BY albumartist
  )
  GROUP BY aartist;
|}

let iter_playlist_tracks_for_path_as_albums db path artist = db |>
  iter_table [|of_text path; of_text artist|] (to_album 0) @@ stmt
{|
  SELECT
    NULL,
    Playlists.track,
    SUM(filesize),
    MAX(filetime),
    MAX(fileage),
    codec,
    MIN(channels),
    MIN(depth),
    MIN(rate),
    AVG(bitrate),
    SUM(size),
    SUM(Tracks.time),
    Playlists.artist,
    Playlists.title,
    COUNT(Playlists.track),
    COUNT(DISTINCT disc),
    CASE
      WHEN albumartist NOT NULL THEN albumartist
      WHEN Tracks.artist NOT NULL THEN Tracks.artist
      WHEN Playlists.artist NOT NULL THEN Playlists.artist
      ELSE "[unknown]"
    END AS aartist,
    CASE
      WHEN albumtitle NOT NULL THEN albumtitle
      ELSE "[unknown]"
    END AS atitle,
    MAX(date),
    label,
    country,
    SUM(Tracks.length),
    MAX(rating),
    cover,
    NULL,
    MAX(status)
  FROM Playlists LEFT JOIN Tracks on Tracks.path = Playlists.track
  WHERE
    (Playlists.path = ?1) AND
    (?2 = '' OR Tracks.artist = ?2 OR albumartist = ?2 OR Playlists.artist = ?2)
  GROUP BY aartist, atitle, codec, label;
|}

let iter_playlist_tracks_for_path db path artist album = db |>
  iter_table [|
    of_text path; of_text artist; of_text album;
  |] (to_playlist_track 0) @@ stmt
{|
  SELECT
    Tracks.rowid, Tracks.*,
    Playlists.pos, Playlists.track, Playlists.artist, Playlists.title, Playlists.time
  FROM Playlists LEFT JOIN Tracks on Tracks.path = Playlists.track
  WHERE
    (Playlists.path = ?1) AND
    (?2 = '' OR Tracks.artist = ?2 OR albumartist = ?2 OR Playlists.artist = ?2) AND
    (?3 = '' OR albumtitle = ?3);
|}


let iter_tracks_and_playlist_tracks_for_path db path artist album with_pos = db |>
  iter_table [|
    of_text path; of_text artist; of_text album; of_bool with_pos;
  |] (to_playlist_track 0) @@ stmt
{|
  SELECT Tracks.rowid, Tracks.*, NULL, NULL, NULL, NULL, NULL
  FROM Tracks
  WHERE
    (Tracks.path LIKE ?1) AND
    (?2 = '' OR Tracks.artist = ?2 OR albumartist = ?2) AND
    (?3 = '' OR albumtitle = ?3)
  UNION
  SELECT
    Tracks.rowid, Tracks.*,
    CASE WHEN ?4 THEN Playlists.pos ELSE NULL END,
    Playlists.track, Playlists.artist, Playlists.title, Playlists.time
  FROM Playlists LEFT JOIN Tracks on Tracks.path = Playlists.track
  WHERE
    (Playlists.path LIKE ?1) AND
    (?2 = '' OR Tracks.artist = ?2 OR albumartist = ?2 OR Playlists.artist = ?2) AND
    (?3 = '' OR albumtitle = ?3) AND
    (?4 OR NOT (Playlists.track LIKE 'separator://%'));
|}


(* Initialization *)

let filename = "library.db"

let init () =
  let db = Sqlite3.db_open (Storage.path filename) in
  create_dirs db;
  create_albums db;
  create_tracks db;
  create_playlists db;
  db

let exit db =
  ignore (Sqlite3.db_close db)
