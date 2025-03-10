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

let bind_null stmt i = Sqlite3.bind stmt i Sqlite3.Data.NULL

let bind_opt bind_x stmt i = function
  | None -> bind_null stmt i
  | Some x -> bind_x stmt i x

let bind_bool_opt stmt i = bind_opt bind_bool stmt i
let bind_int_opt stmt i = bind_opt bind_int stmt i
let bind_float_opt stmt i = bind_opt bind_float stmt i
let bind_text_opt stmt i = bind_opt bind_text stmt i

let bind_default bind_x def stmt i v =
  if v = def then bind_null stmt i else bind_x stmt i v

let bind_int_default stmt i = bind_default bind_int 0 stmt i
let bind_float_default stmt i = bind_default bind_float 0.0 stmt i
let bind_text_default stmt i = bind_default bind_text "" stmt i

let of_bool b = Sqlite3.Data.INT (if b then 1L else 0L)
let of_int i = Sqlite3.Data.INT (Int64.of_int i)
let of_float z = Sqlite3.Data.FLOAT z
let of_text s = Sqlite3.Data.TEXT s

let of_opt of_x = function
  | None -> Sqlite3.Data.NULL
  | Some x -> of_x x

let to_bool i data = Sqlite3.Data.to_bool_exn data.(i)
let to_int i data = Sqlite3.Data.to_int_exn data.(i)
let to_float i data = Sqlite3.Data.to_float_exn data.(i)
let to_text i data = Sqlite3.Data.to_string_exn data.(i)

let to_bool_opt i data = Sqlite3.Data.to_bool data.(i)
let to_int_opt i data = Sqlite3.Data.to_int data.(i)
let to_float_opt i data = Sqlite3.Data.to_float data.(i)
let to_text_opt i data = Sqlite3.Data.to_string data.(i)

let to_default to_x def i data =
  if data.(i) = Sqlite3.Data.NULL then def else to_x i data

let to_bool_default i data = to_default to_bool false i data
let to_int_default i data = to_default to_int 0 i data
let to_float_default i data = to_default to_float 0.0 i data
let to_text_default i data = to_default to_text "" i data

let to_pair to_x i to_y j data = (to_x i data, to_y j data)


let set_bit i b = if b then 1 lsl i else 0
let get_bit i n = n land (1 lsl i) <> 0

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

let of_display i j = function
  | None -> 0
  | Some `Table -> set_bit i true
  | Some `Grid -> set_bit i true + set_bit j true

let to_display i j n =
  match get_bit i n, get_bit j n with
  | false, _ -> None
  | true, false -> Some `Table
  | true, true -> Some `Grid

let paren s = "(" ^ s ^ ")"
let list ss = String.concat ", " ss
let tuple k n = paren (list (List.init n (fun i -> "?" ^ string_of_int (k + i))))
let tuples k n m = list (List.init m (fun i -> (tuple (k + i*n) n)))


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

let count_table_prefix stmt db path =
  let& () = db in
  let stmt = prepare db stmt in
  let* () = bind_text stmt 1 (path ^ "%") in
  let* () = Sqlite3.step stmt in
  Sqlite3.column_int stmt 0

let mem_table stmt db path =
  let& () = db in
  let stmt = prepare db stmt in
  let* () = bind_text stmt 1 path in
  let* () = Sqlite3.step stmt in
  Sqlite3.column_int stmt 0 > 0

let find_in_table of_data stmt db path =
  let& () = db in
  let stmt = prepare db stmt in
  let* () = bind_text stmt 1 path in
  let result = ref None in
  let f data = result := Some (of_data data) in
  let* () = Sqlite3.iter stmt ~f in
  !result

let insert_into_table bind_x stmt db x =
  let& () = db in
  let stmt = prepare db stmt in
  let* () = bind_x stmt 1 x in
  let* () = Sqlite3.step stmt in
  ()

let rec insert_into_table_bulk bind_x cols stmtf db xs =
  if xs <> [] then
  (
    let& () = db in
    match prepare db (stmtf xs) with
    | stmt ->
      List.iteri (fun i x ->
        let* () = bind_x i stmt (1 + i * cols) x in ()
      ) xs;
      let* () = Sqlite3.step stmt in
      ()
    | exception Sqlite3.Error _ ->
      (* Assume the error is due to SQLITE_LIMIT_VARIABLE_NUMBER *)
      let n = 32766 / (cols + 1) in  (* constant not defined in binding *)
      insert_into_table_bulk bind_x cols stmtf db (List.take n xs);
      insert_into_table_bulk bind_x cols stmtf db (List.drop n xs)
  )

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

let delete_from_table_prefix_except stmt db path =
  let& () = db in
  let stmt = prepare db stmt in
  let* () = bind_text stmt 1 (path ^ "%") in
  let* () = bind_text stmt 2 (path ^ File.("%" // "%")) in
  let* () = Sqlite3.step stmt in
  ()

let delete_from_table_bulk stmtf db paths =
  if paths <> [] then
  (
    let& () = db in
    let stmt = prepare db (stmtf paths) in
    List.iteri (fun i path ->
      let* () = bind_text stmt (1 + i) path in ()
    ) paths;
    let* () = Sqlite3.step stmt in
    ()
  )

let update_in_table binds stmt db =
  let& () = db in
  let stmt = prepare db stmt in
  ignore (Array.mapi (fun i data -> Sqlite3.bind stmt (i + 1) data) binds);
  let* () = Sqlite3.step stmt in
  ()

let iter_table binds of_data stmt db f =
  let& () = db in
  let stmt = prepare db stmt in
  ignore (Array.mapi (fun i data -> Sqlite3.bind stmt (i + 1) data) binds);
  let f data = f (of_data data) in
  let* () = Sqlite3.iter stmt ~f in
  ()

let iter_table_prefix of_data stmt db path f =
  iter_table [|of_text (path ^ "%")|] of_data stmt db f

let iter_table_prefix_except of_data stmt db path f =
  iter_table [|of_text (path ^ "%"); of_text (path ^ File.("%" // "%"))|]
    of_data stmt db f


(* Dirs *)

let create_dirs = create_table
  "
    CREATE TABLE IF NOT EXISTS Dirs
    (
      path TEXT NOT NULL PRIMARY KEY,
      parent TEXT,
      name TEXT NOT NULL,
      pos INT NOT NULL,
      nest INT NOT NULL,
      folded INT NOT NULL,
      search TEXT NOT NULL,
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
  "

let dir_cols = 16

let to_dir i data : dir =
  {
    path = to_text (i + 0) data;
    parent = to_text_opt (i + 1) data;
    name = to_text (i + 2) data;
    children = [||];
    pos = to_int (i + 3) data;
    nest = to_int (i + 4) data;
    search = search_of_string (to_text (i + 6) data);
    folded = to_bool (i + 5) data;
    artists_shown = get_bit 0 (to_int (i + 7) data);
    albums_shown = to_display 1 3 (to_int (i + 7) data);
    tracks_shown = to_display 2 4 (to_int (i + 7) data);
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
  let* () = bind_text stmt (i + 6) (string_of_search dir.search) in
  let* () = bind_int stmt (i + 7)
    (
      set_bit 0 dir.artists_shown +
      of_display 1 3 dir.albums_shown +
      of_display 2 4 dir.tracks_shown
    )
  in
  let* () = bind_int stmt (i + 8) dir.divider_width in
  let* () = bind_int stmt (i + 9) dir.divider_height in
  let* () = bind_text stmt (i + 10) (string_of_artist_columns dir.artists_columns) in
  let* () = bind_text stmt (i + 11) (string_of_album_columns dir.albums_columns) in
  let* () = bind_text stmt (i + 12) (string_of_track_columns dir.tracks_columns) in
  let* () = bind_text stmt (i + 13) (string_of_artist_sorting dir.artists_sorting) in
  let* () = bind_text stmt (i + 14) (string_of_album_sorting dir.albums_sorting) in
  let* () = bind_text stmt (i + 15) (string_of_track_sorting dir.tracks_sorting) in
  return


let count_dirs = stmt
  "
    SELECT COUNT(*) FROM Dirs;
  " |> count_table

let mem_dir = stmt
  "
    SELECT COUNT(*) FROM Dirs WHERE path = ?;
  " |> mem_table

let find_dir = stmt
  "
    SELECT * FROM Dirs WHERE path = ?;
  " |> find_in_table (to_dir 0)

let iter_dirs = stmt
  "
    SELECT * FROM Dirs ORDER BY path DESC;
  " |> iter_table [||] (to_dir 0)

let iter_dirs_for_path = stmt
  "
    SELECT * FROM Dirs WHERE path LIKE ? AND NOT (path LIKE ?);
  " |> iter_table_prefix_except (to_dir 0)

let iter_dirs_for_path_rec = stmt
  "
    SELECT * FROM Dirs WHERE path LIKE ?;
  " |> iter_table_prefix (to_dir 0)

let insert_dir = stmt @@
  "
    INSERT OR REPLACE INTO Dirs VALUES " ^ tuple 1 dir_cols ^ ";
  " |> insert_into_table bind_dir

let insert_dirs_bulk = (fun dirs -> stmt @@
  "
    INSERT OR REPLACE INTO Dirs
    VALUES " ^ tuples 1 dir_cols (List.length dirs) ^ ";
  ") |> insert_into_table_bulk (Fun.const bind_dir) dir_cols

let update_dir = insert_dir

let delete_dir = stmt
  "
    DELETE FROM Dirs WHERE path = ?;
  " |> delete_from_table

let delete_dirs_for_path = stmt
  "
    DELETE FROM Dirs WHERE path LIKE ? AND NOT (path LIKE ?);
  " |> delete_from_table_prefix_except

let delete_dirs_for_path_rec = stmt
  "
    DELETE FROM Dirs WHERE path LIKE ?;
  " |> delete_from_table_prefix

let delete_dirs_bulk = (fun paths -> stmt @@
  "
    DELETE FROM Dirs
    WHERE " ^ String.concat " OR " (List.map (fun _ -> "path = ?") paths) ^ ";
  ") |> delete_from_table_bulk

let update_dirs_pos = stmt
  "
    UPDATE Dirs SET pos = pos + ?3 WHERE parent = ?1 AND pos >= ?2;
  " |>
  fun stmt db path first delta ->
    update_in_table [|of_text path; of_int first; of_int delta|] stmt db


(* Artists *)

let artist_cols = 3

let to_artist i data : artist =
  {
    name = to_default to_text "[unknown]" (i + 0) data;
    albums = to_int (i + 1) data;
    tracks = to_int (i + 2) data;
  }


(* Albums *)

let album_cols = 23

let to_album i data : album =
  {
    path = to_text_default (i + 0) data;
    file = to_file (i + 1) data;
    format = to_format (i + 1 + file_cols) data;
    meta = to_meta (i + 1 + file_cols + format_cols) data;
  }

let bind_album stmt i (album : album) =
  let* () = bind_text stmt (i + 0) album.path in
  let* () = bind_file stmt (i + 1) album.file in
  let* () = bind_opt bind_format stmt (i + 1 + file_cols) album.format in
  let* () = bind_opt bind_meta stmt (i + 1 + file_cols + format_cols) album.meta in
  return


(* Tracks *)

let create_tracks = create_table
  "
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
    CREATE INDEX IF NOT EXISTS TracksArtist ON Tracks(artist);
    CREATE INDEX IF NOT EXISTS TracksTitle ON Tracks(title);
    CREATE INDEX IF NOT EXISTS TracksAlbumArtist ON Tracks(albumartist);
    CREATE INDEX IF NOT EXISTS TracksAlbumTitle ON Tracks(albumtitle);
  "

let track_cols = 24

let to_track i data : track =
  let path = to_text_default (i + 0) data in
  {
    path = path;
    file = to_file (i + 1) data;
    format = to_format (i + 1 + file_cols) data;
    meta = to_meta (i + 1 + file_cols + format_cols) data;
    album = None;
    pos = (match Track.(pos_artist_title (fields_of_path path)) with Some (i, _, _) -> i | None -> -1);
    status = to_status (to_int_default (i + 1 + file_cols + format_cols + meta_cols) data);
  }

let bind_track stmt i (track : track) =
  let* () = bind_text stmt (i + 0) track.path in
  let* () = bind_file stmt (i + 1) track.file in
  let* () = bind_opt bind_format stmt (i + 1 + file_cols) track.format in
  let* () = bind_opt bind_meta stmt (i + 1 + file_cols + format_cols) track.meta in
  let* () = bind_int stmt (i + 1 + file_cols + format_cols + meta_cols) (of_status track.status) in
  assert (2 + file_cols + format_cols + meta_cols = track_cols);
  return


let count_tracks = stmt
  "
    SELECT COUNT(*) FROM Tracks;
  " |> count_table

let count_tracks_for_path_rec = stmt
  "
    SELECT COUNT(*) FROM Tracks WHERE path LIKE ?;
  " |> count_table_prefix

let mem_track = stmt
  "
    SELECT COUNT(*) FROM Tracks WHERE path = ?;
  " |> mem_table

let find_track = stmt
  "
    SELECT * FROM Tracks WHERE path = ?;
  " |> find_in_table (to_track 0)

let iter_tracks = stmt
  "
    SELECT * FROM Tracks;
  " |> iter_table [||] (to_track 0)

let iter_tracks_for_path = stmt
  "
    SELECT * FROM Tracks WHERE path LIKE ? AND NOT (path LIKE ?);
  " |> iter_table_prefix_except (to_track 0)

let iter_tracks_for_path_rec = stmt
  "
    SELECT * FROM Tracks WHERE path LIKE ?;
  " |> iter_table_prefix (to_track 0)

let insert_track = stmt @@
  "
    INSERT OR REPLACE INTO Tracks VALUES " ^ tuple 1 track_cols ^ ";
  " |> insert_into_table bind_track

let insert_tracks_bulk = (fun tracks -> stmt @@
  "
    INSERT OR REPLACE INTO Tracks
    VALUES " ^ tuples 1 track_cols (List.length tracks) ^ ";
  ") |> insert_into_table_bulk (Fun.const bind_track) track_cols

let delete_track = stmt
  "
    DELETE FROM Tracks WHERE path = ?;
  " |> delete_from_table

let delete_tracks_for_path = stmt
  "
    DELETE FROM Tracks WHERE path LIKE ? AND NOT (path LIKE ?);
  " |> delete_from_table_prefix_except

let delete_tracks_for_path_rec = stmt
  "
    DELETE FROM Tracks WHERE path LIKE ?;
  " |> delete_from_table_prefix

let delete_tracks_bulk = (fun paths -> stmt @@
  "
    DELETE FROM Tracks
    WHERE " ^ String.concat " OR " (List.map (fun _ -> "path = ?") paths) ^ ";
  ") |> delete_from_table_bulk


(* Complex Queries *)

let artists_table k artists =
  if artists = [||] then "" else
    "WITH Artists (name) AS (VALUES " ^ tuples k 1 (Array.length artists) ^ ")"

let artists_filter _k artists =
  if artists = [||] then "" else
    "JOIN Artists
      ON Artists.name = artist
      OR Artists.name = albumartist"

let albums_filter k albums =
  if albums = [||] then "" else "
    AND albumtitle IN " ^ tuple k (Array.length albums)

let search_filter k searches =
  String.concat "" (
    Array.mapi (fun i _ ->
      let n = string_of_int (k + i) in "
      AND (" ^
        "title LIKE ?" ^ n ^ " OR " ^
        "artist LIKE ?" ^ n ^ " OR " ^
        "albumartist LIKE ?" ^ n ^ " OR " ^
        "albumtitle LIKE ?" ^ n ^ " OR " ^
        "label LIKE ?" ^ n ^ " OR " ^
        "country LIKE ?" ^ n ^
      ")"
    ) searches |> Array.to_list
  )

let tracks_fields =
      "Tracks.*"

let artists_fields artistfield =
      artistfield ^ " AS name,
      COUNT(DISTINCT albumtitle) AS albums,
      COUNT(*) AS tracks"

let albums_fields' pathfield artistfield titlefield trackfield albumartistfields =
      pathfield ^ ",
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
      " ^ artistfield ^ ",
      " ^ titlefield ^ ",
      COUNT(" ^ trackfield ^ "),
      COUNT(DISTINCT disc),
      CASE
        " ^
        String.concat "" (List.map (fun field -> "
        WHEN " ^ field ^ " NOT NULL THEN " ^ field
        ) albumartistfields) ^ "
        ELSE '[unknown]'
      END AS aartist,
      CASE
        WHEN albumtitle NOT NULL THEN albumtitle
        ELSE '[unknown]'
      END AS atitle,
      MAX(date),
      label,
      country,
      SUM(Tracks.length),
      MAX(rating)"

let albums_fields =
  albums_fields' "path" "albumartist" "albumtitle" "track" ["albumartist"]


let iter_tracks_for_path_single = stmt @@
  "
    SELECT " ^ tracks_fields ^ "
    FROM Tracks
    WHERE path LIKE ?1
      AND (?2 = '%' OR artist = ?2 OR albumartist = ?2)
      AND (?3 = '%' OR albumtitle = ?3);
  " |>
  fun stmt db path artist album ->
    iter_table [|of_text path; of_text artist; of_text album|] (to_track 0)
      stmt db

let iter_tracks_for_path_multi db path artist albums = stmt @@
  "
    SELECT " ^ tracks_fields ^ "
    FROM Tracks
    WHERE path LIKE ?1
      AND (?2 = '%' OR artist = ?2 OR albumartist = ?2)
      AND (albumtitle IN " ^ tuple 3 (Array.length albums) ^ ");
  " |>
  fun stmt ->
    let album_binds = Array.map of_text albums in
    iter_table (Array.append [|of_text path; of_text artist|] album_binds)
      (to_track 0) stmt db

let iter_tracks_for_path_search db path artists albums searches = stmt @@
  artists_table 2 artists ^ "
    SELECT " ^ tracks_fields ^ "
    FROM Tracks
    " ^ artists_filter 2 artists ^ "
    WHERE path LIKE ?1
    " ^ albums_filter (2 + Array.length artists) albums ^ "
    " ^ search_filter (2 + Array.(length artists + length albums)) searches ^ ";
  " |>
  fun stmt ->
    let artist_binds = Array.map of_text artists in
    let album_binds = Array.map of_text albums in
    let search_binds = Array.map (fun s -> of_text ("%" ^ s ^ "%")) searches in
    iter_table (Array.concat [[|of_text path|]; artist_binds; album_binds; search_binds])
      (to_track 0) stmt db

let iter_tracks_for_path_filter db path artists albums searches f =
  if Array.length artists > 1 || searches <> [||] then
    iter_tracks_for_path_search db path artists albums searches f
  else if Array.length albums > 1 then
    let artist = if artists = [||] then "%" else artists.(0) in
    iter_tracks_for_path_multi db path artist albums f
  else
    let artist = if artists = [||] then "%" else artists.(0) in
    let album = if albums = [||] then "%" else albums.(0) in
    iter_tracks_for_path_single db path artist album f


let iter_tracks_for_path_as_artists_single = stmt @@
  "
    SELECT name, SUM(albums), SUM(tracks)
    FROM (
      SELECT " ^ artists_fields "artist" ^ "
      FROM Tracks
      WHERE path LIKE ?1
      GROUP BY artist
    UNION
      SELECT " ^ artists_fields "albumartist" ^ "
      FROM Tracks
      WHERE path LIKE ?1 AND artist <> albumartist
      GROUP BY albumartist
    )
    GROUP BY name;
  " |>
  fun stmt db path ->
    iter_table [|of_text path|] (to_artist 0) stmt db

let iter_tracks_for_path_as_artists_search db path searches = stmt @@
  "
    SELECT name, SUM(albums), SUM(tracks)
    FROM (
      SELECT " ^ artists_fields "artist" ^ "
      FROM Tracks
      WHERE path LIKE ?1
      " ^ search_filter 2 searches ^ "
      GROUP BY artist
    UNION
      SELECT " ^ artists_fields "albumartist" ^ "
      FROM Tracks
      WHERE path LIKE ?1 AND artist <> albumartist
      " ^ search_filter 2 searches ^ "
      GROUP BY albumartist
    )
    GROUP BY name;
  " |>
  fun stmt ->
    let search_binds = Array.map (fun s -> of_text ("%" ^ s ^ "%")) searches in
    iter_table (Array.append [|of_text path|] search_binds) (to_artist 0) stmt db

let iter_tracks_for_path_as_artists db path searches f =
  if searches <> [||] then
    iter_tracks_for_path_as_artists_search db path searches f
  else
    iter_tracks_for_path_as_artists_single db path f


let iter_tracks_for_path_as_albums_single = stmt @@
  "
    SELECT " ^ albums_fields ^ "
    FROM Tracks
    WHERE path LIKE ?1
      AND (?2 = '%' OR artist = ?2 OR albumartist = ?2)
    GROUP BY albumartist, albumtitle, codec, label;
  " |>
  fun stmt db path artist ->
    iter_table [|of_text path; of_text artist|] (to_album 0) stmt db

let iter_tracks_for_path_as_albums_search db path artists searches = stmt @@
  artists_table 2 artists ^ "
    SELECT " ^ albums_fields ^ "
    FROM Tracks 
    " ^ artists_filter 2 artists ^ "
    WHERE path LIKE ?1
    " ^ search_filter (2 + Array.length artists) searches ^ "
    GROUP BY albumartist, albumtitle, path;
  " |>
  fun stmt ->
    let artist_binds = Array.map of_text artists in
    let search_binds = Array.map (fun s -> of_text ("%" ^ s ^ "%")) searches in
    iter_table (Array.concat [[|of_text path|]; artist_binds; search_binds])
      (to_album 0) stmt db

let iter_tracks_for_path_as_albums db path artists searches f =
  if Array.length artists > 1 || searches <> [||] then
    iter_tracks_for_path_as_albums_search db path artists searches f
  else
    let artist = if artists = [||] then "%" else artists.(0) in
    iter_tracks_for_path_as_albums_single db path artist f


(* Playlists *)

let create_playlists = create_table
  "
    CREATE TABLE IF NOT EXISTS Playlists
    (
      path TEXT NOT NULL,
      pos INT NOT NULL,
      track TEXT NOT NULL,
      name TEXT,
      artist TEXT,
      title TEXT,
      time REAL
    );
  "

let playlist_cols = 7


let to_playlist_track i data : track =
  let track = to_track i data in
  track.pos <- to_int_default (i + track_cols + 0) data - 1;
  if track.path <> "" then  (* came from tracks table *)
    track
  else
  (
    let path = to_text (i + track_cols + 1) data in
    if M3u.is_separator path then
      {(Data.make_separator ()) with pos = track.pos}
    else
    (
      let artist = to_text_default (i + track_cols + 2) data in
      let title = to_text_default (i + track_cols + 3) data in
      let length = to_float_default (i + track_cols + 4) data in
      let meta =
        if artist = "" && title = "" then None else
        Some {(Meta.meta track.path None) with artist; title; length}
      in
      {track with path; meta; status = if meta = None then `Undet else `Predet}
    )
  )

let bind_playlist path pos stmt i (item : M3u.item) =
  let* () = bind_text stmt (i + 0) path in
  let* () = bind_int stmt (i + 1) pos in
  let* () = bind_text stmt (i + 2) item.path in
  Option.iter (fun (info : M3u.info) ->
    let* () = bind_text stmt (i + 3) info.title in
    let* () = bind_float stmt (i + 6) (float info.time) in
    match Track.(artist_title (fields_of_name info.title)) with
    | Some (artist, title) ->
      let* () = bind_text stmt (i + 4) artist in
      let* () = bind_text stmt (i + 5) title in
      ()
    | None ->
      match Track.(pos_artist_title (fields_of_path path)) with
      | Some (_, artist, title) ->
        let* () = bind_text stmt (i + 4) artist in
        let* () = bind_text stmt (i + 5) title in
        ()
      | None ->
        let* () = bind_text stmt (i + 4) "[unknown]" in
        let* () = bind_text stmt (i + 5) info.title in
        ()
  ) item.info;
  return


let count_playlists = stmt
  "
    SELECT COUNT(DISTINCT path) FROM Playlists;
  " |> count_table

let mem_playlist = stmt
  "
    SELECT COUNT(*) FROM Playlists WHERE path = ?;
  " |> mem_table

let insert_playlists = stmt @@
  "
    INSERT OR REPLACE INTO Playlists VALUES " ^ tuple 1 playlist_cols ^ ";
  " |>
  fun stmt db path pos -> insert_into_table (bind_playlist path pos) stmt db

let insert_playlists_bulk db path items = ((fun items -> stmt @@
  "
    INSERT OR REPLACE INTO Playlists
    VALUES " ^ tuples 1 playlist_cols (List.length items) ^ ";
  ") |>
    insert_into_table_bulk (fun i -> bind_playlist path (i + 1)) playlist_cols)
      db items

let delete_playlists_for_path = stmt
  "
    DELETE FROM Playlists WHERE path LIKE ? AND (NOT path LIKE ?);
  " |> delete_from_table_prefix_except

let delete_playlists_for_path_rec = stmt
  "
    DELETE FROM Playlists WHERE path LIKE ?;
  " |> delete_from_table_prefix

let clear_playlists = stmt
  "
    DELETE FROM Playlists;
  " |> clear_table


let playlist_artists_filter _k artists =
  if artists = [||] then "" else
    "JOIN Artists
      ON Artists.name = Tracks.artist
      OR Artists.name = Tracks.albumartist
      OR Artists.name = Playlists.artist"

let playlist_albums_filter k albums =
  if albums = [||] then "" else "
    AND albumtitle IN " ^ tuple k (Array.length albums)

let playlist_search_filter k searches =
  String.concat "" (
    Array.mapi (fun i _ ->
      let n = string_of_int (k + i) in "
      AND (" ^
        "Tracks.title LIKE ?" ^ n ^ " OR " ^
        "Tracks.artist LIKE ?" ^ n ^ " OR " ^
        "Tracks.albumartist LIKE ?" ^ n ^ " OR " ^
        "Tracks.albumtitle LIKE ?" ^ n ^ " OR " ^
        "Tracks.label LIKE ?" ^ n ^ " OR " ^
        "Tracks.country LIKE ?" ^ n ^ " OR " ^
        "Playlists.title LIKE ?" ^ n ^ " OR " ^
        "Playlists.artist LIKE ?" ^ n ^
      ")"
    ) searches |> Array.to_list
  )

let playlist_tracks_fields =
      "Tracks.*,
      Playlists.pos,
      Playlists.track,
      Playlists.artist,
      Playlists.title,
      Playlists.time"

let playlist_artists_fields artistfields =
      "CASE
        " ^
        String.concat "" (List.map (fun field -> "
        WHEN " ^ field ^ " NOT NULL THEN " ^ field
        ) artistfields) ^ "
        ELSE '[unknown]'
      END AS aartist,
      COUNT(DISTINCT
        CASE
          WHEN albumtitle NOT NULL THEN albumtitle
          ELSE '[unknown]'
        END
      ) AS albums,
      COUNT(*) AS tracks"

let playlist_albums_fields =
  albums_fields' "Playlists.track" "Playlists.artist" "Playlists.title"
    "Playlists.track" ["albumartist"; "Tracks.artist"; "Playlists.artist"]


let iter_playlist_tracks_for_path_single = stmt @@
  "
    SELECT " ^ playlist_tracks_fields ^ "
    FROM Playlists LEFT JOIN Tracks ON Tracks.path = Playlists.track
    WHERE Playlists.path = ?1
      AND (?2 = '%' OR Tracks.artist = ?2 OR albumartist = ?2 OR
        Playlists.artist = ?2)
      AND (?3 = '%' OR albumtitle = ?3);
  " |>
  fun stmt db path artist album ->
(*Printf.printf "[iter_single %s] artist=%s album=%s\n%!" path artist album;*)
    iter_table [|of_text path; of_text artist; of_text album|]
      (to_playlist_track 0) stmt db

let iter_playlist_tracks_for_path_multi db path artist albums = stmt @@
  "
    SELECT " ^ playlist_tracks_fields ^ "
    FROM Playlists LEFT JOIN Tracks ON Tracks.path = Playlists.track
    WHERE Playlists.path = ?1
      AND (?2 = '%' OR Tracks.artist = ?2 OR albumartist = ?2 OR
        Playlists.artist = ?2)
      AND (albumtitle IN " ^ tuple 3 (Array.length albums) ^ ");
  " |>
  fun stmt ->
Printf.printf "[iter_multi]\n%!";
    let album_binds = Array.map of_text albums in
    iter_table (Array.append [|of_text path; of_text artist|] album_binds)
      (to_playlist_track 0) stmt db

let iter_playlist_tracks_for_path_search db path artists albums searches = stmt @@
  artists_table 2 artists ^ "
    SELECT " ^ playlist_tracks_fields ^ "
    FROM
      Playlists
      LEFT JOIN Tracks
        ON Tracks.path = Playlists.track
      " ^ playlist_artists_filter 2 artists ^ "
    WHERE Playlists.path = ?1
    " ^ playlist_albums_filter (2 + Array.length artists) albums ^ "
    " ^ playlist_search_filter (2 + Array.(length artists + length albums)) searches ^ ";
  " |>
  fun stmt ->
Printf.printf "[iter_search]\n%!";
    let artist_binds = Array.map of_text artists in
    let album_binds = Array.map of_text albums in
    let search_binds = Array.map (fun s -> of_text ("%" ^ s ^ "%")) searches in
    iter_table (Array.concat [[|of_text path|]; artist_binds; album_binds; search_binds])
      (to_playlist_track 0) stmt db

let iter_playlist_tracks_for_path db path artists albums searches f =
  if Array.length artists > 1 || searches <> [||] then
    iter_playlist_tracks_for_path_search db path artists albums searches f
  else if Array.length albums > 1 then
    let artist = if artists = [||] then "%" else artists.(0) in
    iter_playlist_tracks_for_path_multi db path artist albums f
  else
    let artist = if artists = [||] then "%" else artists.(0) in
    let album = if albums = [||] then "%" else albums.(0) in
    iter_playlist_tracks_for_path_single db path artist album f


let iter_playlist_tracks_for_path_as_artists_single = stmt @@
  "
    SELECT aartist, SUM(albums), SUM(tracks)
    FROM (
      SELECT " ^ playlist_artists_fields
        ["albumartist"; "Tracks.artist"; "Playlists.artist"] ^ "
      FROM Playlists LEFT JOIN Tracks on Tracks.path = Playlists.track
      WHERE Playlists.path = ?1
        AND NOT (Playlists.track LIKE 'separator:%')
      GROUP BY aartist
    UNION
      SELECT " ^ playlist_artists_fields ["albumartist"] ^ "
      FROM Playlists LEFT JOIN Tracks on Tracks.path = Playlists.track
      WHERE Playlists.path = ?1 AND Tracks.artist <> albumartist
        AND NOT (Playlists.track LIKE 'separator:%')
      GROUP BY albumartist
    )
    GROUP BY aartist;
  " |>
  fun stmt db path ->
    iter_table [|of_text path|] (to_artist 0) stmt db

let iter_playlist_tracks_for_path_as_artists_search db path searches = stmt @@
  "
    SELECT aartist, SUM(albums), SUM(tracks)
    FROM (
      SELECT " ^ playlist_artists_fields
        ["albumartist"; "Tracks.artist"; "Playlists.artist"] ^ "
      FROM Playlists LEFT JOIN Tracks on Tracks.path = Playlists.track
      WHERE Playlists.path = ?1
        AND NOT (Playlists.track LIKE 'separator:%')
        " ^ playlist_search_filter 2 searches ^ "
      GROUP BY aartist
    UNION
      SELECT " ^ playlist_artists_fields ["albumartist"] ^ "
      FROM Playlists LEFT JOIN Tracks on Tracks.path = Playlists.track
      WHERE Playlists.path = ?1 AND Tracks.artist <> albumartist
        AND NOT (Playlists.track LIKE 'separator:%')
        " ^ playlist_search_filter 2 searches ^ "
      GROUP BY albumartist
    )
    GROUP BY aartist;
  " |>
  fun stmt ->
    let search_binds = Array.map (fun s -> of_text ("%" ^ s ^ "%")) searches in
    iter_table (Array.append [|of_text path|] search_binds) (to_artist 0) stmt db

let iter_playlist_tracks_for_path_as_artists db path searches f =
  if searches <> [||] then
    iter_playlist_tracks_for_path_as_artists_search db path searches f
  else
    iter_playlist_tracks_for_path_as_artists_single db path f


let iter_playlist_tracks_for_path_as_albums_single = stmt @@
  "
    SELECT " ^ playlist_albums_fields ^ "
    FROM Playlists LEFT JOIN Tracks ON Tracks.path = Playlists.track
    WHERE Playlists.path = ?1
      AND NOT (Playlists.track LIKE 'separator:%')
      AND (?2 = '%' OR Tracks.artist = ?2 OR albumartist = ?2 OR
        Playlists.artist = ?2)
    GROUP BY aartist, atitle, codec, label;
  " |>
  fun stmt db path artist ->
    iter_table [|of_text path; of_text artist|] (to_album 0) stmt db

let iter_playlist_tracks_for_path_as_albums_search db path artists searches = stmt @@
  artists_table 2 artists ^ "
    SELECT " ^ playlist_albums_fields ^ "
    FROM
      Playlists
      LEFT JOIN Tracks ON Tracks.path = Playlists.track
      " ^ playlist_artists_filter 2 artists ^ "
    WHERE Playlists.path = ?1
      AND NOT (Playlists.track LIKE 'separator:%')
      " ^ playlist_search_filter (2 + Array.length artists) searches ^ "
    GROUP BY aartist, atitle, codec, label;
  " |>
  fun stmt ->
    let artist_binds = Array.map of_text artists in
    let search_binds = Array.map (fun s -> of_text ("%" ^ s ^ "%")) searches in
    iter_table (Array.concat [[|of_text path|]; artist_binds; search_binds])
      (to_album 0) stmt db

let iter_playlist_tracks_for_path_as_albums db path artists searches f =
  if Array.length artists > 1 || searches <> [||] then
    iter_playlist_tracks_for_path_as_albums_search db path artists searches f
  else
    let artist = if artists = [||] then "%" else artists.(0) in
    iter_playlist_tracks_for_path_as_albums_single db path artist f

(*
let iter_tracks_and_playlist_tracks_for_path = stmt
  "
    SELECT Tracks.*, NULL, NULL, NULL, NULL, NULL
    FROM Tracks
    WHERE
      (Tracks.path LIKE ?1) AND
      (?2 = '' OR Tracks.artist = ?2 OR albumartist = ?2) AND
      (?3 = '' OR albumtitle = ?3)
    UNION
    SELECT
      Tracks.*,
      CASE WHEN ?4 THEN Playlists.pos ELSE NULL END,
      Playlists.track, Playlists.artist, Playlists.title, Playlists.time
    FROM Playlists LEFT JOIN Tracks on Tracks.path = Playlists.track
    WHERE
      (Playlists.path LIKE ?1) AND
      (?2 = '' OR Tracks.artist = ?2 OR albumartist = ?2 OR
        Playlists.artist = ?2) AND
      (?3 = '' OR albumtitle = ?3) AND
      (?4 OR NOT (Playlists.track LIKE 'separator://%'));
  " |>
  fun stmt db path artist album with_pos ->
    iter_table [|of_text path; of_text artist; of_text album; of_bool with_pos|]
      (to_playlist_track 0) stmt db
*)


(* Initialization *)

let filename = "library.db"

let init () =
  let db = Sqlite3.db_open (Storage.path filename) in
  create_dirs db;
  create_tracks db;
  create_playlists db;
  db

let exit db =
  ignore (Sqlite3.db_close db)
