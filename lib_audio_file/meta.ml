(* Warnings *)

let warn_hook = ref (fun _ ->  ignore)
let progress_hook = ref ignore

let warn path msg = !warn_hook path msg
let progress () = !progress_hook ()


(* Tags *)

type path = string
type time = float

type tag =
  | Id3v2Tag of Id3v2.tag
  | VorbisTag of Vorbis.tag

let load_tag path : tag option =
  progress ();
  let tag_opt, errors =
    File.with_open_in `Bin path (fun file ->
      try
        match Filename.extension path with
        | ".mp3" ->
          let tag_opt, errors = Id3v2.input_tag file in
          Option.map (fun tag -> Id3v2Tag tag) tag_opt, errors
        | ".flac" ->
          (match Vorbis.input_flac_tag file with
          | None, [] ->
            let tag_opt, errors = Id3v2.input_tag file in
            Option.map (fun tag -> Id3v2Tag tag) tag_opt,
            (0, "no Vorbis tag in .flac file, falling back to Id3v2") :: errors
          | tag_opt, errors ->
            Option.map (fun tag -> VorbisTag tag) tag_opt, errors
          )
        | ".ogg" ->
          let tag_opt, errors = Vorbis.input_ogg_tag file in
          Option.map (fun tag -> VorbisTag tag) tag_opt, errors
        | ".opus" ->
          let tag_opt, errors = Vorbis.input_opus_tag file in
          Option.map (fun tag -> VorbisTag tag) tag_opt, errors
        | ext ->
          None, [(0, "unknown file type " ^ ext)]
      with End_of_file ->
        None, [(pos_in file, "unexpected end of file")]
    )
  in
  List.iter (fun (_, msg) -> warn path msg) errors;
  if tag_opt = None && errors = [] then
    warn path "no tag found";
  tag_opt


(* Meta data *)

type picture =
{
  mime : string;
  width : int;
  height : int;
  progressive : bool;
  data : string;
}

type t =
{
  loaded : bool;
  artist : string;
  title : string;
  track : int;
  tracks : int;
  track_txt : string;
  disc : int;
  discs : int;
  disc_txt : string;
  albumartist : string;
  albumtitle : string;
  year : int;
  date : time;
  date_txt : string;
  label : string;
  country : string;
  length : time;
  rating : int;
  cover : picture option;
}

let tag_field (id3v2_name, vorbis_name) tag : string option =
  match tag with
  | None -> None
  | Some (Id3v2Tag tag) ->
    let open Id3v2 in
    (match
      List.find_opt (fun f -> f.id = id3v2_name && f.contents <> []) tag.frames
    with
    | Some frame -> Some (List.hd frame.contents)
    | None ->
      match
        List.find_opt (fun f -> f.id = "TXXX" &&
        List.length f.contents > 1 && List.hd f.contents = id3v2_name) tag.frames
      with
      | Some frame -> Some (List.hd (List.tl frame.contents))
      | None -> None
    )
  | Some (VorbisTag tag) ->
    let open Vorbis in
    match List.find_opt (fun c -> c.key = vorbis_name) tag.comments with
    | Some comment -> Some comment.value
    | None -> None

let warn_field (id3v2_name, vorbis_name) path tag s =
  if path = "" then () else
  let name, kind =
    match tag with
    | None -> assert false
    | Some (Id3v2Tag _) -> id3v2_name, "Id3v2"
    | Some (VorbisTag _) -> vorbis_name, "Vorbis"
  in
  warn path
    ( "invalid " ^ name ^
      " value \"" ^ String.escaped s ^ "\" in " ^ kind ^ " tag")

let text_tag_field names _path tag : string =
  match tag_field names tag with
  | None -> ""
  | Some s -> s

let int_tag_field names path tag : int =
  match tag_field names tag with
  | None -> 0
  | Some s ->
    let k = Option.value (String.index_opt s '/') ~default:(String.length s) in
    match int_of_string_opt (String.sub s 0 k) with
    | Some i -> i
    | None -> warn_field names path tag s; 0

let maxint_tag_field names _path tag : int =
  match tag_field names tag with
  | None -> 0
  | Some s ->
    let k = Option.value (String.index_opt s '/') ~default:(String.length s - 1) in
    match int_of_string_opt (String.sub s (k + 1) (String.length s - k - 1)) with
    | Some i -> i
    | None -> 0

let time_tag_field names path tag : float =
  match tag_field names tag with
  | None -> 0.0
  | Some s ->
    match float_of_string_opt s with
    | Some t -> t /. 1000.0
    | None -> warn_field names path tag s; 0.0

let year_of_string s =
  let n = Option.value (String.index_opt s '-') ~default:(String.length s) in
  let s' = String.sub s 0 n in
  match int_of_string_opt s' with
  | Some n -> n
  | None when s' = "????" -> 1
  | None -> 0

let year_tag_field names path tag : int =
  match tag_field names tag with
  | None -> 0
  | Some s ->
    let year = year_of_string s in
    if year = 0 then warn_field names path tag s;
    year

let date0 = -1970.0 *. 365.0 *. 24.0 *. 60.0 *. 60.0  (* year 0 *)

let date y m d =
    let min_year = 1971 in  (* Windows mktime cannot handle earlier dates *)
    let tm =
      Unix.{
        tm_year = max y min_year - 1900;
        tm_mon = m - 1;
        tm_mday = d;
        tm_hour = 0;
        tm_min = 0;
        tm_sec = 0;
        tm_yday = 0;
        tm_wday = 0;
        tm_isdst = false;
      }
    in
    let t = fst (Unix.mktime tm) in
    if y >= min_year then t else t -. float (min_year - y) *. 365.0 *. 24.0 *. 60.0 *. 60.0

let date_of_string s =
  let num_from i default =
    let j_opt = String.index_from_opt s i '-' in
    let n = Option.value j_opt ~default:(String.length s) - i in
    let s' = String.sub s i n in
    let i' = min (i + n + 1) (String.length s) in
    match int_of_string_opt s' with
    | Some x -> x, i'
    | None when String.for_all ((=) '?') s' -> 1, i'
    | None -> default, String.length s
  in
  let y, i = num_from 0 0 in
  let m, j = num_from i 1 in
  let d, _ = num_from j 1 in
  try date y m d with _ -> date0

let date_tag_field names path tag : float =
  match tag_field names tag with
  | None -> date0
  | Some s ->
    let date = date_of_string s in
    if date = date0 then warn_field names path tag s;
    date

let rating_tag_field names path tag : int =
  match tag_field names tag with
  | None -> 0
  | Some s ->
    match String.index_opt s '\x00' with
    | None -> 0
    | Some i when i + 1 = String.length s ->
      warn_field names path tag s;
      0
    | Some i ->
      let n = Char.code s.[i + 1] in
      let r =
        if n = 0 then 0 else
        if n < 32 then 1 else  (* * = 1 *)
        if n < 96 then 2 else  (* ** = 64 *)
        if n < 160 then 3 else (* *** = 128 *)
        if n < 224 then 4 else (* **** = 196 (!) *)
        5                      (* ***** = 255 *)
      in r

let string_of_dims w h =
  if w = -1 || h = -1 then "??x??" else
  Printf.sprintf "%dx%d" w h

let picture_dims path tag mime data w h =
  let w', h', prog =
    match mime with
    | "image/jpeg" ->
      (try
        let open Jpeg in
        let format = Jpeg.read_format data in
        format.width, format.height, format.mode = Jpeg.Progressive
      with Jpeg.Format ->
        warn path ("unrecognized JPEG format in " ^ tag ^ " tag");
        w, h, false
      )
    | "image/png" ->
      (try
        let open Png in
        let format = Png.read_format data in
        format.width, format.height, false
      with Png.Format ->
        warn path ("unrecognized PNG format in " ^ tag ^ " tag");
        w, h, false
      )
    | _ ->
      warn path ("unrecognized cover mime type " ^ mime ^ " in " ^ tag ^ " tag");
      w, h, false
  in
  if w <> -1 && (w' <> w || h' <> h) then
    warn path
      ( "inconsistent cover dimensions in " ^ tag ^ " tag (" ^
        string_of_dims w h ^ " in tag vs " ^
        string_of_dims w' h' ^ " in image)" );
  w', h', prog

let picture_tag_field path tag : picture option =
  match tag with
  | None -> None
  | Some (Id3v2Tag id3v2) ->
    (match
      List.find_opt (fun f -> f.Id3v2.id = "APIC") id3v2.Id3v2.frames
    with
    | Some {Id3v2.picture = Some pic; _} ->
      let width, height, progressive =
        picture_dims path "Id3v2" pic.Id3v2.mime pic.Id3v2.data (-1) (-1)
      in
      Some {
        mime = pic.Id3v2.mime;
        width;
        height;
        progressive;
        data = pic.Id3v2.data;
      }
    | _ -> None
    )
  | Some (VorbisTag vorbis) ->
    match vorbis.Vorbis.pictures with
    | [] -> None
    | pic::_ ->
      let width, height, progressive =
        picture_dims path "Vorbis" pic.Vorbis.mime pic.Vorbis.data
          pic.Vorbis.width pic.Vorbis.height
      in
      Some {
        mime = pic.Vorbis.mime;
        width;
        height;
        progressive;
        data = pic.Vorbis.data;
      }

let meta path tag =
  try
    {
      loaded = tag <> None;
      artist = text_tag_field ("TPE1", "ARTIST") path tag;
      title = text_tag_field ("TIT2", "TITLE") path tag;
      track = int_tag_field ("TRCK", "TRACKNUMBER") path tag;
      tracks = maxint_tag_field ("TRCK", "TRACKNUMBER") path tag;
      track_txt = text_tag_field ("TRCK", "TRACKNUMBER") path tag;
      disc = int_tag_field ("TPOS", "DISCNUMBER") path tag;
      discs = maxint_tag_field ("TPOS", "DISCNUMBER") path tag;
      disc_txt = text_tag_field ("TPOS", "DISCNUMBER") path tag;
      albumartist = text_tag_field ("TPE2", "ALBUMARTIST") path tag;
      albumtitle = text_tag_field ("TALB", "ALBUM") path tag;
      year = year_tag_field ("TYER", "DATE") path tag;
      date = date_tag_field ("TDAT", "DATE") path tag;
      date_txt = text_tag_field ("TDAT", "DATE") path tag;
      label = text_tag_field ("TPUB", "ORGANIZATION") path tag;
      country = text_tag_field ("COUNTRY", "COUNTRY") path tag;
      length = time_tag_field ("TLEN", "???") path tag;
      rating = rating_tag_field ("POPM", "RATING WMP") path tag;
      cover = picture_tag_field path tag;
    }
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    warn path "error while loading tag";
    Printexc.raise_with_backtrace exn bt


let load path = meta path (load_tag path)
