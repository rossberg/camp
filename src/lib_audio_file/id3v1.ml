(* https://id3.org/ID3v1 *)
(* http://mpgedit.org/mpgedit/mpeg_format/mpeghdr.htm#MPEGTAG *)

type tag =
{
  offset : int;
  title : string;
  artist : string;
  album : string;
  year : int option;
  comment : string;
  track : int option;
  genre : int;
}

type error = int * string

let quote s = "\"" ^ String.escaped s ^ "\""

let _require b msg pos errors =
  if not b then errors := !errors @ [(pos, msg)]

let seq prefix errors (result, new_errors) =
  errors := !errors @ List.map (fun (pos, msg) -> pos, prefix ^ ": " ^ msg) new_errors;
  result

let input_padded_string ic size =
  let pos = pos_in ic in
  let s = really_input_string ic size in
  let len = try String.index s '\x00' with Not_found -> size in
  let ok = ref true in
  for i = len + 1 to size - 1 do
    if s.[i] <> '\x00' then ok := false
  done;
  String.sub s 0 len, (if !ok then [] else [pos, "invalid string " ^ quote s])

let input_tag_body ic =
  let errors = ref [] in
  let offset = pos_in ic - 3 in
  let title = seq "title" errors (input_padded_string ic 30) in
  let artist = seq "artist" errors (input_padded_string ic 30) in
  let album = seq "album" errors (input_padded_string ic 30) in
  let year_pos = pos_in ic in
  let year_raw = seq "year" errors (input_padded_string ic 4) in
  let year = seq "year" errors
    (if year_raw = "" then None, []
	else if String.length year_raw < 4 then
	  None, [year_pos, "invalid string " ^ quote year_raw]
	else
      try Some (int_of_string year_raw), [] with Failure _ ->
	    None, [year_pos, "invalid string " ^ quote year_raw]
	)
  in
  let comment_raw = seq "comment" errors (input_padded_string ic 29) in
  let track_raw = input_byte ic in
  let track, comment =
    if track_raw = 0 then None, comment_raw
    else if String.length comment_raw < 29 then Some track_raw, comment_raw
	else None, comment_raw ^ String.make 1 (Char.chr track_raw)
  in
  let genre = input_byte ic in
  {offset; title; artist; album; year; comment; track; genre}, !errors

let input_tag ic =
  let offset = pos_in ic in
  let tag = try really_input_string ic 3 with End_of_file -> "" in
  if tag = "TAG" then
    let tag, errors = input_tag_body ic in
	Some tag, errors
  else
    (seek_in ic offset; None, [])
