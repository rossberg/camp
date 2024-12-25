(* https://www.rfc-editor.org/rfc/rfc9639.html, Section 8.6 *)
(* https://wiki.xiph.org/index.php/VorbisComment *)

type comment =
{
  offset : int;
  key : string;
  value : string;
}

type picture =
{
  offset : int;
  picture_type : int;
  mime : string;
  width : int;
  height : int;
  depth : int;
  num_colors : int;
  description : string;
  data : string;
}

type tag =
{
  offset : int;
  vendor : string;
  comments : comment list;
  pictures : picture list;
}

type error = int * string

let seq prefix errors (result, new_errors) =
  let prefix' = if prefix = "" then "" else prefix ^ ": " in
  errors := !errors @ List.map (fun (pos, msg) -> pos, prefix' ^ msg) new_errors;
  result

let rec input_int ic len =
  if len = 0 then 0 else
  let byte = input_byte ic in
  byte + 256 * input_int ic (len - 1)

let rec input_int_be ic len =
  if len = 0 then 0 else
  let byte = input_byte ic in
  byte lsl (8*(len - 1)) + input_int_be ic (len - 1)

let input_comment ic =
  let offset = pos_in ic in
  let length = input_int ic 4 in
  let s = really_input_string ic length in
  match String.index_opt s '=' with
  | None -> [], [(pos_in ic - length, "not an equation")]
  | Some i ->
    let key = String.uppercase_ascii (String.sub s 0 i) in
    let value = String.sub s (i + 1) (String.length s - i - 1) in
    [{key; value; offset}], []

let rec input_comments ic length i =
  if length = 0 then [], [] else
  let errors = ref [] in
  let comment = seq ("comment " ^ string_of_int i) errors (input_comment ic) in
  let comments = seq "" errors (input_comments ic (length - 1) (i + 1)) in
  comment @ comments, !errors

let input_vorbis_comment ic =
  let offset = pos_in ic - 4 in
  let errors = ref [] in
  let vendor_length = input_int ic 4 in
  let vendor = really_input_string ic vendor_length in
  let comment_count = input_int ic 4 in
  let comments = seq "" errors (input_comments ic comment_count 0) in
  Some {vendor; comments; pictures = []; offset}, !errors

let input_picture ic =
  let offset = pos_in ic - 4 in
  let picture_type = input_int_be ic 4 in
  let mime_length = input_int_be ic 4 in
  let mime = really_input_string ic mime_length in
  let description_length = input_int_be ic 4 in
  let description = really_input_string ic description_length in
  let width = input_int_be ic 4 in
  let height = input_int_be ic 4 in
  let depth = input_int_be ic 4 in
  let num_colors = input_int_be ic 4 in
  let length = input_int_be ic 4 in
  let data = really_input_string ic length in
  {picture_type; description; mime; width; height; depth; num_colors; data; offset}

let rec input_metadata ic =
  let byte = input_byte ic in
  let last_block = byte land 0x80 <> 0 in
  let block_type = byte land 0x7f in
  let length = input_int_be ic 3 in
  let pos = pos_in ic in
  let tag_opt, errors =
    if block_type = 4 then
      input_vorbis_comment ic
    else if block_type = 6 then
      let picture = input_picture ic in
      Some {vendor = ""; comments = []; pictures = [picture]; offset = 0}, []
    else
      (seek_in ic (pos + length); None, [])
  in
  let size = pos_in ic - pos in
  if size <> length then
    tag_opt,
    (pos, "inconsistent block length (type " ^ string_of_int block_type ^
      ", expected " ^ string_of_int length ^
      ", found " ^ string_of_int size ^ ")") :: errors
  else if last_block then
    tag_opt, errors
  else
    let tag_opt', errors' = input_metadata ic in
    let tag_opt'', errors'' =
      match tag_opt, tag_opt' with
      | None, _ -> tag_opt', []
      | Some _, None -> tag_opt, []
      | Some tag, Some tag' ->
        Some {
          vendor = if tag.vendor = "" then tag'.vendor else tag.vendor;
          comments = tag.comments @ tag'.comments;
          pictures = tag.pictures @ tag'.pictures;
          offset = if tag.offset = 0 then tag'.offset else tag.offset;
        },
        if tag.vendor = "" || tag'.vendor = "" then [] else
          [(pos, "multiple comment blocks")]
    in
    tag_opt'', errors'' @ errors @ errors'

let rec input_tag ic =
  let offset = pos_in ic in
  match really_input_string ic 4 with
  | exception End_of_file -> None, []
  | "fLaC" -> input_metadata ic
  | _ -> seek_in ic (offset + 1); input_tag ic
