(* https://id3.org/id3v2.3.0 *)
(* https://id3.org/id3v2.4.0-structure, https://id3.org/id3v2.4.0-frames *)

type flags =
{
  unsynchronisation : bool;
  extended : bool;
  experimental : bool;
  footer : bool;
}

type header =
{
  offset : int;
  version : int;
  revision : int;
  flags : flags;
  size : int
}

type restrictions =
{
  tag_size : [`Max1MB | `Max128KB | `Max40KB | `Max4KB];
  text_encoding : [`ISO8859_1_or_UTF8] option;
  text_size : [`Max1024 | `Max128 | `Max30] option;
  image_encoding : [`JPEG_or_PNG] option;
  image_size : [`Max256 | `Max64 | `Eq64] option;
}

type extended_header =
{
  offset : int;
  size : int;
  update : bool;
  crc : Int32.t option;
  restrictions : restrictions option
}

type status_flags =
{
  tag_alter_discard : bool;
  file_alter_discard : bool;
  read_only : bool;
}

type format_flags =
{
  group : bool;
  compression : bool;
  encryption : bool;
  unsynchronisation : bool;
  data_length : bool;
}

type picture =
{
  picture_type : int;
  mime : string;
  description : string;
  data : string;
}

type frame =
{
  offset : int;
  id : string;
  size : int;
  status_flags : status_flags;
  format_flags : format_flags;
  group : int option;
  data_length : int option;
  contents : string list;
  picture : picture option;
}

type tag =
{
  header : header;
  extended : extended_header option;
  frames : frame list;
  padding : int
}

type error = int * string

let quote s = "\"" ^ String.escaped s ^ "\""

let require b msg pos errors =
  if not b then errors := !errors @ [(pos, msg)]

let seq prefix errors (result, new_errors) =
  let prefix' = if prefix = "" then "" else prefix ^ ": " in
  errors := !errors @ List.map (fun (pos, msg) -> pos, prefix' ^ msg) new_errors;
  result

let input_unsync_byte ic header =
  let byte = input_byte ic in
  if byte = 0xff && header.flags.unsynchronisation then begin
    let pos = pos_in ic in
    try
      let next = input_byte ic in
      if next <> 0x00 then seek_in ic pos
	with End_of_file -> ()
  end;
  byte

let input_int ic header len =
  let value = ref 0 in
  for _ = 0 to len - 1 do
    let byte = input_unsync_byte ic header in
	value := (!value lsl 8) lor byte
  done;
  !value

let input_int32 ic header len =
  let value = ref 0l in
  for _ = 0 to len - 1 do
    let byte = input_unsync_byte ic header in
	value := Int32.(logor (shift_left !value 8) (Int32.of_int byte))
  done;
  !value

let input_syncsafe_int ic len =
  let errors = ref [] in
  let value = ref 0 in
  for _ = 0 to len - 1 do
    let byte = input_byte ic in
    require (byte land 0x80 = 0) "invalid byte" (pos_in ic - 1) errors;
	value := (!value lsl 7) lor byte
  done;
  !value, !errors

let input_syncsafe_int32 ic =
  let errors = ref [] in
  let value = ref 0l in
  for _ = 0 to 4 do
    let byte = input_byte ic in
    require (byte land 0x80 = 0) "invalid byte" (pos_in ic - 1) errors;
	value := Int32.(logor (shift_left !value 7) (Int32.of_int byte))
  done;
  !value, !errors

let input_blob ic header len =
  if header.flags.unsynchronisation then
    let buf = Buffer.create len in
	for _ = 1 to len do
	  Buffer.add_char buf (Char.chr (input_unsync_byte ic header))
	done;
	Buffer.contents buf
  else
    really_input_string ic len

let input_flags ic version =
  let errors = ref [] in
  let pos = pos_in ic in
  let flags = input_byte ic in
  require (flags land 0x0f = 0) "invalid flags" pos errors;
  let unsynchronisation = (flags land 0x80 <> 0) in
  let extended = (flags land 0x40 <> 0) in
  let experimental = (flags land 0x20 <> 0) in
  let footer = (flags land 0x10 <> 0) in
  require (version >= 3 || not extended) "extended invalid in version" pos errors;
  require (version >= 3 || not experimental) "expermental invalid in version" pos errors;
  require (version >= 4 || not footer) "footer invalid in version" pos errors;
  {unsynchronisation; extended; experimental; footer}, !errors

let input_header ic =
  let errors = ref [] in
  let offset = pos_in ic - 3 in
  let version = input_byte ic in
  require (version <= 4) ("unknown version " ^ string_of_int version) (pos_in ic - 1) errors;
  let revision = input_byte ic in
  let flags = seq "flags" errors (input_flags ic version) in
  let size = seq "size" errors (input_syncsafe_int ic 4) in
  {offset; version; revision; flags; size}, !errors

let input_restrictions ic =
  let flags = input_byte ic in
  let tag_size =
	match flags land 0xc0 with
	| 0x00 -> `Max1MB
	| 0x40 -> `Max128KB
	| 0x80 -> `Max40KB
	| 0xc0 -> `Max4KB
	| _ -> assert false
  in
  let text_encoding =
    match flags land 0x20 with
	| 0x00 -> None
	| 0x20 -> Some `ISO8859_1_or_UTF8
	| _ -> assert false
  in
  let text_size =
    match flags land 0x18 with
	| 0x00 -> None
	| 0x08 -> Some `Max1024
	| 0x10 -> Some `Max128
	| 0x18 -> Some `Max30
	| _ -> assert false
  in
  let image_encoding =
    match flags land 0x04 with
	| 0x00 -> None
	| 0x04 -> Some `JPEG_or_PNG
	| _ -> assert false
  in
  let image_size =
    match flags land 0x03 with
	| 0x00 -> None
	| 0x01 -> Some `Max256
	| 0x02 -> Some `Max64
	| 0x03 -> Some `Eq64
	| _ -> assert false
  in {tag_size; text_encoding; text_size; image_encoding; image_size}

let input_extended_header_v4 ic _header =
  let errors = ref [] in
  let offset = pos_in ic in
  let size = seq "size" errors (input_syncsafe_int ic 4) in
  let flags_size = input_byte ic in
  require (flags_size = 1) "flags size" (pos_in ic - 1) errors;
  let flags = input_byte ic in
  require (flags land 0x8f = 0) "invalid flags" (pos_in ic - 1) errors;
  let update = (flags land 0x40 <> 0) in
  let has_crc = (flags land 0x20 <> 0) in
  let has_restrictions = (flags land 0x10 <> 0) in
  let () =
	if not update then () else
	let length = input_byte ic in
	require (length = 0) "invalid update data length" (pos_in ic - 1) errors;
  in
  let crc =
	if not has_crc then None else
	let length = input_byte ic in
	require (length = 5) "invalid CRC data length" (pos_in ic - 1) errors;
	Some (seq "crc" errors (input_syncsafe_int32 ic))
  in
  let restrictions =
	if not has_restrictions then None else
    let length = input_byte ic in
    require (length = 1) "invalid restrictions data length" (pos_in ic - 1) errors;
	Some (input_restrictions ic)
  in Some {offset; size; update; crc; restrictions}, !errors

let input_extended_header_v3 ic header =
  let errors = ref [] in
  let offset = pos_in ic in
  let size = input_int ic header 4 in
  let flags1 = input_byte ic in
  require (flags1 land 0x7f = 0) "invalid flags" (pos_in ic - 1) errors;
  let flags2 = input_byte ic in
  require (flags2 = 0) "invalid flags" (pos_in ic - 1) errors;
  let has_crc = (flags1 land 0x80 <> 0) in
  let crc =	if not has_crc then None else Some (input_int32 ic header 4) in
  Some {offset; size; crc; update = false; restrictions = None}, !errors

let input_extended_header ic header =
  (if header.version = 4 then input_extended_header_v4 else input_extended_header_v3) ic header

let is_id_letter c = ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9')

let input_status_flags_v4 ic =
  let errors = ref [] in
  let flags = input_byte ic in
  require (flags land 0x8f = 0) "invalid flags" (pos_in ic - 1) errors;
  let tag_alter_discard = (flags land 0x40 <> 0) in
  let file_alter_discard = (flags land 0x20 <> 0) in
  let read_only = (flags land 0x10 <> 0) in
  {tag_alter_discard; file_alter_discard; read_only}, !errors

let input_status_flags_v3 ic =
  let errors = ref [] in
  let flags = input_byte ic in
  require (flags land 0x1f = 0) "invalid flags" (pos_in ic - 1) errors;
  let tag_alter_discard = (flags land 0x80 <> 0) in
  let file_alter_discard = (flags land 0x40 <> 0) in
  let read_only = (flags land 0x20 <> 0) in
  {tag_alter_discard; file_alter_discard; read_only}, !errors

let input_status_flags ic header =
  (if header.version = 4 then input_status_flags_v4 else input_status_flags_v3) ic

let input_format_flags_v4 ic =
  let errors = ref [] in
  let flags = input_byte ic in
  require (flags land 0xb0 = 0) "invalid flags" (pos_in ic - 1) errors;
  let group = (flags land 0x40 <> 0) in
  let compression = (flags land 0x08 <> 0) in
  let encryption = (flags land 0x04 <> 0) in
  let unsynchronisation = (flags land 0x02 <> 0) in
  let data_length = (flags land 0x01 <> 0) in
  require (not compression || data_length)
    "missing data length" (pos_in ic - 1) errors;
  {group; compression; encryption; unsynchronisation; data_length}, !errors

let input_format_flags_v3 ic =
  let errors = ref [] in
  let flags = input_byte ic in
  require (flags land 0x1f = 0) "invalid flags" (pos_in ic - 1) errors;
  let compression = (flags land 0x80 <> 0) in
  let encryption = (flags land 0x40 <> 0) in
  let group = (flags land 0x20 <> 0) in
  let unsynchronisation = false in
  let data_length = false in
  {group; compression; encryption; unsynchronisation; data_length}, !errors

let input_format_flags ic header =
  (if header.version = 4 then input_format_flags_v4 else input_format_flags_v3) ic

let encode_utf8 c buf =
  if c < 0x80 then
  (
    Buffer.add_char buf (Char.chr c);
  )
  else if c < 0x800 then
  (
    Buffer.add_char buf (Char.chr (0xc0 lor (c lsr 6)));
    Buffer.add_char buf (Char.chr (0x80 lor (c land 0x3f)));
  )
  else if c <> 0xfeff then
  (
    Buffer.add_char buf (Char.chr (0xe0 lor (c lsr 12)));
    Buffer.add_char buf (Char.chr (0x80 lor ((c lsr 6) land 0x3f)));
    Buffer.add_char buf (Char.chr (0x80 lor (c land 0x3f)));
  )

let input_iso ic header size =
  let raw = input_blob ic header size in
  let buf = Buffer.create size in
  for i = 0 to size - 1 do
	encode_utf8 (Char.code raw.[i]) buf
  done;
  String.split_on_char '\x00' (Buffer.contents buf), []

let input_utf8 ic size =
  let errors = ref [] in
  let pos = pos_in ic in
  let contents = really_input_string ic size in
  require (Unicode.is_utf8 contents) "invalid UTF-8" pos errors;
  String.split_on_char '\x00' contents, !errors

let input_utf16 ic header size = (* TODO: handle supplementary planes *)
  let errors = ref [] in
  let raw = input_blob ic header size in
  require (size land 1 = 0) "unicode text has odd length" (pos_in ic) errors;
  let buf = Buffer.create (size / 2) in
  for i = 0 to (size - 1) / 2 do
    let c = (Char.code raw.[2*i + 1] lsl 8) lor Char.code raw.[2*i] in
	encode_utf8 c buf
  done;
  String.split_on_char '\x00' (Buffer.contents buf), !errors

let input_text ic header size =
  let pos = pos_in ic in
  match input_byte ic with
  | 0x00 -> input_iso ic header (size - 1)
  | 0x01 ->
    let errors = ref [] in
    let bom = input_blob ic header 2 in
	require (bom = "\xff\xfe") "missing BOM in unicode" pos errors;
	let text, errors' = input_utf16 ic header (size - 3) in
	text, !errors @ errors'
  | 0x02 -> input_utf16 ic header (size - 1)
  | 0x03 -> input_utf8 ic (size - 1)
  | _ -> [input_blob ic header (size - 1)], [pos, "invalid encoding"]

let input_pic ic header size =
  let buf = input_blob ic header size in
  let i = try String.index_from buf 1 '\x00' with Not_found -> String.length buf in
  let mime = String.sub buf 1 (i - 1) in
  let picture_type = Char.code buf.[i + 1] in
  let j = try String.index_from buf (i + 2) '\x00' with Not_found -> String.length buf in
  let description = String.sub buf (i + 2) (j - i - 2) in
  let data = String.sub buf (j + 1) (String.length buf - j - 1) in
  {picture_type; mime; description; data}, []

let input_frame ic limit header =
  let offset = pos_in ic in
  let errors = ref [] in
  let id = really_input_string ic 4 in
  require
	(is_id_letter id.[0] && is_id_letter id.[1] &&
	 is_id_letter id.[0] && is_id_letter id.[1])
	("invalid frame id " ^ quote id) (offset - 4) errors;
  let size =
    if header.version <= 3 then input_int ic header 4 else
    seq "frame size" errors (input_syncsafe_int ic 4)
  in
  let status_flags = seq "status flags" errors (input_status_flags ic header) in
  let format_flags = seq "format flags" errors (input_format_flags ic header) in
  let group =
    if not format_flags.group then None else
    Some (input_byte ic)
  in
  let data_length, size =
    if not format_flags.data_length then None, size else
    Some (seq "data length" errors (input_syncsafe_int ic 4)), size - 4
  in
  require (pos_in ic + size <= limit) "frame size out of bounds" (pos_in ic) errors;
  let size = min size (limit - pos_in ic) in
  let header =
    if not format_flags.unsynchronisation then header else
	{header with flags = {header.flags with unsynchronisation = true}} 
  in
  let contents, picture =
    match id.[0] with
	| 'T' -> seq (id ^ " text") errors (input_text ic header size), None
	| 'A' when id = "APIC" -> [], Some (seq id errors (input_pic ic header size))
	| _ -> [input_blob ic header size], None
  in
  {offset; id; size; status_flags; format_flags; group; data_length; contents; picture}, !errors

let rec input_frames ic limit header =
  let offset = pos_in ic in
  if offset + 10 > limit then [], [] else
  let b = try input_byte ic with End_of_file -> 0 in
  seek_in ic offset;
  if b = 0 then [], [] else
  let frame, errors1 = input_frame ic limit header in
  let frames, errors2 = input_frames ic limit header in
  frame::frames, errors1 @ errors2

let input_tag_body ic =
  let errors = ref [] in
  let header = seq "header" errors (input_header ic) in
  let extended =
    if not header.flags.extended then None else
	seq "extended header" errors (input_extended_header ic header)
  in
  let frames_pos = pos_in ic in
  let limit = min (frames_pos + header.size) (in_channel_length ic) in
  let frames = seq "" errors (input_frames ic limit header) in
  let padding =
    if header.flags.footer then
    (
      require (pos_in ic = frames_pos + header.size) "inconsistent tag size" (pos_in ic) errors;
      let tag = really_input_string ic 3 in
      require (tag = "3DI") "invalid footer tag" (pos_in ic - 3) errors;
      let footer = seq "footer" errors (input_header ic) in
      require (footer = header) "footer inconsistent" (frames_pos + header.size) errors;
      0
    )
    else
    (
      require (pos_in ic <= frames_pos + header.size) "inconsistent tag size" (pos_in ic) errors;
      let padding_size = max 0 (frames_pos + header.size - pos_in ic) in
      let padding = really_input_string ic padding_size in
      let ok = ref true in
      for i = 0 to padding_size - 1 do
        if padding.[i] <> '\x00' then ok := false
      done;
      require !ok ("invalid padding string " ^ quote padding) (frames_pos + header.size - padding_size) errors;
      padding_size
    )
  in {header; extended; frames; padding}, !errors

let input_tag ic =
  let offset = pos_in ic in
  let tag = try really_input_string ic 3 with End_of_file -> "" in
  if tag = "ID3" then
    let tag, errors = input_tag_body ic in
	Some tag, errors
  else
    (seek_in ic offset; None, [])
