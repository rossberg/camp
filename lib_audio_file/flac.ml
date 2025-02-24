(* https://www.rfc-editor.org/rfc/rfc9639.html *)

type format =
{
  min_block_size : int; (* 16 to 65535 *)
  max_block_size : int; (* 16 to 65535 *)
  min_frame_size : int;
  max_frame_size : int;
  rate : int; (* 1 to 655350 Hz *)
  channels : int; (* 1 to 8 *)
  depth : int; (* 4 to 32 *)
  samples : int;
  md5 : string;
  size : int;
}

type analysis =
{
  format : format;
  vorbis : Vorbis.tag option;
}

type error = int * string


let rec input_int_be ic len =
  if len = 0 then 0 else
  let byte = input_byte ic in
  byte lsl (8*(len - 1)) + input_int_be ic (len - 1)

let read_streaminfo_from ic =
  let pos = pos_in ic in
  let min_block_size = input_int_be ic 2 in
  let max_block_size = input_int_be ic 2 in
  let min_frame_size = input_int_be ic 3 in
  let max_frame_size = input_int_be ic 3 in
  let rate_hi = input_int_be ic 2 in
  let byte1 = input_byte ic in
  let byte2 = input_byte ic in
  let rate = rate_hi lsl 4 + (byte1 land 0xf0) lsr 4 in
  let channels = (byte1 land 0x0e) lsr 1 + 1 in
  let depth = (byte1 land 0x01) lsl 4 + (byte2 land 0xf0) lsr 4 + 1 in
  let samples_lo = input_int_be ic 4 in
  let samples = (byte2 land 0x0f) lsl 32 + samples_lo in
  let md5 = really_input_string ic 16 in
  let errors = ref [] in
  let check b s = if not b then errors := (pos, s) :: !errors in
  check (min_block_size < 16) "invalid min block size";
  check (max_block_size < 16) "invalid max block size";
  check (min_block_size <= max_block_size) "min block size larger than max";
  check (min_frame_size = 0 || max_frame_size = 0 || min_frame_size < max_frame_size)
    "min frame size larger than max";
  check (rate > 0 && rate <= 655350) "invalid sample rate";
  { min_block_size; max_block_size; min_frame_size; max_frame_size;
    rate; channels; depth; samples; md5; size = 0 }, List.rev !errors

let rec read_size ic =
  let byte = input_byte ic in
  let last_block = byte land 0x80 <> 0 in
  let length = input_int_be ic 3 in
  let pos = pos_in ic in
  if last_block then
    Int64.to_int (In_channel.length ic) - pos - length
  else
    (seek_in ic (pos + length); read_size ic)

let rec read_format_from ic =
  let byte = input_byte ic in
  let last_block = byte land 0x80 <> 0 in
  let block_type = byte land 0x7f in
  let length = input_int_be ic 3 in
  let pos = pos_in ic in
  if block_type = 0 then
    let format, errors = read_streaminfo_from ic in
    {format with size = read_size ic}, errors
  else if last_block then
    failwith "Flac.read_format"
  else
    (seek_in ic (pos + length); read_format_from ic)

let rec read_magic ic =
  if input_char ic <> 'f' then read_magic ic else
  if input_char ic <> 'L' then read_magic ic else
  if input_char ic <> 'a' then read_magic ic else
  if input_char ic <> 'C' then read_magic ic

let read_format path =
  In_channel.with_open_bin path (fun ic ->
    (try read_magic ic with End_of_file -> failwith "Flac.read_format");
    fst (read_format_from ic)
  )


let rec analyze_from ic =
  let offset = pos_in ic in
  match really_input_string ic 4 with
  | "fLaC" ->
    let format, errors1 = read_format_from ic in
    seek_in ic 0;
    let vorbis, errors2 = Vorbis.input_tag ic in
    let errors = errors1 @ errors2 in
    {format; vorbis},
    if offset = 0 then errors else (0, "junk before stream marker") :: errors
  | _ -> seek_in ic (offset + 1); analyze_from ic

let analyze path =
  In_channel.with_open_bin path analyze_from
