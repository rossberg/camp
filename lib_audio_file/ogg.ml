(* https://www.rfc-editor.org/rfc/rfc9639.html *)

type format =
{
  channels : int;
  rate : int;
  bitrate_maximum : int;
  bitrate_nominal : int;
  bitrate_minimum : int;
  samples : int;
  size : int;
}

type analysis =
{
  format : format;
  vorbis : Vorbis.tag option;
}

type error = int * string


let rec input_int_le ic len =
  if len = 0 then 0 else
  let byte = input_byte ic in
  byte + input_int_le ic (len - 1) lsl 8


let read_packet_header ic =
  let packet_type = input_byte ic in
  let vorbis = really_input_string ic 6 in
  if vorbis <> "vorbis" then -1 else packet_type

let read_identification_header ic =
  let pos = pos_in ic in
  let version = input_int_le ic 4 in
  let channels = input_byte ic in
  let rate = input_int_le ic 4 in
  let bitrate_maximum = input_int_le ic 4 in
  let bitrate_nominal = input_int_le ic 4 in
  let bitrate_minimum = input_int_le ic 4 in
  let blocksize = input_byte ic in
  let blocksize_0 = blocksize land 0xf in
  let blocksize_1 = (blocksize lsr 4) land 0xf in
  let framing = input_byte ic in
  let errors = ref [] in
  let check b s = if not b then errors := (pos, s) :: !errors in
  check (version = 0) "invalid vorbis version";
  check (blocksize_0 <= blocksize_1) "invalid block sizes";
  check (framing land 0xfe = 0) "invalid framing flag";
  { channels; rate; bitrate_maximum; bitrate_nominal; bitrate_minimum;
    samples = 0; size = 0 }, !errors

let read_page ic =
  match really_input_string ic 4 with
  | "OggS" ->
    seek_in ic (pos_in ic - 4 + 26);
    let segments = input_byte ic in
    if segments = 0 then failwith "Ogg.read_format";
    seek_in ic (pos_in ic + segments - 1);
    let last = input_byte ic in
    let size = 255 * (segments - 1) + last in
    seek_in ic (pos_in ic + size);
  | _ -> failwith "Ogg.read_format"

let read_samples ic =
  seek_in ic (in_channel_length ic - 4);
  while really_input_string ic 4 <> "OggS" do
    seek_in ic (pos_in ic - 5)
  done;
  let _version = input_byte ic in
  let _header_type = input_byte ic in
  input_int_le ic 8

let rec read_format_from ic =
  match really_input_string ic 4 with
  | "OggS" ->
    seek_in ic (pos_in ic + 26 - 4);
    let segments = input_byte ic in
    if segments = 0 then failwith "Ogg.read_format";
    seek_in ic (pos_in ic + segments - 1);
    let last = input_byte ic in
    let size = 255 * (segments - 1) + last in
    let packet_type = read_packet_header ic in
    if packet_type = 1 then
      let format, errors = read_identification_header ic in
      read_page ic;
      (*read_page ic;*)
      let size = in_channel_length ic - pos_in ic in
      let samples = read_samples ic in
      {format with size; samples}, errors
    else
      (seek_in ic (pos_in ic + size); read_format_from ic)
  | _ -> failwith "Ogg.read_format"

let read_format path =
  File.with_open_in `Bin path (fun ic ->
    fst (read_format_from ic)
  )


let rec analyze_from ic =
  let offset = pos_in ic in
  match really_input_string ic 4 with
  | "OggS" ->
    seek_in ic offset;
    let format, errors1 = read_format_from ic in
    seek_in ic 0;
    let vorbis, errors2 = Vorbis.input_ogg_tag ic in
    let errors = errors1 @ errors2 in
    {format; vorbis},
    if offset = 0 then errors else (0, "junk before stream marker") :: errors
  | _ -> seek_in ic (offset + 1); analyze_from ic

let analyze path =
  File.with_open_in `Bin path analyze_from
