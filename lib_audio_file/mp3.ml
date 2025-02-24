(* https://www.datavoyage.com/mpgscript/mpeghdr.htm *)

type version = Mpeg1 | Mpeg2 | Mpeg2_5
type joint = {intensity : bool; ms : bool}
type channels = Stereo | Joint of joint | Dual | Mono
type encoding = CBR | VBR
type emphasis = Emph50 | CCIT | Reserved

type format =
{
  version : version;
  layer : int;
  bitrate : int; (* kb/s *)
  rate : int; (* Hz *)
  channels : channels;
  encoding : encoding;
  emphasis : emphasis option;
  protection : bool;
  privatebit : bool;
  copyright : bool;
  original : bool;
  frames : int;
  time : float; (* s *)
  size : int; (* B *)
  offset : int (* B *)
}

type analysis =
{
  format : format;
  id3v1 : Id3v1.tag option;
  id3v2 : Id3v2.tag option;
}

type error = int * string

let bitrates =
[|
  [|
    [|0; 32; 64; 96; 128; 160; 192; 224; 256; 288; 320; 352; 384; 416; 448|];
    [|0; 32; 48; 56; 64; 80; 96; 112; 128; 160; 192; 224; 256; 320; 384|];
    [|0; 32; 40; 48; 56; 64; 80; 96; 112; 128; 160; 192; 224; 256; 320|]
  |];
  [|
    [|0; 32; 48; 56; 64; 80; 96; 112; 128; 144; 160; 176; 192; 224; 256|];
    [|0; 8; 16; 24; 32; 40; 48; 56; 64; 80; 96; 112; 128; 144; 160|];
    [|0; 8; 16; 24; 32; 40; 48; 56; 64; 80; 96; 112; 128; 144; 160|]
  |];
  [|
    [|0; 32; 48; 56; 64; 80; 96; 112; 128; 144; 160; 176; 192; 224; 256|];
    [|0; 8; 16; 24; 32; 40; 48; 56; 64; 80; 96; 112; 128; 144; 160|];
    [|0; 8; 16; 24; 32; 40; 48; 56; 64; 80; 96; 112; 128; 144; 160|]
  |]
|]

let rates =
[|
  [|44100; 48000; 32000|];
  [|22050; 24000; 16000|];
  [|11025; 12000; 8000|]
|]

let samples = [|384; 1152; 1152|]

let is_id3_header buf =
  buf.[0] = 'I' && buf.[1] = 'D' && buf.[2] = '3' &&
  Char.code buf.[3] < 0xFF && Char.code buf.[4] < 0xFF &&
  Char.code buf.[6] < 0x80 && Char.code buf.[7] < 0x80 &&
  Char.code buf.[8] < 0x80 && Char.code buf.[9] < 0x80

let read_id3_header_size ic =
  seek_in ic 0;
  let buf = really_input_string ic 10 in
  if not (is_id3_header buf) then 0 else
  let footer = Char.code buf.[5] land 0x10 <> 0 in
  let size =
    (Char.code buf.[6] lsl 21) lor
    (Char.code buf.[7] lsl 14) lor
    (Char.code buf.[8] lsl 7) lor
    (Char.code buf.[9])
  in size + (if footer then 20 else 10)

let input_int ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  let b4 = input_byte ic in
  b1 >= 0x80, (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4

let read_xing_header ic format =
  let offset =
    match format.version, format.channels with
    | Mpeg2_5, Mono -> 9
    | Mpeg2_5, _ -> 17
    | _, Mono -> 17
    | _, _ -> 32
  in seek_in ic (pos_in ic + offset);
  let buf = really_input_string ic 4 in
  let _, flags = input_int ic in
  if buf <> "Xing" || flags land 0x3 <> 0x3 then None else
  let _, frames = input_int ic in
  let _, bytes = input_int ic in
  Some (frames, bytes)

let decode_frame_header (hibit, header) =
  if not hibit || header land 0x7FE00000 <> 0x7FE00000 then None else
  let version, version_index =
    match (header lsr 19) land 0x3 with
    | 0 -> Mpeg2_5, 2
    | 2 -> Mpeg2, 1
    | 3 -> Mpeg1, 0
    | _ -> Mpeg1, -1
  in
  let layer = 4 - ((header lsr 17) land 0x3) in
  let bitrate_index = (header lsr 12) land 0xF in
  let rate_index = (header lsr 10) land 0x3 in
  let extension = (header lsr 4) land 0x3 in
  if
    version_index = -1 || layer = 4 ||
    rate_index = 3 || bitrate_index = 15
  then None
  else Some
  {
    version = version;
    layer = layer;
    bitrate = bitrates.(version_index).(layer - 1).(bitrate_index);
    rate = rates.(version_index).(rate_index);
    channels =
    (
      match (header lsr 6) land 0x3 with
      | 0 -> Stereo
      | 1 -> Joint {intensity = extension >= 2; ms = extension land 0x1 <> 0}
      | 2 -> Dual
      | _ -> Mono
    );
    encoding = CBR;
    emphasis =
    (
      match header land 0x3 with
      | 1 -> Some Emph50
      | 2 -> Some Reserved
      | 3 -> Some CCIT
      | _ -> None
    );
    protection = header land 0x10000 <> 0;
    privatebit = header land 0x100 <> 0;
    copyright = header land 0x8 <> 0;
    original = header land 0x4 <> 0;
    frames = 0; (* dummy *)
    time = 0.0; (* dummy *)
    size = 0; (* dummy *)
    offset = 0 (* dummy *)
  }

let read_frame_header ic =
  let pos = pos_in ic in
  let rec sync i =
    match decode_frame_header (input_int ic) with
    | Some format -> format, i
    | None -> seek_in ic (pos + i + 1); sync (i + 1)
  in
  let format, i = sync 0 in
  format, (if i = 0 then [] else [pos, Printf.sprintf "0x%x (%d) bytes of junk at start" i i])

let kbits = 8.0 /. 1000.0

let read_format_from ic =
  seek_in ic (read_id3_header_size ic);
  let f, _ = read_frame_header ic in
  let offset = pos_in ic - 4 in
  let size = in_channel_length ic in
  let encoding, frames, time, bitrate =
    match read_xing_header ic f with
    | None ->
      let bytes = size - pos_in ic in
      let time = float bytes *. kbits /. float f.bitrate in
      let frames = time *. float f.rate /. float samples.(f.layer - 1) in
      CBR, truncate (frames +. 0.5), time, f.bitrate
    | Some (frames, bytes) ->
      let time = float (frames * samples.(f.layer - 1)) /. float f.rate in
      let bitrate = float bytes *. kbits /. time in
      VBR, frames, time, truncate (bitrate +. 0.5)
  in
  {f with encoding; bitrate; frames; time; size; offset}

let read_format path =
  File.with_open_in `Bin path read_format_from


let _require b msg pos errors =
  if not b then errors := !errors @ [(pos, msg)]

let seq prefix errors (result, new_errors) =
  errors := !errors @ List.map (fun (pos, msg) -> pos, prefix ^ ": " ^ msg) new_errors;
  result

let analyze_from ic =
  let errors = ref [] in
  let id3_header_size = read_id3_header_size ic in
  seek_in ic 0;  (*TODO*)
  let id3v2 = seq "id3v2 tag" errors
    (if id3_header_size > 0 then Id3v2.input_tag ic else None, []) in
  seek_in ic id3_header_size;
  let f = seq "first frame" errors (read_frame_header ic) in
  let offset = pos_in ic - 4 in
  let size = in_channel_length ic in
  let encoding, frames, time, bitrate =
    match read_xing_header ic f with
    | None ->
      let bytes = size - pos_in ic in
      let time = float bytes *. kbits /. float f.bitrate in
      let frames = time *. float f.rate /. float samples.(f.layer - 1) in
      CBR, truncate (frames +. 0.5), time, f.bitrate
    | Some (frames, bytes) ->
      let time = float (frames * samples.(f.layer - 1)) /. float f.rate in
      let bitrate = float bytes *. kbits /. time in
      VBR, frames, time, truncate (bitrate +. 0.5)
  in
  let format = {f with encoding; bitrate; frames; time; size; offset} in
  seek_in ic (in_channel_length ic - 128);
  let id3v1 = seq "id3v1 tag" errors (Id3v1.input_tag ic) in
  {format; id3v1; id3v2}, !errors

let analyze path =
  File.with_open_in `Bin path analyze_from
