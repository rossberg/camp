type resolution = PCM | Float

type format =
{
  resolution : resolution;
  rate : int; (* 1 to 655350 Hz *)
  channels : int; (* 1 to 8 *)
  depth : int; (* 4 to 32 *)
  size : int;
}


let input_4cc s ic =
  let buf = really_input_string ic 4 in
  String.length buf = 4 &&
  buf.[0] = s.[0] && buf.[1] = s.[1] && buf.[2] = s.[2] && buf.[3] = s.[3]

let input_int2 ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  b1 lor (b2 lsl 8)

let input_int4 ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  let b4 = input_byte ic in
  b1 lor (b2 lsl 8) lor (b3 lsl 16) lor (b4 lsl 24)

let read_wave_header ic =
  seek_in ic 0;
  if not (input_4cc "RIFF" ic) then 0 else
  let size = input_int4 ic in
  if not (input_4cc "WAVE" ic) then 0 else
  size - 4

let rec read_data ic =
  if input_4cc "data" ic then
    input_int4 ic
  else
    let size = input_int4 ic in
    seek_in ic (pos_in ic + size);
    read_data ic

let read_format_from ic =
  let pos = pos_in ic in
  let header_size = read_wave_header ic in
  let errors = ref [] in
  let check b s = if not b then errors := (pos, s) :: !errors in
  check (header_size > 32) "invalid master chunk or size";
  check (input_4cc "fmt " ic) "invalid data format chunk";
  let format_size = input_int4 ic in
  check (format_size = 16) "invalid data format chunk size";
  let res = input_int2 ic in
  check (res = 1 || res = 3) "unknown data type";
  let resolution = if res = 1 then PCM else Float in
  let channels = input_int2 ic in
  let rate = input_int4 ic in
  let byterate = input_int4 ic in
  let byteblockrate = input_int2 ic in
  let depth = input_int2 ic in
  check (byterate = rate * byteblockrate) "inconsistent byte rate";
  check (byteblockrate = channels * depth / 8) "inconsistent depth";
  let size = read_data ic in
  { resolution; rate; channels; depth; size }, List.rev !errors

let read_format path =
  fst (In_channel.with_open_bin path read_format_from)
