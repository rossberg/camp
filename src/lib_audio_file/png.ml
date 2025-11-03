type format =
{
  width : int;
  height : int;
  depth : int;
  color_type : int;
  compression : int;
  filter : int;
  interlace : int;
}


exception Format

let check png n =
  if String.length png < n then raise Format

let read_byte png i =
  check png (i + 1);
  Char.code png.[i], i + 1

let read_int png i =
  check png (i + 4);
  Char.code png.[i] lsl 24 + Char.code png.[i + 1] lsl 16 +
  Char.code png.[i + 2] lsl 8 + Char.code png.[i + 3],
  i + 4

let read_chunk png i =
  check png (i + 4);
  let len, i = read_int png i in
  check png (i + len + 8);
  if String.sub png i 4 <> "IHDR" then raise Format;
  let width, i = read_int png (i + 4) in
  let height, i = read_int png i in
  let depth, i = read_byte png i in
  let color_type, i = read_byte png i in
  let compression, i = read_byte png i in
  let filter, i = read_byte png i in
  let interlace, i = read_byte png i in
  let _crc, _ = read_int png i in
  {width; height; depth; color_type; compression; filter; interlace}

let read_format png =
  check png 8;
  if String.sub png 0 8 <> "\x89PNG\x0d\x0a\x1a\x0a" then raise Format;
  read_chunk png 8
