(* Unicode Helpers *)

let rec is_ascii' s i =
  i = String.length s || s.[i] < '\x80' || is_ascii' s (i + 1)

let is_ascii s = is_ascii' s 0


let is_con c = Char.code c land 0xc0 = 0x80

let rec is_utf8' s i =
  let len = String.length s in
  i = len ||
  if s.[i] < '\x80' then
    is_utf8' s (i + 1)
  else if s.[i] < '\xc0' then
    false
  else if s.[i] < '\xe0' then
    len > i + 1 && is_con s.[i + 1] &&
    is_utf8' s (i + 2)
  else if s.[i] < '\xf0' then
    len > i + 2 && is_con s.[i + 1] && is_con s.[i + 2] &&
    is_utf8' s (i + 3)
  else if s.[i] < '\xf8' then
    len > i + 3 && is_con s.[i + 1] && is_con s.[i + 2] && is_con s.[i + 3] &&
    is_utf8' s (i + 4)
  else
    false

let is_utf8 s = is_utf8' s 0


let rec utf8_length' s i n =
  if i = String.length s then n else
  utf8_length' s (i + 1) (if is_con s.[i] then n else n + 1)

let utf8_length s = utf8_length' s 0 0


let has_utf16_bom s =
  String.length s >= 2 &&
  (s.[0] = '\xff' && s.[1] = '\xfe' || s.[0] = '\xfe' && s.[1] = '\xff')

let little_endian b1 b2 = b1 + 256 * b2
let big_endian b1 b2 = 256 * b1 + b2

(* fairly dumb, doesn't handle supplementary planes *)
let transcode_utf16 s =
  let endian =
    if has_utf16_bom s && s.[0] = '\xfe' then big_endian else little_endian in
  let len = String.length s in
  let buf = Buffer.create len in
  let i = ref 0 in
  while !i < len - 1 do
    let c = endian (Char.code s.[!i]) (Char.code s.[!i + 1]) in
    if c < 0x80 then
      Buffer.add_char buf (Char.chr c)
    else if c < 0x800 then
    (
      Buffer.add_char buf (Char.chr (0xc0 lor (c lsr 6)));
      Buffer.add_char buf (Char.chr (0x80 lor (c land 0x3f)))
    )
    else if c <> 0xfeff then
    (
      Buffer.add_char buf (Char.chr (0xe0 lor (c lsr 12)));
      Buffer.add_char buf (Char.chr (0x80 lor ((c lsr 6) land 0x3f)));
      Buffer.add_char buf (Char.chr (0x80 lor (c land 0x3f)))
    );
    i := !i + 2
  done;
  Buffer.contents buf


let asciify s =
  let len = String.length s in
  let buf = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    let c = Char.code s.[!i] in
    if 0x20 < c && c < 0x80 then
      Buffer.add_char buf s.[!i]
    else
      Buffer.add_string buf ("%" ^ Printf.sprintf "%02x" c);
    incr i
  done;
  Buffer.contents buf
