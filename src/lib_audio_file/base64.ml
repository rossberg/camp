exception Invalid

let is_space = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let skip_space s i =
  while !i < String.length s && is_space s.[!i] do incr i done

let rec decode_char s i =
  if !i = String.length s then raise Invalid;
  let c = s.[!i] in
  incr i;
  match c with
  | 'A'..'Z' -> Char.(code c - code 'A')
  | 'a'..'z' -> Char.(code c - code 'a' + 0b011010)
  | '0'..'9' -> Char.(code c - code '0' + 0b110100)
  | '+' -> 0b111110
  | '/' -> 0b111111
  | '=' -> -1
  | _ when is_space c -> decode_char s i
  | _ -> raise Invalid

let decode s =
  let len = String.length s in
  let buf = Buffer.create (3 * len / 4) in
  let i = ref 0 in
  let padding = ref false in
  while skip_space s i; !i < len do
    if !padding then raise Invalid;
    let b1 = decode_char s i in
    let b2 = decode_char s i in
    let b3 = decode_char s i in
    let b4 = decode_char s i in
    Buffer.add_uint8 buf (b1 lsl 2 + b2 lsr 4);
    if b1 = -1 || b2 = -1 then raise Invalid;
    if b3 = -1 then
      (if b2 land 0xf <> 0 || b4 <> -1 then raise Invalid)
    else
    (
      Buffer.add_uint8 buf ((b2 land 0xf) lsl 4 + b3 lsr 2);
      if b4 = -1 then
        (if b3 land 0x3 <> 0 then raise Invalid)
      else
        Buffer.add_uint8 buf ((b3 land 0x3) lsl 6 + b4)
    );
    padding := b4 = -1;
  done;
  Buffer.contents buf


let encode_char b =
  if b < 26 then b + Char.code 'A' else
  if b < 52 then b - 26 + Char.code 'a' else
  if b < 62 then b - 52 + Char.code '0' else
  if b = 62 then Char.code '+' else
  if b = 63 then Char.code '/' else
  assert false

let encode s =
  let len = String.length s in
  let buf = Bytes.create ((len + 2)/3*4) in
  let i = ref 0 in
  let j = ref 0 in
  while !i < len do
    let c1 = Char.code s.[!i] in
    let c2 = if !i + 1 = len then 0 else Char.code s.[!i + 1] in
    let c3 = if !i + 2 >= len then 0 else Char.code s.[!i + 2] in
    let b1 = encode_char (c1 lsr 2) in
    let b2 = encode_char ((c1 land 0x3) lsl 4 + c2 lsr 4) in
    let b3 = encode_char ((c2 land 0xf) lsl 2 + c3 lsr 6) in
    let b4 = encode_char (c3 land 0x3f) in
    Bytes.set_int8 buf !j b1;
    Bytes.set_int8 buf (!j + 1) b2;
    Bytes.set_int8 buf (!j + 2) (if !i + 1 = len then Char.code '=' else b3);
    Bytes.set_int8 buf (!j + 3) (if !i + 2 >= len then Char.code '=' else b4);
    i := !i + 3;
    j := !j + 4;
  done;
  Bytes.to_string buf
