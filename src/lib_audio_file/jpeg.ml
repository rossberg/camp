type mode = Baseline | Sequential | Progressive | Lossless | Hierarchical
type coding = Huffman | Arithmetic
type differentiality = NonDifferential | Differential

type format =
{
  mode : mode;
  coding : coding;
  differentiality : differentiality;
  width : int;
  height : int;
  precision : int;
}


exception Format

let check jpg n =
  if String.length jpg < n then raise Format

let read_byte jpg i =
  check jpg (i + 1);
  Char.code jpg.[i], i + 1

let read_int jpg i =
  check jpg (i + 2);
  256 * Char.code jpg.[i] + Char.code jpg.[i + 1], i + 2

let skip_frame jpg i =
  let n, _ = read_int jpg i in
  i + n

let read_sof_frame jpg i mode differentiality coding =
  let _len, i = read_int jpg i in
  let precision, i = read_byte jpg i in
  let height, i = read_int jpg i in
  let width, _ = read_int jpg i in
  {mode; coding; differentiality; precision; height; width}

let rec read_frame jpg i =
  check jpg (i + 2);
  if jpg.[i] <> '\xff' then raise Format;
  let j = i + 2 in
  match jpg.[i + 1] with
  | '\xc0' -> read_sof_frame jpg j Baseline NonDifferential Huffman
  | '\xc1' -> read_sof_frame jpg j Sequential NonDifferential Huffman
  | '\xc2' -> read_sof_frame jpg j Progressive NonDifferential Huffman
  | '\xc3' -> read_sof_frame jpg j Lossless NonDifferential Huffman
  | '\xc4' -> read_frame jpg (skip_frame jpg j)  (* DHT, Define Huffman table *)
  | '\xc5' -> read_sof_frame jpg j Sequential Differential Huffman
  | '\xc6' -> read_sof_frame jpg j Progressive Differential Huffman
  | '\xc7' -> read_sof_frame jpg j Lossless Differential Huffman
  | '\xc8' -> raise Format  (* Reserved *)
  | '\xc9' -> read_sof_frame jpg j Sequential NonDifferential Arithmetic
  | '\xca' -> read_sof_frame jpg j Progressive NonDifferential Arithmetic
  | '\xcb' -> read_sof_frame jpg j Lossless NonDifferential Arithmetic
  | '\xcc' -> read_frame jpg (skip_frame jpg j)  (* DAC, Define Arithmetic Coding *)
  | '\xcd' -> read_sof_frame jpg j Sequential Differential Arithmetic
  | '\xce' -> read_sof_frame jpg j Progressive Differential Arithmetic
  | '\xcf' -> read_sof_frame jpg j Lossless Differential Arithmetic
  | '\xd0'..'\xd7' -> raise Format  (* RST0..RST7 *)
  | '\xd8' -> read_frame jpg j  (* SOI, Start of Image *)
  | '\xd9' -> raise Format  (* EOI, End of Image *)
  | '\xda' -> raise Format  (* SOS, Start of Scan *)
  | '\xdb' -> read_frame jpg (skip_frame jpg j)  (* DQT, Define Quantization Table *)
  | '\xdc' -> read_frame jpg (skip_frame jpg j)  (* DNL, Define Number of Lines *)
  | '\xdd' -> read_frame jpg (skip_frame jpg j)  (* DRI, Define Restart Interval *)
  | '\xde' -> read_sof_frame jpg j Hierarchical NonDifferential Huffman (* DHP, Define Hierarchical Progression *)
  | '\xdf' -> read_frame jpg (skip_frame jpg j)  (* EXP, Expand Reference Components *)
  | '\xe0'..'\xef' -> read_frame jpg (skip_frame jpg j)  (* APP0..APP15 *)
  | '\xf0'..'\xfd' -> raise Format  (* Reserved *)
  | '\xfe' -> read_frame jpg (skip_frame jpg j) (* Comment *)
  | _ -> raise Format

let read_format jpg =
  check jpg 2;
  if jpg.[0] <> '\xff' || jpg.[1] <> '\xd8' then raise Format;
  read_frame jpg 0
