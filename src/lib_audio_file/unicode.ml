(* Unicode Helpers *)

let rec is_ascii' s i =
  i = String.length s || s.[i] < '\x80' && is_ascii' s (i + 1)

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


(*
module UCol = Camomile.UCol.Make (Camomile.UTF8)
module UCase = Camomile.CaseMap.Make (Camomile.UTF8)
module UNorm = Camomile.UNF.Make (Camomile.UTF8)

let normalize_utf_8 s = UNorm.nfc s
let casefold_utf_8 s = UCase.casefolding s
let sort_key_utf_8 s = UCol.sort_key ~prec: `Primary s
let compare_utf_8 s1 s2 = UCol.compare ~prec: `Primary s1 s2
(*
let contains_utf_8 ~inner s =
  try ignore (UCol.search ~prec: `Primary s inner 0); true with Not_found -> false
*)
*)

(*
let normalize_utf_8 s = Uunf_string.normalize_utf_8 `NFC s
*)

let mapping = Confero_ducet.mapping

let sort_key_utf_8 s = (Confero.Sort_key.of_string ~mapping s :> string)
let compare_utf_8 s1 s2 = Confero.collate ~mapping s1 s2

(* Adopted from https://erratique.ch/software/uucp/doc/Uucp/Case/index.html#caselesseq *)
let casefold_utf_8 s =
  let buf = Buffer.create (String.length s * 3) in
  let to_nfd_and_utf_8 =
    let n = Uunf.create `NFD in
    let rec add v = match Uunf.add n v with
    | `Await | `End -> ()
    | `Uchar u -> Buffer.add_utf_8_uchar buf u; add `Await
    in
    add
  in
  let add =
    let n = Uunf.create `NFD in
    let rec add v = match Uunf.add n v with
    | `Await | `End -> ()
    | `Uchar u ->
        begin match Uucp.Case.Fold.fold u with
        | `Self -> to_nfd_and_utf_8 (`Uchar u)
        | `Uchars us -> List.iter (fun u -> to_nfd_and_utf_8 (`Uchar u)) us
        end;
        add `Await
    in
    add
  in
  let rec loop buf s i max =
    if i > max then (add `End; to_nfd_and_utf_8 `End; Buffer.contents buf) else
    let dec = String.get_utf_8_uchar s i in
    add (`Uchar (Uchar.utf_decode_uchar dec));
    loop buf s (i + Uchar.utf_decode_length dec) max
  in
  loop buf s 0 (String.length s - 1)


(* TODO: with Camomile, use contains_utf_8 (and remove UCase), once Camomile
 * bug https://github.com/ocaml-community/Camomile/issues/10 is fixed. *)
let rec string_contains_at' s i s' j =
  j = String.length s' ||
  s.[i + j] = s'.[j] && string_contains_at' s i s' (j + 1)

let string_contains_at s i s' =
  String.length s - i >= String.length s' &&
  string_contains_at' s i s' 0

let rec index_sub_from_opt s i s' =
  if s' = "" then Some i else
  match String.index_from_opt s i s'.[0] with
  | None -> None
  | Some j ->
    if j + String.length s' > String.length s then
      None
    else if string_contains_at s j s' then
      Some j
    else if j + String.length s' >= String.length s then
      None
    else
      index_sub_from_opt s (j + 1) s'

let contains_utf_8 ~inner s = index_sub_from_opt s 0 inner <> None

let contains_utf_8_caseless ~inner s =
  contains_utf_8 ~inner: (casefold_utf_8 inner) (casefold_utf_8 s)
