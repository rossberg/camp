module Encode =
struct
  type buf = Buffer.t

  open Buffer

  let nat8 n buf = add_uint8 buf n
  let nat16 n buf = add_uint16_le buf n
  let nat32 n buf = add_int32_le buf (Int32.of_int n)
  let nat64 n buf = add_int64_le buf (Int64.of_int n)
  let int8 i buf = add_int8 buf i
  let int16 i buf = add_int16_le buf i
  let int32 i buf = add_int32_le buf (Int32.of_int i)
  let int64 i buf = add_int64_le buf (Int64.of_int i)

  let rec nat n buf =
    if n < 0x80 then nat8 n buf else
    (
      nat8 (n land 0x7f + 0x80) buf;
      nat (n lsr 7) buf
    )

  let flip = Fun.flip

  let unit () = ignore
  let bool b = nat8 (Bool.to_int b)
  let float q buf = add_int64_le buf (Int64.bits_of_float q)
  let string s buf = nat (String.length s) buf; add_string buf s
  let enum nxs x = nat (fst (List.find (fun (_, x') -> x = x') nxs))
  let option f x buf =
    bool (Option.is_some x) buf; Option.(iter (flip f buf) x)
  let array f xs buf = nat (Array.length xs) buf; Array.iter (flip f buf) xs
  let list f xs = array f (Array.of_list xs)
  let tuple f x buf = List.iter (fun g -> g buf) (f x)
  let pair f1 f2 (x1, x2) buf = f1 x1 buf; f2 x2 buf
  let triple f1 f2 f3 (x1, x2, x3) buf = f1 x1 buf; f2 x2 buf; f3 x3 buf
  let map f = list (pair string f)
  let record f x buf =
    let gs = f x in nat (List.length gs) buf; List.iter (fun g -> g buf) gs
  let variant f x buf = let n, g = f x in nat n buf; g buf
end

module Decode =
struct
  type buf = string * int ref

  exception Decode_error of int

  open String

  let src (s, _i) = s
  let pos (_s, i) = !i
  let save (s, i) = (s, ref !i)
  let error (_s, i) = raise (Decode_error !i)
  let eat (s, i) n =
    let j = !i in
    if j + n > length s then error (s, i) else
    (i := j + n; j)

  let as_nat g f s j =
    let i = g (f s j) in
    if i >= 0 then i else raise (Decode_error j)

  let as_clamp x1 x2 f b =
    let j = pos b in
    let x = f b in
    if x1 <= x && x <= x2 then x else raise (Decode_error j)

  let nat8 buf = get_uint8 (src buf) (eat buf 1)
  let nat16 buf = get_uint16_le (src buf) (eat buf 2)
  let nat32 buf = as_nat Int32.to_int get_int32_le (src buf) (eat buf 4)
  let nat64 buf = as_nat Int64.to_int get_int64_le (src buf) (eat buf 8)
  let int8 buf = get_int8 (src buf) (eat buf 1)
  let int16 buf = get_int16_le (src buf) (eat buf 2)
  let int32 buf = Int32.to_int (get_int32_le (src buf) (eat buf 4))
  let int64 buf = Int64.to_int (get_int64_le (src buf) (eat buf 8))

  let rec nat buf =
    let b = nat8 buf in
    if b < 0x80 then b else (b - 0x80 + 0x80 * nat buf)

  let unit _buf = ()
  let bool buf = (as_clamp 0 1 nat8 buf = 1)
  let float buf = Int64.float_of_bits (get_int64_le (src buf) (eat buf 8))
  let num n1 n2 buf = as_clamp n1 n2 nat buf
  let num8 n1 n2 buf = as_clamp n1 n2 nat8 buf
  let num16 n1 n2 buf = as_clamp n1 n2 nat16 buf
  let num32 n1 n2 buf = as_clamp n1 n2 nat32 buf
  let num64 n1 n2 buf = as_clamp n1 n2 nat64 buf
  let interval q1 q2 buf = as_clamp q1 q2 float buf
  let string buf = let n = nat buf in sub (src buf) (eat buf n) n
  let enum nxs buf =
    let buf' = save buf in
    match List.assoc_opt (nat buf) nxs with
    | Some x -> x
    | None -> error buf'
  let option f buf = if bool buf then Some (f buf) else None
  let array f buf = Array.init (nat buf) (fun _ -> f buf)
  let list f buf = Array.to_list (array f buf)
  let tuple f = f
  let pair f1 f2 buf = let x1 = f1 buf in let x2 = f2 buf in (x1, x2)
  let triple f1 f2 f3 buf =
    let x1 = f1 buf in let x2 = f2 buf in let x3 = f3 buf in (x1, x2, x3)
  let map f = list (pair string f)
  let record f buf = f (nat buf) buf
  let variant f buf = f (nat buf) buf
end


let encode f x =
  let buf = Buffer.create 0x1_0000_0000 in
  f x buf;
  Buffer.contents buf

let decode f s =
  let st = (s, ref 0) in
  let x = f st in
  if !(snd st) <> String.length s then
    raise (Decode.Decode_error (String.length s));
  x
