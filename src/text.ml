(* Serialisation *)

type flat = bool
type t =
  | Int of int
  | Float of float
  | Text of string
  | Brack of t list * flat
  | Brace of (t * t) list * flat


(* Helpers *)

let is_digit = function '0'..'9' | '-' | '.' -> true | _ -> false
let is_letter = function 'A'..'Z' | 'a'..'z' | '_' -> true | _ -> false
let is_alphanum c = is_digit c || is_letter c


(* Printing *)

let hex d = Char.(chr (d + (if d < 10 then code '0' else code 'A' - 10)))
let eol d = "\n" ^ String.make d ' '
let sep d flat l ss r =
  if flat || ss = [] then
    l ^ String.concat ", " ss ^ r
  else
    l ^ eol d ^ String.concat ("," ^ eol d) ss ^ eol (max 0 (d - 1)) ^ r

let flat = function
  | Int _ | Float _ | Text _ -> true
  | Brack (_, b) | Brace (_, b) -> b

let rec print' d = function
  | Int i -> Int.to_string i
  | Float q ->
    let s = Printf.sprintf "%.3f" q in
    let i = ref (String.length s - 1) in
    while s.[!i] = '0' do decr i done;
    if s.[!i] = '.' then incr i;
    String.sub s 0 (!i + 1)
  | Text s when s <> "" && is_letter s.[0] && String.for_all is_alphanum s -> s
  | Text s ->
    let b = Buffer.create (String.length s + 2) in
    String.iter (function
      | '\\' | '"' as c -> Buffer.add_char b '\\'; Buffer.add_char b c
      | c when c < '\x20' ->
        Buffer.add_char b '\\';
        Buffer.add_char b (hex (Char.code c / 16));
        Buffer.add_char b (hex (Char.code c mod 16));
      | c -> Buffer.add_char b c
    ) s;
    "\"" ^ Buffer.contents b ^ "\""
  | Brack (xs, b) -> sep d b "[" (List.map (print' (d + 1)) xs) "]"
  | Brace (xys, b) -> sep d b "{" (List.map (print_eq (d + 1)) xys) "}"

and print_eq d (x, y) = print' 0 x ^ " = " ^ print' d y

let print = print' 1


(* Parsing *)

exception Syntax_error of int
exception Type_error

module Token =
struct
  type token =
    | Int of int
    | Float of float
    | Text of string
    | Eq
    | Comma
    | LBrack | RBrack
    | LBrace | RBrace
    | End

  let scan_hex s i =
    if !i = String.length s then raise (Syntax_error !i);
    match s.[!i] with
    | '0'..'9' | 'A'..'F' as c ->
      incr i; Char.code c - Char.code (if is_digit c then '0' else 'A')
    | _ -> raise (Syntax_error !i)

  let scan_word s i =
    let j = !i in
    while !i < String.length s && is_alphanum s.[!i] do
      incr i
    done;
    String.sub s j (!i - j)

  let scan_text s i =
    let b = Buffer.create 256 in
    let j = !i in
    while !i < String.length s && s.[!i] <> '"' do
      let c = s.[!i] in
      incr i;
      if c <> '\\' then Buffer.add_char b c else
      (
        match s.[!i] with
        | '\\' | '"' as c' -> incr i; Buffer.add_char b c'
        | _ ->
          let d1 = scan_hex s i in
          let d2 = scan_hex s i in
          Buffer.add_char b (Char.chr (16 * d1 + d2))
      )
    done;
    if !i = String.length s then raise (Syntax_error j);
    incr i; Buffer.contents b

  let rec scan s i =
    if !i = String.length s then End else
    match s.[!i] with
    | ' ' | '\t' | '\r' | '\n' -> incr i; scan s i
    | '=' -> incr i; Eq
    | ',' -> incr i; Comma
    | '[' -> incr i; LBrack
    | ']' -> incr i; RBrack
    | '{' -> incr i; LBrace
    | '}' -> incr i; RBrace
    | '\"' -> incr i; Text (scan_text s i)
    | c when is_letter c -> Text (scan_word s i)
    | c when is_digit c ->
      let j = !i in
      let s = scan_word s i in
      if not (String.for_all is_digit s) then raise (Syntax_error j);
      (match int_of_string_opt s with
      | Some i -> Int i
      | None -> Float (float_of_string s)
      )
    | _ -> raise (Syntax_error !i)
end

let stream s = (s, ref 0)
let next (s, i) = Token.scan s i
let save (_s, i) = !i
let reset (_s, i) i' = i := i'

let expect tok st =
  let j = save st in
  if next st <> tok then raise (Syntax_error j)

let rec parse_sep parse_x term st =
  let st' = save st in
  if next st = term then [] else
  (
    reset st st';
    let x = parse_x st in
    let st' = save st in
    if next st = Token.Comma then
      x :: parse_sep parse_x term st
    else
      (reset st st'; expect term st; [x])
  )

let rec parse_val st =
  let j = save st in
  match next st with
  | Token.Int i -> Int i
  | Token.Float q -> Float q
  | Token.Text s -> Text s
  | Token.LBrack -> Brack (parse_sep parse_val Token.RBrack st, true)
  | Token.LBrace -> Brace (parse_sep parse_eq Token.RBrace st, true)
  | _ -> raise (Syntax_error j)

and parse_eq st =
  let u1 = parse_val st in
  expect Eq st;
  let u2 = parse_val st in
  u1, u2

let parse s =
  let st = stream s in
  let u = parse_val st in
  expect End st;
  u


(* Typed Conversion *)

module Print =
struct
  type nonrec t = t

  let is_short us = List.length us <= 3 && List.for_all flat us

  let unit () = Text "null"
  let bool b = Text (Bool.to_string b)
  let int i = Int i
  let nat n = assert (n >= 0); int n
  let num i1 i2 i = assert (i1 <= i && i <= i2); int i
  let float q = Float q
  let interval q1 q2 q = assert (q1 <= q && q <= q2); float q
  let string s = Text s
  let enum sxs x = string (fst (List.find (fun (_, x') -> x = x') sxs))
  let option f x = Option.(value (map f x) ~default: (string "null"))
  let list f xs = let us = List.map f xs in Brack (us, is_short us)
  let array f xs = list f (Array.to_list xs)
  let iarray f xs = list f (Iarray.to_list xs)
  let tuple f x = list Fun.id (f x)
  let pair f1 f2 = tuple (fun (x1, x2) -> [f1 x1; f2 x2])
  let triple f1 f2 f3 = tuple (fun (x1, x2, x3) -> [f1 x1; f2 x2; f3 x3])
  let map f xys = let lus = List.map (fun (x, y) -> string x, f y) xys in Brace (lus, is_short (List.map snd lus))
  let record f x = map Fun.id (f x)
  let variant f x = let l, t = f x in Brace ([string l, t], flat t)
  let transform f g x = f (g x)

  let (@@@) u1 u2 =
    match u1, u2 with
    | Brack (us1, b1), Brack (us2, b2) -> Brack (us1 @ us2, b1 && b2)
    | Brace (lus1, b1), Brace (lus2, b2) -> Brace (lus1 @ lus2, b1 && b2)
    | _, _ -> raise (Invalid_argument "(@@)")
end

module Parse =
struct
  type nonrec t = t

  let unit = function
    | Text "null" -> ()
    | _ -> raise Type_error

  let bool = function
    | Text "true" -> true
    | Text "false" -> false
    | _ -> raise Type_error

  let int = function
    | Int i -> i
    | _ -> raise Type_error

  let nat u =
    match int u with
    | n when n >= 0 -> n
    | _ -> raise Type_error

  let num i1 i2 u =
    max i1 (min i2 (int u))

  let float = function
    | Float q -> q
    | Int i -> float_of_int i
    | _ -> raise Type_error

  let interval q1 q2 u =
    max q1 (min q2 (float u))

  let string = function
    | Text s -> s
    | _ -> raise Type_error

  let enum sxs u =
    match List.assoc_opt (string u) sxs with
    | Some x -> x
    | None -> raise Type_error

  let option f = function
    | Text "null" -> None
    | x -> Some (f x)

  let list f = function
    | Brack (us, _) -> List.map f us
    | _ -> raise Type_error

  let array f u = Array.of_list (list f u)

  let iarray f u = Iarray.of_list (list f u)

  let tuple f u = f (list Fun.id u)

  let pair f1 f2 =
    tuple (function
      | [u1; u2] -> (f1 u1, f2 u2)
      | _ -> raise Type_error
    )

  let triple f1 f2 f3 =
    tuple (function
      | [u1; u2; u3] -> (f1 u1, f2 u2, f3 u3)
      | _ -> raise Type_error
    )

  let map f = function
    | Brace (lus, _) -> List.map (fun (l, u) -> string l, f u) lus
    | _ -> raise Type_error

  let record f u = f (map Fun.id u)

  let ($?) r l = List.assoc_opt l r

  let ($) r l =
    match r $? l with
    | Some u -> u
    | _ -> raise Type_error

  let variant f = function
    | Brace ([(l, u)], _) -> f (string l, u)
    | _ -> raise Type_error

  let (|||) f g u =
    try f u with Type_error -> g u

  let (>->) f g u = g (f u)

  let default x f uo =
    match uo with
    | Some u -> f u
    | _ -> x

  let apply uo f g =
    match uo with
    | Some u -> g (f u)
    | _ -> ()
end
