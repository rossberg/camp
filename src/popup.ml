(* Pop-up Menu *)

type menu =
{
  mutable hscroll : int;
  mutable vscroll : int;
  items : Ui.menu_entry iarray;
  op : int -> unit;
}

type cover =
  | Current
  | Track of Data.track
  | Album of Data.album

type custom =
{
  name : Edit.t;
  expr : Edit.t;
  valid : string -> bool;
  ok : string -> string -> unit;
}

type t =
{
  mutable kind : [`Menu of menu | `Cover of cover | `Custom of custom] option;
}


(* Constructors *)

let make () =
  {
    kind = None;
  }


(* Accessors *)

let clear pop =
  pop.kind <- None

let set_menu pop items op =
  pop.kind <- Some (`Menu {hscroll = 0; vscroll = 0; items; op})

let set_cover pop cover =
  pop.kind <- Some (`Cover cover)

let set_custom pop l s valid ok =
  let name = Edit.make_with 100 l in
  let expr = Edit.make_with 100 s in
  Edit.focus (if l = "" then name else expr);
  Edit.move_end name;
  Edit.move_end expr;
  pop.kind <- Some (`Custom {name; expr; valid; ok})


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]

let ok pop =
  List.concat (Option.to_list (Option.map (function
    | `Menu _menu -> []
    | `Cover _cover -> []
    | `Custom _custom -> []
  ) pop.kind)) @
  []


(* Focus *)

let foci pop =
  match pop.kind with
  | None  | Some (`Menu _) | Some (`Cover _) -> []
  | Some (`Custom custom) -> [custom.name; custom.expr]

let defocus pop =
  List.iter Edit.defocus (foci pop)


(* Persistence *)

let print_state pop =
  let open Text.Print in
  record (fun _pop -> []) pop

let print_menu menu =
  let open Text.Print in
  record (fun menu -> [
    "hscroll", nat menu.hscroll;
    "vscroll", nat menu.vscroll;
    "items", nat (Iarray.length menu.items);
  ]) menu

let print_cover cover =
  let open Text.Print in
  variant (function
    | Current -> "current", unit ()
    | Track track -> "track", string track.path
    | Album album -> "album", string album.path
  ) cover

let print_custom cus =
  let open Text.Print in
  record (fun menu -> [
    "name", string cus.name.text;
    "expr", string cus.expr.text;
  ]) cus

let print_intern pop =
  let open Text.Print in
  print_state pop @@@
  record (fun pop -> [
    "kind", option (variant (function
      | `Menu menu -> "menu", print_menu menu
      | `Cover cover -> "cover", print_cover cover
      | `Custom custom -> "custom", print_custom custom
    )) pop.kind
  ]) pop

let parse_state _pop =
  let open Text.Parse in
  record (fun _ -> ())
