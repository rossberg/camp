(* Edit state *)

type undo =
{
  undo_text : string;
  undo_scroll : int;
  undo_sel_range : (int * int * int) option;
}

type t =
{
  mutable text : string;
  mutable prev : string list;
  mutable next : string list;
  mutable focus : bool;
  mutable scroll : int;
  mutable sel_range : (int * int * int) option;  (* primary l/r and secondary pos *)
  undos : undo list ref;
  redos : undo list ref;
  undo_depth : int;
}


(* Constructor *)

let make undo_depth =
  {
    text = "";
    prev = [];
    next = [];
    focus = false;
    scroll = 0;
    sel_range = None;
    undos = ref [];
    redos = ref [];
    undo_depth;
  }


(* Accessors *)

let focus ed = ed.focus <- true
let defocus ed = ed.focus <- false

let scroll ed i = ed.scroll <- i
let select ed sel = ed.sel_range <- sel


(* History *)

let add_history ed =
  match List.rev ed.next with
  | [] -> ()
  | s::next ->
    let next' = (if s = "" then next else s::next) in
    ed.prev <- next' @ (if ed.text = "" then [] else [ed.text]) @ ed.prev;
    ed.next <- [];
    if List.length ed.prev >= ed.undo_depth then
      ed.prev <- List.take ed.undo_depth ed.prev


(* Undo *)

let make_undo ed =
  { undo_text = ed.text;
    undo_scroll = ed.scroll;
    undo_sel_range = ed.sel_range;
  }

let push_undo ed =
  if ed.undo_depth > 0 then
  (
    if List.length !(ed.undos) >= ed.undo_depth then
      ed.undos := List.take ed.undo_depth !(ed.undos);
    ed.undos := make_undo ed :: !(ed.undos);
    ed.redos := []
  )

let pop_unredo ed undos redos =
  match !undos with
  | [] -> ()
  | undo :: undos' ->
    redos := make_undo ed :: !redos;
    undos := undos';
    add_history ed;
    ed.text <- undo.undo_text;
    ed.scroll <- undo.undo_scroll;
    ed.sel_range <- undo.undo_sel_range

let pop_undo ed = pop_unredo ed ed.undos ed.redos
let pop_redo ed = pop_unredo ed ed.redos ed.undos

let drop_undo ed = ed.undos := List.tl !(ed.undos)
let drop_redo ed = ed.redos := List.tl !(ed.redos)

let clear_undo ed =
  ed.undos := [];
  ed.redos := []


(* Editing *)

let move_begin ed =
  ed.sel_range <- Some (0, 0, 0);
  ed.scroll <- 0

let move_end ed =
  let i = String.length ed.text in
  ed.sel_range <- Some (i, i, i);
  ed.scroll <- 0  (* depend on view adjustment to scroll rightmost *)

let shift x i n = if x < i then x else max i (x + n)

let update' ed s i n =
  if s <> ed.text then
  (
    push_undo ed;
    ed.text <- s;
    Option.iter (fun (a, b, c) ->
      ed.sel_range <- Some (shift a i n, shift b i n, shift c i n)
    ) ed.sel_range
  )

let update ed s =
  add_history ed;
  update' ed s 0 0;
  move_end ed

let insert ed i s =
  add_history ed;
  update' ed String.(sub ed.text 0 i ^ s ^ sub ed.text i (length s - i))
    i (String.length s)

let remove ed i n =
  add_history ed;
  update' ed
    String.(sub ed.text 0 i ^ sub ed.text (i + n) (length ed.text - i - n))
    i (-n)

let set ed s =
  if ed.next = [] then ed.next <- [""];
  update ed s

let clear ed =
  if ed.next <> [""] then add_history ed;
  set ed ""


(* History *)

let prev_history ed =
  match ed.prev with
  | [] -> ()
  | s::prev' ->
    ed.prev <- prev';
    ed.next <- ed.text::ed.next;
    update' ed s 0 0;
    move_end ed

let next_history ed =
  let s =
    match ed.next with
    | [] -> ""
    | s::next' -> ed.next <- next'; s
  in
  ed.prev <- (if ed.text = "" then ed.prev else ed.text::ed.prev);
  update' ed s 0 0;
  move_end ed

let clear_history ed =
  clear ed;
  ed.prev <- [];
  ed.next <- []

let history ed =
  let next' =
    match List.rev ed.next with
    | ""::next' -> next'
    | next -> next
  in next' @ (if ed.text = "" then [] else [ed.text]) @ ed.prev
