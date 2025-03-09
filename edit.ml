(* Edit state *)

type undo =
{
  undo_text : string;
  undo_scroll : int;
  undo_sel_range : (int * int) option;
}

type t =
{
  mutable text : string;
  mutable focus : bool;
  mutable scroll : int;
  mutable sel_range : (int * int) option;  (* primary and secondary pos *)
  undos : undo list ref;
  redos : undo list ref;
  undo_depth : int;
}


(* Constructor *)

let make undo_depth =
  {
    text = "";
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

let shift x i n = if x < i then x else max i (x + n)

let set' ed s i n =
  if s <> ed.text then
  (
    push_undo ed;
    ed.text <- s;
    match ed.sel_range with
    | Some (a, b) -> ed.sel_range <- Some (shift a i n, shift b i n)
    | None -> ()
  )

let set ed s =
  set' ed s 0 0;
  ed.sel_range <- Some (String.length s, String.length s)

let insert ed i s =
  set' ed String.(sub ed.text 0 i ^ s ^ sub ed.text i (length s - i))
    i (String.length s)

let remove ed i n =
  set' ed String.(sub ed.text 0 i ^ sub ed.text (i + n) (length ed.text - i - n))
    i (-n)

let clear ed =
  set' ed "" 0 0


let move_begin ed = ed.sel_range <- Some (0, 0)
let move_end ed = ed.sel_range <- Some (String.length ed.text, String.length ed.text)