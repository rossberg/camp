(* Table state *)

module IntSet = Set.Make(Int)
module KeySet = Set.Make(String)

type 'a undo =
{
  undo_entries : 'a array;
  undo_pos : int option;
  undo_vscroll : int;
  undo_sel_range : (int * int) option;
  undo_selected : IntSet.t;
}

type 'a t =
{
  mutable entries : 'a array;
  mutable pos : int option;                (* current position in table *)
  mutable focus : bool;
  mutable vscroll : int;                   (* in number of rows *)
  mutable hscroll : int;                   (* in pixels *)
  mutable sel_range : (int * int) option;  (* primary and secondary pos *)
  mutable selected : IntSet.t;
  mutable undos : 'a undo list ref;
  mutable redos : 'a undo list ref;
  mutable undo_depth : int;
}


(* Constructor *)

let make () =
  {
    entries = [||];
    pos = None;
    focus = false;
    vscroll = 0;
    hscroll = 0;
    sel_range = None;
    selected = IntSet.empty;
    undos = ref [];
    redos = ref [];
    undo_depth = 100;
  }


(* Accessors *)

let length tab = Array.length tab.entries
let current_opt tab = Option.map (fun i -> tab.entries.(i)) tab.pos
let current tab = Option.get (current_opt tab)

let adjust_pos tab =
  let len = Array.length tab.entries in
  if tab.pos = None && len > 0 then
    tab.pos <- Some 0
  else if tab.pos <> None && len = 0 then
    tab.pos <- None

let adjust_scroll tab pos fit =
  let i = Option.value pos ~default: 0 in
  if i < tab.vscroll || i >= tab.vscroll + fit then
    tab.vscroll <- max 0 (min (Array.length tab.entries - fit)
      (i - (fit - 2)/2))


(* Validation *)

type error = string

let check name msg b = if b then [] else [name ^ " " ^ msg]

let opt_forall f o = List.for_all f (Option.to_list o)

let ok name tab =
  let len = Array.length tab.entries in
  check name "position in range" (
    tab.pos = None && len = 0 ||
    tab.pos <> None && Option.get tab.pos >= 0 && Option.get tab.pos < len
  ) @
  check name "vertical scroll position in range"
    (tab.vscroll = 0 || tab.vscroll > 0 && tab.vscroll < len) @
  check name "horizontal scroll position in range"
    (tab.hscroll >= 0) @
  check name "selections in range"
    (IntSet.max_elt_opt tab.selected <= Some (len - 1)) @
  check name "selection range when selection" (
    tab.sel_range <> None || IntSet.cardinal tab.selected = 0
  ) @
  check name "primary selection position in range" (
    opt_forall (fun (pos1, _) -> pos1 >= 0 && pos1 < len) tab.sel_range
  ) @
  check name "secondary selection position in range" (
    opt_forall (fun (_, pos2) -> pos2 >= 0 && pos2 < len) tab.sel_range
  ) @
  []


(* Selection *)

let has_selection tab = tab.sel_range <> None
let num_selected tab = IntSet.cardinal tab.selected
let first_selected tab = IntSet.min_elt_opt tab.selected
let last_selected tab = IntSet.max_elt_opt tab.selected
let is_selected tab i = IntSet.mem i tab.selected

let selected tab =
  let d = ref 0 in
  Array.init (num_selected tab) (fun i ->
    while not (is_selected tab (i + !d)) do incr d done;
    tab.entries.(i + !d)
  )

let max_sel_range tab =
  if num_selected tab = 0 then None else
  Some (Option.get (first_selected tab), Option.get (last_selected tab))

let select_all tab =
  let len = Array.length tab.entries in
  for i = 0 to len - 1 do
    tab.selected <- IntSet.add i tab.selected
  done;
  tab.sel_range <- max_sel_range tab

let deselect_all tab =
  tab.selected <- IntSet.empty;
  tab.sel_range <- None

let select_invert tab =
  let selected = tab.selected in
  deselect_all tab;
  for i = 0 to Array.length tab.entries - 1 do
    if not (IntSet.mem i selected) then
      tab.selected <- IntSet.add i tab.selected
  done;
  tab.sel_range <- max_sel_range tab

let select tab i0 j0 =
  let i, j = min i0 j0, max i0 j0 in
  for k = i to j do
    if not (IntSet.mem k tab.selected) then
      tab.selected <- IntSet.add k tab.selected
  done;
  tab.sel_range <- Some (i0, j0)

let deselect tab i0 j0 =
  let i, j = min i0 j0, max i0 j0 in
  for k = i to j do
    if IntSet.mem k tab.selected then
      tab.selected <- IntSet.remove k tab.selected
  done;
  tab.sel_range <- Some (i0, j0)


let save_selection tab =
  let selection = selected tab in
  deselect_all tab;
  selection

let restore_selection tab selection key =
  let set =
    Array.fold_right (fun x -> KeySet.add (key x)) selection KeySet.empty in
  Array.iteri (fun i x -> if KeySet.mem (key x) set then select tab i i)
    tab.entries;
  adjust_scroll tab (first_selected tab) 4


(* Undo *)

let make_undo tab =
  { undo_entries = Array.map Fun.id tab.entries;
    undo_pos = tab.pos;
    undo_vscroll = tab.vscroll;
    undo_sel_range = tab.sel_range;
    undo_selected = tab.selected;
  }

let take n xs = List.filteri (fun i _ -> i < n - 1) xs

let push_undo tab =
  if tab.undo_depth > 0 then
  (
    if List.length !(tab.undos) >= tab.undo_depth then
      tab.undos := take tab.undo_depth !(tab.undos);
    tab.undos := make_undo tab :: !(tab.undos);
    tab.redos := []
  )

let pop_unredo tab undos redos =
  match !undos with
  | [] -> ()
  | undo :: undos' ->
    redos := make_undo tab :: !redos;
    undos := undos';
    deselect_all tab;
    tab.entries <- undo.undo_entries;
    tab.pos <- undo.undo_pos;
    tab.vscroll <- undo.undo_vscroll;
    tab.sel_range <- undo.undo_sel_range;
    tab.selected <- undo.undo_selected

let pop_undo tab = pop_unredo tab tab.undos tab.redos
let pop_redo tab = pop_unredo tab tab.redos tab.undos

let drop_undo tab = tab.undos := List.tl !(tab.undos)
let drop_redo tab = tab.redos := List.tl !(tab.redos)


(* Editing *)

let move_pos tab i j len =
  let j' = min j (len - 1) in
  if i <> j' then
  (
    if Some i = tab.pos then tab.pos <- Some j';
    match tab.sel_range with
    | None -> ()
    | Some (pos1, pos2) ->
      tab.sel_range <- Some
        ( (if i = pos1 then j' else pos1),
          (if i = pos2 then j' else pos2) )
  );
  if i <> j then
  (
    tab.selected <- IntSet.remove j tab.selected;
    if IntSet.mem i tab.selected then
      tab.selected <- IntSet.add j (IntSet.remove i tab.selected)
  )


let insert tab pos entries =
  assert (pos <= Array.length tab.entries);
  if entries <> [||] then
  (
    push_undo tab;
    let len = Array.length tab.entries in
    let len' = Array.length entries in
    if len = 0 then
    (
      tab.entries <- entries;
      if len' > 0 then
      (
        tab.pos <- Some 0;
      )
    )
    else
    (
      tab.entries <-
        Array.init (len + len') (fun i ->
          if i < pos then tab.entries.(i) else
          if i < pos + len' then entries.(i - pos) else
          tab.entries.(i - len')
        );
      for i = len - 1 downto pos do
        move_pos tab i (i + len') (len + len')
      done;
    )
  )


let remove_all tab =
  if tab.entries <> [||] then
  (
    push_undo tab;
    deselect_all tab;
    tab.entries <- [||];
    tab.pos <- None;
    tab.vscroll <- 0;
  )

let remove_if p tab n =
  if n = 0 then [||] else
  (
    push_undo tab;
    let len = Array.length tab.entries in
    let len' = len - n in
    let d = ref 0 in
    let js = Array.make len (-2) in
    let rec skip i =
      let j = i + !d in
      if j < len then
      (
        assert (js.(j) = -2);
        let b = p j in
        move_pos tab j i len';  (* could affect (p j)! *)
        if b then
        (
          deselect tab j j;
          incr d;
          js.(j) <- -1;
          skip i;
        )
        else
        (
          js.(j) <- i;
        )
      )
    in
    let entries' = Array.init len' (fun i -> skip i; tab.entries.(i + !d)) in
    skip len';
    assert (len' + !d = len);
    tab.entries <- entries';
    if len' = 0 then tab.pos <- None;
    tab.sel_range <- max_sel_range tab;
    tab.vscroll <- max 0 (min (len' - 1) tab.vscroll);
    js
  )


let move_selected tab d =
  if num_selected tab = 0 then [||] else
  (
    push_undo tab;
    let len = Array.length tab.entries in
    let d =
      if d < 0
      then max d (- Option.value (first_selected tab) ~default: (len - 1))
      else min d (len - Option.value (last_selected tab) ~default: 0 - 1)
    in
    let js = Array.init len Fun.id in
    if d < 0 then
      for i = 0 to len - 1 do
        if is_selected tab i then
        (
          assert (i >= -d);
          let temp = tab.entries.(i) in
          move_pos tab i (-1) len;  (* temp position *)
          for j = i - 1 downto i + d do
            tab.entries.(j + 1) <- tab.entries.(j);
            move_pos tab j (j + 1) len;
            js.(j) <- j + 1;
          done;
          move_pos tab (-1) (i + d) len;
          js.(i) <- i + d;
          tab.entries.(i + d) <- temp;
        )
      done
    else
      for i = len - 1 downto 0 do
        if is_selected tab i then
        (
          assert (i < len - d);
          let temp = tab.entries.(i) in
          move_pos tab i (-1) len;  (* temp position *)
          for j = i + 1 to i + d do
            tab.entries.(j - 1) <- tab.entries.(j);
            move_pos tab j (j - 1) len;
            js.(j) <- j - 1;
          done;
          move_pos tab (-1) (i + d) len;
          js.(i) <- i + d;
          tab.entries.(i + d) <- temp;
        )
      done;
    js
  )
