(* Run Menu UI *)

type state = State.t


(* Menu creation *)

let menu' (st : state) items op =
  st.layout.menu_shown <- true;
  Menu.set st.menu (Api.Mouse.pos (Ui.window st.layout.ui)) op items;
  Ui.modal st.layout.ui


let command_menu st cmds =
  menu' st (Array.map fst cmds) (fun k -> snd cmds.(k) ())


let header_menu (st : state) (view : _ Library.view) i current_attrs unused_attrs =
  let c = Ui.text_color st.layout.ui in
  if current_attrs = [] && unused_attrs = [] then
  (
    menu' st [|`Entry (c, "(Nothing to add)", Layout.nokey, false)|] ignore
  )
  else
  (
    let removes = current_attrs |>
      List.map (fun a ->
        `Entry (c, "Remove " ^ Library.attr_name a, Layout.nokey, true), a)
      |> List.sort compare in
    let adds = unused_attrs |>
      List.map (fun a ->
        `Entry (c, "Add " ^ Library.attr_name a, Layout.nokey, true), a)
      |> List.sort compare in
    let sep = if removes = [] || adds = [] then [] else [`Separator] in
    let items = Array.of_list List.(map fst removes @ sep @ map fst adds) in
    menu' st items (fun k ->
      let n = if removes = [] then -1 (* no sep! *) else List.length removes in
      let attrs = Array.to_list view.columns in
      let attrs' =
        match compare k n with
        | -1 ->  (* remove entry *)
          let _, attr = List.nth removes k in
          view.sorting <- List.filter (fun (a, _) -> a <> attr) view.sorting;
          List.filter (fun (a, _) -> a <> attr) attrs
        | +1 when adds <> [] ->  (* add entry *)
          let _, attr = List.nth adds (k - n - 1) in
          let i' = min (i + 1) (List.length attrs) in
          List.take i' attrs @ [attr, 40] @ List.drop i' attrs
        | _ ->  (* separator *)
          attrs
      in
      view.columns <- Array.of_list attrs'
    )
  )


(* Running *)

let run (st : state) =
  let lay = st.layout in
  let menu = st.menu in
  let x, y = menu.pos in

  match Ui.menu lay.ui x y (lay.margin / 2) lay.gutter lay.text menu.items with
  | `None -> ()

  | `Close ->
    Ui.nonmodal lay.ui;
    lay.menu_shown <- false;
    Menu.clear menu

  | `Click k ->
    Option.get st.menu.op k;
    Ui.nonmodal lay.ui;
    lay.menu_shown <- false;
    Menu.clear menu
