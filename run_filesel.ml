(* File Selection UI *)

open Audio_file

type state = State.t


(* Initiate *)

let filesel (st : state) rw df path ext f =
  let f' path =
    try f path with Sys_error msg ->
      let op = match rw with `Read -> "reading" | `Write -> "writing" in
      Library.error st.library ("Error " ^ op ^ " file " ^ path ^ ", " ^ msg);
      Layout.browser_error_box st.layout;  (* flash *)
  in
  if path <> "" then Filesel.set_dir_path st.filesel path;
  st.filesel.op <- Some (rw, df, f');
  st.layout.filesel_shown <- true;
  Filesel.init st.filesel;
  Edit.set st.filesel.input ext;
  Edit.move_begin st.filesel.input;
  State.defocus_all st;
  Filesel.focus_input st.filesel


(* Runner *)

let run (st : state) =
  let fs = st.filesel in
  let lay = st.layout in
  let rw, ty, f = Option.get fs.op in

  (* Update after possible window resize *)
  lay.directories_width <-
    Layout.clamp (Layout.directories_min lay) (Layout.directories_max lay)
    lay.directories_width;

  (* Directories *)

  Layout.directories_pane lay;

  let dirs = fs.dirs in

  let pp_entry i =
    let dir = dirs.entries.(i) in
    let name =
      if File.dir dir.path <> dir.path then
        File.name dir.path
      else if dir.path = File.sep then
        dir.path
      else  (* Special case for Windows drives: strip slash *)
        String.(sub dir.path 0 (length dir.path - length File.sep))
    and c =
      if dir.path = Storage.home_dir
      || dir.folded
      && String.starts_with ~prefix: (File.(//) dir.path "") Storage.home_dir
      then `White
      else Ui.text_color lay.ui
    in
    let c' = if dir.accessible then c else Ui.semilit_color c in
    dir.nest, Some dir.folded, c', name
  in

  let dir = Filesel.selected_dir fs in
  (match Layout.directories_table lay dirs pp_entry with
  | `None | `Scroll | `Move _ | `Drag _ | `Drop -> ()

  | `Select ->
    (* Select dir: refresh file list *)
    State.focus_filesel dirs st;
    if Table.num_selected dirs = 0 then
      Filesel.select_dir fs dir  (* override *)
    else
      let dir = dirs.entries.(Filesel.selected_dir fs) in
      Filesel.set_dir_path fs dir.path;

  | `Fold i ->
    (* Click on triangle: fold/unfold entry *)
    let dir = dirs.entries.(i) in
    Filesel.fold_dir fs dir (not dir.folded)

  | `Click (Some i) ->
    (* Click on dir name: switch view *)
    if Api.Mouse.is_pressed `Left then
      State.focus_filesel dirs st;
    if Table.num_selected dirs = 0 then
      Filesel.select_dir fs dir  (* override *)
    else
      let dir = dirs.entries.(i) in
      Filesel.set_dir_path fs dir.path;

  | `Click None ->
    (* Click into empty space: focus *)
    State.focus_filesel dirs st;
    if Table.num_selected dirs = 0 then
      Filesel.select_dir fs dir;  (* override *)

  | `Menu _ ->
    (* Right-click on dir: ignore *)
    State.focus_filesel dirs st;
  );


  (* Files *)

  Layout.files_pane lay;

  let files = fs.files in
  let cols = Filesel.columns fs in

  let pp_row i =
    let file = files.entries.(i) in
    let c =
      if file.name = fs.input.text then `White else Ui.text_color lay.ui in
    (if file.accessible then c else Ui.semilit_color c),
    Array.map (fun s -> `Text s) (Filesel.row file)
  in

  let ok =
    match Layout.files_table lay cols (Some Filesel.heading) files pp_row with
    | `None | `Scroll | `Move _ | `Drag _ | `Drop -> false

    | `Click (Some i) when Api.Mouse.is_doubleclick `Left ->
      (* Double-click on file: change dir or copy to input and accept *)
      let file = files.entries.(i) in
      if not file.is_dir then
        Edit.set fs.input file.name;
      true

    | `Select | `Click (Some _) ->
      State.focus_filesel files st;
      Option.iter (fun i ->
        let file = files.entries.(i) in
        if not file.is_dir || ty = `Dir then
          Edit.set fs.input file.name
      ) (Filesel.selected_file fs);
      false

    | `Sort i ->
      (* Click on column header: reorder view accordingly *)
      Filesel.reorder_files fs i;
      false

    | `Resize ws ->
      (* Column resizing: update column widths *)
      Array.blit ws 0 fs.columns 0 (Array.length ws);
      false

    | `Reorder _ ->
      (* Column reordering: ignore *)
      false

    | `Click None ->
      (* Click into empty space: focus *)
      State.focus_filesel files st;
      false

    | `Menu _ ->
      (* Right-click on dir: ignore *)
      State.focus_filesel files st;
      false

    | `HeadMenu _ ->
      (* Right-click on header: ignore *)
      State.focus_filesel files st;
      false
  in

  (* Input *)
  Layout.file_label lay;
  Layout.file_box lay;
  if Layout.file_button lay then
  (
    (* Click on File label: clear search *)
    if fs.input.text <> "" then
      Edit.clear fs.input;
  );

  let ch = Layout.file_edit lay fs.input in
  if fs.input.focus then
  (
    (* Have or gained focus: make sure it's consistent *)
    Filesel.deselect_file fs;
    State.defocus_all st;
    Filesel.focus_input fs;
  );
  let ok = ok || ch = Uchar.of_char '\n' in

  (* Buttons *)

  let is_write = (rw = `Write) in
  let is_valid = fs.input.text <> "" && fs.input.text.[0] <> '.' in
  let dir_avail =
    fs.dirs.focus || fs.files.focus && Filesel.current_sel_is_dir fs in
  let file_avail = not dir_avail && Filesel.current_file_exists fs in
  let dir_file_avail =
    ty = `Dir && fs.files.focus && Filesel.current_sel_is_dir fs in
  let overwrite_avail = file_avail && is_write in
  let ok_avail = dir_file_avail ||
    not (dir_avail || overwrite_avail) && (file_avail || is_write && is_valid)
  in

  let ok_button lay =
    ok_avail && not (Layout.ok_button lay (Some true)) ||
    not ok_avail && Layout.ok_button lay None
  and overwrite_button lay =
    not (Layout.overwrite_button lay (Some true))
  in

  if
    overwrite_avail && overwrite_button lay ||
    not overwrite_avail && ok_button lay ||
    dir_avail && Layout.return_key lay ||
    (ok_avail || dir_avail) && ok
  then
  (
    if dir_avail && not dir_file_avail then
    (
      (* Return or double-click on directory *)
      let dir' = fs.dirs.entries.(Filesel.selected_dir fs) in
      if fs.dirs.focus then
        (* Folded directory in dirs view: unfold *)
        Filesel.fold_dir fs dir' (not dir'.folded)
      else
        (* Directory in files view: open *)
        let file = fs.files.entries.(Option.get (Filesel.selected_file fs)) in
        Filesel.set_dir_path fs File.(dir'.path // file.name)
    )
    else
    (
      (* Return, double-click, or OK button on regular file *)
      f (Option.get (Filesel.current_file_path fs));
      Filesel.reset fs;
      lay.filesel_shown <- false;
    )
  );

  if Layout.cancel_button lay (Some false) && not ok then
  (
    Filesel.reset fs;
    lay.filesel_shown <- false;
    State.focus_playlist st;
  );

  (* Pane divider *)

  let directories_width' = Layout.directories_divider lay lay.directories_width
    (Layout.directories_min lay) (Layout.directories_max lay) in
  (* Possible drag of divider: update pane width *)
  lay.directories_width <- directories_width'
