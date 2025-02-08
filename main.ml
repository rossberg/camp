(* Main Program *)


(* Helpers *)

let rec log10 n = if n < 10 then 0 else 1 + log10 (n / 10)

let clamp min max v =
  if v < min then min else
  if v > max then max else
  v

let float_of_bool b = float (Bool.to_int b)


let start_time = Unix.gettimeofday ()

let time () = Unix.gettimeofday () -. start_time


let fmt = Printf.sprintf

let fmt_time t =
  let t' = int_of_float (Float.trunc t) in
  fmt "%d:%02d" (t' / 60) (t' mod  60)

let _fmt_time2 t =
  let t' = int_of_float (Float.trunc t) in
  fmt "%02d:%02d" (t' / 60) (t' mod  60)

let fmt_time3 t =
  let t' = int_of_float (Float.trunc t) in
  if t' < 3600 then
    fmt_time t
  else
    fmt "%d:%02d:%02d" (t' / 3600) (t' / 60 mod 60) (t' mod  60)


let exec prog args =
  let cmd = Filename.quote_command prog args in
  let cmd' = if not Sys.win32 then cmd else
    "\"start /b ^\"^\" " ^ String.sub cmd 1 (String.length cmd - 1) in
  ignore (Sys.command cmd')


(* Control Section *)

let run_control (st : State.t) =
  let ctl = st.control in
  let pl = st.playlist in
  let lay = st.layout in
  let win = Ui.window lay.ui in

  Layout.control_pane lay;

  (* Exit button *)
  (* This has to come first, otherwise Raylib crashes? *)
  if not (Layout.power_button lay (Some true)) then exit 0;
  Layout.power_label lay;

  (* Current status *)
  let silence = ctl.sound = Api.Audio.silence ctl.audio in
  let length = Api.Audio.length ctl.audio ctl.sound in
  let elapsed = Api.Audio.played ctl.audio ctl.sound in
  let remaining = length -. elapsed in
  let playing = Api.Audio.is_playing ctl.audio ctl.sound in
  let paused = not playing && elapsed > 0.0 in
  let stopped = not playing && not paused in

  (* LCD *)
  Layout.info_box lay;
  let sign, d1, d2, d3, d4 =
    if paused && int_of_float (time ()) mod 2 = 0 then
      '+', ' ', ' ', ' ', ' ' else
    let sign, time =
      match ctl.timemode with
      | `Elapse -> '+', elapsed
      | `Remain -> '-', remaining
    in
    Layout.lcd_colon lay ':';
    let seconds = int_of_float (Float.round (if silence then 0.0 else time)) in
    sign,
    (Char.chr (Char.code '0' + seconds mod 6000 / 600)),
    (Char.chr (Char.code '0' + seconds mod 600 / 60)),
    (Char.chr (Char.code '0' + seconds mod 60 / 10)),
    (Char.chr (Char.code '0' + seconds mod 10))
  in
  Layout.lcd_minus lay sign;
  Layout.lcd1 lay d1;
  Layout.lcd2 lay d2;
  Layout.lcd3 lay d3;
  Layout.lcd4 lay d4;

  if Layout.lcd_button lay then
  (
    (* Click on time LCD: toggle time mode *)
    ctl.timemode <-
      match ctl.timemode with
      | `Elapse -> `Remain
      | `Remain -> `Elapse
  );

  let ncol = Ui.num_palette lay.ui in
  let dcol =
    (if Layout.color_button_fwd lay then +1 else 0) +
    (if Layout.color_button_bwd lay then -1 else 0)
  in
  (* Possible click on color button: cycle color palette *)
  Ui.set_palette lay.ui ((Ui.get_palette lay.ui + dcol + ncol) mod ncol);

  (* FPS *)
  if ctl.fps then
    Layout.fps_text lay `Regular true (fmt "%d FPS" (Api.Window.fps win));
  (* Press of FPS key: toggle FPS display *)
  if Layout.fps_key lay then ctl.fps <- not ctl.fps;

  (* Audio properties *)
  if not silence then
  (
    let track = Option.get ctl.current in
    let ext = Filename.extension track.path in
    let format = if ext = "" || ext.[0] <> '.' then "???" else
      String.uppercase_ascii (String.sub ext 1 (String.length ext - 1)) in
    let bitrate = Api.Audio.bitrate ctl.audio ctl.sound in
    let rate = Api.Audio.rate ctl.audio ctl.sound in
    let channels = Api.Audio.channels ctl.audio ctl.sound in
    let depth = bitrate /. float rate /. float channels in
    Layout.prop_text lay `Regular true
      (fmt "%s   %.0f KBPS   %.1f KHZ   %s BIT   %s"
        format (bitrate /. 1000.0) (float rate /. 1000.0)
        (fmt (if depth = Float.round depth then "%.0f" else "%.1f") depth)
        (match channels with
        | 1 -> "MONO"
        | 2 -> "STEREO"
        | n -> fmt "%d CHAN" n
        )
      );
  );

  (* Title info *)
  let name =
    match ctl.current with
    | Some track when not (Track.is_separator track) ->
      track.name ^ " - " ^ fmt_time track.time
    | _ -> App.(name ^ " " ^ version)
  in
  Layout.title_ticker lay name;

  (* Volume control *)
  let volume' =
    Layout.volume_bar lay ctl.volume +.
    0.05 *. Layout.volume_wheel lay +.
    0.05 *. (float_of_bool (Layout.volup_key lay) -.
    float_of_bool (Layout.voldown_key lay))
  in
  Layout.mute_text lay (Ui.error_color lay.ui) `Inverted ctl.mute "MUTE";
  let mute' =
    if Layout.mute_button lay || Layout.mute_key lay
    then not ctl.mute
    else ctl.mute
  in
  if volume' <> ctl.volume || mute' <> ctl.mute then
  (
    (* Click or drag on volume bar or mute button: adjust volume *)
    (* Hack to overlap volume bar with mute button. *)
    if Layout.mute_drag lay (0, 0) = `None then
      ctl.volume <- clamp 0.0 1.0 volume';
    ctl.mute <- mute';
    Api.Audio.volume ctl.audio ctl.sound (if ctl.mute then 0.0 else ctl.volume);
  );

  (* Seek bar *)
  let progress =
    if length > 0.0 && not silence then elapsed /. length else 0.0 in
  let progress' =
    Layout.seek_bar lay progress +.
    0.05 *. float_of_bool (Layout.ff_key lay) -.
    0.05 *. float_of_bool (Layout.rw_key lay)
  in
  if progress' <> progress && not silence then
  (
    (* Click or drag on seek bar: reposition audio *)
    Control.seek ctl (clamp 0.0 1.0 progress');
  );
(*
  let s1 = fmt_time2 elapsed in
  let s2 = "-" ^ fmt_time2 remaining in
  let w2 = Api.Draw.text_width win 11 (Ui.font lay.ui 11) s2 in
  Api.Draw.text win 14 91 11 `White (Ui.font lay.ui 11) s1;
  Api.Draw.text win (278 - w2) 91 11 `White (Ui.font lay.ui 11) s2;
*)

  (* Looping *)
  (match ctl.loop with
  | `AB (t1, t2) when playing && t2 < elapsed ->
    (* End of loop reached: jump back to start *)
    Control.seek ctl (t1 /. length);
  | _ -> ()
  );

  (* Play controls *)
  let len = Playlist.length pl in
  let _, _, _, h = Ui.dim lay.ui (Layout.playlist_area lay) in
  let page = max 1 (int_of_float (Float.floor (float h /. float lay.text))) in
  let bwd = if Layout.bwd_button lay (Some false) then -1 else 0 in
  let fwd = if Layout.fwd_button lay (Some false) then +1 else 0 in
  let off = if len = 0 then 0 else bwd + fwd in
  if off <> 0 then
  (
    (* Click on one of the skip buttons: jump to track *)
    let more = Playlist.skip pl off (ctl.repeat <> `None) in
    Control.switch ctl (Playlist.current pl) more;
    Playlist.adjust_scroll pl page;
  );

  let playing' = Layout.play_button lay (Some playing) in
  if stopped && playing' && len > 0 then
  (
    (* Click on play button: start track *)
    Control.switch ctl (Playlist.current pl) true;
    Playlist.adjust_scroll pl page;
  );

  let paused' = Layout.pause_button lay (Some paused) in
  if playing' && paused' then
  (
    (* Click on pause button when playing: pause track *)
    Api.Audio.pause ctl.audio ctl.sound
  )
  else if (not stopped && not paused' || stopped && paused') && not silence then
  (
    (* Click on pause button when paused: resume track *)
    Api.Audio.resume ctl.audio ctl.sound;
  );

  if Layout.stop_button lay (Some false) && not stopped then
  (
    (* Click on stop button when playing: stop track *)
    Api.Audio.pause ctl.audio ctl.sound;
    (match Playlist.current_opt pl with
    | None -> Control.eject ctl
    | Some track -> Control.switch ctl track false
    );
    Playlist.adjust_scroll pl page;
  );

  if Layout.eject_button lay (Some false) then
  (
    (* Click on eject button: stop and clear playlist *)
    Control.eject ctl;
    Playlist.remove_all pl;
  );

  if Layout.start_stop_key lay then
  (
    (* Press of space key: pause or resume *)
    if playing then
      Api.Audio.pause ctl.audio ctl.sound
    else if paused then
      Api.Audio.resume ctl.audio ctl.sound
    else if stopped && len > 0 then
      Control.switch ctl (Playlist.current pl) true;
    Playlist.adjust_scroll pl page;
  );

  (* End of track *)
  (* Check must occur after possible Audio.resume above,
   * otherwise the last track would be restarted. *)
  if playing && (remaining < 0.2 || silence) then
  (
    (* Close to end: switch to next track *)
    let more =
      match ctl.repeat with
      | `One -> true
      | `All -> Playlist.skip pl (+1) true
      | `None -> Playlist.skip pl (+1) false
    in
    let next_track =
      if pl.table.pos = None
      then Option.get ctl.current
      else Playlist.current pl
    in
    Control.switch ctl next_track more;
    Playlist.adjust_scroll pl page;
  );

  (* Play modes *)
  let shuffle = pl.shuffle <> None in
  Layout.shuffle_label lay;
  Layout.shuffle_indicator lay shuffle;
  let shuffle' = Layout.shuffle_button lay (Some shuffle) in
  if shuffle' <> shuffle then
  (
    (* Click on Shuffle button: toggle shuffle *)
    if shuffle' then
    (
      Playlist.shuffle pl (if stopped then None else pl.table.pos);
      if stopped && pl.table.pos <> None then
        Control.switch ctl (Playlist.current pl) false;
      Playlist.adjust_scroll pl page;
    )
    else
      Playlist.unshuffle pl
  );

  Layout.repeat_label lay;
  Layout.repeat_indicator1 lay (ctl.repeat <> `None);
  Layout.repeat_indicator2 lay (ctl.repeat = `All);
  let repeat' = Layout.repeat_button lay (Some (ctl.repeat <> `None)) in
  ctl.repeat <-
    (* Click on Repeat button: cycle repeat mode *)
    (match ctl.repeat, repeat' with
    | `None, false | `All, false -> `None
    | `None, true | `One, true -> `One
    | `One, false | `All, true -> `All
    );

  Layout.loop_label lay;
  Layout.loop_indicator1 lay (ctl.loop <> `None);
  Layout.loop_indicator2 lay
    (match ctl.loop with `AB _ -> true | _ -> false);
  let loop' = Layout.loop_button lay (Some (ctl.loop <> `None)) in
  ctl.loop <-
    (* Click on Loop button: cycle loop mode *)
    (match ctl.loop, loop' with
    | `None, false | `AB _, false -> `None
    | `None, true -> `A elapsed
    | `A t1, true -> `A t1
    | `A t1, false when t1 > elapsed -> `A elapsed
    | `A t1, false -> `AB (t1, elapsed)
    | `AB (t1, t2), true -> `AB (t1, t2)
    )


(* Pane Activation *)

let run_toggle_panes (st : State.t) =
  let lay = st.layout in
  let win = Ui.window lay.ui in

  Layout.playlist_label lay;
  Layout.playlist_indicator lay lay.playlist_shown;
  let playlist_shown' = Layout.playlist_button lay (Some lay.playlist_shown) in
  (* Click on playlist activation button: toggle playlist *)
  lay.playlist_shown <- playlist_shown';

  Layout.library_label lay;
  Layout.library_indicator lay lay.library_shown;
  let library_shown' = Layout.library_button lay (Some lay.library_shown) in
  let wx, _ = Api.Window.pos win in
  let sx, _ = Api.Window.min_pos win in
  let sw, _ = Api.Window.max_size win in
  (* Click on library activation button: toggle library *)
  if not lay.library_shown && library_shown'
  && not (Api.Key.is_modifier_down `Shift) then
  (
    (* Library was off: show; switch side if window is at the respective border *)
    if lay.library_side = `Left && wx <= sx then
      lay.library_side <- `Right;
    if lay.library_side = `Right && wx + Layout.control_w lay >= sx + sw then
      lay.library_side <- `Left;
    lay.library_shown <- library_shown';
  )
  else if
    lay.library_shown <> library_shown' && Api.Key.is_modifier_down `Shift ||
    Layout.library_mouse lay || Layout.library_key lay
  then
  (
    (* Shift-click: switch sides for library pane *)
    lay.library_side <- if lay.library_side = `Left then `Right else `Left
  )
  else
  (
    (* Library was on: turn off *)
    lay.library_shown <- library_shown';
  );

  (* Minimize button *)
  if Layout.minimize_button lay then
  (
    (* Right-click on power button: minimize window *)
    Api.Window.minimize win
  )


(* Playlist Pane *)

let run_playlist (st : State.t) =
  let pl = st.playlist in
  let lib = st.library in
  let lay = st.layout in
  let win = Ui.window lay.ui in
  let tab = pl.table in
  let len = Table.length tab in
  let now = Unix.time () in

  Layout.playlist_pane lay;

  (* Playlist table *)
  let _, y, _, h = Ui.dim lay.ui (Layout.playlist_area lay) in
  let page = max 1 (int_of_float (Float.floor (float h /. float lay.text))) in
  let digits_pos = log10 (len + 1) + 1 in
  let digits_time = ref 1 in
  for i = tab.vscroll to min len (tab.vscroll + page) - 1 do
    if tab.entries.(i).time > 599.4 then
      digits_time := max !digits_time
        (if tab.entries.(i).time > 5999.4 then 3 else 2)
  done;
  let font = Ui.font lay.ui lay.text in
  let s_pos = String.make digits_pos '0' ^ "." in
  let s_time = String.make !digits_time '0' ^ ":00" in
  let cw_pos = Api.Draw.text_width win lay.text font s_pos + 1 in
  let cw_time = Api.Draw.text_width win lay.text font s_time + 1 in
  let cols = [|cw_pos, `Right; -1, `Left; cw_time, `Right|] in

  let pp_row i =
    let track = tab.entries.(i) in
    if now -. track.last_update > st.config.delay_track_update then
      Track.update track;
    let c =
      match track.status with
      | _ when tab.pos = Some i ->
        if track.path = (Option.get st.control.current).path then `White else `Gray 0xc0
      | _ when Track.is_separator track -> Ui.text_color lay.ui
      | `Absent -> Ui.error_color lay.ui
      | `Invalid -> Ui.warn_color lay.ui
      | `Undet -> Ui.semilit_color (Ui.text_color lay.ui)
      | `Predet | `Det -> Ui.text_color lay.ui
    in
    let time = if track.time = 0.0 then "" else fmt_time track.time in
    c, [|fmt "%0*d." digits_pos (i + 1); track.name; time|]
  in

  (match Layout.playlist_table lay cols None tab pp_row with
  | `None | `Scroll | `Drop -> ()
  | `Sort _ | `Arrange -> assert false

  | `Select ->
    Playlist.update_total_selected pl

  | `Click (Some i) when Api.Mouse.is_doubleclick `Left ->
    (* Double-click on track: switch to track *)
    tab.pos <- Some i;
    Control.switch st.control tab.entries.(i) true;
    if pl.shuffle <> None then 
      Playlist.shuffle_next pl i;

  | `Click _ ->
    (* Single-click: grab focus *)
    Playlist.update_total_selected pl;
    Library.defocus lib;
    Library.deselect_all lib;

  | `Move delta ->
    (* Drag or Cmd-cursor movement: move selection *)
    if Api.Key.are_modifiers_down [] && Api.Mouse.is_drag `Left then
    (
      (* Actual drag *)
      if Playlist.num_selected pl > 0 then
        Api.Mouse.set_cursor win `Point;
    );
    Playlist.move_selected pl delta
  );

  (* Save button *)
  if Layout.save_button lay None then
  (
    (* Click on Save button: save playlist *)
    (* TODO: file dialog for chosing file path and name *)
  );

  (* Tag button *)
  let selected = Playlist.num_selected pl > 0 in
  let is_executable path =
    try Unix.(access path [X_OK]); true with Unix.Unix_error _ -> false
  in
  let tag_available = selected && is_executable st.config.exec_tag in
  if Layout.tag_button lay (if tag_available then Some false else None) then
  (
    (* Click on Tag button: execute tagging program *)
    let tracks = Array.to_list (Playlist.selected pl) in
    Domain.spawn (fun () ->
      let tracks' = List.filter (fun tr -> not (Track.is_separator tr)) tracks in
      let paths = List.map (fun (track : Track.t) -> track.path) tracks' in
      if st.config.exec_tag_max_len = 0 then
        exec st.config.exec_tag paths
      else
      (
        (* Work around Windows command line limits *)
        let args = ref paths in
        let rec pick len max =
          match !args with
          | [] -> []
          | arg1::args' ->
            let len' = len + String.length arg1 + 5 in
            if len <> 0 && len' > max then [] else
            (
              args := args';
              arg1 :: pick len' max
            )
        in
        (* Mp3tag immediately resorts the tracks by current column, unless added
         * with /add. However, /add only works with individual tracks and exec's,
         * which is very slow, so only use that when (a) we have less then a
         * certain number of tracks, or (b) when the command line gets too long
         * for a single call anyways. *)
        let max = if List.length tracks < 20 then 1 else st.config.exec_tag_max_len in
        exec st.config.exec_tag (pick 0 max);
        List.iter (fun arg -> exec st.config.exec_tag ["/add"; arg]) !args;
      )
    ) |> ignore;
  );

  (* Spearator button *)
  if Layout.sep_button lay (Some false) then
  (
    (* Click on Separator button: insert separator *)
    let pos = Option.value (Playlist.first_selected pl) ~default: 0 in
    Playlist.insert pl pos [|Track.make_separator ()|];
    Control.switch_if_empty st.control (Playlist.current_opt pl);
    if Playlist.num_selected pl = 1 then
    (
      (* Selection was singular: change it to new insertion *)
      Library.deselect_all lib;
      Playlist.deselect_all pl;
      Playlist.select pl pos pos;
    )
  );

  (* Edit buttons *)
  if Layout.del_button lay (if selected then Some false else None) then
  (
    (* Click on Delete button: remove selected tracks from playlist *)
    Playlist.remove_selected pl;
  );

  let unselected = Playlist.num_selected pl < len in
  if Layout.crop_button lay (if unselected then Some false else None) then
  (
    (* Click on Crop button: remove unselected tracks from playlist *)
    Playlist.remove_unselected pl;
  );

  let clean_avail = if snd pl.total > 0 then Some false else None in
  if Layout.clean_button lay clean_avail then
  (
    (* Click on Clean button: remove invalid tracks from playlist *)
    Playlist.remove_invalid pl;
  );

  let undo_avail = if !(tab.undos) <> [] then Some false else None in
  if Layout.undo_button lay undo_avail then
  (
    (* Click on Undo button: pop undo *)
    Playlist.pop_undo pl;
    Control.switch_if_empty st.control (Playlist.current_opt pl);
  );

  let redo_avail = if !(tab.redos) <> [] then Some false else None in
  if Layout.redo_button lay redo_avail then
  (
    (* Click on Redo button: pop redo *)
    Playlist.pop_redo pl;
    Control.switch_if_empty st.control (Playlist.current_opt pl);
  );

  (* Edit keys *)
  if Layout.cut_key lay then
  (
    (* Press of Cut key: remove selected tracks and write them to clipboard *)
    let s = Playlist.string_of_playlist (Playlist.selected pl) in
    Api.Clipboard.write win s;
    Playlist.remove_selected pl;
  );

  if Layout.copy_key lay then
  (
    (* Press of Copy key: write selected tracks to clipboard *)
    let s = Playlist.string_of_playlist (Playlist.selected pl) in
    Api.Clipboard.write win s;
  );

  if Layout.paste_key lay then
  (
    (* Press of Paste key: insert tracks from clipboard *)
    match Api.Clipboard.read win with
    | None -> ()
    | Some s ->
      let tracks = Playlist.playlist_of_string s in
      let pos = Option.value (Playlist.first_selected pl) ~default: 0 in
      Playlist.insert pl pos tracks;
      Control.switch_if_empty st.control (Playlist.current_opt pl);
      if Playlist.num_selected pl = 1 && tracks <> [||] then
      (
        (* Selection was singular: change it to new insertion *)
        Library.deselect_all lib;
        Playlist.deselect_all pl;
        Playlist.select pl pos (pos + Array.length tracks - 1);
      )
  );

  (* Playlist drag & drop *)
  let dropped = Layout.playlist_drop lay in
  if dropped <> [] then
  (
    (* Files drop on playlist: insertion paths at pointed position *)
    let _, my = Api.Mouse.pos win in
    let pos = min len ((my - y) / lay.text + tab.vscroll) in
    Playlist.deselect_all pl;
    let len = Playlist.length pl in
    Playlist.insert_paths pl pos dropped;
    let len' = Playlist.length pl in
    if pos > 0 && pos < len then Playlist.select pl pos (pos + len' - len - 1);
    Control.switch_if_empty st.control (Playlist.current_opt pl);
  );

  (* Playlist total *)
  if int_of_float (time ()) mod 10 = 0 then Playlist.update_total pl;
  let fmt_total (t, n) = fmt_time3 t ^ if n > 0 then "+" else "" in
  let s1 =
    if pl.total_selected = (0.0, 0) then "" else
    fmt_total pl.total_selected ^ "/"
  in
  let s2 = fmt_total pl.total in
  Layout.playlist_total_box lay;
  Layout.playlist_total_text lay `Regular true (s1 ^ s2)


(* Library Panes *)

let symbol_empty = " ○"
let symbol_folded = "►" (* "▸" *)
let symbol_unfolded = "▼" (* "▾" *)

let run_library (st : State.t) =
  let pl = st.playlist in
  let lib = st.library in
  let lay = st.layout in
  let win = Ui.window lay.ui in
  let (mx, my) as m = Api.Mouse.pos win in

  (* Update after possible window resize *)
  lay.browser_width <-
    clamp (Layout.browser_min lay) (Layout.browser_max lay) lay.browser_width;
  lay.left_width <-
    clamp (Layout.left_min lay) (Layout.left_max lay) lay.left_width;
  lay.upper_height <-
    clamp (Layout.upper_min lay) (Layout.upper_max lay) lay.upper_height;


  Layout.browser_pane lay;

  (* Background rescanning *)
  Library.update_after_rescan lib;

  (* Browser *)
  let browser = lib.browser in
  let len = Table.length browser in

  let cols = [|-1, `Left|] in
  let c = Ui.text_color lay.ui in
  let pre =
    Array.map (fun (dir : Data.dir) ->
      let sym =
        if dir.children = [||] then symbol_empty else
        if dir.folded then symbol_folded else symbol_unfolded
      in
      if dir.nest = -1 then "" else String.make (2 * dir.nest) ' ' ^ sym ^ " "
    ) browser.entries
  in
  let pp_row i = c, [|pre.(i) ^ browser.entries.(i).name|]in

  let dir = Library.selected_dir lib in
  let selected = browser.selected in
  (match Layout.browser_table lay cols None browser pp_row with
  | `None | `Scroll -> ()
  | `Sort _ | `Arrange -> assert false

  | `Select ->
    (* TODO: allow multiple selections *)
    if Table.num_selected lib.browser > 1 then
      browser.selected <- selected;  (* override *)
    if Library.selected_dir lib <> dir then
    (
      (match Library.selected_dir lib with
      | None -> Library.deselect_dir lib
      | Some i -> Library.select_dir lib i  (* do bureaucracy *)
      );
      Library.deselect_all lib;
      Library.update_views lib;
    );

  | `Click (Some i) ->
    (* Click on dir: fold/unfold or switch view *)
    let x, _, _, _ = Ui.dim lay.ui (Layout.browser_area lay) in
    let tw = Api.Draw.text_width win lay.text (Ui.font lay.ui lay.text) pre.(i) in
    if mx < x + tw && (Api.Mouse.is_down `Left || Api.Mouse.is_released `Left) then
    (
      (* CLick on triangle: fold/unfold entry *)
      let dir = browser.entries.(i) in
      browser.selected <- selected;  (* override selection change *)
      if Api.Mouse.is_pressed `Left then
        Library.fold_dir lib dir (not dir.folded);
    )
    else
    (
      (* Click on directory name: change view if necessary *)
      (* TODO: allow multiple selections *)
      if Table.num_selected browser > 1 then
        browser.selected <- selected;  (* override *)
      Library.focus_browser lib;
      Playlist.defocus pl;
      if Library.selected_dir lib <> dir then
      (
        Library.select_dir lib i;  (* do bureaucracy *)
        Library.deselect_all lib;
        Library.update_views lib;
      );
      if Api.Mouse.is_doubleclick `Left then
      (
        (* Double-click on directory name: send track view to playlist *)
        Table.deselect_all lib.artists;  (* deactivate possible inner filters *)
        Table.deselect_all lib.albums;
        Library.update_albums lib;
        let tracks = Array.map Track.make_from_data lib.tracks.entries in
        Playlist.replace_all pl tracks;
        Control.eject st.control;
        if tracks <> [||] then Control.switch st.control tracks.(0) true;
      )
    )

  | `Click None ->
    (* Click into empty space: deselect everything *)
    Library.deselect_dir lib;
    Library.deselect_all lib;
    Library.update_views lib;
    Library.focus_browser lib;
    Playlist.defocus pl;

  | `Move _ ->
    (* Drag or Cmd-cursor movement: adjust cursor *)
    if Api.Key.are_modifiers_down [] && Api.Mouse.is_drag `Left then
    (
      (* Actual drag *)
      Api.Mouse.set_cursor win
        (if
          Api.inside m (Ui.dim lay.ui (Layout.browser_area lay)) ||
          Api.inside m (Ui.dim lay.ui (Layout.playlist_area lay))
        then `Point else `Blocked)
    );

  | `Drop ->
    (* Drag & drop originating from tracks *)
    let (_, y, _, _) as r = Ui.dim lay.ui (Layout.playlist_area lay) in
    if Api.inside m r then
    (
      (* Drag & drop onto playlist: send directory contents to playlist *)
      let tracks = Array.map Track.make_from_data lib.tracks.entries in
      let len = Playlist.length pl in
      let pos = min len ((my - y) / lay.text + pl.table.vscroll) in
      Playlist.insert pl pos tracks;
      Library.deselect_all lib;
      Playlist.deselect_all pl;
      Playlist.select pl pos (pos + Array.length tracks - 1);
      Control.switch_if_empty st.control (Playlist.current_opt pl);
    )
  );

  (* Browser drag & drop *)
  let dropped = Layout.browser_drop lay in
  if dropped <> [] then
  (
    let _, my = Api.Mouse.pos win in
    let _, y, _, _ = Ui.dim lay.ui (Layout.browser_area lay) in
    let pos = min len ((my - y) / lay.text + browser.vscroll) in
    let rec find_root_pos i j =
      if i = pos then j else
      find_root_pos (i + 1) (if browser.entries.(i).nest = 0 then j + 1 else j)
    in
    if Library.add_roots lib dropped (find_root_pos 0 0) then
      Library.update_views lib
    else
      Layout.browser_error_box lay;  (* flash *)
  );

  (* Keys *)
  if Layout.del_key lay then
  (
    match Library.selected_dir lib with
    | Some i when browser.entries.(i).parent = Some "" ->
      Library.remove_roots lib [browser.entries.(i).path];
      Library.update_views lib
    | _ -> Layout.browser_error_box lay;  (* flash *)
  );

  (* Scanning indicator *)
  Layout.scan_label lay;
  Layout.scan_indicator lay (Library.rescan_busy lib);
  if Layout.scan_button lay && not (Library.rescan_busy lib) then
  (
    (* Inactive scanning indicator clicked: rescan *)
    let mode = if Api.Key.is_modifier_down `Shift then `Thorough else `Fast in
    Library.rescan_roots lib mode;
  );

  (* Browse modes *)
  let have_dir = lib.current <> None in
  let dir = Option.value lib.current ~default: browser.entries.(0) in

  let artists = have_dir && dir.artists_shown in
  Layout.artists_label lay;
  Layout.artists_indicator lay artists;
  let artists' =
    Layout.artists_button lay (if have_dir then Some artists else None) in
  if have_dir && artists' <> artists then
  (
    (* Click on Artists button: toggle artist pane *)
    dir.artists_shown <- artists';
    if not (artists' || dir.albums_shown || dir.tracks_shown) then
      dir.tracks_shown <- true;
    Library.update_dir lib dir;
  );

  let albums = have_dir && dir.albums_shown in
  Layout.albums_label lay;
  Layout.albums_indicator lay albums;
  let albums' =
    Layout.albums_button lay (if have_dir then Some albums else None) in
  if have_dir && albums' <> albums then
  (
    (* Click on Albums button: toggle artist pane *)
    dir.albums_shown <- albums';
    if not (albums' || dir.artists_shown || dir.tracks_shown) then
      dir.tracks_shown <- true;
    Library.update_dir lib dir;
  );

  let tracks = have_dir && dir.tracks_shown in
  Layout.tracks_label lay;
  Layout.tracks_indicator lay tracks;
  let tracks' =
    Layout.tracks_button lay (if have_dir then Some tracks else None) in
  if have_dir && tracks' <> tracks then
  (
    (* Click on Tracks button: toggle artist pane *)
    dir.tracks_shown <- tracks';
    if not (tracks' || dir.artists_shown || dir.albums_shown) then
      dir.artists_shown <- true;
    Library.update_dir lib dir;
  );

  let show_artists = have_dir && dir.artists_shown && lay.playlist_shown in
  let show_albums = have_dir && dir.albums_shown && lay.playlist_shown in
  let show_tracks = not have_dir || dir.tracks_shown || not lay.playlist_shown in
  lay.right_shown <- show_artists && show_albums;
  lay.lower_shown <- show_tracks && (show_artists || show_albums);


  (* Error pane *)

  Layout.info_pane lay;

  Layout.error_box lay;
  let now = Unix.gettimeofday () in
  if now -. lib.error_time < 10.0 then
    Layout.error_text lay (Ui.error_color lay.ui) `Regular true
      lib.error;


  (* Artists view *)

  if show_artists then
  (
    let artists_pane, artists_area, artists_table = Layout.left_view in
    artists_pane lay;

    let tab = lib.artists in
    let cols =
      Array.map (fun (attr, cw) -> cw, Library.attr_align attr) dir.artists_columns
    and headings =
      Array.map (fun (attr, _) -> Library.attr_name attr) dir.artists_columns
    in

    let pp_row i =
        let artist = tab.entries.(i) in
        Ui.text_color lay.ui,
        Array.map (fun (attr, _) -> Library.artist_attr_string artist attr)
          dir.artists_columns
    in

    let selected = tab.selected in
    (match artists_table lay cols (Some headings) tab pp_row with
    | `None | `Scroll -> ()

    | `Select ->
      (* TODO: allow multiple selections *)
      if Table.num_selected tab > 1 then
        tab.selected <- selected;  (* override *)
      Library.update_albums lib;

    | `Sort i ->
      (* Click on column header: reorder view accordingly *)
      let attr, order = dir.artists_sorting in
      let attr' = fst dir.artists_columns.(i) in
      let order' = if attr' = attr then Data.rev_order order else `Asc in
      dir.artists_sorting <- attr', order';
      Library.update_dir lib dir;
      Library.reorder_artists lib;

    | `Arrange ->
      (* Column resizing: update column widths *)
      Array.mapi_inplace (fun i (a, _) -> a, fst cols.(i)) dir.artists_columns;
      if have_dir then Library.update_dir lib dir;

    | `Click (Some _i) when Api.Mouse.is_doubleclick `Left ->
      (* Double-click on track: clear playlist and send tracks to it *)
      Table.deselect_all lib.albums;  (* deactivate possible inner filter *)
      Library.update_tracks lib;
      let tracks = Array.map Track.make_from_data lib.tracks.entries in
      Playlist.replace_all pl tracks;
      Control.eject st.control;
      if tracks <> [||] then Control.switch st.control tracks.(0) true;

    | `Click _ ->
      (* Single-click: grab focus *)
      (* TODO: allow multiple selections *)
      if Table.num_selected tab > 1 then
        tab.selected <- selected;  (* override *)
      Library.update_albums lib;
      Library.focus_artists lib;
      Playlist.defocus pl;
      Playlist.deselect_all pl;

    | `Move _ ->
      (* Drag or Cmd-cursor movement: adjust cursor *)
      if Api.Key.are_modifiers_down [] && Api.Mouse.is_drag `Left then
      (
        (* Actual drag *)
        Api.Mouse.set_cursor win
          (if
            Api.inside m (Ui.dim lay.ui (artists_area lay)) ||
            Api.inside m (Ui.dim lay.ui (Layout.playlist_area lay))
          then `Point else `Blocked)
      );

    | `Drop ->
      (* Drag & drop originating from tracks *)
      let (_, y, _, _) as r = Ui.dim lay.ui (Layout.playlist_area lay) in
      if Api.inside m r then
      (
        (* Drag & drop onto playlist: send selection to playlist *)
        let tracks = lib.tracks.entries in
        let len = Playlist.length pl in
        let pos = min len ((my - y) / lay.text + pl.table.vscroll) in
        Playlist.insert pl pos (Array.map Track.make_from_data tracks);
        Library.defocus lib;
        Library.deselect_all lib;
        Playlist.focus pl;
        Playlist.deselect_all pl;
        Playlist.select pl pos (pos + Array.length tracks - 1);
        Control.switch_if_empty st.control (Playlist.current_opt pl);
      )
    );
  );


  (* Albums view *)

  if show_albums then
  (
    let albums_pane, albums_area, albums_table =
      Layout.(if lay.right_shown then right_view else left_view) in
    albums_pane lay;

    let tab = lib.albums in
    let cols =
      Array.map (fun (attr, cw) -> cw, Library.attr_align attr) dir.albums_columns
    and headings =
      Array.map (fun (attr, _) -> Library.attr_name attr) dir.albums_columns
    in

    let pp_row i =
        let album = tab.entries.(i) in
        Ui.text_color lay.ui,
        Array.map (fun (attr, _) -> Library.album_attr_string album attr)
          dir.albums_columns
    in

    let selected = tab.selected in
    (match albums_table lay cols (Some headings) tab pp_row with
    | `None | `Scroll -> ()

    | `Select ->
      (* TODO: allow multiple selections *)
      if Table.num_selected tab > 1 then
        tab.selected <- selected;  (* override *)
      Library.update_tracks lib;

    | `Sort i ->
      (* Click on column header: reorder view accordingly *)
      let attr, order = dir.albums_sorting in
      let attr' = fst dir.albums_columns.(i) in
      let order' = if attr' = attr then Data.rev_order order else `Asc in
      dir.albums_sorting <- attr', order';
      Library.update_dir lib dir;
      Library.reorder_albums lib;

    | `Arrange ->
      (* Column resizing: update column widths *)
      Array.mapi_inplace (fun i (a, _) -> a, fst cols.(i)) dir.albums_columns;
      if have_dir then Library.update_dir lib dir;

    | `Click (Some _i) when Api.Mouse.is_doubleclick `Left ->
      (* Double-click on track: clear playlist and send tracks to it *)
      let tracks = Array.map Track.make_from_data lib.tracks.entries in
      Playlist.replace_all pl tracks;
      Control.eject st.control;
      if tracks <> [||] then Control.switch st.control tracks.(0) true;

    | `Click _ ->
      (* Single-click: grab focus *)
      (* TODO: allow multiple selections *)
      if Table.num_selected tab > 1 then
        tab.selected <- selected;  (* override *)
      Library.update_tracks lib;
      Library.focus_albums lib;
      Playlist.defocus pl;
      Playlist.deselect_all pl;

    | `Move _ ->
      (* Drag or Cmd-cursor movement: adjust cursor *)
      if Api.Key.are_modifiers_down [] && Api.Mouse.is_drag `Left then
      (
        (* Actual drag *)
        Api.Mouse.set_cursor win
          (if
            Api.inside m (Ui.dim lay.ui (albums_area lay)) ||
            Api.inside m (Ui.dim lay.ui (Layout.playlist_area lay))
          then `Point else `Blocked)
      );

    | `Drop ->
      (* Drag & drop originating from tracks *)
      let (_, y, _, _) as r = Ui.dim lay.ui (Layout.playlist_area lay) in
      if Api.inside m r then
      (
        (* Drag & drop onto playlist: send selection to playlist *)
        let tracks = lib.tracks.entries in
        let len = Playlist.length pl in
        let pos = min len ((my - y) / lay.text + pl.table.vscroll) in
        Playlist.insert pl pos (Array.map Track.make_from_data tracks);
        Library.defocus lib;
        Library.deselect_all lib;
        Playlist.focus pl;
        Playlist.deselect_all pl;
        Playlist.select pl pos (pos + Array.length tracks - 1);
        Control.switch_if_empty st.control (Playlist.current_opt pl);
      )
    );

    (* Divider *)
    if lay.right_shown then
    (
      let left_width' = Layout.right_divider lay lay.left_width
        (Layout.left_min lay) (Layout.left_max lay) in
      (* Possible drag of divider: update pane width *)
      lay.left_width <- left_width';
    );
  );


  (* Tracks view *)

  if show_tracks then
  (
    let tracks_pane, tracks_area, tracks_table =
      Layout.(if lay.lower_shown then lower_view else left_view) in
    tracks_pane lay;

    let tab = lib.tracks in
    let cols =
      Array.map (fun (attr, cw) -> cw, Library.attr_align attr) dir.tracks_columns
    and headings =
      Array.map (fun (attr, _) -> Library.attr_name attr) dir.tracks_columns
    and current =
      match st.control.current with Some track -> track.path | None -> ""
    in

    let pp_row i =
        let track = tab.entries.(i) in
        let c =
          match track.status with
          | _ when track.path = current -> `White
          | `Absent -> Ui.error_color lay.ui
          | `Invalid -> Ui.warn_color lay.ui
          | `Undet -> Ui.error_color lay.ui
          | `Predet | `Det -> Ui.text_color lay.ui
        in
        c,
        Array.map (fun (attr, _) -> Library.track_attr_string track attr)
          dir.tracks_columns
    in

    (match tracks_table lay cols (Some headings) tab pp_row with
    | `None | `Select | `Scroll -> ()

    | `Sort i ->
      (* Click on column header: reorder view accordingly *)
      let attr, order = dir.tracks_sorting in
      let attr' = fst dir.tracks_columns.(i) in
      let order' = if attr' = attr then Data.rev_order order else `Asc in
      dir.tracks_sorting <- attr', order';
      Library.update_dir lib dir;
      Library.reorder_tracks lib;

    | `Arrange ->
      (* Column resizing: update column widths *)
      Array.mapi_inplace (fun i (a, _) -> a, fst cols.(i)) dir.tracks_columns;
      if have_dir then Library.update_dir lib dir;

    | `Click (Some i) when Api.Mouse.is_doubleclick `Left ->
      (* Double-click on track: clear playlist and send tracks to it *)
      let tracks =
        if Api.Key.are_modifiers_down [`Command]
        then Library.selected lib
        else [|tab.entries.(i)|]
      in
      Playlist.replace_all pl (Array.map Track.make_from_data tracks);
      Control.eject st.control;
      Control.switch st.control (Playlist.current pl) true;

    | `Click _ ->
      (* Single-click: grab focus *)
      Library.focus_tracks lib;
      Playlist.defocus pl;
      Playlist.deselect_all pl;

    | `Move _ ->
      (* Drag or Cmd-cursor movement: adjust cursor *)
      if Api.Key.are_modifiers_down [] && Api.Mouse.is_drag `Left then
      (
        (* Actual drag *)
        Api.Mouse.set_cursor win
          (if
            Api.inside m (Ui.dim lay.ui (tracks_area lay)) ||
            Api.inside m (Ui.dim lay.ui (Layout.playlist_area lay))
          then `Point else `Blocked)
      );

    | `Drop ->
      (* Drag & drop originating from tracks *)
      let (_, y, _, _) as r = Ui.dim lay.ui (Layout.playlist_area lay) in
      if Api.inside m r then
      (
        (* Drag & drop onto playlist: send selection to playlist *)
        let tracks = Library.selected lib in
        let len = Playlist.length pl in
        let pos = min len ((my - y) / lay.text + pl.table.vscroll) in
        Playlist.insert pl pos (Array.map Track.make_from_data tracks);
        Library.defocus lib;
        Library.deselect_all lib;
        Playlist.focus pl;
        Playlist.deselect_all pl;
        Playlist.select pl pos (pos + Array.length tracks - 1);
        Control.switch_if_empty st.control (Playlist.current_opt pl);
      )
    );

    (* Divider *)
    if lay.lower_shown then
    (
      let upper_height' = Layout.lower_divider lay lay.upper_height
        (Layout.upper_min lay) (Layout.upper_max lay) in
      (* Possible drag of divider: update pane width *)
      lay.upper_height <- upper_height';
    );
  );

  (* Pane divider *)

  let browser_width' = Layout.browser_divider lay lay.browser_width
    (Layout.browser_min lay) (Layout.browser_max lay) in
  (* Possible drag of divider: update pane width *)
  lay.browser_width <- browser_width'


(* Runner *)

let rec run (st : State.t) =
  State.ok st;
  let lay = st.layout in
  let win = Ui.window lay.ui in
  if Api.Window.closed win then exit 0;

  (* Start drawing *)
  Ui.start lay.ui;

  (* Update geometry *)
  let ww, wh = Api.Window.size win in
  if lay.playlist_shown then lay.playlist_height <- wh - Layout.control_h lay;
  if lay.library_shown then lay.library_width <- ww - Layout.control_w lay;

  (* Remember current geometry for later *)
  let playlist_shown = lay.playlist_shown in
  let library_shown = lay.library_shown in
  let library_side = lay.library_side in
  let library_width = lay.library_width in

  (* Run panes *)
  run_control st;
  if not (Api.Window.is_minimized win) then
  (
    if playlist_shown then run_playlist st;
    if library_shown then run_library st;
  );
  run_toggle_panes st;

  (* Adjust font size *)
  let text' = lay.text +
    (if Layout.enlarge_key lay then +1 else 0) +
    (if Layout.reduce_key lay then -1 else 0)
  in
  lay.text <- clamp 8 64 text';

  (* Adjust window size *)
  Api.Window.set_size win
    (Layout.control_w lay + (if lay.library_shown then lay.library_width else 0))
    (Layout.control_h lay + (if lay.playlist_shown then lay.playlist_height else 0));

  (* Adjust window position after opening/closing library *)
  let dx =
    match library_shown, lay.library_shown, library_side, lay.library_side with
    | false, true, _, `Left
    | true, true, `Right, `Left -> -lay.library_width  (* opened on the left *)
    | true, false, `Left, _
    | true, true, `Left, `Right -> +library_width      (* closed on the left *)
    | _ -> 0
  in
  let x, y = Api.Window.pos win in
  if dx <> 0 then Api.Window.set_pos win (x + dx) y;

  (* Finish drawing *)
  let minw, maxw =
    if library_shown
    then Layout.(control_w lay + library_min lay, -1)
    else Layout.(control_w lay, control_w lay)
  and minh, maxh =
    if playlist_shown
    then Layout.(control_h lay + playlist_min lay, -1)
    else Layout.(control_h lay, control_h lay)
  in
  Ui.finish lay.ui (Layout.margin lay) (minw, minh) (maxw, maxh);

  run st


(* Startup *)

let startup () =
  Storage.clear_temp ();
  let db = Db.init () in
  let win = Api.Window.init 0 0 Layout.control_min_w Layout.control_min_h App.name in
  let ui = Ui.make win in
  let audio = Api.Audio.init () in
  let rst = ref (State.make ui audio db) in
  let st = if State.load !rst then !rst else State.make ui audio db in
  Playlist.focus st.playlist;
  at_exit (fun () -> State.save st; Storage.clear_temp (); Db.exit db);
  st

let _main =
  try
    Printexc.record_backtrace true;
    let st = startup () in
    run st
  with exn ->
    prerr_endline ("internal error: " ^ Printexc.to_string exn);
    Printexc.print_backtrace stderr;
    Stdlib.exit 2
