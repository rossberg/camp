(* Main Program *)

open Audio_file


(* Helpers *)

let refresh_delay = 9

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

let run_control (st : _ State.t) =
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
  let length = Api.Audio.length ctl.audio in
  let elapsed = Api.Audio.played ctl.audio in
  let remaining = length -. elapsed in
  let playing = Api.Audio.is_playing ctl.audio in
  let paused = not playing && elapsed > 0.0 in
  let stopped = not playing && not paused in
  let focus = pl.table.focus || not (lay.library_shown || lay.filesel_shown) in

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
  if dcol <> 0 then
  (
    (* Click on color button: cycle color palette *)
    Ui.set_palette lay.ui ((Ui.get_palette lay.ui + dcol + ncol) mod ncol);
    Table.dirty pl.table;
    Table.dirty st.library.browser;
    Table.dirty st.library.artists;
    Table.dirty st.library.albums;
    Table.dirty st.library.tracks;
  );

  (* Cover *)
  if ctl.cover then
  (
    Option.iter (fun (track : Data.track) ->
      Option.iter (fun img ->
        let x, y, w, h = Ui.dim lay.ui (Layout.cover_area lay) in
        let iw, ih = Api.Image.size img in
        let q = float w /. float h in
        let iq = float iw /. float ih in
        let ih' = int_of_float (float ih *. iq /. q) in
        Api.Draw.image_part win x y w h 0 0 iw ih' img;
      ) (Library.load_cover st.library win track.path)
    ) ctl.current
  );
  if Layout.cover_key lay then ctl.cover <- not ctl.cover;

  (* FPS *)
  if ctl.fps then
    Layout.fps_text lay `Regular true (fmt "%d FPS" (Api.Window.fps win));
  (* Press of FPS key: toggle FPS display *)
  if Layout.fps_key lay then ctl.fps <- not ctl.fps;

  (* Audio properties *)
  if not silence then
  (
    let track = Option.get ctl.current in
    let ext = File.extension track.path in
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
    | Some track when not (Data.is_separator track) ->
      Track.name track ^ " - " ^ fmt_time (Track.time track)
    | _ -> App.(name ^ " " ^ version)
  in
  Layout.title_ticker lay name;

  (* Volume control *)
  let mute_mouse = Ui.mouse_inside lay.ui (Layout.mute_area lay) in
  let vol_mouse = Layout.volume_bar lay ctl.volume in
  let volume' =
    (if mute_mouse then ctl.volume else vol_mouse) +.
    0.05 *. snd (Layout.volume_wheel lay) +.
    0.05 *. (float_of_bool (Layout.volup_key lay focus) -.
    float_of_bool (Layout.voldown_key lay focus))
  in
  Layout.mute_text lay (Ui.error_color lay.ui) `Inverted ctl.mute "MUTE";
  let mute' =
    if Layout.mute_button lay || Layout.mute_key lay focus
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
    Api.Audio.volume ctl.audio (if ctl.mute then 0.0 else ctl.volume);
  );

  (* Seek bar *)
  let progress =
    if length > 0.0 && not silence then elapsed /. length else 0.0 in
  let progress' =
    Layout.seek_bar lay progress +.
    0.05 *. float_of_bool (Layout.ff_key lay focus) -.
    0.05 *. float_of_bool (Layout.rw_key lay focus)
  in
  if (progress' <> ctl.progress || Api.Mouse.is_pressed `Left)
  && progress' <> progress && not silence then
  (
    (* Click or drag on seek bar at new position: reposition audio *)
    Control.seek ctl (clamp 0.0 1.0 progress');
    ctl.progress <- progress';
  );
(*
  let s1 = fmt_time2 elapsed in
  let s2 = "-" ^ fmt_time2 remaining in
  let w2 = Api.Draw.text_width win 11 (Ui.font lay.ui 11) s2 in
  Api.Draw.text win 14 91 11 `White (Ui.font lay.ui 11) s1;
  Api.Draw.text win (278 - w2) 91 11 `White (Ui.font lay.ui 11) s2;
*)

  (* Mouse reflection *)
  Layout.info_refl lay;

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
  let bwd = if Layout.bwd_button lay focus (Some false) then -1 else 0 in
  let fwd = if Layout.fwd_button lay focus (Some false) then +1 else 0 in
  let off = if len = 0 then 0 else bwd + fwd in
  if off <> 0 then
  (
    (* Click on one of the skip buttons: jump to track *)
    let more = Playlist.skip pl off (ctl.repeat <> `None) in
    Control.switch ctl (Playlist.current pl) more;
    Playlist.adjust_scroll pl page;
    Table.dirty st.library.tracks;
    Table.dirty st.library.browser;
  );

  let playing' = Layout.play_button lay focus (Some playing) in
  if stopped && playing' && len > 0 then
  (
    (* Click on play button: start track *)
    Control.switch ctl (Playlist.current pl) true;
    Playlist.adjust_scroll pl page;
    Table.dirty st.library.tracks;
    Table.dirty st.library.browser;
  );

  let paused' = Layout.pause_button lay focus (Some paused) in
  if playing' && paused' then
  (
    (* Click on pause button when playing: pause track *)
    Api.Audio.pause ctl.audio;
  )
  else if (not stopped && not paused' || stopped && paused') && not silence then
  (
    (* Click on pause button when paused: resume track *)
    Api.Audio.resume ctl.audio;
  );

  if Layout.stop_button lay focus (Some false) && not stopped then
  (
    (* Click on stop button when playing: stop track *)
    Api.Audio.pause ctl.audio;
    (match Playlist.current_opt pl with
    | None -> Control.eject ctl
    | Some track -> Control.switch ctl track false
    );
    Playlist.adjust_scroll pl page;
    Table.dirty st.library.tracks;
    Table.dirty st.library.browser;
  );

  if Layout.eject_button lay focus (Some false) then
  (
    (* Click on eject button: stop and clear playlist *)
    Control.eject ctl;
    Playlist.remove_all pl;
    Table.dirty st.library.tracks;
    Table.dirty st.library.browser;
  );

  if Layout.start_stop_key lay focus then
  (
    (* Press of space key: pause or resume *)
    if playing then
      Api.Audio.pause ctl.audio
    else if paused then
      Api.Audio.resume ctl.audio
    else if stopped && len > 0 then
    (
      Control.switch ctl (Playlist.current pl) true;
      Table.dirty st.library.tracks;
      Table.dirty st.library.browser;
    );
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
    Table.dirty st.library.tracks;
    Table.dirty st.library.browser;
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
      (
        Control.switch ctl (Playlist.current pl) false;
        Table.dirty st.library.tracks;
        Table.dirty st.library.browser;
      );
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

let run_toggle_panes (st : _ State.t) =
  let lay = st.layout in
  let win = Ui.window lay.ui in

  Layout.playlist_label lay;
  Layout.playlist_indicator lay lay.playlist_shown;
  let playlist_shown' = Layout.playlist_button lay (Some lay.playlist_shown) in
  (* Click on playlist activation button: toggle playlist *)
  lay.playlist_shown <- playlist_shown';
  if not playlist_shown' then Playlist.defocus st.playlist;

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
    (* Library was off: show; switch side if window is at respective border *)
    if not lay.filesel_shown then
    (
      if lay.library_side = `Left && wx <= sx then
        lay.library_side <- `Right;
      if lay.library_side = `Right && wx + Layout.control_w lay >= sx + sw then
        lay.library_side <- `Left;
    );
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
    (* Otherwise: keep or toggle *)
    lay.library_shown <- library_shown';
    if not library_shown' then Library.defocus st.library;
  );

  if not (library_shown' || lay.filesel_shown) then State.focus_playlist st;

  (* Minimize button *)
  if Layout.minimize_button lay then
  (
    (* Right-click on power button: minimize window *)
    Api.Window.minimize win
  )


(* Generic Handling of Track Views *)

let expand_paths lib paths =
  let tracks = ref [] in
  let add_track (track : Data.track) =
    tracks := track :: !tracks
  in
  let add_playlist path =
    let s = File.load `Bin path in
    List.iter (fun item -> add_track (Track.of_m3u_item item)) (M3u.parse_ext s)
  in
  let add_viewlist path =
    let s = File.load `Bin path in
    match Query.parse_query s with
    | Error msg -> Library.error lib msg
    | Ok _query -> ()  (* TODO
      List.iter add_track (Library.) *)
  in
  let rec add_path path =
    try
      if File.exists_dir path then
        Array.iter (fun file ->
          add_path File.(path // file)
        ) (File.read_dir path)
      else if Data.is_playlist_path path then
        add_playlist path
      else if Data.is_viewlist_path path then
        add_viewlist path
      else if Data.is_track_path path then
        add_track (Data.make_track path)
    with Sys_error _ -> ()
  in
  List.iter add_path paths;
  Array.of_list (List.rev !tracks)


module type TracksView =  (* target view for edit ops *)
sig
  open Data

  type 'cache t

  val it : Ui.cached t
  val focus : (track, Ui.cached) Table.t -> Ui.cached State.t -> unit

(*  val length : Ui.cached t -> int*)
  val tracks : Ui.cached t -> track array
  val table : Ui.cached t -> (track, Ui.cached) Table.t

  val num_selected : Ui.cached t -> int
  val first_selected : Ui.cached t -> int option
  val selected : Ui.cached t -> track array
(*  val select_all : Ui.cached t -> unit*)
  val deselect_all : Ui.cached t -> unit
(*  val select_invert : Ui.cached t -> unit*)
(*  val select : Ui.cached t -> int -> int -> unit*)
(*  val deselect : Ui.cached t -> int -> int -> unit*)

  val insert : Ui.cached t -> int -> track array -> unit
(*  val replace_all : Ui.cached t -> track array -> unit*)
(*  val remove_all : Ui.cached t -> unit*)
  val remove_selected : Ui.cached t -> unit
  val remove_unselected : Ui.cached t -> unit
  val remove_invalid : Ui.cached t -> unit
(*  val move_selected : Ui.cached t -> int -> unit*)
  val undo : Ui.cached t -> unit
  val redo : Ui.cached t -> unit
end

module Playlist = struct include Playlist let focus _ = State.focus_playlist end
module Library = struct include Library let focus = State.focus_library end


let drop (st : _ State.t) tracks table_mouse (module View : TracksView) =
  if tracks <> [||] then
  (
    let lay = st.layout in
    let view = View.it in
    let tab = View.table view in
    Option.iter (fun pos ->
      (* Drag & drop onto table: send tracks there *)
      View.insert view pos tracks;
      State.defocus_all st;
      View.focus tab st;
      Control.switch_if_empty st.control (Playlist.current_opt st.playlist);
      Table.dirty st.library.tracks;
      Table.dirty st.library.browser;
    ) (table_mouse lay tab)
  )

let drop_on_playlist (st : _ State.t) tracks =
  if st.layout.playlist_shown then
  (
    let module View = struct let it = st.playlist include Playlist end in
    drop st tracks Layout.playlist_mouse (module View);
  )

let library_mouse (lay : Layout.t) =
  if lay.lower_shown then Layout.lower_mouse lay else
  if lay.right_shown then Layout.right_mouse lay else Layout.left_mouse lay

let drop_on_library (st : _ State.t) tracks =
  if st.layout.library_shown && Library.current_is_shown_playlist st.library then
  (
    let module View = struct let it = st.library include Library end in
    drop st tracks library_mouse (module View)
  )

let drop_on_browser (st : _ State.t) tracks =
  let lay = st.layout in
  let lib = st.library in
  let browser = lib.browser in
  if st.layout.library_shown then
  (
    Option.iter (fun i ->
      if i < Array.length browser.entries then
      (
        let dir = browser.entries.(i) in
        if Data.is_playlist dir then
        (
          (* Drag & drop onto playlist browser entry: send tracks there *)
          (* Since the dir might not be selected, and updating views is
           * asynchronous, write to file directly *)
          (try
            let s = Track.to_m3u tracks in
            let s' = File.load `Bin dir.path in
            File.store `Bin dir.path (s' ^ s)
          with exn ->
            Storage.log_exn "file" exn ("modifying playlist " ^ dir.path)
          );
          if Library.selected_dir lib = Some i then
          (
            Library.deselect_dir lib;  (* force reload *)
            Library.select_dir lib i;
            Library.refresh_artists_albums_tracks lib;
          );
        )
      )
    ) (Layout.browser_mouse lay browser)
  )

let set_drop_cursor (st : _ State.t) =
  let lay = st.layout in
  let pl = st.playlist in
  let lib = st.library in
  let droppable =
    lay.playlist_shown && Layout.playlist_mouse lay pl.table <> None ||
    lay.library_shown && (
      Library.current_is_shown_playlist lib &&
        library_mouse lay lib.tracks <> None ||
      match Layout.browser_mouse lay lib.browser with
      | Some i ->
        i < Table.length lib.browser && Data.is_playlist lib.browser.entries.(i)
      | None -> false
    )
  in
  Api.Mouse.set_cursor (Ui.window lay.ui)
    (if droppable then `Point else `Blocked)


(* Edit Pane *)

let run_edit (st : _ State.t) =
  let pl = st.playlist in
  let lib = st.library in
  let lay = st.layout in
  let win = Ui.window lay.ui in

  Layout.edit_pane lay;

  let pl_focus = pl.table.focus in
  let lib_focus = lib.tracks.focus || lib.albums.focus || lib.artists.focus ||
    lib.browser.focus in
  let pl_edit = pl_focus in
  let lib_edit = lib_focus && Library.current_is_shown_playlist lib in

  assert (not (pl_focus && lib_focus));
  assert (not (pl_edit && lib_edit));
  assert (lay.playlist_shown || not pl_focus);
  assert (lay.library_shown || not lib_focus);

  let pl_len = Playlist.length pl in
  let lib_len = Library.length lib in
  let pl_sel = Playlist.num_selected pl in
  let lib_sel = Library.num_selected lib in

  let playlist = (module struct let it = pl include Playlist end : TracksView) in
  let library = (module struct let it = lib include Library end : TracksView) in
  let view, other = if pl_focus then playlist, library else library, playlist in
  let module View = (val view) in
  let module Other = (val other) in
  let view = View.it in
  let other = Other.it in

  (* Separator button *)
  let sep_avail = pl_edit || lib_edit in
  if Layout.sep_button lay (if sep_avail then Some false else None) then
  (
    (* Click on Separator button: insert separator *)
    let pos = Option.value (View.first_selected view) ~default: 0 in
    View.insert view pos [|Data.make_separator ()|];
    Other.deselect_all other;
    Control.switch_if_empty st.control (Playlist.current_opt pl);
    Table.dirty st.library.tracks;
    Table.dirty st.library.browser;
  );

  (* Edit buttons *)
  let pl_del_avail = pl_edit && pl_sel > 0 in
  let lib_del_avail = lib_edit && lib_sel > 0 in
  let del_avail = pl_del_avail || lib_del_avail in
  if Layout.del_button lay (if del_avail then Some false else None)
  || del_avail && Layout.del_button_alt lay then
  (
    (* Click on Delete button: remove selected tracks from playlist *)
    View.remove_selected view;
  );

  let pl_crop_avail = pl_edit && pl_sel < pl_len in
  let lib_crop_avail = lib_edit && lib_sel < lib_len in
  let crop_avail = pl_crop_avail || lib_crop_avail in
  if Layout.crop_button lay (if crop_avail then Some false else None)
  || crop_avail && Layout.crop_button_alt lay then
  (
    (* Click on Crop button: remove unselected tracks from playlist *)
    View.remove_unselected view;
  );

  let pl_clean_avail = pl_edit && snd pl.total > 0 in
  let lib_clean_avail = lib_edit (* TODO: && snd pl.total > 0 *) in
  let clean_avail = pl_clean_avail || lib_clean_avail in
  if Layout.clean_button lay (if clean_avail then Some false else None)
  || clean_avail && Layout.clean_button_alt lay then
  (
    (* Click on Clean button: remove invalid tracks from playlist *)
    View.remove_invalid view;
  );

  let pl_undo_avail = pl_edit && !(pl.table.undos) <> [] in
  let lib_undo_avail = lib_edit && !(lib.tracks.undos) <> [] in
  let undo_avail = pl_undo_avail || lib_undo_avail in
  if Layout.undo_button lay (if undo_avail then Some false else None) then
  (
    (* Click on Undo button: pop undo *)
    View.undo view;
    Control.switch_if_empty st.control (Playlist.current_opt pl);
    Table.dirty st.library.tracks;
    Table.dirty st.library.browser;
  );

  let pl_redo_avail = pl_edit && !(pl.table.redos) <> [] in
  let lib_redo_avail = lib_edit && !(lib.tracks.redos) <> [] in
  let redo_avail = pl_redo_avail || lib_redo_avail in
  if Layout.redo_button lay (if redo_avail then Some false else None) then
  (
    (* Click on Redo button: pop redo *)
    View.redo view;
    Control.switch_if_empty st.control (Playlist.current_opt pl);
    Table.dirty st.library.tracks;
    Table.dirty st.library.browser;
  );

  (* Edit keys *)
  let pl_cut_avail = pl_edit && pl_sel > 0 in
  let lib_cut_avail = lib_edit && lib_sel > 0 in
  let cut_avail = pl_cut_avail || lib_cut_avail in
  if cut_avail && Layout.cut_key lay then
  (
    (* Press of Cut key: remove selected tracks and write them to clipboard *)
    let s = Track.to_m3u (View.selected view) in
    View.remove_selected view;
    Api.Clipboard.write win s;
  );

  let pl_copy_avail = pl_focus && pl_sel > 0 in
  let lib_copy_avail = lib_focus && lib_sel > 0 in
  let copy_avail = pl_copy_avail || lib_copy_avail in
  if copy_avail && Layout.copy_key lay then
  (
    (* Press of Copy key: write selected tracks to clipboard *)
    let s = Track.to_m3u (View.selected view) in
    Api.Clipboard.write win s;
  );

  let pl_paste_avail = pl_edit in
  let lib_paste_avail = lib_edit in
  let paste_avail = pl_paste_avail || lib_paste_avail in
  if paste_avail && Layout.paste_key lay then
  (
    (* Press of Paste key: insert tracks from clipboard *)
    match Api.Clipboard.read win with
    | None -> ()
    | Some s ->
      let tracks = Track.of_m3u s in
      let found_proper =
        Array.exists (fun (track : Data.track) ->
          Data.is_track_path track.path
        ) tracks
      in
      if found_proper && tracks <> [||] then
      (
        let pos = Option.value (View.first_selected view) ~default: 0 in
        View.insert view pos tracks;
        Other.deselect_all other;
        Control.switch_if_empty st.control (Playlist.current_opt pl);
        Table.dirty st.library.tracks;
        Table.dirty st.library.browser;
      )
  );

  (* Tag button *)
  let exec_avail =
    try Unix.(access st.config.exec_tag [X_OK]); true
    with Unix.Unix_error _ -> false
  in
  let pl_tag_avail = pl_focus && pl_len > 0 in
  let lib_tag_avail = lib_focus && lib_len > 0 in
  let tag_avail = exec_avail && (pl_tag_avail || lib_tag_avail) in
  if Layout.tag_button lay (if tag_avail then Some false else None) then
  (
    (* Click on Tag button: execute tagging program *)
    let tracks =
      View.(if num_selected view > 0 then selected view else tracks view) in
    let paths = Array.map (fun (track : Data.track) -> track.path) tracks in
    (* Command-click: add tracks to tagger if it's already open *)
    let additive = Api.Key.is_modifier_down `Command in
    Domain.spawn (fun () ->
      let paths' =
        List.filter (fun p -> not (M3u.is_separator p)) (Array.to_list paths) in
      if st.config.exec_tag_max_len = 0 && not additive then
        exec st.config.exec_tag paths'
      else
      (
        (* Work around Windows command line limits *)
        let args = ref paths' in
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
        let max =
          if List.length paths' < 20 then 1 else st.config.exec_tag_max_len in
        if not additive then exec st.config.exec_tag (pick 0 max);
        List.iter (fun arg -> exec st.config.exec_tag ["/add"; arg]) !args;
      )
    ) |> ignore;
  );

  (* Save button *)
  let pl_save_avail = pl_focus in
  let lib_save_avail = lib_focus &&
    (match lib.current with
    | Some dir -> dir.view.tracks_shown <> None
    | None -> false
    )
  in
  let save_avail = not lay.filesel_shown && (pl_save_avail || lib_save_avail) in
  if Layout.save_button lay (if save_avail then Some false else None) then
  (
    (* Click on Save button: save playlist *)
    let tab = if pl_save_avail then st.playlist.table else st.library.tracks in
    st.filesel.op <- Some (`SavePlaylist tab);
    st.layout.filesel_shown <- true;
    Edit.set st.filesel.input ".m3u";
    Edit.move_begin st.filesel.input;
    State.defocus_all st;
    Filesel.focus_input st.filesel;
  );

  (* Load button *)
  let load_avail = pl_focus in
  if Layout.load_button lay (if load_avail then Some false else None) then
  (
    (* Click on Load button: load playlist *)
    st.filesel.op <- Some `LoadPlaylist;
    st.layout.filesel_shown <- true;
    Edit.set st.filesel.input ".m3u";
    Edit.move_begin st.filesel.input;
    State.defocus_all st;
    Filesel.focus_input st.filesel;
  );

  (* Focus buttons *)
  if Layout.focus_next_key lay then State.focus_next st;
  if Layout.focus_prev_key lay then State.focus_prev st


(* Playlist Pane *)

let run_playlist (st : _ State.t) =
  let pl = st.playlist in
  let lay = st.layout in
  let win = Ui.window lay.ui in
  let tab = pl.table in
  let len = Table.length tab in
  let now = Unix.time () in

  Layout.playlist_pane lay;

  (* Playlist table *)
  let _, _, _, h = Ui.dim lay.ui (Layout.playlist_area lay) in
  let page = max 1 (int_of_float (Float.floor (float h /. float lay.text))) in
  let digits_pos = log10 (len + 1) + 1 in
  let digits_time = ref 1 in
  for i = tab.vscroll to min len (tab.vscroll + page) - 1 do
    let time = Track.time tab.entries.(i) in
    if time > 599.4 then
      digits_time := max !digits_time (if time > 5999.4 then 3 else 2)
  done;
  let font = Ui.font lay.ui lay.text in
  let s_pos = String.make digits_pos '0' ^ "." in
  let s_time = String.make !digits_time '0' ^ ":00" in
  let cw_pos = Api.Draw.text_width win lay.text font s_pos + 1 in
  let cw_time = Api.Draw.text_width win lay.text font s_time + 1 in
  let cols = [|cw_pos, `Right; -1, `Left; cw_time, `Right|] in

  if Api.Draw.frame win mod refresh_delay = 0 then
    Table.dirty tab;  (* to capture track updates *)

  let pp_row i =
    let track = tab.entries.(i) in
    if now -. track.file.age > st.config.delay_track_update then
      Track.update track;
    let c =
      match track.status with
      | _ when tab.pos = Some i ->
        if track.path = (Option.get st.control.current).path then `White else `Gray 0xc0
      | _ when Data.is_separator track -> Ui.text_color lay.ui
      | `Absent -> Ui.error_color lay.ui
      | `Invalid -> Ui.warn_color lay.ui
      | `Undet -> Ui.semilit_color (Ui.text_color lay.ui)
      | `Predet | `Det -> Ui.text_color lay.ui
    in
    let time = Track.time track in
    let stime = if time = 0.0 then "" else fmt_time time in
    c, [|
      `Text (fmt "%0*d." digits_pos (i + 1));
      `Text (Track.name track);
      `Text stime
    |]
  in

  (match Layout.playlist_table lay cols None tab pp_row with
  | `None | `Scroll -> ()
  | `Sort _ | `Arrange -> assert false

  | `Select ->
    State.focus_playlist st;
    Playlist.refresh_total_selected pl

  | `Click (Some i) when Api.Mouse.is_doubleclick `Left ->
    (* Double-click on track: switch to track *)
    Table.set_pos tab (Some i);
    Control.switch st.control tab.entries.(i) true;
    Table.dirty st.library.tracks;
    Table.dirty st.library.browser;
    if pl.shuffle <> None then
      Playlist.shuffle_next pl i;

  | `Click _ ->
    (* Single-click: grab focus *)
    State.focus_playlist st;
    Playlist.refresh_total_selected pl;

  | `Move delta ->
    (* Cmd-cursor movement: move selection *)
    Playlist.move_selected pl delta;

  | `Drag (delta, way) ->
    (* Drag: move selection if inside *)
    if Api.Key.are_modifiers_down [] then
    (
      State.focus_playlist st;
      if Playlist.num_selected pl > 0 then set_drop_cursor st;

      (* Invariant:
       * - on Start: no undo or redo added yet
       * - when Inside: one undo for returning to original state on undo stack
       * - when Outside: one redo for creating new state on redo stack
       *)
      (match way with
      | `Start ->
        (* Start of drag & drop: remember original configuration *)
        Table.push_undo pl.table;
      | `Outward ->
        (* Leaving area: snap back to original state *)
        Playlist.undo pl;
        Playlist.save_playlist pl;
      | `Inward ->
        (* Reentering area: restore updated state *)
        Playlist.redo pl
      | `Inside | `Outside -> ()
      );

      if delta <> 0 && Playlist.num_selected pl > 0 then
      (
        match way with
        | `Start | `Inside | `Inward ->
          Playlist.move_selected pl delta;
          (* Erase intermediate new state *)
          Table.drop_undo pl.table;
        | `Outward ->
          (* Temporarily restore new state, modify, and immediately undo *)
          (* Restore new state *)
          Playlist.redo pl;
          Playlist.move_selected pl delta;
          (* Erase intermediate new state *)
          Table.drop_undo pl.table;
          (* Undo new state, recovering original *)
          Playlist.undo pl;
          Playlist.save_playlist pl;
        | `Outside -> ()
      )
    )

  | `Drop ->
    if not (Ui.mouse_inside lay.ui (Layout.playlist_area lay)) then
    (
      (* Dropping outside playlist: drop aux redo for new state *)
      Table.drop_redo pl.table;

      let tracks = Playlist.selected pl in
      drop_on_library st tracks;
      drop_on_browser st tracks;
    );
  );

  (* Playlist drag & drop *)
  let dropped = Api.Files.dropped win in
  if dropped <> [] then
  (
    (* Files drop: insert paths at pointed position *)
    drop_on_playlist st (expand_paths st.library dropped);
  );

  (* Playlist total *)
  if int_of_float (time ()) mod 10 = 0 then Playlist.refresh_total pl;
  let fmt_total (t, n) = fmt_time3 t ^ if n > 0 then "+" else "" in
  let s1 =
    if pl.total_selected = (0.0, 0) then "" else
    fmt_total pl.total_selected ^ "/"
  in
  let s2 = fmt_total pl.total in
  Layout.playlist_total_box lay;
  Layout.playlist_total_text lay `Regular true (s1 ^ s2)


(* Library Panes *)

let spin_delay = 3
let spins = [|"|"; "/"; "-"; "\\"|]
let spin win = spins.(Api.Draw.frame win / spin_delay mod Array.length spins)

let convert_sorting columns sorting =
  let index attr = Array.find_index (fun (a, _) -> a = attr) columns in
  List.map (fun (attr, order) -> Option.get (index attr), order) sorting

let busy_artists = Table.make 0
let busy_albums = Table.make 0
let busy_tracks = Table.make 0


let run_library (st : _ State.t) =
  let pl = st.playlist in
  let lib = st.library in
  let lay = st.layout in
  let win = Ui.window lay.ui in

  (* Update after possible window resize *)
  lay.browser_width <-
    clamp (Layout.browser_min lay) (Layout.browser_max lay) lay.browser_width;
  lay.left_width <-
    clamp (Layout.left_min lay) (Layout.left_max lay) lay.left_width;
  lay.upper_height <-
    clamp (Layout.upper_min lay) (Layout.upper_max lay) lay.upper_height;

  Layout.browser_pane lay;

  (* Background rescanning *)
  Library.refresh_after_rescan lib;

  (* Browser *)
  let browser = lib.browser in
  let current_path =
    match st.control.current with Some track -> track.path | None -> "" in

  if Library.rescan_busy lib <> None
  && Api.Draw.frame win mod spin_delay = 0 then
    Table.dirty browser;   (* to draw spinner *)

  let entries = browser.entries in  (* could change concurrently *)
  let pp_entry i =
    let dir = entries.(i) in
    let spinning =
      match Library.rescan_busy lib with
      | None -> false
      | Some path ->
        path = dir.path ||
        dir.view.folded && String.starts_with ~prefix: dir.path path
    in
    let spin = if not spinning then "" else " " ^ spin win
    and folded = if dir.children = [||] then None else Some dir.view.folded
    and c =
      if dir.path = File.(dir current_path // "")
      || dir.view.folded && String.starts_with ~prefix: dir.path current_path
      then `White
      else Ui.text_color lay.ui
    in dir.nest, folded, c, dir.name ^ spin
  in

  let dir = Library.selected_dir lib in
  (match Layout.browser_table lay browser pp_entry with
  | `None | `Scroll | `Move _ -> ()

  | `Select ->
    (* TODO: allow multiple selections *)
    State.focus_library browser st;
    if Library.selected_dir lib <> dir then
    (
      (match Library.selected_dir lib with
      | None -> Library.deselect_dir lib
      | Some i -> Library.select_dir lib i  (* do bureaucracy *)
      );
      Library.deselect_all lib;
      Library.refresh_artists_albums_tracks lib;
    );

  | `Fold i ->
    (* Click on triangle: fold/unfold entry *)
    let dir = entries.(i) in
    Library.fold_dir lib dir (not dir.view.folded)

  | `Click (Some i) ->
    (* Click on dir name: switch view *)
    (* TODO: allow multiple selections *)
    if Api.Mouse.is_pressed `Left then
      State.focus_library browser st;
    if Library.selected_dir lib <> dir then
    (
      Library.select_dir lib i;  (* do bureaucracy *)
      Library.deselect_all lib;
      Library.refresh_artists_albums_tracks lib;
    );
    if Api.Mouse.is_doubleclick `Left then
    (
      (* Double-click on directory name: send track view to playlist *)
      let n_artists = Table.num_selected lib.artists in
      let n_albums = Table.num_selected lib.albums in
      if n_artists <> 0 && n_artists <> Table.length lib.artists
      || n_albums <> 0 && n_albums <> Table.length lib.albums then
      (
        Table.deselect_all lib.artists;   (* deactivate inner filters *)
        Table.deselect_all lib.albums;
        Library.refresh_albums_tracks_sync lib;  (* could be slow... *)
      );
      let tracks = lib.tracks.entries in
      if tracks <> [||] then
      (
        Playlist.replace_all pl (Array.copy tracks);
        State.focus_playlist st;
        Control.eject st.control;
        Control.switch st.control tracks.(0) true;
        Table.dirty st.library.tracks;
        Table.dirty st.library.browser;
      )
    )

  | `Click None ->
    (* Click into empty space: deselect everything *)
    Library.deselect_dir lib;
    Library.deselect_all lib;
    Library.refresh_artists_albums_tracks lib;
    State.focus_library browser st;

  | `Drag _ ->
    (* Drag: adjust cursor *)
    if Api.Key.are_modifiers_down [] then
    (
      State.focus_library browser st;
      if lib.tracks.entries <> [||] then set_drop_cursor st;
    )

  | `Drop ->
    (* Drag & drop originating from browser *)

    (* Drag & drop onto playlist: send directory contents to playlist *)
    let tracks = lib.tracks.entries in
    drop_on_playlist st tracks;

    Option.iter (fun i ->
      if Library.selected_dir lib <> Some i then
      (
        (* Drag & drop on other browser entry *)
        drop_on_browser st tracks;
      )
    ) (Layout.browser_mouse lay browser)
  );

  (* Browser drag & drop *)
  let dropped = Api.Files.dropped win in
  if dropped <> [] then
  (
    Option.iter (fun pos ->
      let rec find_root_pos i j =
        if i = pos then j else
        find_root_pos (i + 1) (if entries.(i).nest = 0 then j + 1 else j)
      in
      if Library.add_dirs lib dropped (find_root_pos 0 0) then
        Library.refresh_artists_albums_tracks lib
      else
        Layout.browser_error_box lay;  (* flash *)
    ) (Layout.browser_mouse lay browser)
  );

  (* Keys *)
  if browser.focus && (Layout.del_key lay || Layout.backspace_key lay) then
  (
    match Library.selected_dir lib with
    | Some i when entries.(i).parent = Some "" ->
      Library.remove_dirs lib [entries.(i).path];
      Library.refresh_artists_albums_tracks lib
    | _ ->
      Library.error lib "Cannot remove non-root directories";
      Layout.browser_error_box lay;  (* flash *)
  );

  (* Scanning indicator *)
  Layout.scan_label lay;
  Layout.scan_indicator lay (Library.rescan_busy lib <> None);
  if Layout.scan_button lay then
  (
    let shift = Api.Key.is_modifier_down `Shift in
    if shift then
      (* Scanning indicator shift-clicked: purge cached covers *)
      Library.purge_covers lib;

    if Library.rescan_busy lib = None then
    (
      (* Inactive scanning indicator clicked: rescan *)
      let mode = if shift then `Thorough else `Quick in
      match Library.selected_dir lib with
      | None -> Library.rescan_root lib mode
      | Some i ->
        let dir = entries.(i) in
        if Data.is_dir dir then Library.rescan_dirs lib mode [|dir|]
    )
  );

  (* Browse modes *)
  let have_dir = lib.current <> None in
  let dir = Option.value lib.current ~default: entries.(0) in
  let view = dir.view in
  let cycle_shown = function
    | None -> Some `Table
    | Some `Table -> Some `Grid
    | Some `Grid -> None
  in

  let nothing_shown (view : Library.view) =
    not view.artists_shown &&
    view.albums_shown = None &&
    view.tracks_shown = None
  in

  let artists = have_dir && view.artists_shown in
  Layout.artists_label lay;
  Layout.artists_indicator lay artists;
  let artists' =
    Layout.artists_button lay (if have_dir then Some artists else None) in
  if have_dir && artists' <> artists then
  (
    (* Click on Artists button: toggle artist pane *)
    view.artists_shown <- artists';
    if nothing_shown view then
      view.tracks_shown <- Some `Table;  (* switch to tracks *)
    Library.update_dir lib dir;
  );

  let albums = have_dir && view.albums_shown <> None in
  Layout.albums_label lay;
  Layout.albums_indicator1 lay (view.albums_shown = Some `Table);
  Layout.albums_indicator2 lay (view.albums_shown = Some `Grid);
  let albums' =
    Layout.albums_button lay (if have_dir then Some albums else None) in
  if have_dir && albums' <> albums then
  (
    (* Click on Albums button: toggle artist pane *)
    view.albums_shown <- cycle_shown view.albums_shown;
    if nothing_shown view then
      view.albums_shown <- Some `Table;
    Library.update_dir lib dir;
  );

  let tracks = have_dir && view.tracks_shown <> None in
  Layout.tracks_label lay;
  Layout.tracks_indicator1 lay (view.tracks_shown = Some `Table);
  Layout.tracks_indicator2 lay (view.tracks_shown = Some `Grid);
  let tracks' =
    Layout.tracks_button lay (if have_dir then Some tracks else None) in
  if have_dir && tracks' <> tracks then
  (
    (* Click on Tracks button: toggle artist pane *)
    view.tracks_shown <- cycle_shown view.tracks_shown;
    if nothing_shown view then
      view.tracks_shown <- Some `Table;
    Library.update_dir lib dir;
  );

  let show_artists = have_dir && view.artists_shown && lay.playlist_shown in
  let show_albums =
    have_dir && view.albums_shown <> None && lay.playlist_shown in
  let show_tracks =
    not have_dir || view.tracks_shown <> None || not lay.playlist_shown in
  lay.right_shown <- show_artists && show_albums;
  lay.lower_shown <- show_tracks && (show_artists || show_albums);

  (* Search *)
  Layout.search_label lay;
  Layout.search_box lay;
  if have_dir then
  (
    if Layout.search_button lay then
    (
      (* Click on Search label: clear search *)
      if lib.search.text <> "" then
      (
        Edit.clear lib.search;
        Library.set_search lib "";
      )
    );

    let ch = Layout.search_text lay lib.search in
    if lib.search.focus then
    (
      (* Have or gained focus: make sure it's consistent *)
      State.defocus_all st;
      Library.focus_search lib;
    );
    if ch = Uchar.of_char '\n' || ch = Uchar.of_char ' ' then
    (
      (* Entered Space or Return: update search in dir *)
      Library.set_search lib lib.search.text;
    )
  );


  (* Info pane *)

  Layout.info_pane lay;

  Layout.msg_box lay;
  let now = Unix.gettimeofday () in
  if lib.error <> "" && now -. lib.error_time < 10.0 then
    Layout.msg_text lay (Ui.error_color lay.ui) `Regular true lib.error
  else
  (
    let tr = Table.length lib.tracks in
    let al = Table.length lib.albums in
    let ar = Table.length lib.artists in
    let trs = Table.num_selected lib.tracks in
    let als = Table.num_selected lib.albums in
    let ars = Table.num_selected lib.artists in
    let sel n = if n = 0 then "" else string_of_int n ^ "/" in
    let plu n = if n = 1 then "" else "s" in
    let count name m n shown =
      if shown then Some (fmt "%s%d %s%s" (sel m) n name (plu n)) else None in
    let counts =
      [ count "artist" ars ar artists;
        count "album" als al albums;
        count "track" trs tr tracks;
      ]
    in
    Layout.msg_text lay (Ui.text_color lay.ui) `Regular true
      (String.concat ", " (List.filter_map Fun.id counts))
  );


  (* Artists view *)

  if show_artists then
  (
    let artists_pane, artists_area, artists_table, _grid, artists_spin =
      Layout.left_view in
    artists_pane lay;

    let busy = Library.refresh_artists_busy lib in
    let tab = if busy then busy_artists else lib.artists in
    let cols =
      Array.map (fun (attr, cw) -> cw, Library.attr_align attr)
        view.artists_columns
    and headings =
      Array.map (fun (attr, _) -> Library.attr_name attr) view.artists_columns
    in

    let entries = tab.entries in  (* could change concurrently *)
    let pp_row i =
      let artist = entries.(i) in
      Ui.text_color lay.ui,
      Array.map (fun (attr, _) -> `Text (Data.artist_attr_string artist attr))
        view.artists_columns
    in

    let sorting = convert_sorting view.artists_columns view.artists_sorting in
    (match artists_table lay cols (Some (headings, sorting)) tab pp_row with
    | `None | `Scroll | `Move _ -> ()

    | `Select ->
      (* New selection: grab focus, update filter *)
      State.focus_library tab st;
      Library.refresh_albums_tracks lib;

    | `Sort i ->
      (* Click on column header: reorder view accordingly *)
      let attr = fst view.artists_columns.(i) in
      let k =
        Bool.to_int (Api.Key.is_modifier_down `Shift) +
        Bool.to_int (Api.Key.is_modifier_down `Alt) * 2 +
        Bool.to_int (Api.Key.is_modifier_down `Command) * (-4)
      in
      view.artists_sorting <-
        Data.insert_sorting `Artist attr k 4 view.artists_sorting;
      Library.update_dir lib dir;
      Library.reorder_artists lib;

    | `Arrange ->
      (* Column resizing: update column widths *)
      Array.mapi_inplace (fun i (a, _) ->
        a, fst cols.(i)) view.artists_columns;
      if have_dir then Library.update_dir lib dir;

    | `Click (Some _i) when Api.Mouse.is_doubleclick `Left ->
      (* Double-click on track: clear playlist and send tracks to it *)
      let n_albums = Table.num_selected lib.albums in
      if n_albums <> 0 && n_albums <> Table.length lib.albums then
      (
        Table.deselect_all lib.albums;    (* deactivate inner filter *)
        Library.refresh_tracks_sync lib;  (* could be slow... *)
      );
      let tracks = lib.tracks.entries in
      if tracks <> [||] then
      (
        Playlist.replace_all pl (Array.copy tracks);
        State.focus_playlist st;
        Control.eject st.control;
        Control.switch st.control tracks.(0) true;
        Table.dirty st.library.tracks;
        Table.dirty st.library.browser;
      )

    | `Click _ ->
      (* Single-click: grab focus, update filter *)
      State.focus_library tab st;
      Library.refresh_albums_tracks lib;

    | `Drag _ ->
      (* Drag: adjust cursor *)
      if Api.Key.are_modifiers_down [] then
      (
        State.focus_library tab st;
        if Table.num_selected lib.artists > 0 && lib.tracks.entries <> [||] then
          set_drop_cursor st;
      );

    | `Drop ->
      if not (Ui.mouse_inside lay.ui (artists_area lay)) then
      (
        (* Drag & drop originating from artists view *)

        (* Drag & drop onto playlist or browser: send tracks to playlist *)
        let tracks = lib.tracks.entries in
        drop_on_playlist st tracks;
        drop_on_browser st tracks;
      )
    );

    if busy then
      artists_spin lay (spin win);
  );

  (* Albums view *)

  if show_albums then
  (
    let albums_pane, albums_area, albums_table, albums_grid, albums_spin =
      Layout.(if lay.right_shown then right_view else left_view) in
    albums_pane lay;

    let busy = Library.refresh_albums_busy lib in
    let tab = if busy then busy_albums else lib.albums in
    let cols =
      Array.map (fun (attr, cw) -> cw, Library.attr_align attr)
        view.albums_columns
    and headings =
      Array.map (fun (attr, _) -> Library.attr_name attr) view.albums_columns
    in

    if Option.get view.albums_shown = `Grid
    && Api.Draw.frame win mod refresh_delay = 2 then
      Table.dirty tab;  (* to capture cover updates *)

    let entries = tab.entries in  (* could change concurrently *)
    let pp_row i =
      let album = entries.(i) in
      Ui.text_color lay.ui,
      Array.map (fun (attr, _) ->
        if attr <> `Cover then
          `Text (Data.album_attr_string album attr)
        else if lib.cover then
          match Library.load_cover lib win album.path with
          | Some img -> `Image img
          | None -> `Text ""
        else `Text ""
      ) view.albums_columns
    in

    let pp_cell i =
      let album = entries.(i) in
      let img =
        match Library.load_cover lib win album.path with
        | Some img -> img
        | None -> Ui.nocover lay.ui
      and txt =
        Data.album_attr_string album `AlbumArtist ^ " - " ^
        Data.album_attr_string album `AlbumTitle ^ " (" ^
        Data.album_attr_string album `Year ^ ")"
      in img, txt
    in

    let sorting = convert_sorting view.albums_columns view.albums_sorting in
    let header = Some (headings, sorting) in
    (match
      match Option.get view.albums_shown with
      | `Table -> albums_table lay cols header tab pp_row
      | `Grid -> albums_grid lay lay.albums_grid header tab pp_cell
    with
    | `None | `Scroll | `Move _ -> ()

    | `Select ->
      (* New selection: grab focus, update filter *)
      State.focus_library tab st;
      Library.refresh_tracks lib;

    | `Sort i ->
      (* Click on column header: reorder view accordingly *)
      let attr = fst view.albums_columns.(i) in
      let k =
        Bool.to_int (Api.Key.is_modifier_down `Shift) +
        Bool.to_int (Api.Key.is_modifier_down `Alt) * 2 +
        Bool.to_int (Api.Key.is_modifier_down `Command) * (-4)
      in
      view.albums_sorting <-
        Data.insert_sorting `None attr k 4 view.albums_sorting;
      Library.update_dir lib dir;
      Library.reorder_albums lib;

    | `Arrange ->
      (* Column resizing: update column widths *)
      Array.mapi_inplace (fun i (a, _) -> a, fst cols.(i)) view.albums_columns;
      if have_dir then Library.update_dir lib dir;

    | `Click (Some _i) when Api.Mouse.is_doubleclick `Left ->
      (* Double-click on track: clear playlist and send tracks to it *)
      let tracks = lib.tracks.entries in
      if tracks <> [||] then
      (
        Playlist.replace_all pl (Array.copy tracks);
        State.focus_playlist st;
        Control.eject st.control;
        Control.switch st.control tracks.(0) true;
        Table.dirty st.library.tracks;
        Table.dirty st.library.browser;
      )

    | `Click _ ->
      (* Single-click: grab focus, update filter *)
      State.focus_library tab st;
      Library.refresh_tracks lib;

    | `Drag _ ->
      (* Drag: adjust cursor *)
      if Api.Key.are_modifiers_down [] then
      (
        State.focus_library tab st;
        if Table.num_selected lib.albums > 0 && lib.tracks.entries <> [||] then
          set_drop_cursor st;
      );

    | `Drop ->
      if not (Ui.mouse_inside lay.ui (albums_area lay)) then
      (
        (* Drag & drop originating from albums view *)

        (* Drag & drop onto playlist or browser: send tracks to playlist *)
        let tracks = lib.tracks.entries in
        drop_on_playlist st tracks;
        drop_on_browser st tracks;
      )
    );

    if busy then
      albums_spin lay (spin win);

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
    let tracks_pane, tracks_area, tracks_table, tracks_grid, tracks_spin =
      Layout.(if lay.lower_shown then lower_view else left_view) in
    tracks_pane lay;

    let busy = Library.refresh_tracks_busy lib in
    let tab = if busy then busy_tracks else lib.tracks in
    let cols =
      Array.map (fun (attr, cw) -> cw, Library.attr_align attr)
        view.tracks_columns
    and headings =
      Array.map (fun (attr, _) -> Library.attr_name attr) view.tracks_columns
    in

    if Option.get view.tracks_shown = `Grid
    && Api.Draw.frame win mod refresh_delay = 5 then
      Table.dirty tab;  (* to capture cover updates *)

    let entries = tab.entries in  (* may update concurrently *)
    let pp_row i =
      let track = entries.(i) in
      let c =
        if (track.status = `Undet || track.status = `Predet)
        && Library.rescan_busy lib = None then
          Track.update track;
        match track.status with
        | _ when track.path = current_path -> `White
        | `Absent -> Ui.error_color lay.ui
        | `Invalid -> Ui.warn_color lay.ui
        | `Undet -> Ui.semilit_color (Ui.text_color lay.ui)
        | `Predet | `Det ->
          if track.pos = -1 || Data.is_separator track || Library.has_track lib track then
            Ui.text_color lay.ui
          else
            Ui.warn_color lay.ui
      in
      c,
      Array.map (fun (attr, _) ->
        if attr <> `Cover then
          `Text (Data.track_attr_string track attr)
        else if lib.cover then
          match Library.load_cover lib win track.path with
          | Some img -> `Image img
          | None -> `Text ""
        else `Text ""
      ) view.tracks_columns
    in

    let pp_cell i =
      let track = entries.(i) in
      let img =
        match Library.load_cover lib win track.path with
        | Some img -> img
        | None -> Ui.nocover lay.ui
      and txt =
        let artist = Data.track_attr_string track `Artist in
        let title = Data.track_attr_string track `Title in
        let year = Data.track_attr_string track `Year in
        artist ^ " - " ^ title ^ (if year = "" then "" else " (" ^ year ^ ")")
      in img, txt
    in

    let sorting = convert_sorting view.tracks_columns view.tracks_sorting in
    let header = Some (headings, sorting) in
    (match
      match Option.get view.tracks_shown with
      | `Table -> tracks_table lay cols header tab pp_row
      | `Grid -> tracks_grid lay lay.tracks_grid header tab pp_cell
    with
    | `None | `Scroll -> ()

    | `Select ->
      State.focus_library tab st;

    | `Sort i ->
      (* Click on column header: reorder view accordingly *)
      let attr = fst view.tracks_columns.(i) in
      let k =
        Bool.to_int (Api.Key.is_modifier_down `Shift) +
        Bool.to_int (Api.Key.is_modifier_down `Alt) * 2 +
        Bool.to_int (Api.Key.is_modifier_down `Command) * (-4)
      in
      let primary =
        if Library.current_is_playlist lib
        || Library.current_is_viewlist lib then `Pos else `FilePath in
      view.tracks_sorting <-
        Data.insert_sorting primary attr k 4 view.tracks_sorting;
      Library.update_dir lib dir;
      Library.reorder_tracks lib;

    | `Arrange ->
      (* Column resizing: update column widths *)
      Array.mapi_inplace (fun i (a, _) -> a, fst cols.(i)) view.tracks_columns;
      if have_dir then Library.update_dir lib dir;

    | `Click (Some i) when Api.Mouse.is_doubleclick `Left ->
      (* Double-click on track: clear playlist and send tracks to it *)
      let tracks =
        if Api.Key.are_modifiers_down [`Command]
        then Library.selected lib
        else [|entries.(i)|]
      in
      if tracks <> [||] then
      (
        Playlist.replace_all pl tracks;
        State.focus_playlist st;
        Control.eject st.control;
        Control.switch st.control (Playlist.current pl) true;
        Table.dirty st.library.tracks;
        Table.dirty st.library.browser;
      )

    | `Click _ ->
      (* Single-click: grab focus *)
      State.focus_library tab st;

    | `Move delta ->
      (* Cmd-cursor movement: move selection *)
      if Data.is_playlist dir then
        Library.move_selected lib delta;

    | `Drag (delta, way) ->
      (* Drag: move selection if inside *)
      if Api.Key.are_modifiers_down [] then
      (
        State.focus_library tab st;
        if Library.num_selected lib > 0 then set_drop_cursor st;

        if Data.is_playlist dir then
        (
          (* Invariant as for playlist view *)
          (match way with
          | `Start ->
            (* Start of drag & drop: remember original configuration *)
            Table.push_undo lib.tracks;
          | `Outward ->
            (* Leaving area: snap back to original state *)
            Library.undo lib;
            Library.save_playlist lib;
          | `Inward ->
            (* Reentering area: restore updated state *)
            Library.redo lib
          | `Inside | `Outside -> ()
          );

          if delta <> 0 && Library.num_selected lib > 0 && sorting <> [] then
          (
            let prim_attr, order = List.hd view.tracks_sorting in
            if prim_attr = `Pos && order = `Asc then
            (
              match way with
              | `Start | `Inside | `Inward ->
                Library.move_selected lib delta;
                (* Erase intermediate new state *)
                Table.drop_undo lib.tracks;
              | `Outward ->
                (* Temporarily restore new state, modify, immediately undo *)
                (* Restore new state *)
                Library.redo lib;
                Library.move_selected lib delta;
                (* Erase intermediate new state *)
                Table.drop_undo lib.tracks;
                (* Undo new state, recovering original *)
                Library.undo lib;
                Library.save_playlist lib;
              | `Outside -> ()
            )
          )
        )
      )

    | `Drop ->
      if not (Ui.mouse_inside lay.ui (tracks_area lay)) then
      (
        (* Drag & drop originating from tracks *)

        (* Dropping outside tracks: drop aux redo for new state *)
        if Data.is_playlist dir then
          Table.drop_redo lib.tracks;

        (* Drag & drop onto playlist or browser: send tracks to playlist *)
        let tracks = Library.selected lib in
        drop_on_playlist st tracks;
        drop_on_browser st tracks;
      )
    );

    if busy then
      tracks_spin lay (spin win);

    (* Playlist drag & drop *)
    let dropped = Api.Files.dropped win in
    if dropped <> [] then
    (
      (* Files drop: insert paths at pointed position *)
      drop_on_library st (expand_paths lib dropped);
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

  (* Keys *)

  if
    Ui.key lay.ui ([`Command], `Char 'C')
      ( (lib.browser.focus || lib.artists.focus || lib.albums.focus) &&
        lib.tracks.entries <> [||] )
  then
  (
    (* Press of Copy key: write selected tracks to clipboard *)
    let s = Track.to_m3u lib.tracks.entries in
    Api.Clipboard.write win s;
  );

  if Layout.lib_cover_key lay then lib.cover <- not lib.cover;

  let grid_delta n =
    if n <= 20 then 2 else
    if n <= 60 then 4 else
    if n <= 140 then 8 else
    if n <= 300 then 16 else 32
  in
  if lib.albums.focus then
  (
    let delta = grid_delta lay.tracks_grid in
    let grid' = lay.albums_grid +
      (if Layout.enlarge_grid_key lay then +delta else 0) +
      (if Layout.reduce_grid_key lay then -delta else 0)
    in
    lay.albums_grid <- clamp 10 1000 grid';
  )
  else if lib.tracks.focus then
  (
    let delta = grid_delta lay.tracks_grid in
    let grid' = lay.tracks_grid +
      (if Layout.enlarge_grid_key lay then +delta else 0) +
      (if Layout.reduce_grid_key lay then -delta else 0)
    in
    lay.tracks_grid <- clamp 10 1000 grid';
  );

  (* Pane divider *)

  let browser_width' = Layout.browser_divider lay lay.browser_width
    (Layout.browser_min lay) (Layout.browser_max lay) in
  (* Possible drag of divider: update pane width *)
  lay.browser_width <- browser_width'


(* File Selection *)

let run_filesel (st : _ State.t) =
  let fs = st.filesel in
  let lay = st.layout in

  (* Update after possible window resize *)
  lay.directories_width <-
    clamp (Layout.directories_min lay) (Layout.directories_max lay)
    lay.directories_width;

  (* Directories *)

  Layout.directories_pane lay;

  let dirs = fs.dirs in

  let pp_entry i =
    let dir = dirs.entries.(i) in
    let folded = if dir.children = [||] then None else Some dir.folded in
    let name =
      if File.dir dir.path <> dir.path then
        File.name dir.path
      else if dir.path = File.sep then
        dir.path
      else  (* Special case for Windows drives: strip slash *)
        String.(sub dir.path 0 (length dir.path - length File.sep))
    and c =
      if dir.path = Storage.home_dir then `White else Ui.text_color lay.ui in
    dir.nest, folded, c, name
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
  );


  (* Files *)

  Layout.files_pane lay;

  let files = fs.files in
  let cols = Filesel.columns fs in

  let pp_row i =
    let file = files.entries.(i) in
    (if file.name = fs.input.text then `White else Ui.text_color lay.ui),
    Array.map (fun s -> `Text s) (Filesel.row file)
  in

  let ok =
    match Layout.files_table lay cols (Some (Filesel.headings, [])) files pp_row with
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
        if not file.is_dir then
          Edit.set fs.input file.name
      ) (Filesel.selected_file fs);
      false

    | `Sort i ->
      (* Click on column header: reorder view accordingly *)
      Filesel.reorder_files fs i;
      false

    | `Arrange ->
      (* Column resizing: update column widths *)
      Array.mapi_inplace (fun i _ -> fst cols.(i)) fs.columns;
      false

    | `Click None ->
      (* Click into empty space: focus *)
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

  let ch = Layout.file_text lay fs.input in
  if fs.input.focus then
  (
    (* Have or gained focus: make sure it's consistent *)
    Filesel.deselect_file fs;
    State.defocus_all st;
    Filesel.focus_input fs;
  );
  let ok = ok || ch = Uchar.of_char '\n' in

  (* Buttons *)

  let is_write = fs.op <> Some `LoadPlaylist in
  let is_valid = fs.input.text <> "" && fs.input.text.[0] <> '.' in
  let dir_avail =
    fs.dirs.focus || fs.files.focus && Filesel.current_sel_is_dir fs in
  let file_avail = not dir_avail && Filesel.current_file_exists fs in
  let overwrite_avail = file_avail && is_write in
  let ok_avail =
    not (dir_avail || overwrite_avail) && (file_avail || is_write && is_valid) in

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
    if dir_avail then
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
      let path = Option.get (Filesel.current_file_path fs) in
      (match fs.op with
      | None -> assert false

      | Some `LoadPlaylist ->
        (try
          let tracks = Track.of_m3u (File.load `Bin path) in
          Playlist.replace_all st.playlist tracks;
          State.focus_playlist st;
          Control.eject st.control;
          Control.switch st.control tracks.(0) true;
          Table.dirty st.library.tracks;
          Table.dirty st.library.browser;
        with Sys_error _ ->
          Library.error st.library ("Error reading file " ^ path);
          Layout.browser_error_box lay;  (* flash *)
        )

      | Some (`SavePlaylist tab) ->
        (try
          File.store `Bin path (Track.to_m3u tab.entries)
        with Sys_error _ ->
          Library.error st.library ("Error writing file " ^ path);
          Layout.browser_error_box lay;  (* flash *)
        )
      );
      Filesel.reset fs;
      lay.filesel_shown <- false;
      State.focus_playlist st;
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


(* Runner *)

let rec run (st : _ State.t) =
  State.ok st;
  (try run' st with exn -> Storage.log_exn "internal" exn "");
  run st

and run' (st : _ State.t) =
  let lay = st.layout in
  let win = Ui.window lay.ui in
  if Api.Window.closed win then exit 0;

  (* Start drawing *)
  Ui.start lay.ui;

  (* Remember current geometry for later *)
  let playlist_shown = lay.playlist_shown in
  let library_shown = lay.library_shown in
  let filesel_shown = lay.filesel_shown in
  let overlay_shown = library_shown || filesel_shown in
  let library_side = lay.library_side in
  let library_width = lay.library_width in

  (* Update geometry *)
  let ww, wh = Api.Window.size win in
  if playlist_shown then lay.playlist_height <- wh - Layout.control_h lay;
  if overlay_shown then lay.library_width <- ww - Layout.control_w lay;

  (* Run panes *)
  run_control st;
  if not (Api.Window.is_minimized win) then
  (
    if playlist_shown then run_playlist st;
    if filesel_shown then run_filesel st
    else if library_shown then run_library st;
    if playlist_shown || overlay_shown then run_edit st;
  );
  run_toggle_panes st;

  (* Adjust font size *)
  let text' = lay.text +
    (if Layout.enlarge_key lay then +1 else 0) +
    (if Layout.reduce_key lay then -1 else 0)
  in
  lay.text <- clamp 8 64 text';

  (* Adjust window size *)
  let overlay_shown' = lay.library_shown || lay.filesel_shown in
  let extra_w = if overlay_shown' then lay.library_width else 0 in
  let extra_h =
    if lay.playlist_shown then lay.playlist_height else
    if overlay_shown' then Layout.bottom_h lay else 0
  in
  Api.Window.set_size win
    (Layout.control_w lay + extra_w) (Layout.control_h lay + extra_h);

  (* Adjust window position after opening/closing library *)
  let dx =
    match overlay_shown, overlay_shown', library_side, lay.library_side with
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
    if overlay_shown
    then Layout.(control_w lay + library_min lay, -1)
    else Layout.(control_w lay, control_w lay)
  and minh, maxh =
    if playlist_shown
    then Layout.(control_h lay + playlist_min lay, -1)
    else Layout.(control_h lay, control_h lay)
  in
  Ui.finish lay.ui (Layout.margin lay) (minw, minh) (maxw, maxh);

  if Api.Window.is_hidden win then  (* after startup *)
    Api.Window.reveal win


(* Startup *)

let startup () =
  Storage.clear_temp ();
  let audio = Api.Audio.init () in
  let win = Api.Window.init 0 0 0 0 App.name in
  Api.Window.hide win;  (* hide during initialisation *)
  let ui = Ui.make win in
  let st0 = State.make ui audio in
  let success, (x, y) = State.load st0 in
  let st = if success then st0 else State.make ui audio in
  let w = Layout.control_min_w + st.layout.library_width in
  let h = Layout.control_min_h + st.layout.playlist_height in
  Api.Draw.start win `Black;
  Api.Window.set_pos win x y;
  Api.Window.set_size win w h;
  Api.Draw.finish win;
  at_exit (fun () ->
    State.save st;
    Storage.clear_temp ();
  );
  st

let _main =
  try
    Printexc.record_backtrace true;
    (* Work around apparent bug in GC scheduler. *)
    Gc.(set {(get ()) with space_overhead = 10});
    let st = startup () in
    run st
  with exn ->
    Storage.log_exn "internal" exn "";
    Stdlib.exit 2
