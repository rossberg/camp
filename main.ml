(* Main Program *)

open Audio_file


(* State *)

type state = State.t
type dir = Library.dir


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


(* Display operations *)

let quit _st =
  exit 0

let minimize (st : state) =
  Api.Window.minimize (Ui.window st.layout.ui)

let toggle_playlist (st : state) =
  let lay = st.layout in
  lay.playlist_shown <- not lay.playlist_shown;
  if not lay.playlist_shown then Playlist.defocus st.playlist

let toggle_library (st : state) =
  let lay = st.layout in
  if lay.library_shown then
  (
    lay.library_shown <- false;
    Library.defocus st.library;
  )
  else
  (
    lay.library_shown <- true;
    (* Switch side if window exceeds respective border *)
    if not lay.filesel_shown then
    (
      let win = Ui.window lay.ui in
      let wx, _ = Api.Window.pos win in
      let sx, _ = Api.Window.min_pos win in
      let sw, _ = Api.Window.max_size win in
      if lay.library_side = `Left && wx <= sx then
        lay.library_side <- `Right;
      if lay.library_side = `Right && wx + Layout.control_w lay >= sx + sw then
        lay.library_side <- `Left;
    )
  );
  if not (lay.library_shown || lay.filesel_shown) then State.focus_playlist st

let toggle_side (st : state) =
  let lay = st.layout in
  lay.library_side <- if lay.library_side = `Left then `Right else `Left

let toggle_cover (st : state) =
  let ctl = st.control in
  ctl.cover <- not ctl.cover

let toggle_fps (st : state) =
  let ctl = st.control in
  ctl.fps <- not ctl.fps

let cycle_color (st : state) d =
  let lay = st.layout in
  let n = Ui.num_palette lay.ui in
  Ui.set_palette lay.ui ((Ui.get_palette lay.ui + d + n) mod n);
  Table.dirty st.playlist.table;
  Table.dirty st.library.browser;
  Table.dirty st.library.artists;
  Table.dirty st.library.albums;
  Table.dirty st.library.tracks

let clamp_text = clamp 10 64

let resize_text_avail (st : state) delta =
  clamp_text (st.layout.text + delta) <> st.layout.text

let resize_text (st : state) delta =
  st.layout.text <- clamp_text (st.layout.text + delta)

let clamp_grid = clamp 20 1000

let resize_grid_avail (st : state) delta =
  match st.library.current with
  | None -> false
  | Some (dir : Library.dir) ->
    let lay = st.layout in
    dir.view.albums.shown = Some `Grid &&
      clamp_grid (lay.albums_grid + delta) <> lay.albums_grid ||
    dir.view.tracks.shown = Some `Grid &&
      clamp_grid (lay.tracks_grid + delta) <> lay.tracks_grid

let resize_grid (st : state) delta =
  Option.iter (fun (dir : Library.dir) ->
    let inc n =
      n + delta *
      if n <= 20 then 2 else
      if n <= 60 then 4 else
      if n <= 140 then 8 else
      if n <= 300 then 16 else 32
    in
    let lay = st.layout in
    if dir.view.albums.shown = Some `Grid then
      lay.albums_grid <- clamp_grid (inc lay.albums_grid);
    if dir.view.tracks.shown = Some `Grid then
      lay.tracks_grid <- clamp_grid (inc lay.tracks_grid);
  ) st.library.current


(* Control Section *)

let run_control (st : state) =
  let ctl = st.control in
  let pl = st.playlist in
  let lay = st.layout in
  let win = Ui.window lay.ui in

  Layout.control_pane lay;

  (* Exit button *)
  (* This has to come first, otherwise Raylib crashes? *)
  if not (Layout.power_button lay (Some true))
  && not (Api.Key.is_modifier_down `Shift) then
  (
    (* Power button clicked: quit *)
    quit st
  );
  Layout.power_label lay;

  (* Current status *)
  let silence = ctl.sound = Api.Audio.silence ctl.audio in
  let length = Api.Audio.length ctl.audio in
  let elapsed = Api.Audio.played ctl.audio in
  let remaining = length -. elapsed in
  let playing = Api.Audio.is_playing ctl.audio in
  let paused = not playing && elapsed > 0.0 in
  let stopped = not playing && not paused in
  let focus = pl.table.focus || not (lay.library_shown || lay.filesel_shown || lay.menu_shown) in

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

  if Layout.color_button lay then
  (
    (* Click on color button: cycle color palette *)
    cycle_color st (if Api.Key.is_modifier_down `Shift then -1 else +1)
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
  if Layout.cover_key lay then toggle_cover st;

  (* FPS *)
  if ctl.fps then
    Layout.fps_text lay `Regular true (fmt "%d FPS" (Api.Window.fps win));
  (* Press of FPS key: toggle FPS display *)
  if Layout.fps_key lay then toggle_fps st;

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
  let shift_volume (st : state) delta =
    if delta <> 0.0 then
      Control.volume st.control (ctl.volume +. 0.05 *. delta)
  in
  let vol_mouse = Layout.volume_bar lay ctl.volume in
  (* Hack to overlap volume bar with mute button. *)
  let mute_mouse = Ui.mouse_inside lay.ui (Layout.mute_area lay) in
  if not mute_mouse && Layout.mute_drag lay (0, 0) = `None
  && vol_mouse <> ctl.volume then
  (
    (* Click or drag on volume bar: adjust volume *)
    Control.volume ctl vol_mouse;
  );
  let vol_delta =
    snd (Layout.volume_wheel lay) +.
    float_of_bool (Layout.volup_key lay focus) -.
    float_of_bool (Layout.voldown_key lay focus)
  in
  (* Volume key pressed or mouse wheel used: shift volume *)
  shift_volume st vol_delta;

  let toggle_mute (st : state) =
    Control.mute st.control (not st.control.mute)
  in
  Layout.mute_text lay (Ui.error_color lay.ui) `Inverted ctl.mute "MUTE";
  if Layout.mute_button lay then
  (
    (* Click on mute label: toggle muting *)
    toggle_mute st;
  );

  (* Seek bar *)
  let seek (st : state) delta =
    if delta <> 0.0 then
      Control.seek st.control (st.control.progress +. 0.05 *. delta)
  in
  let progress =
    if length > 0.0 && not silence then elapsed /. length else 0.0 in
  let progress' = Layout.seek_bar lay progress in
  if (progress' <> ctl.progress || Api.Mouse.is_pressed `Left)
  && progress' <> progress && not silence then
  (
    (* Click or drag on seek bar at new position: reposition audio *)
    Control.seek ctl progress'
  );
  let seek_delta =
    float_of_bool (Layout.ff_key lay focus) -.
    float_of_bool (Layout.rw_key lay focus)
  in
  (* Seek key pressed: seek *)
  seek st seek_delta;

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

  let skip (st : state) delta =
    if delta <> 0 then
    (
      let ctl = st.control in
      let more = Playlist.skip st.playlist delta (ctl.repeat <> `None) in
      Control.switch ctl (Playlist.current st.playlist) more;
      Playlist.adjust_scroll st.playlist page;
      Table.dirty st.library.tracks;
      Table.dirty st.library.browser;
    );
  in
  let bwd = Layout.bwd_button lay focus (Some false) in
  let fwd = Layout.fwd_button lay focus (Some false) in
  skip st (Bool.to_int fwd - Bool.to_int bwd);

  let play (st : state) =
    if stopped && len > 0 then
    (
      (* Click on play button: start track *)
      Control.switch st.control (Playlist.current st.playlist) true;
      Playlist.adjust_scroll st.playlist page;
      Table.dirty st.library.tracks;
      Table.dirty st.library.browser;
    )
  in
  let playing' = Layout.play_button lay focus (Some playing) in
  if playing' && not playing then
  (
    (* Click on play button: start track *)
    play st
  );

  let pause (_st : state) b =
    if playing' && b then
      Api.Audio.pause ctl.audio
    else if (not stopped && not b || stopped && b) && not silence then
      Api.Audio.resume ctl.audio
  in
  let paused' = Layout.pause_button lay focus (Some paused) in
  if paused <> paused' then
  (
    (* Click on pause button when playing: pause track *)
    pause st paused'
  );

  let stop (st : state) =
    if not stopped then
    (
      let ctl = st.control in
      Api.Audio.pause ctl.audio;
      (match Playlist.current_opt st.playlist with
      | None -> Control.eject ctl
      | Some track -> Control.switch ctl track false
      );
      Playlist.adjust_scroll st.playlist page;
      Table.dirty st.library.tracks;
      Table.dirty st.library.browser;
    )
  in
  if Layout.stop_button lay focus (Some false) then
  (
    (* Click on stop button when playing: stop track *)
    stop st
  );

  let eject (st : state) =
    Control.eject st.control;
    Playlist.remove_all st.playlist;
    Table.dirty st.library.tracks;
    Table.dirty st.library.browser
  in
  if Layout.eject_button lay focus (Some false) then
  (
    (* Click on eject button: stop and clear playlist *)
    eject st
  );

  let start_stop (st : state) =
    let ctl = st.control in
    if playing then
      Api.Audio.pause ctl.audio
    else if paused then
      Api.Audio.resume ctl.audio
    else if stopped && len > 0 then
    (
      Control.switch ctl (Playlist.current st.playlist) true;
      Table.dirty st.library.tracks;
      Table.dirty st.library.browser;
    );
    Playlist.adjust_scroll st.playlist page
  in
  if Layout.start_stop_key lay focus then
  (
    (* Press of space key: pause or resume *)
    start_stop st
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
  let toggle_shuffle (st : state) =
    let pl = st.playlist in
    if pl.shuffle = None then
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
  in
  let shuffle = pl.shuffle <> None in
  Layout.shuffle_label lay;
  Layout.shuffle_indicator lay shuffle;
  let shuffle' = Layout.shuffle_button lay (Some shuffle) in
  if shuffle' <> shuffle then
  (
    (* Click on Shuffle button: toggle shuffle *)
    toggle_shuffle st
  );

  let cycle_repeat (st : state) =
    st.control.repeat <-
      match st.control.repeat with
      | `None -> `One
      | `One -> `All
      | `All -> `None
  in
  Layout.repeat_label lay;
  Layout.repeat_indicator1 lay (ctl.repeat <> `None);
  Layout.repeat_indicator2 lay (ctl.repeat = `All);
  if Layout.repeat_button lay (Some false) then
  (
    (* Click on Repeat button: cycle repeat mode *)
    cycle_repeat st
  );

  let cycle_loop (st : state) =
    let t = Api.Audio.played st.control.audio in
    st.control.loop <-
      match st.control.loop with
      | `None -> `A t
      | `A t1 when t1 > t -> `A t
      | `A t1 -> `AB (t1, t)
      | `AB _ -> `None
  in
  Layout.loop_label lay;
  Layout.loop_indicator1 lay (ctl.loop <> `None);
  Layout.loop_indicator2 lay
    (match ctl.loop with `AB _ -> true | _ -> false);
  if Layout.loop_button lay (Some false) then
  (
    (* Click on Loop button: cycle loop mode *)
    cycle_loop st
  );

  (* Context menu *)

  if Layout.(control_context lay || seek_context lay || volume_context lay) then
  (
    let c = Ui.text_color lay.ui in
    let unpause x = if x then "Unpause" else "Pause" in
    let shuffle s x = s ^ (if x = None then " On " else " Off ") in
    let repeat s x = s ^ (match x with `None -> " One" | `One -> " All" | `All -> " Off") in
    let loop s x = s ^ (match x with `None -> " Start" | `A _ -> " End" | `AB _ -> " Off") in
    let unmute x = if x then "Unmute" else "Mute" in
    Run_menu.command_menu st [|
      `Entry (c, "Start/Stop", Layout.key_startstop, paused || len > 0),
        (fun () -> start_stop st);
      `Entry (c, "Play", Layout.key_play, stopped && len > 0),
        (fun () -> play st);
      `Entry (c, unpause (not playing), Layout.key_pause, playing || not silence),
        (fun () -> pause st (not paused));
      `Entry (c, "Stop", Layout.key_stop, not stopped),
        (fun () -> stop st);
      `Entry (c, "Next", Layout.key_fwd, len > 0),
        (fun () -> skip st (+1));
      `Entry (c, "Previous", Layout.key_bwd, len > 0),
        (fun () -> skip st (-1));
      `Entry (c, "Eject", Layout.key_eject, len <> 0 || ctl.current <> None),
        (fun () -> eject st);
      `Separator, ignore;
      `Entry (c, "Seek Backwards", Layout.key_rw, ctl.progress > 0.0),
        (fun () -> seek st (-1.0));
      `Entry (c, "Seek Forwards", Layout.key_ff, not silence && ctl.progress < 1.0),
        (fun () -> seek st (+1.0));
      `Separator, ignore;
      `Entry (c, shuffle "Shuffle" pl.shuffle, Layout.key_shuffle, true),
        (fun () -> toggle_shuffle st);
      `Entry (c, repeat "Repeat" ctl.repeat, Layout.key_repeat, true),
        (fun () -> cycle_repeat st);
      `Entry (c, loop "Loop" ctl.loop, Layout.key_loop, true),
        (fun () -> cycle_loop st);
      `Separator, ignore;
      `Entry (c, unmute ctl.mute, Layout.key_mute, true),
        (fun () -> toggle_mute st);
      `Entry (c, "Volume Up", Layout.key_volup, ctl.volume < 1.0),
        (fun () -> shift_volume st (+1.0));
      `Entry (c, "Volume Down", Layout.key_voldn, ctl.volume > 0.0),
        (fun () -> shift_volume st (-1.0));
    |]
  )


(* Pane Activation *)

let run_toggle_panes (st : state) =
  let lay = st.layout in

  Layout.playlist_label lay;
  Layout.playlist_indicator lay lay.playlist_shown;
  let playlist_shown' = Layout.playlist_button lay (Some lay.playlist_shown) in
  (* Click on playlist activation button: toggle playlist *)
  lay.playlist_shown <- playlist_shown';
  if not playlist_shown' then Playlist.defocus st.playlist;

  Layout.library_label lay;
  Layout.library_indicator lay lay.library_shown;
  let library_shown' = Layout.library_button lay (Some lay.library_shown) in
  (* Click on library activation button: toggle library *)
  if library_shown' <> lay.library_shown then
  (
    if Api.Key.is_modifier_down `Shift then
      (* Shift-click: switch sides for library pane *)
      toggle_side st
    else
      toggle_library st
  )
  else if Layout.library_side_key lay then
  (
    (* Library side toggle key pressed: switch sides for library pane *)
    toggle_side st
  );

  (* Minimize button *)
  if Layout.minimize_button lay then
  (
    (* Right-click on power button: minimize window *)
    minimize st
  );

  (* Context menu *)
  if Layout.(info_context lay || shown_context lay) then
  (
    let c = Ui.text_color lay.ui in
    let show s b = (if b then "Show " else "Hide ") ^ s in
    let side s d = s ^ (match d with `Left -> " Right" | `Right -> " Left") in
    Run_menu.command_menu st [|
      `Entry (c, "Quit", Layout.key_quit, true),
        (fun () -> quit st);
      `Entry (c, "Minimize", Layout.key_min, true),
        (fun () -> minimize st);
      `Separator, ignore;
      `Entry (c, show "Playlist" (not lay.playlist_shown), Layout.key_pl, true),
        (fun () -> toggle_playlist st);
      `Entry (c, show "Library" (not lay.library_shown), Layout.key_lib, true),
        (fun () -> toggle_library st);
      `Entry (c, side "Expand to" lay.library_side, Layout.key_side, true),
        (fun () -> toggle_side st);
      `Separator, ignore;
      `Entry (c, "Cycle Color", Layout.key_color, true),
        (fun () -> cycle_color st (+1));
      `Entry (c, show "Cover" (not lay.playlist_shown), Layout.key_cover, true),
        (fun () -> toggle_cover st);
      `Entry (c, show "FPS" (not lay.playlist_shown), Layout.key_fps, true),
        (fun () -> toggle_fps st);
      `Separator, ignore;
      `Entry (c, "Increase Text Size", Layout.key_textup, resize_text_avail st (+1)),
        (fun () -> resize_text st (+1));
      `Entry (c, "Decrease Text Size", Layout.key_textdn, resize_text_avail st (-1)),
        (fun () -> resize_text st (-1));
      `Entry (c, "Increase Cover Size", Layout.key_gridup, resize_grid_avail st (+1)),
        (fun () -> resize_grid st (+1));
      `Entry (c, "Decrease Cover Size", Layout.key_griddn, resize_grid_avail st (-1)),
        (fun () -> resize_grid st (-1));
    |]
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
  val focus : (track, Ui.cached) Table.t -> State.t -> unit

  val length : Ui.cached t -> int
  val tracks : Ui.cached t -> track array
  val table : Ui.cached t -> (track, Ui.cached) Table.t

  val num_selected : Ui.cached t -> int
  val first_selected : Ui.cached t -> int option
  val selected : Ui.cached t -> track array
  val select_all : Ui.cached t -> unit
  val deselect_all : Ui.cached t -> unit
  val select_invert : Ui.cached t -> unit
(*  val select : Ui.cached t -> int -> int -> unit*)
(*  val deselect : Ui.cached t -> int -> int -> unit*)

  val insert : Ui.cached t -> int -> track array -> unit
  val replace_all : Ui.cached t -> track array -> unit
  val remove_all : Ui.cached t -> unit
  val remove_selected : Ui.cached t -> unit
  val remove_unselected : Ui.cached t -> unit
  val remove_invalid : Ui.cached t -> unit
(*  val move_selected : Ui.cached t -> int -> unit*)
  val reverse_selected : Ui.cached t -> unit
  val reverse_all : Ui.cached t -> unit
  val undo : Ui.cached t -> unit
  val redo : Ui.cached t -> unit
end

module Playlist = struct include Playlist let focus _ = State.focus_playlist end
module Library = struct include Library let focus = State.focus_library end


let update_control (st : state) =
  if Control.switch_if_empty st.control (Playlist.current_opt st.playlist) then
  (
    Table.dirty st.library.tracks;  (* current song has changed *)
    Table.dirty st.library.browser;
  )

let current_is_grid (st : state) =
  match st.library.current with
  | None -> false
  | Some dir -> dir.view.tracks.shown = Some `Grid

let drag (st : state) table_drag (module View : TracksView) =
  let lay = st.layout in
  let tab = View.table View.it in
  (* Drag over table: highlight target entry *)
  Ui.delay lay.ui (fun () -> table_drag lay tab)

let drag_on_playlist (st : state) =
  if st.layout.playlist_shown then
  (
    let module View = struct let it = st.playlist include Playlist end in
    drag st Layout.playlist_drag (module View)
  )

let tracks_drag is_grid (lay : Layout.t) =
  let drag, grid_drag =
    if lay.lower_shown then Layout.(lower_drag, lower_grid_drag) else
    if lay.right_shown then Layout.(right_drag, right_grid_drag) else
    Layout.(left_drag, left_grid_drag)
  in if is_grid then grid_drag lay lay.tracks_grid else drag lay

let drag_on_tracks (st : state) =
  if st.layout.library_shown && Library.current_is_shown_playlist st.library then
  (
    let module View = struct let it = st.library include Library end in
    drag st (tracks_drag (current_is_grid st)) (module View)
  )

let drop (st : state) tracks table_mouse (module View : TracksView) =
  if tracks <> [||] then
  (
    let lay = st.layout in
    let view = View.it in
    let tab = View.table view in
    Option.iter (fun pos ->
      (* Drop onto table: send tracks there *)
      View.insert view pos tracks;
      State.defocus_all st;
      View.focus tab st;
      update_control st;
    ) (table_mouse lay tab)
  )

let drop_on_playlist (st : state) tracks =
  if st.layout.playlist_shown then
  (
    let module View = struct let it = st.playlist include Playlist end in
    drop st tracks Layout.playlist_mouse (module View)
  )

let tracks_mouse is_grid (lay : Layout.t) =
  let mouse, grid_mouse =
    if lay.lower_shown then Layout.(lower_mouse, lower_grid_mouse) else
    if lay.right_shown then Layout.(right_mouse, right_grid_mouse) else
    Layout.(left_mouse, left_grid_mouse)
  in if is_grid then grid_mouse lay lay.tracks_grid else mouse lay

let drop_on_tracks (st : state) tracks =
  if st.layout.library_shown && Library.current_is_shown_playlist st.library then
  (
    let module View = struct let it = st.library include Library end in
    drop st tracks (tracks_mouse (current_is_grid st)) (module View)
  )

let drag_on_browser (st : state) =
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
          (* Drag over playlist browser entry: highlight target entry *)
          Ui.delay lay.ui (fun () -> Layout.browser_drag lay `Inside browser)
        )
      )
    ) (Layout.browser_mouse lay browser)
  )

let drop_on_browser (st : state) tracks =
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
          (* Drop onto playlist browser entry: send tracks there *)
          (* Since the dir might not be selected, and updating views is
           * asynchronous, write to file directly *)
          (try
            let s = File.load `Bin dir.path in
            let s' = Track.to_m3u (Array.append (Track.of_m3u s) tracks) in
            File.store `Bin dir.path s'
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

let set_drop_cursor (st : state) =
  let lay = st.layout in
  let pl = st.playlist in
  let lib = st.library in
  let droppable =
    lay.playlist_shown &&
      (* over playlist *)
      Layout.playlist_mouse lay pl.table <> None
    ||
    lay.library_shown && (
      (* over library playlist view? *)
      Library.current_is_playlist lib &&
        tracks_mouse (current_is_grid st) lay lib.tracks <> None
      ||
      (* over browser entry that is a playlist? *)
      match Layout.browser_mouse lay lib.browser with
      | None -> false
      | Some i ->
        i < Table.length lib.browser && Data.is_playlist lib.browser.entries.(i)
    )
  in
  Api.Mouse.set_cursor (Ui.window lay.ui)
    (if droppable then `Point else `Blocked)


(* Edit Pane *)

let editable (st : state) (module View : TracksView) =
  View.(table it) == st.playlist.table ||
  Library.current_is_playlist st.library

let separator_avail st view =
  editable st view
let separator _st (module View : TracksView) (module Other : TracksView) pos =
  View.(insert it) pos [|Data.make_separator ()|];
  Other.(deselect_all it)

let remove_avail st (module View : TracksView) =
  editable st (module View) && View.(num_selected it > 0)
let remove _st (module View : TracksView) _other =
  View.(remove_selected it)

let crop_avail st (module View : TracksView) =
  editable st (module View) &&
  View.(num_selected it > 0 && num_selected it < length it)
let crop _st (module View : TracksView) _other =
  View.(remove_unselected it)

let wipe_avail st (module View : TracksView) =
  editable st (module View) &&
  Array.exists (fun track -> track.Data.status = `Absent) View.(table it).entries
let wipe _st (module View : TracksView) _other =
  View.(remove_invalid it)

let clear_avail st (module View : TracksView) =
  editable st (module View) && View.(length it > 0)
let clear _st (module View : TracksView) _other =
  View.(remove_all it)

let undo_avail _st (module View : TracksView) =
  !(View.(table it).undos) <> []
let undo (st : state) (module View : TracksView) _other =
  View.(undo it);
  update_control st

let redo_avail _st (module View : TracksView) =
  !(View.(table it).redos) <> []
let redo (st : state) (module View : TracksView) _other =
  View.(redo it);
  update_control st

let copy_avail _st (module View : TracksView) =
  View.(num_selected it > 0)
let copy (st : state) (module View : TracksView) _other =
  let s = Track.to_m3u View.(selected it) in
  Api.Clipboard.write (Ui.window st.layout.ui) s

let cut_avail st view =
  copy_avail st view && remove_avail st view
let cut st view other =
  copy st view other;
  remove st view other

let paste_avail (st : state) view =
  editable st view && Api.Clipboard.read (Ui.window st.layout.ui) <> None
let paste (st : state) (module View : TracksView) (module Other : TracksView) =
  let s = Option.value (Api.Clipboard.read (Ui.window st.layout.ui)) ~default: "" in
  let tracks = Track.of_m3u s in
  let found_proper =
    Array.exists (fun (track : Data.track) ->
      Data.is_track_path track.path
    ) tracks
  in
  if found_proper && tracks <> [||] then
  (
    let pos = Option.value (View.first_selected View.it) ~default: 0 in
    View.(insert it) pos tracks;
    Other.(deselect_all it);
    update_control st;
  )

let select_all_avail _st (module View : TracksView) =
  View.(num_selected it < length it)
let select_all _st (module View : TracksView) (module Other : TracksView) =
  View.(select_all it);
  Other.(deselect_all it)

let select_none_avail _st (module View : TracksView) =
  View.(num_selected it > 0)
let select_none _st (module View : TracksView) _other =
  View.(deselect_all it)

let select_invert_avail _st (module View : TracksView) =
  View.(num_selected it > 0)
let select_invert _st (module View : TracksView) _other =
  View.(select_invert it)

let reverse_avail _st (module View : TracksView) =
  View.(num_selected it > 1)
let reverse _st (module View : TracksView) _other =
  View.(reverse_selected it)

let reverse_all_avail _st (module View : TracksView) =
  View.(length it > 1)
let reverse_all _st (module View : TracksView) _other =
  View.(reverse_all it)

let load_avail (st : state) (module View : TracksView) =
  editable st (module View) && not st.layout.filesel_shown
let load (st : state) (module View : TracksView) _other =
  Run_filesel.filesel st `Read `File "" ".m3u" (fun path ->
    let tracks = Track.of_m3u (File.load `Bin path) in
    View.(replace_all it) tracks;
    View.(focus (table it) st);
    if View.(table it) == st.playlist.table then
    (
      Control.eject st.control;
      Control.switch st.control tracks.(0) true;
      Table.dirty st.library.tracks;
      Table.dirty st.library.browser;
    )
  )

let save_avail (st : state) _view =
  not st.layout.filesel_shown
let save (st : state) (module View : TracksView) _other =
  Run_filesel.filesel st `Write `File "" ".m3u" (fun path ->
    File.store `Bin path (Track.to_m3u View.(table it).entries)
  )

let rescan_avail _st (module View : TracksView) =
  View.(length it > 0)
let rescan (st : state) tracks =
  Library.rescan_tracks st.library `Thorough tracks

let tag_avail (st : state) (module View : TracksView) =
  View.(length it > 0) &&
  try Unix.(access st.config.exec_tag [X_OK]); true
  with Unix.Unix_error _ -> false
let tag (st : state) tracks additive =
  let paths = Array.map (fun (track : Data.track) -> track.path) tracks in
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
  ) |> ignore


let run_edit (st : state) =
  let pl = st.playlist in
  let lib = st.library in
  let lay = st.layout in

  Layout.edit_pane lay;

  let lib_shows_tracks =
    match lib.current with
    | None -> false
    | Some dir -> dir.view.tracks.shown <> None
  in
  let pl_focus = pl.table.focus in
  let lib_focus = lib_shows_tracks &&
    ( lib.tracks.focus || lib.albums.focus || lib.artists.focus ||
      lib.browser.focus || lib.search.focus )
  in

  assert (not (pl_focus && lib_focus));
  assert (lay.playlist_shown || not pl_focus);
  assert (lay.library_shown || not lib_focus);

  let playlist = (module struct let it = pl include Playlist end : TracksView) in
  let library = (module struct let it = lib include Library end : TracksView) in
  let view, other = if pl_focus then playlist, library else library, playlist in
  let module View = (val view) in
  let module Other = (val other) in

  let active_if avail =
    if (pl_focus || lib_focus) && avail st view then Some false else None
  in

  (* Separator button *)
  if Layout.sep_button lay (active_if separator_avail) then
  (
    (* Click on Separator button: insert separator *)
    let pos = Option.value View.(first_selected it) ~default: 0 in
    separator st view other pos
  );

  (* Edit buttons *)
  if Layout.del_button lay (active_if remove_avail)
  || remove_avail st view && Layout.del_button_alt lay then
  (
    (* Click on Delete button: remove selected tracks from playlist *)
    remove st view other
  );

  if Layout.crop_button lay (active_if crop_avail) then
  (
    (* Click on Crop button: remove unselected tracks from playlist *)
    crop st view other
  );

  if Layout.wipe_button lay (active_if wipe_avail) then
  (
    (* Click on Wipe button: remove invalid tracks from playlist *)
    wipe st view other
  );

  if Layout.undo_button lay (active_if undo_avail) then
  (
    (* Click on Undo button: pop undo *)
    undo st view other
  );

  if Layout.redo_button lay then
  (
    (* Redo key pressed or Shift-click on Undo button: pop redo *)
    redo st view other
  );

  (* Edit keys *)
  if cut_avail st view && Layout.cut_key lay then
  (
    (* Press of Cut key: remove selected tracks and write them to clipboard *)
    cut st view other
  );

  if copy_avail st view && Layout.copy_key lay then
  (
    (* Press of Copy key: write selected tracks to clipboard *)
    copy st view other
  );

  if paste_avail st view && Layout.paste_key lay then
  (
    (* Press of Paste key: insert tracks from clipboard *)
    paste st view other
  );

  (* Tag button *)
  if Layout.tag_button lay (active_if tag_avail) then
  (
    (* Click on Tag button: execute tagging program *)
    let tracks =
      View.(if num_selected it > 0 then selected it else tracks it) in
    (* Command-click: add tracks to tagger if it's already open *)
    let additive = Api.Key.is_modifier_down `Command in
    tag st tracks additive;
  );

  (* Load button *)
  if Layout.load_button lay (active_if load_avail) then
  (
    (* Click on Load button: load playlist *)
    load st view other
  );

  (* Save button *)
  if Layout.save_button lay (active_if save_avail) then
  (
    (* Click on Save button: save playlist *)
    save st view other
  );

  (* Focus buttons *)
  if Layout.focus_next_key lay then State.focus_next st;
  if Layout.focus_prev_key lay then State.focus_prev st


let edit_menu (st : state) view other pos_opt =
  let lay = st.layout in
  let module View = (val view : TracksView) in 

  let pos = Option.value pos_opt ~default: View.(length it) in
  let cmd = Api.Key.is_modifier_down `Command in
  let c = Ui.text_color lay.ui in
  let all, quant, tracks =
    if View.(num_selected it) > 0
    then false, "", View.selected
    else true, " All", View.tracks
  in
  Run_menu.command_menu st [|
    `Entry (c, "Insert Separator", Layout.key_sep, separator_avail st view),
      (fun () -> separator st view other pos);
    `Separator, ignore;
    `Entry (c, "Tag" ^ quant, Layout.key_tag, tag_avail st view),
      (fun () -> tag st (tracks View.it) cmd);
    `Entry (c, "Rescan" ^ quant, Layout.key_rescan, rescan_avail st view),
      (fun () -> rescan st (tracks View.it));
    `Entry (c, "Remove" ^ quant, Layout.key_del,
      if all then clear_avail st view else remove_avail st view),
      (fun () -> (if all then clear else remove) st view other);
    `Entry (c, "Reverse" ^ quant, Layout.key_rev,
      if all then reverse_all_avail st view else reverse_avail st view),
      (fun () -> (if all then reverse_all else reverse) st view other);
    `Entry (c, "Wipe", Layout.key_wipe, wipe_avail st view),
      (fun () -> wipe st view other);
    `Separator, ignore;
    `Entry (c, "Cut", Layout.key_cut, cut_avail st view),
      (fun () -> cut st view other);
    `Entry (c, "Copy", Layout.key_copy, copy_avail st view),
      (fun () -> copy st view other);
    `Entry (c, "Paste", Layout.key_paste, paste_avail st view),
      (fun () -> paste st view other);
    `Entry (c, "Crop", Layout.key_crop, crop_avail st view),
      (fun () -> crop st view other);
    `Separator, ignore;
    `Entry (c, "Select All", Layout.key_all, select_all_avail st view),
      (fun () -> select_all st view other);
    `Entry (c, "Select None", Layout.key_none, select_none_avail st view),
      (fun () -> select_none st view other);
    `Entry (c, "Invert Selection", Layout.key_invert, select_invert_avail st view),
      (fun () -> select_invert st view other);
    `Separator, ignore;
    `Entry (c, "Undo", Layout.key_undo, undo_avail st view),
      (fun () -> undo st view other);
    `Entry (c, "Redo", Layout.key_redo, redo_avail st view),
      (fun () -> redo st view other);
    `Separator, ignore;
    `Entry (c, "Load...", Layout.key_load, load_avail st view),
      (fun () -> load st view other);
    `Entry (c, "Save...", Layout.key_save, save_avail st view),
      (fun () -> save st view other);
  |]


(* Playlist Pane *)

let run_playlist (st : state) =
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
  | `Sort _ | `Resize _ | `Reorder _ | `HeadMenu _ -> assert false

  | `Select ->
    State.focus_playlist st;
    Playlist.refresh_total_selected pl

  | `Click (Some i) when Api.Mouse.is_doubleclick `Left ->
    (* Double-click on track: switch to track *)
    Table.set_pos tab (Some i);
    Control.switch st.control tab.entries.(i) true;
    Table.dirty st.library.tracks;  (* redraw for current track *)
    Table.dirty st.library.browser;
    if pl.shuffle <> None then
      Playlist.shuffle_next pl i;

  | `Click _ ->
    (* Single-click: grab focus *)
    if Api.Mouse.is_pressed `Left then State.focus_playlist st;
    Playlist.refresh_total_selected pl;

  | `Move delta ->
    (* Cmd-cursor movement: move selection *)
    Playlist.move_selected pl delta;

  | `Drag (delta, way, motion) ->
    (* Drag: move selection if inside *)
    if Api.Key.are_modifiers_down [] then
    (
      State.focus_playlist st;
      if Playlist.num_selected pl > 0 then
      (
        if motion <> `Unmoved then set_drop_cursor st;
        match way with
        | `Inside | `Inward -> ()
        | `Outward | `Outside ->
          drag_on_tracks st;
          drag_on_browser st;
      );

      (* Invariant:
       * - on Start: no undo or redo added yet
       * - when Inside: one undo for returning to original state on undo stack
       * - when Outside: one redo for creating new state on redo stack
       *)
      if motion = `Moving then
      (
        (* Start of drag & drop: remember original configuration *)
        Table.push_undo pl.table;
      );

      (match way with
      | `Outward ->
        (* Leaving area: snap back to original state *)
        Playlist.undo pl;
        Playlist.save_playlist pl;
      | `Inward ->
        (* Reentering area: restore updated state *)
        Playlist.redo pl
      | `Inside | `Outside -> ()
      );

      (* Positional movement *)
      if delta <> 0 && Playlist.num_selected pl > 0 then
      (
        match way with
        | `Inside | `Inward ->
          Playlist.move_selected pl delta;
          (* Erase intermediate new state *)
          Table.drop_undo pl.table;
        | `Outside | `Outward -> ()  (* ignore *)
      );
    )

  | `Drop ->
    if Api.Key.are_modifiers_down [] then
    (
      if Ui.mouse_inside lay.ui (Layout.playlist_area lay) then
      (
        (* Dropping inside playlist: drop aux undo if no change *)
        Table.clean_undo pl.table
      )
      else
      (
        (* Dropping outside playlist: drop aux redo for new state *)
        Table.drop_redo pl.table;

        let tracks = Playlist.selected pl in
        drop_on_tracks st tracks;
        drop_on_browser st tracks;
      )
    );

  | `Menu i_opt ->
    (* Right-click on content: context menu *)
    State.focus_playlist st;
    let module View = struct let it = st.playlist include Playlist end in
    let module Other = struct let it = st.library include Library end in
    edit_menu st (module View) (module Other) i_opt
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

let rescan_all_avail (st : state) =
  st.library.root.children <> [||]
let rescan_all (st : state) mode =
  Option.iter (fun dir ->
    if Data.is_dir dir then Library.rescan_dirs st.library mode [|dir|]
  ) st.library.current

let rescan_one_avail (st : state) =
  st.library.current <> None && rescan_all_avail st
let rescan_one (st : state) mode =
  Option.iter (fun dir ->
    if Data.is_dir dir then Library.rescan_dirs st.library mode [|dir|]
  ) st.library.current

let insert_avail (st : state) =
  not st.layout.filesel_shown
let insert (st : state) =
  Run_filesel.filesel st `Read `Dir "" "" (fun path ->
    let roots = st.library.root.children in
    if not (Library.insert_roots st.library [path] (Array.length roots)) then
      Layout.browser_error_box st.layout;  (* flash *)
    State.focus_library st.library.browser st;
  )

let remove_avail (st : state) =
  Library.current_is_root st.library

let remove (st : state) =
  Option.iter (fun (dir : dir) ->
    if not (Library.remove_roots st.library [dir.path]) then
      Layout.browser_error_box st.layout  (* flash *)
  ) st.library.current

let remove_list_avail (st : state) =
  Library.current_is_playlist st.library ||
  Library.current_is_viewlist st.library

let remove_list (st : state) =
  let lib = st.library in
  Option.iter (fun (dir : dir) ->
    if Data.is_playlist dir && dir.tracks <> [||] then
    (
      Library.error lib "Playlist is not empty";
      Layout.browser_error_box st.layout  (* flash *)
    )
    else
    (
      assert (Data.is_playlist dir || Data.is_viewlist dir);
      (try File.delete dir.path with Sys_error msg ->
        Library.error lib ("Error deleting file " ^ dir.path ^ ", " ^ msg);
        Layout.browser_error_box st.layout  (* flash *)
      );
      if not (Library.remove_dir lib dir.path) then
        Layout.browser_error_box st.layout  (* flash *)
      else
        Library.refresh_artists_albums_tracks lib
    )
  ) lib.current


let create_list (st : state) ext s view_opt path =
  let lib = st.library in
  (match Library.find_dir lib File.(dir path // "") with
  | None ->
    Library.error lib
      ("Error creating file " ^ path ^ ", path is outside library");
    Layout.browser_error_box st.layout;  (* flash *)
  | Some parent ->
    let path =
      if String.lowercase_ascii (File.extension path) = ext
      then path
      else path ^ ext
    in
    File.store `Bin path s;
    match Library.insert_dir lib path with
    | None -> raise (Sys_error "library is out of sync")
    | Some dir ->
      Library.fold_dir lib parent false;
      Option.iter (fun view -> dir.view <- view) view_opt;
      Option.iter (Library.select_dir lib)
        (Array.find_index ((==) dir) lib.browser.entries)
  );
  State.focus_table lib.tracks st

let create_playlist_avail (st : state) =
  Library.current_is_dir st.library &&
  not (Library.current_is_all st.library)

let create_playlist (st : state) =
  Option.iter (fun (dir : dir) ->
    let path = if Data.is_dir dir then dir.path else File.dir dir.path in
    Run_filesel.filesel st `Write `File path ".m3u"
      (create_list st ".m3u" "" None);
  ) st.library.current

let create_viewlist_avail (st : state) =
  st.library.search.text <> "" && st.library.tracks.entries <> [||]

let create_viewlist (st : state) =
  Option.iter (fun (dir : dir) ->
    let prefix =
      if Data.is_all dir || Data.is_viewlist dir then ""
      else "\"" ^ dir.path ^ "\" @ #filepath "
    in
    let query = prefix ^ st.library.search.text in
    let view = Library.copy_views dir.view in
    view.search <- "";
    let path = if Data.is_dir dir then dir.path else File.dir dir.path in
    Run_filesel.filesel st `Write `File path ".m3v"
      (create_list st ".m3v" query (Some view));
  ) st.library.current


let spin_delay = 3
let spins = [|"|"; "/"; "-"; "\\"|]
let spin win = spins.(Api.Draw.frame win / spin_delay mod Array.length spins)

let convert_sorting columns sorting =
  let index attr = Array.find_index (fun (a, _) -> a = attr) columns in
  List.map (fun (attr, order) -> Option.get (index attr), order) sorting

let busy_artists = Table.make 0
let busy_albums = Table.make 0
let busy_tracks = Table.make 0

let run_library (st : state) =
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
    let dir = browser.entries.(i) in
    Library.fold_dir st.library dir (not dir.view.folded)

  | `Click (Some i) ->
    (* Click on dir name: switch view *)
    (* TODO: allow multiple selections *)
    if Api.Mouse.is_pressed `Left then State.focus_library browser st;
    if Library.selected_dir lib <> dir then
    (
      Library.select_dir lib i;  (* do bureaucracy *)
      Library.deselect_all lib;
      Library.refresh_artists_albums_tracks lib;
      let dir' = entries.(i) in
      lay.left_width <- dir'.view.divider_width;
      lay.upper_height <- dir'.view.divider_height;
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
        Table.dirty st.library.tracks;  (* redraw for current track *)
        Table.dirty st.library.browser;
      )
    )

  | `Click None ->
    (* Click into empty space: deselect everything *)
    Library.deselect_dir lib;
    Library.deselect_all lib;
    Library.refresh_artists_albums_tracks lib;
    if Api.Mouse.is_pressed `Left then State.focus_library browser st;

  | `Drag (_, _, motion) ->
    (* Drag: adjust cursor *)
    if Api.Key.are_modifiers_down [] then
    (
      (* State.focus_library browser st; *)  (* don't steal after double-click! *)
      if lib.tracks.entries <> [||] then
      (
        if motion <> `Unmoved then set_drop_cursor st;
        drag_on_playlist st;
      );

      (* Intra-browser drag *)
      Option.iter (fun i ->
        if Library.selected_dir lib <> Some i then
        (
          Option.iter (fun j ->
            let dir = browser.entries.(j) in
            if
              i = Table.length browser && Data.is_root dir ||
              browser.entries.(i).parent = dir.parent
            then
            (
              (* Drag over sibling: reorder entry *)
              Api.Mouse.set_cursor (Ui.window lay.ui) `Point;
              Layout.browser_drag lay `Above browser;
            )
            else
            (
              (* Drag over other browser entry *)
              drag_on_browser st;
            )
          ) (Library.selected_dir lib)
        )
      ) (Layout.browser_mouse lay browser)
    )

  | `Drop ->
    (* Drop originating from browser *)
    if Api.Key.are_modifiers_down [] then
    (
      (* Drop onto playlist: send directory contents to playlist *)
      let tracks = lib.tracks.entries in
      drop_on_playlist st tracks;

      (* Intra-browser drop *)
      Option.iter (fun i ->
        if Library.selected_dir lib <> Some i then
        (
          Option.iter (fun j ->
            let dir = browser.entries.(j) in
            if
              i = Table.length browser && Data.is_root dir ||
              browser.entries.(i).parent = dir.parent
            then
            (
              (* Drop on sibling: reorder entry *)
              let parent = Option.get (Library.find_parent lib dir) in
              let pos = Library.find_parent_pos lib dir in
              let pos' =
                if i = Table.length browser
                then Array.length lib.root.children
                else Library.find_parent_pos lib entries.(i)
              in
              Library.move_dir lib parent pos
                (if pos' > pos then pos' - 1 else pos');
            )
            else
            (
              (* Drop on other browser entry *)
              drop_on_browser st tracks;
            )
          ) (Library.selected_dir lib)
        )
      ) (Layout.browser_mouse lay browser)
    )

  | `Menu _ ->
    (* Right-click on browser: context menu *)
    State.focus_library browser st;
    let c = Ui.text_color lay.ui in
    let all, quant =
      if lib.current = None then true, " All" else false, "" in
    Run_menu.command_menu st [|
      `Entry (c, "Rescan" ^ quant ^ " Quick", Layout.key_rescan,
        if all then rescan_all_avail st else rescan_one_avail st),
        (fun () -> (if all then rescan_all else rescan_one) st `Quick);
      `Entry (c, "Rescan" ^ quant ^ " Thorough", Layout.key_rescan2,
        if all then rescan_all_avail st else rescan_one_avail st),
        (fun () -> (if all then rescan_all else rescan_one) st `Thorough);
      `Separator, ignore;
      `Entry (c, "Add Root...", Layout.key_adddir, insert_avail st),
        (fun () -> insert st);
      `Entry (c, "Remove Root", Layout.key_deldir, remove_avail st),
        (fun () -> remove st);
      `Separator, ignore;
      `Entry (c, "Create Playlist...", Layout.key_newdir, create_playlist_avail st),
        (fun () -> create_playlist st);
      `Entry (c, "Create Viewlist...", Layout.key_viewdir, create_viewlist_avail st),
        (fun () -> create_playlist st);
      `Entry (c, "Remove " ^
        (if Library.current_is_viewlist lib then "Viewlist" else "Playlist"),
        Layout.key_deldir, remove_list_avail st),
        (fun () -> remove_list st);
      `Separator, ignore;
      `Entry (c, "Search...", Layout.key_search, lib.current <> None),
        (fun () -> State.focus_edit lib.search st);
    |]
  );

  let entries = browser.entries in  (* might have changed from un/folding *)

  (* Browser drag & drop *)
  let dropped = Api.Files.dropped win in
  if dropped <> [] then
  (
    Option.iter (fun i ->
      let pos =
        if i = Array.length entries
        then Array.length lib.root.children
        else Library.find_parent_pos lib entries.(i)
      in
      if not (Library.insert_roots lib dropped pos) then
        Layout.browser_error_box lay;  (* flash *)
    ) (Layout.browser_mouse lay browser)
  );

  (* Buttons *)
  let active_if avail = if avail st then Some false else None in
  let active_if2 avail1 avail2 =
    if avail1 st || avail2 st then Some false else None in

  if Layout.insert_button lay (active_if insert_avail) then
  (
    (* Click on Insert (Add) button: add directory or playlist *)
    insert st
  );

  if Layout.remove_button lay (active_if2 remove_avail remove_list_avail) then
  (
    (* Click on Remove (Del) button: remove directory or playlist *)
    let dir = Option.get (lib.current) in
    if Data.is_dir dir then
      remove st
    else if Data.is_playlist dir || Data.is_viewlist dir then
      remove_list st
  );

  if Layout.create_button lay (active_if create_playlist_avail) then
  (
    (* Click on Create (New) button: create new playlist *)
    create_playlist st
  );

  if Layout.view_button lay (active_if create_viewlist_avail) then
  (
    (* Click on View button: create new viewlist *)
    create_viewlist st
  );

  if Layout.rescan_button lay (active_if rescan_one_avail) then
  (
    (* Click on Rescan (Scan) button: rescan directory, view, or files *)
    Option.iter (fun (dir : dir) ->
      let mode =
        if Api.Key.is_modifier_down `Shift
        || dir.view.tracks.shown <> None && Table.has_selection lib.tracks
        || dir.view.albums.shown <> None && Table.has_selection lib.albums
        || dir.view.artists.shown <> None && Table.has_selection lib.artists
        then `Thorough else `Quick
      in
      if Table.has_selection lib.tracks then
        Library.rescan_tracks lib mode (Library.selected lib)
      else if
        Table.has_selection lib.artists || Table.has_selection lib.albums ||
        dir.view.search <> ""
      then
        Library.rescan_tracks lib mode lib.tracks.entries
      else
        Library.rescan_dirs lib mode [|dir|]
    ) lib.current
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
  let default =
    if Array.length entries > 0 then entries.(0) else
    Data.make_dir "" None 0 (Library.make_views "")
  in
  let dir = Option.value lib.current ~default in
  let view = dir.view in
  let cycle_shown = function
    | None -> Some `Table
    | Some `Table -> Some `Grid
    | Some `Grid -> None
  in

  let nothing_shown (view : Library.views) =
    view.artists.shown = None &&
    view.albums.shown = None &&
    view.tracks.shown = None
  in

  let artists = have_dir && view.artists.shown <> None in
  Layout.artists_label lay;
  Layout.artists_indicator lay artists;
  let artists' =
    Layout.artists_button lay (if have_dir then Some artists else None) in
  if have_dir && artists' <> artists then
  (
    (* Click on Artists button: toggle artist pane *)
    view.artists.shown <- if artists' then Some `Table else None;
    if nothing_shown view then
      view.tracks.shown <- Some `Table;  (* switch to tracks *)
    Library.save_dir lib dir;
  );

  let albums = have_dir && view.albums.shown <> None in
  Layout.albums_label lay;
  Layout.albums_indicator1 lay (view.albums.shown = Some `Table);
  Layout.albums_indicator2 lay (view.albums.shown = Some `Grid);
  let albums' =
    Layout.albums_button lay (if have_dir then Some albums else None) in
  if have_dir && albums' <> albums then
  (
    (* Click on Albums button: toggle artist pane *)
    view.albums.shown <- cycle_shown view.albums.shown;
    if nothing_shown view then
      view.albums.shown <- Some `Table;
    Library.save_dir lib dir;
  );

  let tracks = have_dir && view.tracks.shown <> None in
  Layout.tracks_label lay;
  Layout.tracks_indicator1 lay (view.tracks.shown = Some `Table);
  Layout.tracks_indicator2 lay (view.tracks.shown = Some `Grid);
  let tracks' =
    Layout.tracks_button lay (if have_dir then Some tracks else None) in
  if have_dir && tracks' <> tracks then
  (
    (* Click on Tracks button: toggle artist pane *)
    view.tracks.shown <- cycle_shown view.tracks.shown;
    if nothing_shown view then
      view.tracks.shown <- Some `Table;
    Library.save_dir lib dir;
  );

  let show_artists =
    have_dir && view.artists.shown <> None && lay.playlist_shown in
  let show_albums =
    have_dir && view.albums.shown <> None && lay.playlist_shown in
  let show_tracks =
    not have_dir || view.tracks.shown <> None || not lay.playlist_shown in
  lay.right_shown <- show_artists && show_albums;
  lay.lower_shown <- show_tracks && (show_artists || show_albums);

  (* Search *)
  Layout.search_label lay;
  Layout.search_box lay;
  if have_dir then
  (
    if Layout.search_key lay then
    (
      (* Search button pressed: focus search *)
      Library.focus_search lib;
    )
    else if Layout.search_button lay then
    (
      (* Click on Search label: clear and focus search *)
      if lib.search.text <> "" then
      (
        Edit.clear lib.search;
        Library.set_search lib "";
      );
      Library.focus_search lib;
    );

    let search = lib.search.text in
    let _ = Layout.search_text lay lib.search in
    if lib.search.focus then
    (
      (* Have or gained focus: make sure it's consistent *)
      State.defocus_all st;
      Library.focus_search lib;
    );
    if lib.search.text <> search then
    (
      (* Changed search text: update search in dir *)
      Library.set_search lib lib.search.text;
    );

    if Layout.search_context lay then
    (
      let rec nub = function
        | [] -> []
        | x::xs -> x :: nub (List.filter ((<>) x) xs)
      in
      let c = Ui.text_color lay.ui in
      let history = Edit.history lib.search in
      let history' = nub history in
      Run_menu.command_menu st ([
        `Entry (c, "Clear Search", Layout.key_clear_search, lib.search.text <> ""),
          (fun () -> Edit.clear lib.search; Library.set_search lib "";
            State.focus_edit lib.search st);
        `Entry (c, "Clear Search History", Layout.key_clear_history, history <> []),
          (fun () ->
            Edit.clear_history lib.search;
            Data.iter_dir (fun (dir : Library.dir) ->
              if dir.view.search <> "" then
              (
                dir.view.search <- "";
                Library.save_dir lib dir;
              )
            ) lib.root;
            State.focus_edit lib.search st
          );
      ] @ (
        if history = [] then [] else [`Separator, ignore]
      ) @ List.map (fun s ->
        `Entry (c, "Search for " ^ s, Layout.nokey, true),
          (fun () -> Edit.set lib.search s; Library.set_search lib s;
            State.focus_edit lib.search st)
      ) history' |> Array.of_list)
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
(*
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
*)
    let count name m n = fmt "%s%d %s%s" (sel m) n name (plu n) in
    let counts = String.concat ", "
      [count "artist" ars ar; count "album" als al; count "track" trs tr] in
    Layout.msg_text lay (Ui.text_color lay.ui) `Regular true counts
  );


  (* Artists view *)

  if show_artists then
  (
    let artists_pane, artists_area, artists_table, _grid, artists_spin =
      Layout.left_view in
    artists_pane lay;

    let busy = Library.refresh_artists_busy lib in
    let tab = if busy then busy_artists else lib.artists in
    let old_selected = tab.selected in
    let cols =
      Array.map (fun (attr, cw) -> cw, Library.attr_align attr)
        view.artists.columns
    and headings =
      Array.map (fun (attr, _) -> Library.attr_name attr) view.artists.columns
    in

    let entries = tab.entries in  (* could change concurrently *)
    let pp_row i =
      let artist = entries.(i) in
      Ui.text_color lay.ui,
      Array.map (fun (attr, _) -> `Text (Data.artist_attr_string artist attr))
        view.artists.columns
    in

    let sorting = convert_sorting view.artists.columns view.artists.sorting in
    (match artists_table lay cols (Some (headings, sorting)) tab pp_row with
    | `None | `Scroll | `Move _ -> ()

    | `Select ->
      (* New selection: grab focus, update filter *)
      State.focus_library tab st;
      Library.refresh_albums_tracks lib;

    | `Sort i ->
      (* Click on column header: reorder view accordingly *)
      let attr = fst view.artists.columns.(i) in
      let k =
        Bool.to_int (Api.Key.is_modifier_down `Shift) +
        Bool.to_int (Api.Key.is_modifier_down `Alt) * 2 +
        Bool.to_int (Api.Key.is_modifier_down `Command) * (-4)
      in
      view.artists.sorting <-
        Data.insert_sorting `Artist attr k 4 view.artists.sorting;
      Library.save_dir lib dir;
      Library.reorder_artists lib;

    | `Resize ws ->
      (* Column resizing: update column widths *)
      Array.mapi_inplace (fun i (a, _) -> a, ws.(i)) view.artists.columns;
      if have_dir then Library.save_dir lib dir;

    | `Reorder perm ->
      (* Column reordering: update columns *)
      Data.permute perm view.artists.columns;
      if have_dir then Library.save_dir lib dir;

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
        Table.dirty st.library.tracks;  (* redraw for current track *)
        Table.dirty st.library.browser;
      )

    | `Click _ ->
      (* Single-click: grab focus, update filter *)
      if Api.Mouse.is_pressed `Left then State.focus_library tab st;
      if not (Table.IntSet.equal tab.selected old_selected) then
        Library.refresh_albums_tracks lib;

    | `Drag (_, _, motion) ->
      (* Drag: adjust cursor *)
      if Api.Key.are_modifiers_down [] then
      (
        (* State.focus_library tab st; *)  (* don't steal after double-click! *)
        if Table.num_selected lib.artists > 0 && lib.tracks.entries <> [||] then
        (
          if motion <> `Unmoved then set_drop_cursor st;
          drag_on_playlist st;
          drag_on_browser st;
        )
      );

    | `Drop ->
      if Api.Key.are_modifiers_down []
      && not (Ui.mouse_inside lay.ui (artists_area lay)) then
      (
        (* Drag & drop originating from artists view *)

        (* Drag & drop onto playlist or browser: send tracks to playlist *)
        let tracks = lib.tracks.entries in
        drop_on_playlist st tracks;
        drop_on_browser st tracks;
      )

    | `Menu _ ->
      (* Right-click on artists: context menu *)
      State.focus_library tab st;
      let c = Ui.text_color lay.ui in
      let cmd = Api.Key.is_modifier_down `Command in
      let tracks = lib.tracks.entries in
      let quant = if Table.has_selection tab then "" else " All" in
      Run_menu.command_menu st [|
        `Entry (c, "Tag" ^ quant, Layout.key_tag, tracks <> [||]),
          (fun () -> tag st tracks cmd);
        `Entry (c, "Rescan" ^ quant, Layout.key_rescan, tracks <> [||]),
          (fun () -> Library.rescan_tracks lib `Thorough tracks);
        `Separator, ignore;
        `Entry (c, "Select All", Layout.key_all,  Table.(num_selected tab < length tab)),
          (fun () -> Table.select_all tab; Library.refresh_albums_tracks lib);
        `Entry (c, "Select None", Layout.key_none, Table.(num_selected tab > 0)),
          (fun () -> Table.deselect_all tab; Library.refresh_albums_tracks lib);
        `Entry (c, "Invert Selection", Layout.key_invert, Table.(num_selected tab > 0)),
          (fun () -> Table.select_invert tab; Library.refresh_albums_tracks lib);
        `Separator, ignore;
        `Entry (c, "Search...", Layout.key_search, lib.current <> None),
          (fun () -> State.focus_edit lib.search st);
      |]

    | `HeadMenu i_opt ->
      (* Right-click on artists header: header menu *)
      State.focus_library tab st;
      let used_attrs = Array.to_list (Array.map fst view.artists.columns) in
      let unused_attrs = Data.diff_attrs Data.artist_attrs used_attrs in
      let i, current_attrs =
        match i_opt with
        | None -> Array.length view.artists.columns, []
        | Some i -> i, [fst view.artists.columns.(i)]
      in
      Run_menu.header_menu st dir.view.artists i current_attrs unused_attrs
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
    let old_selected = tab.selected in
    let cols =
      Array.map (fun (attr, cw) -> cw, Library.attr_align attr)
        view.albums.columns
    and headings =
      Array.map (fun (attr, _) -> Library.attr_name attr) view.albums.columns
    in

    if Option.get view.albums.shown = `Grid
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
      ) view.albums.columns
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
      in img, Ui.text_color lay.ui, txt
    in

    let sorting = convert_sorting view.albums.columns view.albums.sorting in
    let header = Some (headings, sorting) in
    (match
      match Option.get view.albums.shown with
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
      let attr = fst view.albums.columns.(i) in
      let k =
        Bool.to_int (Api.Key.is_modifier_down `Shift) +
        Bool.to_int (Api.Key.is_modifier_down `Alt) * 2 +
        Bool.to_int (Api.Key.is_modifier_down `Command) * (-4)
      in
      view.albums.sorting <-
        Data.insert_sorting `None attr k 4 view.albums.sorting;
      Library.save_dir lib dir;
      Library.reorder_albums lib;

    | `Resize ws ->
      (* Column resizing: update column widths *)
      Array.mapi_inplace (fun i (a, _) -> a, ws.(i)) view.albums.columns;
      if have_dir then Library.save_dir lib dir;

    | `Reorder perm ->
      (* Column reordering: update columns *)
      Data.permute perm view.albums.columns;
      if have_dir then Library.save_dir lib dir;

    | `Click (Some _i) when Api.Mouse.is_doubleclick `Left ->
      (* Double-click on track: clear playlist and send tracks to it *)
      let tracks = lib.tracks.entries in
      if tracks <> [||] then
      (
        Playlist.replace_all pl (Array.copy tracks);
        State.focus_playlist st;
        Control.eject st.control;
        Control.switch st.control tracks.(0) true;
        Table.dirty st.library.tracks;  (* redraw for current track *)
        Table.dirty st.library.browser;
      )

    | `Click _ ->
      (* Single-click: grab focus, update filter *)
      if Api.Mouse.is_pressed `Left then State.focus_library tab st;
      if not (Table.IntSet.equal tab.selected old_selected) then
        Library.refresh_tracks lib;

    | `Drag (_, _, motion) ->
      (* Drag: adjust cursor *)
      if Api.Key.are_modifiers_down [] then
      (
        (* State.focus_library tab st; *)  (* don't steal after double-click! *)
        if Table.num_selected lib.albums > 0 && lib.tracks.entries <> [||] then
        (
          if motion <> `Unmoved then set_drop_cursor st;
          drag_on_playlist st;
          drag_on_browser st;
        )
      );

    | `Drop ->
      if Api.Key.are_modifiers_down []
      && not (Ui.mouse_inside lay.ui (albums_area lay)) then
      (
        (* Drag & drop originating from albums view *)

        (* Drag & drop onto playlist or browser: send tracks to playlist *)
        let tracks = lib.tracks.entries in
        drop_on_playlist st tracks;
        drop_on_browser st tracks;
      )

    | `Menu _ ->
      (* Right-click on albums content: context menu *)
      State.focus_library tab st;
      let c = Ui.text_color lay.ui in
      let cmd = Api.Key.is_modifier_down `Command in
      let tracks = lib.tracks.entries in
      let quant = if Table.has_selection tab then "" else " All" in
      Run_menu.command_menu st [|
        `Entry (c, "Tag" ^ quant, Layout.key_tag, tracks <> [||]),
          (fun () -> tag st tracks cmd);
        `Entry (c, "Rescan" ^ quant, Layout.key_rescan, tracks <> [||]),
          (fun () -> Library.rescan_tracks lib `Thorough tracks);
        `Separator, ignore;
        `Entry (c, "Select All", Layout.key_all, Table.(num_selected tab < length tab)),
          (fun () -> Table.select_all tab; Library.refresh_tracks lib);
        `Entry (c, "Select None", Layout.key_none, Table.(num_selected tab > 0)),
          (fun () -> Table.deselect_all tab; Library.refresh_tracks lib);
        `Entry (c, "Invert Selection", Layout.key_invert, Table.(num_selected tab > 0)),
          (fun () -> Table.select_invert tab; Library.refresh_tracks lib);
        `Separator, ignore;
        `Entry (c, "Search...", Layout.key_search, lib.current <> None),
          (fun () -> State.focus_edit lib.search st);
      |]

    | `HeadMenu i_opt ->
      (* Right-click on albums header: header menu *)
      State.focus_library tab st;
      let used_attrs = Array.to_list (Array.map fst view.albums.columns) in
      let unused_attrs = Data.diff_attrs Data.album_attrs used_attrs in
      let i, current_attrs =
        match i_opt with
        | None -> Array.length view.albums.columns, []
        | Some i -> i, [fst view.albums.columns.(i)]
      in
      Run_menu.header_menu st dir.view.albums i current_attrs unused_attrs
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
      dir.view.divider_width <- left_width';
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
        view.tracks.columns
    and headings =
      Array.map (fun (attr, _) -> Library.attr_name attr) view.tracks.columns
    in

    if Option.get view.tracks.shown = `Grid
    && Api.Draw.frame win mod refresh_delay = 5 then
      Table.dirty tab;  (* to capture cover updates *)

    let track_color (track : Data.track) =
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

    let entries = tab.entries in  (* may update concurrently *)
    let pp_row i =
      let track = entries.(i) in
      track_color track,
      Array.map (fun (attr, _) ->
        if attr <> `Cover then
          `Text (Data.track_attr_string track attr)
        else if lib.cover then
          match Library.load_cover lib win track.path with
          | Some img -> `Image img
          | None -> `Text ""
        else `Text ""
      ) view.tracks.columns
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
      in img, track_color track, txt
    in

    let sorting = convert_sorting view.tracks.columns view.tracks.sorting in
    let header = Some (headings, sorting) in
    (match
      match Option.get view.tracks.shown with
      | `Table -> tracks_table lay cols header tab pp_row
      | `Grid -> tracks_grid lay lay.tracks_grid header tab pp_cell
    with
    | `None | `Scroll -> ()

    | `Select ->
      State.focus_library tab st;

    | `Sort i ->
      (* Click on column header: reorder view accordingly *)
      let attr = fst view.tracks.columns.(i) in
      let k =
        Bool.to_int (Api.Key.is_modifier_down `Shift) +
        Bool.to_int (Api.Key.is_modifier_down `Alt) * 2 +
        Bool.to_int (Api.Key.is_modifier_down `Command) * (-4)
      in
      let primary =
        if Library.current_is_playlist lib
        || Library.current_is_viewlist lib then `Pos else `FilePath in
      view.tracks.sorting <-
        Data.insert_sorting primary attr k 4 view.tracks.sorting;
      Library.save_dir lib dir;
      Library.reorder_tracks lib;

    | `Resize ws ->
      (* Column resizing: update column widths *)
      Array.mapi_inplace (fun i (a, _) -> a, ws.(i)) view.tracks.columns;
      if have_dir then Library.save_dir lib dir;

    | `Reorder perm ->
      (* Column reordering: update columns *)
      Data.permute perm view.tracks.columns;
      if have_dir then Library.save_dir lib dir;

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
        Table.dirty st.library.tracks;  (* redraw for current track *)
        Table.dirty st.library.browser;
      )

    | `Click _ ->
      (* Single-click: grab focus *)
      if Api.Mouse.is_pressed `Left then State.focus_library tab st;

    | `Move delta ->
      (* Cmd-cursor movement: move selection *)
      if Data.is_playlist dir then
        Library.move_selected lib delta;

    | `Drag (delta, way, motion) ->
      (* Drag: move selection if inside *)
      if Api.Key.are_modifiers_down [] then
      (
        (* State.focus_library tab st; *)  (* don't steal after double-click! *)
        if Library.num_selected lib > 0 then
        (
          if motion <> `Unmoved then set_drop_cursor st;
          (match way with
          | `Inside | `Inward -> ()
          | `Outside | `Outward ->
            drag_on_playlist st;
            drag_on_browser st;
          );

          if Library.current_is_plain_playlist lib then
          (
            (* Invariant as for playlist view *)
            if motion = `Moving then
            (
              (* Start of drag & drop: remember original configuration *)
              Table.push_undo lib.tracks;
            );
            (match way with
            | `Outward ->
              (* Leaving area: snap back to original state *)
              Library.undo lib;
              Library.save_playlist lib;
            | `Inward ->
              (* Reentering area: restore updated state *)
              Library.redo lib
            | `Inside | `Outside -> ()
            );

            (* Positional movement *)
            if delta <> 0 then
            (
              match way with
              | `Inside | `Inward ->
                Library.move_selected lib delta;
                (* Erase intermediate new state *)
                Table.drop_undo lib.tracks;
              | `Outside | `Outward -> ()  (* ignore *)
            );
          )
        )
      )

    | `Drop ->
      if Api.Key.are_modifiers_down [] then
      (
        if Ui.mouse_inside lay.ui (tracks_area lay) then
        (
          (* Dropping inside tracks: drop aux undo if no change *)
          Table.clean_undo lib.tracks
        )
        else
        (
          (* Drag & drop originating from tracks *)

          (* Dropping outside tracks: drop aux redo for new state *)
          if Library.current_is_plain_playlist lib then
            Table.drop_redo lib.tracks;

          (* Drag & drop onto playlist or browser: send tracks to playlist *)
          let tracks = Library.selected lib in
          drop_on_playlist st tracks;
          drop_on_browser st tracks;
        )
      )

    | `Menu i_opt ->
      (* Right-click on tracks content: context menu *)
      State.focus_library tab st;
      let module View = struct let it = st.library include Library end in
      let module Other = struct let it = st.playlist include Playlist end in
      let view = (module View : TracksView) in
      let other = (module Other : TracksView) in
      if Library.current_is_playlist lib then
      (
        edit_menu st view other i_opt
      )
      else
      (
        let c = Ui.text_color lay.ui in
        let cmd = Api.Key.is_modifier_down `Command in
        let quant, tracks =
          if Library.has_selection lib
          then "", Library.selected
          else " All", Fun.const entries
        in
        Run_menu.command_menu st [|
          `Entry (c, "Tag" ^ quant, Layout.key_tag, tag_avail st view),
            (fun () -> tag st (tracks lib) cmd);
          `Entry (c, "Rescan" ^ quant, Layout.key_rescan, rescan_avail st view),
            (fun () -> rescan st (tracks lib));
          `Separator, ignore;
          `Entry (c, "Select All", Layout.key_all, select_all_avail st view),
            (fun () -> select_all st view other);
          `Entry (c, "Select None", Layout.key_none, select_none_avail st view),
            (fun () -> select_none st view other);
          `Entry (c, "Invert Selection", Layout.key_invert, select_invert_avail st view),
            (fun () -> select_invert st view other);
          `Separator, ignore;
          `Entry (c, "Search...", Layout.key_search, lib.current <> None),
            (fun () -> State.focus_edit lib.search st);
          `Separator, ignore;
          `Entry (c, "Save...", Layout.key_save, save_avail st view),
            (fun () -> save st view other);
        |]
      )

    | `HeadMenu i_opt ->
      (* Right-click on tracks header: header menu *)
      State.focus_library tab st;
      let used_attrs = Array.to_list (Array.map fst view.tracks.columns) in
      let unused_attrs = Data.diff_attrs Data.track_attrs used_attrs in
      let i, current_attrs =
        match i_opt with
        | None -> Array.length view.tracks.columns, []
        | Some i -> i, [fst view.tracks.columns.(i)]
      in
      Run_menu.header_menu st dir.view.tracks i current_attrs unused_attrs
    );

    if busy then
      tracks_spin lay (spin win);

    (* Playlist drag & drop *)
    let dropped = Api.Files.dropped win in
    if dropped <> [] then
    (
      (* Files drop: insert paths at pointed position *)
      drop_on_tracks st (expand_paths lib dropped);
    );

    (* Divider *)
    if lay.lower_shown then
    (
      let upper_height' = Layout.lower_divider lay lay.upper_height
        (Layout.upper_min lay) (Layout.upper_max lay) in
      (* Possible drag of divider: update pane width *)
      lay.upper_height <- upper_height';
      dir.view.divider_height <- upper_height';
    );
  );

  (* Keys *)

  if
    Ui.key lay.ui Layout.key_copy
      ( (lib.browser.focus || lib.artists.focus || lib.albums.focus) &&
        lib.tracks.entries <> [||] )
  then
  (
    (* Press of Copy key: write selected tracks to clipboard *)
    let s = Track.to_m3u lib.tracks.entries in
    Api.Clipboard.write win s;
  );

  (* Pane divider *)

  let browser_width' = Layout.browser_divider lay lay.browser_width
    (Layout.browser_min lay) (Layout.browser_max lay) in
  (* Possible drag of divider: update pane width *)
  lay.browser_width <- browser_width'


(* Runner *)

let rec run (st : state) =
  State.ok st;
  (try run' st with exn -> Storage.log_exn "internal" exn ""; exit 0);
  run st

and run' (st : state) =
  let lay = st.layout in
  let win = Ui.window lay.ui in
  if Api.Window.closed win then quit st;

  (* Start drawing *)
  Ui.start lay.ui;

  (* Remember current geometry for later *)
  let playlist_shown = lay.playlist_shown in
  let library_shown = lay.library_shown in
  let filesel_shown = lay.filesel_shown in
  let overlay_shown = library_shown || filesel_shown in
  let menu_shown = lay.menu_shown in
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
    if filesel_shown then Run_filesel.run st
    else if library_shown then run_library st;
    if playlist_shown || overlay_shown then run_edit st;
    run_toggle_panes st;
    if menu_shown then Run_menu.run st;
  );

  (* Adjust font and grid size *)
  let text_delta =
    Bool.to_int (Layout.enlarge_key lay) -
    Bool.to_int (Layout.reduce_key lay)
  in
  resize_text st text_delta;

  let grid_delta =
    Bool.to_int (Layout.enlarge_grid_key lay) -
    Bool.to_int (Layout.reduce_grid_key lay)
  in
  resize_grid st grid_delta;

  if Layout.lib_cover_key lay then
    Library.activate_covers st.library (not st.library.cover);

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
    Api.Window.reveal win;

  (* Save state regularly every second *)
  State.save_after st 1.0


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
    Api.Audio.pause st.control.audio;
    State.save st;
    Storage.clear_temp ();
  );
  st

let _main =
  try
    Printexc.record_backtrace true;
    Arg.parse ["--dperf", Arg.Set App.debug_perf, "Log times"] ignore "";
    (* Work around seeming bug in GC scheduler. *)
    Gc.(set {(get ()) with space_overhead = 10});
    run (startup ())
  with exn ->
    Storage.log_exn "internal" exn "";
    Stdlib.exit 2
