(* Main Program *)

open Audio_file


(* State *)

type state = State.t


(* Runner *)

let queue_file = "queue.m3u"

let t_start = Unix.gettimeofday ()
let t_last = ref 0.0

let rec run (st : state) =
  State.ok st;
  (try run' st with exn -> Storage.log_exn "internal" exn ""; exit 0);
  if !App.debug_perf then
  (
    let t = Unix.gettimeofday () -. t_start in
    if t >= !t_last +. 10.0 then
    (
      t_last := t;
      let gc = Gc.quick_stat () in
      Printf.eprintf
        "[%s] GC memory %#d live, %#d total, %d collections, %d compactions\n%!"
        (Data.string_of_time t)
        gc.live_words gc.heap_words gc.major_collections gc.compactions;
    )
  );
  run st

and run' (st : state) =
  let geo = st.geometry in
  let win = Ui.window geo.ui in
  if Api.Window.closed win then Run_control.quit st;

  (* App invocation with arguments *)
  let m3u = ref "" in
  Storage.load_string queue_file ((:=) m3u);
  if !m3u <> "" then
  (
    (* TODO: this could race, should lock the file *)
    Storage.save_string queue_file (fun () -> "");
    (* If we're just starting up, and double-click caused it, force playing *)
    if Api.Draw.frame win <= 1 then Control.stop st.control;
    Run_view.external_queue_on_playlist st (M3u.parse !m3u) `QueueAndJump;
  );

  (* Start drawing *)
  Ui.start geo.ui;

  (* Remember current geometry for later *)
  let extension_shown_w = Geometry.extension_shown_w geo in
  let extension_shown_h = Geometry.extension_shown_h geo in
  let extension_side = geo.extension_side in
  let playlist_shown = geo.playlist_shown in
  let library_shown = geo.library_shown in
  let filesel_shown = geo.filesel_shown in
  let menu_shown = geo.menu_shown in
  let popup_shown = geo.popup_shown <> None in

  (* Update geometry *)
  let ww, wh = Api.Window.size win in
(*
  if extension_shown_w then geo.extension_width <- ww - Geometry.control_w geo;
  if extension_shown_h then geo.extension_height <- wh - Geometry.control_h geo;
  Geometry.update_geo geo;
*)

  (* Run panes *)
  Run_control.run st;
  if not (Api.Window.is_minimized win) then
  (
    if playlist_shown then Run_playlist.run st;
    if filesel_shown then Run_filesel.run st
    else if library_shown then Run_library.run st;
    if playlist_shown then Run_view.run_edit_panel st;
    Run_control.run_toggle_panel st;
    if menu_shown then Run_menu.run st;
    if popup_shown then Run_menu.run_popup st;
  );
  List.iter (fun f -> f ()) st.delayed;
  st.delayed <- [];

  (* Adjust font and grid size *)
  let text_delta =
    Bool.to_int (Layout.enlarge_text_key geo) -
    Bool.to_int (Layout.reduce_text_key geo)
  in
  Run_control.resize_text st text_delta;

  let grid_delta =
    Bool.to_int (Layout.enlarge_grid_key geo) -
    Bool.to_int (Layout.reduce_grid_key geo)
  in
  Run_control.resize_grid st grid_delta;

  let is_modal = Ui.is_modal geo.ui in
  if is_modal then Ui.nonmodal geo.ui;  (* temporarily enable keys *)
  let popup_delta =
    Bool.to_int (Layout.enlarge_popup_key geo) -
    Bool.to_int (Layout.reduce_popup_key geo)
  in
  if is_modal then Ui.modal geo.ui;  (* reenable keys *)
  geo.popup_size <- Geometry.(clamp min_popup_size max_popup_size
    (geo.popup_size + 100 * popup_delta));

  let scale_delta =
    Bool.to_int (Layout.enlarge_scale_key geo) -
    Bool.to_int (Layout.reduce_scale_key geo)
  in
  let scale_old = Api.Window.scale win in
  Ui.rescale geo.ui (scale_delta, scale_delta);

  if Layout.lib_cover_key geo then
    Library.activate_covers st.library (not st.library.covers_shown);

  (* Adjust window size after opening/closing panes *)
  let extension_shown_w' = Geometry.extension_shown_w geo in
  let extension_shown_h' = Geometry.extension_shown_h geo in

  let dw =
    if extension_shown_w' = extension_shown_w then 0 else
    (
      let s = if extension_shown_w' then +1 else -1 in
      let sw, _ = Api.Window.max_size win in
      let dw = min (sw - geo.control_width) (s * geo.extension_width) in
      let ox = if Geometry.extension_left geo then max_int else min_int in
  Printf.printf "[layout extend h %b] ww=%d cw=%d ew=%d sw=%d dw=%d maxew=%d\n%!"
  extension_shown_w' ww geo.control_width geo.extension_width sw dw (sw - geo.control_width);
      Ui.resize geo.ui (ox, -1) (dw, 0);
      geo.extension_width <- abs dw;
      dw
    )
  and dh =
    if extension_shown_h' = extension_shown_h then 0 else
    (
      let s = if extension_shown_h' then +1 else -1 in
      let _, sh = Api.Window.max_size win in
      let dh = min (sh - geo.control_height) (s * geo.extension_height) in
Printf.printf "[layout extend w %b] wh=%d ch=%d eh=%d sh=%d dh=%d maxeh=%d\n%!"
extension_shown_h' wh geo.control_height geo.extension_height sh dh (sh - geo.control_height);
      Ui.resize geo.ui (-1, -1) (0, dh);
      geo.extension_height <- abs dh;
      dh
    )
  in
  Geometry.clamp_geo' geo (ww + dw, wh + dh);

  if extension_shown_w' && geo.extension_side <> extension_side then
  (
    let ox, ox' =
      if Geometry.extension_left geo then min_int, max_int else max_int, min_int in
    Ui.resize geo.ui (ox, -1) (- geo.extension_width, 0);
    Ui.resize geo.ui (ox', -1) (+ geo.extension_width, 0);
  );

  (* Finish drawing *)
  let shift = Api.Key.is_modifier_down `Shift in
  let cmd = Api.Key.is_modifier_down `Command in
  let flex_ctl_w = shift || not extension_shown_w' in
  let flex_ctl_h = shift || not extension_shown_h' in
  let flex_ext_w = extension_shown_w' in
  let flex_ext_h = extension_shown_h' in
  let max_w = Geometry.win_max_w flex_ctl_w flex_ext_w geo in
  let min_w = min max_w (Geometry.win_min_w flex_ctl_w flex_ext_w geo) in
  let max_h = Geometry.win_max_h flex_ctl_h flex_ext_h geo in
  let min_h = min max_h (Geometry.win_min_h flex_ctl_h flex_ext_h geo) in
  let ratio = geo.control_ratio in  (* save *)
  Ui.finish geo.ui (Geometry.margin geo) (min_w, min_h) (max_w, max_h)
    (fun (dx, dy, dw, dh) ->
      (* Window was resized *)
      if !App.debug_layout then
        let w, h = Api.Window.size win in
        Printf.eprintf
          "[layout resize] win=%d%+d,%d%+d min=%d,%d max=%d,%d ctl=%d,%d\n%!"
          w dw h dh min_w min_h max_w max_h geo.control_width geo.control_height;

      let x', y' = Api.add (Api.Window.pos win) (dx, dy) in
      let w', h' = Api.add (Api.Window.size win) (dw, dh) in

if extension_shown_w' = extension_shown_w && extension_shown_h' = extension_shown_h then
(
Geometry.check_geo geo (ww, wh);
);

      if dw <> 0 && extension_shown_w' = extension_shown_w then
      (
        if flex_ctl_w then
(
          Geometry.change_control_width geo dw
;Printf.eprintf "  [change flex ctl w] dw=%+d cw'=%d\n%!" dw geo.control_width
)
        else
(
          Geometry.change_extension_width geo dw
;Printf.eprintf "  [change flex ext w] dw=%+d ew'=%d\n%!" dw geo.extension_width;
)
      );
      if dh <> 0 && extension_shown_h' = extension_shown_h then
      (
        if flex_ctl_h then
(
          Geometry.change_control_height geo dh
;Printf.eprintf "  [change flex ctl h] dh=%+d ch'=%d\n%!" dh geo.control_height
)
        else
(
          Geometry.change_extension_height geo dh
;Printf.eprintf "  [change flex ext h] dh=%+d eh'=%d\n%!" dh geo.extension_height
)
      );

      let w'', h'' =
        if not cmd then w', h' else
        match flex_ext_w, flex_ext_h with
        | true, true ->  (* both pl and lib open, shift + cmd *)
          let ratio' = float geo.control_width /. float geo.control_height in
          (match compare ratio' ratio with
          | -1 ->
Printf.eprintf "  [adapt ctl w 1] ratio=%.4f\n%!" ratio;
          Geometry.adapt_control_width geo; w', h'
          | +1 ->
Printf.eprintf "  [adapt ctl h 1] ratio=%.4f\n%!" ratio;
          Geometry.adapt_control_height geo; w', h'
          | _ -> w', h'
          )
        | false, true ->  (* only pl open, shift + cmd *)
Printf.eprintf "  [adapt ctl h 2] ratio=%.4f\n%!" ratio;
          Geometry.adapt_control_height geo; w', h'
        | true, false ->  (* only lib open, shift + cmd *)
Printf.eprintf "  [adapt ctl w 2] ratio=%.4f\n%!" ratio;
          Geometry.adapt_control_width geo; w', h'
        | false, false ->  (* neither open, cmd *)
          let rec adapt_w (w, h) =
            let w' = int_of_float (float h *. ratio) in
            let w'' = Geometry.clamp min_w max_w w' in
            if w'' = w' then w'', h else adapt_h (w'', h)
          and adapt_h (w, h) =
            let h' = int_of_float (float w /. ratio) in
            let h'' = Geometry.clamp min_h max_h h' in
            if h'' = h' then w, h'' else adapt_w (w, h'')
          in
          let adapt, s =
            match (dx, dw) <> (0, 0), (dy, dh) <> (0, 0) with
            | false, false -> Fun.id, ""  (* no change, shouldn't occur *)
            | true, false -> adapt_h, "h 2"  (* horizontal resize *)
            | false, true -> adapt_w, "w 2"  (* vertical resize *)
            | true, true ->  (* corner resize *)
              match compare (float w' /. float h') ratio with
              | -1 -> adapt_w, "w 1"
              | +1 -> adapt_h, "h 1"
              | _ -> Fun.id, ""
          in
          let w'', h'' = adapt (w', h') in
          if !App.debug_layout && s <> "" then
            Printf.eprintf
              "  [layout adapt win %s] ratio=%.4f %d,%d(%.4f)->%d,%d(%.4f)\n%!"
              s ratio
              w' h' (float w' /. float h')
              w'' h'' (float w'' /. float h'');
          if (w'', h'') <> (w', h') then
          (
            let dw', dh' = w'' - w', h'' - h' in
            let ax, ay, _, _ = geo.window in
            let origin a = if a >= 1.0 then max_int else min_int in
            Ui.resize geo.ui (origin ax, origin ay) (dw', dh');
            Geometry.change_control_width geo dw';
            Geometry.change_control_height geo dh';
          );
          w'', h''
      in

      if !App.debug_layout then
      (
        Printf.eprintf
          "  [layout set] win=%d,%d ctl=%d,%d ext=%d,%d bw=%d vw=%d\n%!"
          w'' h''
          geo.control_width geo.control_height
          geo.extension_width geo.extension_height
          geo.browser_width geo.left_width;
        Printf.eprintf
          "  [layout min] win=%d,%d ctl=%d,%d ext=%d,%d bw=%d vw=%d\n%!"
          min_w min_h
          Geometry.control_min_w Geometry.control_min_h
          (Geometry.extension_min_w geo) (Geometry.extension_min_h geo)
          (Geometry.browser_min_w geo) (Geometry.left_min_w geo);
      );

      Geometry.update_geo' geo (x', y', w'', h'');
      if cmd then geo.control_ratio <- ratio;  (* restore *)

      if !App.debug_layout then
      (
        Printf.eprintf
          "  [layout new] win=%d,%d ctl=%d,%d ext=%d,%d bw=%d vw=%d\n%!"
          w'' h''
          geo.control_width geo.control_height
          geo.extension_width geo.extension_height
          geo.browser_width geo.left_width;
(*
        Printf.eprintf "[layout min] win=%d,%d ctl=%d,%d ext=%d,%d bw=%d vw=%d\n%!"
          min_w min_h
          Geometry.control_min_w Geometry.control_min_h
          (Geometry.extension_min_w geo) (Geometry.extension_min_h geo)
          (Geometry.browser_min_w geo) (Geometry.left_min_w geo);
*)
      )
    )
    (fun scr ->
      (* Window moved to another screen *)
      Ui.pin geo.ui scr;
      let w, h = geo.extension_width, geo.extension_height in
      Geometry.clamp_geo geo;
      let dw =
        if Geometry.extension_shown_w geo then geo.extension_width - w else 0 in
      let dh =
        if Geometry.extension_shown_h geo then geo.extension_height - h else 0 in
      (* Substract mouse delta to get position relative to current geometry *)
      Ui.resize geo.ui Api.Mouse.(Api.sub (pos win) (delta win)) (dw, dh)
    );

  if Api.Window.is_hidden win then  (* after startup *)
    Api.Window.reveal win;

  let scale_new = Api.Window.scale win in
  let scaling' =
    fst geo.scaling + (fst scale_new - fst scale_old),
    snd geo.scaling + (snd scale_new - snd scale_old)
  in
  geo.scaling <- scaling';
  if scale_delta = 0 then
    (* Not set yet on first frame *)
    geo.window <- Geometry.abstract_geo geo
  else
  (
    (* Reset geometry when scaling was changed *)
(*
    let scale_x, scale_y = Api.Window.scale win in
    let rx = float scale_x /. (float scale_x -. float scale_delta) in
    let ry = float scale_y /. (float scale_y -. float scale_delta) in
    let dims = [ax; ay; aw; ah] in
    let scaled_dims = [rx *. ax; ry *. ay; rx *. aw; ry *. ah] in
    if List.fold_left max 0.0 dims >= 0.97 (* = 1.0 +- eps *)
    || List.fold_left max 0.0 scaled_dims > 1.0 then
*)
      Ui.reset geo.ui (Geometry.apply_geo geo geo.window);
  );

  (* Save state regularly every 3 seconds *)
  State.save_after st 3.0


(* Startup *)

let startup () =
  Storage.clear_temp ();
  let win = Api.Window.init 0 0 0 0 App.name in
  let audio = Api.Audio.init win in
  Api.Window.hide win;  (* hide during initialisation *)
  let ui = Ui.make win in
  let st0 = State.make ui audio in
  let success = State.load st0 in
  let st = if success then st0 else State.make ui audio in
  if !App.debug_layout then State.dump st [];
  at_exit (fun () ->
    Api.Audio.pause st.control.audio;
    State.save st;
    Storage.delete queue_file;
    Storage.clear_temp ();
  );
  st

let args = Arg.align
[
  "-help", Arg.Unit ignore, "";
  "--debug-perf", Arg.Set App.debug_perf, "\tLog execution times";
  "--debug-layout", Arg.Set App.debug_layout, "\tPrint window layout";
]

let _main =
  try
    Printexc.record_backtrace true;
    let paths = ref [] in
    Arg.parse args (fun path -> paths := path :: !paths) "";
    let m3u = if !paths = [] then "" else M3u.make (List.rev !paths) in
    (* Configure GC very aggressive to avoid giga bytes of memory usage *)
    Gc.(set {(get ()) with space_overhead = 20});
    (* Trigger GC compaction if worthwhile *)
    Domain.spawn (fun () ->
      Unix.sleepf 3600.0;  (* roughly once a minute, assuming 60 fps *)
      let gc = Gc.quick_stat () in
      if gc.free_words > gc.live_words then Gc.compact ()
    ) |> ignore;
    if Storage.exists queue_file then
    (
      (* TODO: this could race, should lock the file *)
      Storage.save_string_append queue_file (fun () -> m3u);
      let t1 = Storage.time queue_file in
      Unix.sleepf 1.0;
      let t2 = Storage.time queue_file in
      (* If file has not been modified after 1s, assume it's a zombie. *)
      if t1 = t2 then run (startup ());
    )
    else
    (
      Storage.save_string queue_file (fun () -> m3u);
      Storage.clear_temp ();  (* possible left-overs *)
      run (startup ());
    )
  with exn ->
    Storage.log_exn "internal" exn "";
    Stdlib.exit 2
