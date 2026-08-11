(* Playlist UI *)

open Audio_file


type state = State.t


(* Helper *)

let clamp min max v =
  if v < min then min else
  if v > max then max else
  v


(* Init *)

let init (st : state) =
  let set = st.settings in
  let geo = st.geometry in
  let ctl = st.control in
  let cfg = st.config in
  Edit.set set.text_size (string_of_int geo.text);
  Edit.set set.text_padding (string_of_int geo.pad_y);
  Edit.set set.text_gutter (string_of_int geo.gutter);
  Edit.set set.grid_tracks (string_of_int geo.track_grid);
  Edit.set set.grid_albums (string_of_int geo.album_grid);
  Edit.set set.popup_size (string_of_int geo.popup_size);
  Edit.set set.scroll_width (string_of_int geo.scrollbar);
  Edit.set set.spec_bands (string_of_int ctl.spec_bands);
  Edit.set set.exec_tag cfg.exec_tag;
  Edit.set set.exec_tag_flags cfg.exec_tag_flags


(* Runner *)

let fmt = Printf.sprintf

let run (st : state) focus_change =
  let geo = st.geometry in
  let set = st.settings in
  let ctl = st.control in
  let lib = st.library in
  let cfg = st.config in

  Layout.settings_pane geo;

  if Layout.done_but geo then
  (
    geo.settings_shown <- false;
    State.defocus_all st;
  );

  let focus_edit = State.focus_edit st in
  set.vscroll <-
    Layout.settings geo set.vscroll focus_change (
      [
        "DISPLAY", `Section [
          "COLOR", `Choice (List.init (Ui.num_palette geo.ui) (fun i ->
            String.uppercase_ascii (Ui.name_palette geo.ui i),
            Ui.get_palette geo.ui = i,
            fun _ -> Ui.set_palette geo.ui i
          ));
          "TEXT", `Number ("SIZE", set.text_size, geo.text, 6, 64,
            focus_edit, fun n -> geo.text <- n
          );
          "", `Number ("PADDING", set.text_padding, geo.pad_y, 0, 12,
            focus_edit, fun n -> geo.pad_y <- n
          );
          "", `Number ("GUTTER", set.text_gutter, geo.gutter, 1, 15,
            focus_edit, fun n -> geo.gutter <- n
          );
          "SCROLLBAR", `Number ("WIDTH", set.scroll_width, geo.scrollbar, 5, 25,
            focus_edit, fun n -> geo.scrollbar <- n
          );
        ];
        "COVERS", `Section [
          "TRACK VIEW", `Number ("SIZE", set.grid_tracks, geo.track_grid,
            30, 1000, focus_edit, fun n -> geo.track_grid <- n
          );
          "ALBUM VIEW", `Number ("SIZE", set.grid_albums, geo.album_grid,
            30, 1000, focus_edit, fun n -> geo.album_grid <- n
          );
          "POPUP", `Number ("MAX SIZE", set.popup_size, geo.popup_size,
            100, 1000, focus_edit, fun n -> geo.popup_size <- n
          );
          "", `Choice [
            "DISABLE IN LIBRARY", not lib.covers_shown,
              (fun _ -> Library.activate_covers lib (not lib.covers_shown))
          ];
        ];
        "VISUAL", `Section [
          "TIME", `Choice [
            "ELAPSED", ctl.timemode = `Elapse,
              (fun _ -> ctl.timemode <- `Elapse);
            "REMAINING", ctl.timemode = `Remain,
              (fun _ -> ctl.timemode <- `Remain);
          ];
          "ANIMATION", `Choice [
            "COVER", ctl.visual = `Cover,
              (fun _ -> Control.set_visual ctl `Cover);
            "TURNTABLE", ctl.visual = `Turntable,
              (fun _ -> Control.set_visual ctl `Turntable);
            "SPECTRUM", ctl.visual = `Spectrum,
              (fun _ -> Control.set_visual ctl `Spectrum);
            "WAVE", ctl.visual = `Wave,
              (fun _ -> Control.set_visual ctl `Wave);
            "OSCILLOSCOPE", ctl.visual = `Oscilloscope,
              (fun _ -> Control.set_visual ctl `Oscilloscope);
          ];
          "SPECTRUM", `Number ("BANDS", set.spec_bands, ctl.spec_bands,
            Control.min_spec_bands, Control.max_spec_bands,
            focus_edit, fun n -> ctl.spec_bands <- n
          );
          "FPS", `Choice [
            "SHOW", ctl.fps, fun _ -> ctl.fps <- not ctl.fps
          ]
        ];
        "PLAYLIST", `Section [
          "HEADERS", `Choice [
            "SHOW", geo.playlist_headers,
              (fun _ -> geo.playlist_headers <- not geo.playlist_headers);
          ];
        ];
        "LIBRARY", `Section [
          "EXPAND", `Choice [
            "LEFT", geo.extension_side = `Left,
              (fun _ -> geo.extension_side <- `Left);
            "RIGHT", geo.extension_side = `Right,
              (fun _ -> geo.extension_side <- `Right);
          ];
        ];
        "PROGRAMS", `Section [
          "TAGGING", `Text (set.exec_tag,
            (if File.exists set.exec_tag.text then
              Ui.text_color geo.ui
            else
              Ui.error_color geo.ui
            ),
            focus_edit, fun s -> cfg.exec_tag <- s
          );
          "", `Button ("BROWSE",
            fun () ->
              Run_filesel.filesel st `File `Read "" "" (fun path ->
                cfg.exec_tag <- path;
                Edit.set set.exec_tag path;
                if not (Geometry.settings_shown geo) then
                  st.geometry.settings_shown <- true;
                set.vscroll <- max_int;  (* assume it's at bottom *)
                State.focus_edit st set.exec_tag;
              )
          );
          "...FLAGS", `Text (set.exec_tag_flags, Ui.text_color geo.ui,
            focus_edit, fun s -> cfg.exec_tag_flags <- s
          );
        ];
        "DEVELOPER", `Section [
          "SHOW", `Flag (set.developer, fun b -> set.developer <- b);
        ]
      ] @
      if not set.developer then [] else
      let win = Ui.window geo.ui in
      let scr = Api.Window.screen win in
      let sn = (scr :> int) + 1 in
      let sm = Api.Screen.num () in
      let sx, sy = Api.Screen.pos scr in
      let sw, sh = Api.Screen.size scr in
      let dx, dy = Api.Screen.min_pos scr in
      let dw, dh = Api.Screen.max_size scr in
      let wx, wy = Api.Window.pos win in
      let ww, wh = Api.Window.size win in
      let ax, ay, aw, ah = Geometry.abstract_geo geo scr (wx, wy, ww, wh) in
      let cx, cy = Geometry.(control_x geo, control_y geo) in
      let cw, ch = Geometry.(control_w geo, control_h geo) in
      let cwmin, chmin = Geometry.(control_min_w, control_min_h) in
      let ex, ey = Geometry.(extension_x geo, extension_y geo) in
      let ew, eh = Geometry.(extension_w geo, extension_h geo) in
      let ewmin, ehmin = Geometry.(extension_min_w geo, extension_min_h geo) in
      let mx, my = Api.Mouse.pos win in
      let amx, amy = Api.Mouse.abs_pos win in
      [
        "GEOMETRY", `Section [
          "SCREEN", `Text (
            Edit.make_with 0
              (fmt " %d  %d  %d  %d  (%d/%d)" sx sy sw sh sn sm),
            Ui.text_color geo.ui, ignore, ignore
          );
          "DESKTOP", `Text (
            Edit.make_with 0
              (fmt " %d  %d  %d  %d" dx dy dw dh),
            Ui.text_color geo.ui, ignore, ignore
          );
          "PLACEMENT", `Text (
            Edit.make_with 0 (fmt " %.2f  %.2f  %.2f  %.2f" ax ay aw ah),
            Ui.text_color geo.ui, ignore, ignore
          );
          "WINDOW", `Text (
            Edit.make_with 0 (fmt " %d  %d  %d  %d" wx wy ww wh),
            Ui.text_color geo.ui, ignore, ignore
          );
          "CONTROL PANE", `Text (
            Edit.make_with 0
              (fmt " %d  %d  %d/%d  %d/%d" cx cy cw cwmin ch chmin),
            Ui.text_color geo.ui, ignore, ignore
          );
          "EXTENSION PANES", `Text (
            Edit.make_with 0
              (fmt " %d  %d  %d/%d  %d/%d" ex ey ew ewmin eh ehmin),
            Ui.text_color geo.ui, ignore, ignore
          );
          "MOUSE", `Text (
            Edit.make_with 0
              (fmt " %d  %d  ~  %d  %d" amx amy mx my),
            Ui.text_color geo.ui, ignore, ignore
          );
        ];
      ]
    )
