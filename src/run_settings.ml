(* Playlist UI *)

open Audio_file


type state = State.t


(* Helper *)

let clamp min max v =
  if v < min then min else
  if v > max then max else
  v


(* TODO:
- fix use of history for numbers; don't mess with cursor position
- close with button
- open tagger path with filesel button
- accelerate up/down buttons; hold down
*)


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
  Edit.set set.exec_tag cfg.exec_tag


(* Runner *)

let run (st : state) focus_change =
  let geo = st.geometry in
  let set = st.settings in
  let ctl = st.control in
  let lib = st.library in
  let cfg = st.config in

  Layout.settings_pane geo;

  set.vscroll <-
    Layout.settings geo set.vscroll focus_change
      [
        "DISPLAY", Section [
          "COLOR", Choice (List.init (Ui.num_palette geo.ui) (fun i ->
            String.uppercase_ascii (Ui.name_palette geo.ui i),
            Ui.get_palette geo.ui = i,
            fun _ -> Ui.set_palette geo.ui i
          ));
          "TEXT", Number ("SIZE", set.text_size, geo.text, 6, 64,
            fun n -> geo.text <- n
          );
          "", Number ("PADDING", set.text_padding, geo.pad_y, 0, 12,
            fun n -> geo.pad_y <- n
          );
          "", Number ("GUTTER", set.text_gutter, geo.gutter, 1, 15,
            fun n -> geo.gutter <- n
          );
          "SCROLLBAR", Number ("WIDTH", set.scroll_width, geo.scrollbar, 5, 25,
            fun n -> geo.scrollbar <- n
          );
        ];
        "COVERS", Section [
          "TRACK VIEW", Number ("SIZE", set.grid_tracks, geo.track_grid,
            30, 1000, fun n -> geo.track_grid <- n
          );
          "ALBUM VIEW", Number ("SIZE", set.grid_albums, geo.album_grid,
            30, 1000, fun n -> geo.album_grid <- n
          );
          "POPUP", Number ("MAX SIZE", set.popup_size, geo.popup_size,
            100, 1000, fun n -> geo.popup_size <- n
          );
          "", Choice [
            "DISABLE IN LIBRARY", not lib.covers_shown,
              (fun _ -> Library.activate_covers lib (not lib.covers_shown))
          ];
        ];
        "VISUAL", Section [
          "TIME", Choice [
            "ELAPSED", ctl.timemode = `Elapse,
              (fun _ -> ctl.timemode <- `Elapse);
            "REMAINING", ctl.timemode = `Remain,
              (fun _ -> ctl.timemode <- `Remain);
          ];
          "ANIMATION", Choice [
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
          "SPECTRUM", Number ("BANDS", set.spec_bands, ctl.spec_bands,
            Control.min_spec_bands, Control.max_spec_bands,
            fun n -> ctl.spec_bands <- n
          );
          "FPS", Choice [
            "SHOW", ctl.fps, fun _ -> ctl.fps <- not ctl.fps
          ]
        ];
        "PLAYLIST", Section [
          "HEADERS", Choice [
            "SHOW", geo.playlist_headers,
              (fun _ -> geo.playlist_headers <- not geo.playlist_headers);
          ];
        ];
        "LIBRARY", Section [
          "EXPAND", Choice [
            "LEFT", geo.extension_side = `Left,
              (fun _ -> geo.extension_side <- `Left);
            "RIGHT", geo.extension_side = `Right,
              (fun _ -> geo.extension_side <- `Right);
          ];
        ];
        "PROGRAMS", Section [
          "TAGGING", Text (set.exec_tag,
            (if File.exists set.exec_tag.text then
              Ui.text_color geo.ui
            else
              Ui.error_color geo.ui
            ),
            fun s -> if File.exists s then cfg.exec_tag <- s
          );
          "", Button ("BROWSE", fun () -> Printf.eprintf "BROWSE\n%!")
        ];
      ]
