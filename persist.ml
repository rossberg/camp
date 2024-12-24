(* Persisted State *)

open State


(* Helpers *)

let value = Fun.id
let pair x y = x, y

let clamp l h x = max l (min h x)
let clamp_pair lx ly hx hy (x, y) = clamp lx hx x, clamp ly hy y


(* Playlist *)

let playlist_file = "playlist.m3u"

(* TODO: unify with M3u module *)

let save_playlist tracks =
  File.save playlist_file (fun file ->
    let output fmt = Printf.fprintf file fmt in
    output "%s" "#EXTM3U\n";
    Array.iter (fun track ->
      output "#EXTINF:%.0f,%s\n" track.time track.name;
      output "%s\n" track.path;
    ) tracks
  )

let load_playlist () =
  let tracks = ref [] in
  File.load playlist_file (fun file ->
    let input fmt = File.fscanf file fmt in
    input " # EXTM3U " ();
    while true do
      let time, name = input " # EXTINF : %d , %[\x20-\xff]" pair in
      let path = String.trim (input " %[\x20-\xff]" value) in
      tracks := make_track_predet path name (float time) :: !tracks
    done
  );
  Array.of_list (List.rev !tracks)


(* State *)

let min_w = 360  (* TODO: avoid duplication *)
let min_h = 160

let state_file = "state.conf"

let save_state st =
  File.save state_file (fun file ->
    let output fmt  = Printf.fprintf file fmt in
    output "[%s]\n" App.name;
    let x, y = Api.Window.pos st.win in
    output "win_pos = %d, %d\n" x y;
    let w, h = Api.Window.size st.win in
    output "win_size = %d, %d\n" w h;
    output "volume = %.2f\n" st.volume;
    output "play_pos = %d\n" st.playpos;
    output "play = %s\n" (match st.current with Some s -> s.path | None -> "");
    let length = Api.Audio.length st.audio st.sound in
    let played = Api.Audio.played st.audio st.sound in
    output "seek_pos = %.4f\n" (if length > 0.0 then played /. length else 0.0);
    output "play_scroll = %d\n" st.playscroll;
    output "play_open = %d\n" (Bool.to_int st.playopen);
    output "play_height = %d\n" st.playheight;
    output "repeat = %d\n"
      (match st.repeat with `None -> 0 | `One -> 1 | `All -> 2);
    let a, b =
      match st.loop with `None -> -1.0, -1.0 | `A t1 -> t1, -1.0 | `AB tt -> tt
    in
    output "loop = %.4f, %.4f\n" a b;
    output "shuffle = %d\n" (Bool.to_int st.shuffle);
  );
  save_playlist st.playlist

let load_state st =
  File.load state_file (fun file ->
    let input fmt = File.fscanf file fmt in
    if input " [%s@]" value <> App.name then failwith "load_state";
    let sw, sh = Api.Window.screen_size st.win in
    let x, y = clamp_pair 0 0 (sw - 20) (sh - 20)
      (input " win_pos = %d , %d " pair) in
    Api.Window.set_pos st.win x y;
    let w, h = clamp_pair min_w min_h min_w sh
      (input " win_size = %d , %d " pair) in
    Api.Window.set_size st.win w h;
    Api.Draw.start st.win `Black;
    Api.Draw.finish st.win;

    st.volume <- clamp 0.0 1.0 (input " volume = %f " value);

    st.playlist <- load_playlist ();

    let len = Array.length st.playlist - 1 in
    st.playpos <- clamp 0 (len - 1) (input " play_pos = %d " value);
    let current = make_track (String.trim (input " play = %[\x20-\xff]" value)) in
    State.switch_track st current false;
    State.seek_track st (clamp 0.0 1.0 (input " seek_pos = %f " value));
    st.playscroll <- clamp 0 (len - 1) (input " play_scroll = %d " value);
    st.playopen <- (0 <> input " play_open = %d " value);
    (* TODO: 73 = playlist_min; use constant *)
    st.playheight <- max 83 (input " play_height = %d " value);
    st.repeat <-
      (match input " repeat = %d " value with 1 -> `One | 2 -> `All | _ -> `None);
    st.loop <-
      (match input " loop = %f, %f " pair with
      | t1, _t2 when t1 < 0.0 -> `None
      | t1, t2 when t2 < 0.0 -> `A t1
      | t1, t2 -> `AB (t1, max t1 t2)
      );
    st.shuffle <- (0 <> input " shuffle = %d " value);
  );
  State.update_summary st;
  ok st
