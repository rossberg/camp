(* Persisted State *)

open State


(* Helpers *)

let id = Fun.id
let pair x y = x, y

let clamp l h x = max l (min h x)
let clamp_pair lx ly hx hy (x, y) = clamp lx hx x, clamp ly hy y


(* Playlist *)

let playlist_file = "playlist.m3u"

(* TODO: unify with M3u module *)

let save_playlist songs =
  File.save playlist_file (fun file ->
    let output fmt = Printf.fprintf file fmt in
    output "%s" "#EXTM3U\n";
    Array.iter (fun song ->
      output "#EXTINF:%.0f,%s\n" song.time song.name;
      output "%s\n" song.path;
    ) songs
  )

let load_playlist () =
  let songs = ref [] in
  File.load playlist_file (fun file ->
    let input fmt = File.fscanf file fmt in
    input " # EXTM3U " ();
    while true do
      let time, name = input " # EXTINF : %d , %[\x20-\xff]" pair in
      let path = String.trim (input " %[\x20-\xff]" id) in
      songs := make_song_predet path name (float time) :: !songs
    done
  );
  Array.of_list (List.rev !songs)


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
  );
  save_playlist st.playlist

let load_state st =
  File.load state_file (fun file ->
    let input fmt = File.fscanf file fmt in
    if input " [%s@]" id <> App.name then failwith "load_state";
    let sw, sh = Api.Window.screen_size st.win in
    let x, y = clamp_pair 0 0 (sw - 20) (sh - 20)
      (input " win_pos = %d , %d " pair) in
    Api.Window.set_pos st.win x y;
    let w, h = clamp_pair min_w min_h min_w sh
      (input " win_size = %d , %d " pair) in
    Api.Window.set_size st.win w h;
    st.volume <- clamp 0.0 1.0 (input " volume = %f " id);
    st.playpos <- max 0 (input " play_pos = %d " id);
    let current = make_song (String.trim (input " play = %[\x20-\xff]" id)) in
    State.switch_song st current false;
    State.seek_song st (clamp 0.0 1.0 (input " seek_pos = %f " id));
  );
  st.playlist <- load_playlist ();
  st.playpos <- min st.playpos (max 0 (Array.length st.playlist - 1));
  ok st
