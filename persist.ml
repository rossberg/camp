(* Persisted State *)

open State


(* File Management *)

module Dirs = Directories.Base_dirs ()

let dir =
  match Dirs.data_local_dir with
  | Some path -> Filename.concat path State.app
  | None -> "."


let save file f =
  try
    if not (Sys.file_exists dir) then Sys.mkdir dir 0o770;
    Out_channel.with_open_bin (Filename.concat dir file) f
  with Sys_error _ -> ()

let load file f =
  try
    In_channel.with_open_bin (Filename.concat dir file) f
  with Sys_error _ | End_of_file | Scanf.Scan_failure _ | Failure _ -> ()

let fscanf file =
  match In_channel.input_line file with
  | None -> raise End_of_file
  | Some s -> Scanf.sscanf s


(* Helpers *)

let id = Fun.id
let pair x y = x, y

let clamp l h x = max l (min h x)
let clamp_pair lx ly hx hy (x, y) = clamp lx hx x, clamp ly hy y


(* Playlist *)

let playlist_file = "playlist.m3u"

let save_playlist songs =
  save playlist_file (fun file ->
    let output fmt = Printf.fprintf file fmt in
    output "%s" "#EXTM3U\n";
    Array.iter (fun (song : Song.t) ->
      output "#EXTINF:%.0f,%s\n" song.time song.name;
      output "%s\n" song.path;
    ) songs
  )

let load_playlist () =
  let songs = ref [] in
  load playlist_file (fun file ->
    let input fmt = fscanf file fmt in
    input " # EXTM3U " ();
    while true do
      let time, name = input " # EXTINF : %d , %[\x20-\xff]" pair in
      let path = String.trim (input " %[\x20-\xff]" id) in
      songs := Song.{path; name; time = float time} :: !songs
    done
  );
  Array.of_list (List.rev !songs)


(* State *)

let min_w = 360  (* TODO: avoid duplication *)
let min_h = 160

let state_file = "state.conf"

let save_state st =
  save state_file (fun file ->
    let output fmt  = Printf.fprintf file fmt in
    output "[%s]\n" State.app;
    let x, y = Api.Window.pos st.win in
    output "win_pos = %d, %d\n" x y;
    let w, h = Api.Window.size st.win in
    output "win_size = %d, %d\n" w h;
    output "volume = %.2f\n" st.volume;
    output "play_pos = %d\n" st.playpos;
    output "play = %s\n" (match st.playing with Some s -> s.path | None -> "");
    let length = Api.Audio.length st.audio st.sound in
    let played = Api.Audio.played st.audio st.sound in
    output "seek_pos = %.4f\n" (if length > 0.0 then played /. length else 0.0);
  );
  save_playlist st.playlist

let load_state st =
  load state_file (fun file ->
    let input fmt = fscanf file fmt in
    if input " [%s@]" id <> State.app then failwith "load_state";
    let sw, sh = Api.Window.screen_size st.win in
    let x, y = clamp_pair 0 0 (sw - 20) (sh - 20)
      (input " win_pos = %d , %d " pair) in
    Api.Window.set_pos st.win x y;
    let w, h = clamp_pair min_w min_h min_w sh
      (input " win_size = %d , %d " pair) in
    Api.Window.set_size st.win w h;
    st.volume <- clamp 0.0 1.0 (input " volume = %f " id);
    st.playpos <- max 0 (input " play_pos = %d " id);
    let playing = Song.make (String.trim (input " play = %[\x20-\xff]" id)) in
    State.switch_song st playing false;
    State.seek_song st (clamp 0.0 1.0 (input " seek_pos = %f " id));
  );
  st.playlist <- load_playlist ();
  st.playpos <- min st.playpos (max 0 (Array.length st.playlist - 1))
