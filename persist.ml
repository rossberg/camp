(* Persisted State *)

open Audio_file
open State


(* Helpers *)

let value = Fun.id
let pair x y = x, y

let clamp l h x = max l (min h x)
let clamp_pair lx ly hx hy (x, y) = clamp lx hx x, clamp ly hy y


(* Playlist *)

let string_of_playlist tracks =
  List.init (Array.length tracks) (fun i ->
    let time = int_of_float tracks.(i).time in
    let info =
      if time = 0 then None else Some M3u.{time; title = tracks.(i).name} in
    M3u.{path = tracks.(i).path; info}
  ) |> M3u.make_ext

let playlist_of_string s =
  Array.map (fun (item : M3u.item) ->
    match item.info with
    | None -> State.make_track item.path
    | Some info ->
      State.make_track_predet item.path info.title (float info.time)
  ) (Array.of_list (M3u.parse_ext s))

let playlist_file = "playlist.m3u"

let save_playlist tracks =
  Storage.save playlist_file (fun file ->
    Out_channel.output_string file (string_of_playlist tracks)
  )

let load_playlist () =
  let tracks = ref [||] in
  Storage.load playlist_file (fun file ->
    tracks := playlist_of_string (In_channel.input_all file)
  );
  !tracks


(* State *)

let min_w = 360  (* TODO: avoid duplication *)
let min_h = 160

let state_file = "state.conf"
let config_file = "config.conf"

let state_header = App.name ^ " state"
let config_header = App.name ^ " config"

let to_string st =
  let buf = Buffer.create 1024 in
  let output fmt  = Printf.bprintf buf fmt in
  output "[%s]\n" state_header;
  let x, y = Api.Window.pos (Ui.window st.ui) in
  output "win_pos = %d, %d\n" x y;
  let w, h = Api.Window.size (Ui.window st.ui) in
  output "win_size = %d, %d\n" w h;
  output "color_scheme = %d\n" (Ui.get_color_scheme st.ui);
  output "volume = %.2f\n" st.volume;
  output "mute = %d\n" (Bool.to_int st.mute);
  output "play = %s\n" (match st.current with Some s -> s.path | None -> "");
  let length = Api.Audio.length st.audio st.sound in
  let played = Api.Audio.played st.audio st.sound in
  output "seek = %.4f\n" (if length > 0.0 then played /. length else 0.0);
  output "timemode = %d\n" (Bool.to_int (st.timemode = `Remain));
  output "shuffle = %d\n" (Bool.to_int st.shuffled);
  output "repeat = %d\n"
    (match st.repeat with `None -> 0 | `One -> 1 | `All -> 2);
  let a, b =
    match st.loop with `None -> -1.0, -1.0 | `A t1 -> t1, -1.0 | `AB tt -> tt
  in
  output "loop = %.4f, %.4f\n" a b;
  output "play_pos = %d\n" st.playlist_pos;
  output "play_scroll = %d\n" st.playlist_scroll;
  output "play_open = %d\n" (Bool.to_int st.playlist_open);
  output "play_height = %d\n" st.playlist_height;
  output "exec_tag = %s\n" st.exec_tag;
  output "exec_tag_max_len = %d\n" st.exec_tag_max_len;
  Buffer.contents buf

let _ = State.to_string := to_string

let save_state st =
  Storage.save state_file (fun file ->
    Out_channel.output_string file (to_string st)
  );
  save_playlist st.playlist

let fscanf file =
  match In_channel.input_line file with
  | None -> raise End_of_file
  | Some s -> Scanf.sscanf s

let load_state st =
  Storage.load state_file (fun file ->
    let win = Ui.window st.ui in
    let input fmt = fscanf file fmt in
    if input " [%s@]" value <> state_header then failwith "load_state";
    let sw, sh = Api.Window.screen_size win in
    let x, y = clamp_pair 0 0 (sw - 20) (sh - 20)
      (input " win_pos = %d , %d " pair) in
    Api.Window.set_pos win x y;
    let w, h = clamp_pair min_w min_h min_w sh
      (input " win_size = %d , %d " pair) in
    Api.Window.set_size win w h;
    Api.Draw.start win `Black;
    Api.Draw.finish win;

    st.playlist <- load_playlist ();

    Ui.set_color_scheme st.ui (clamp 0 (Array.length Ui.color_schemes - 1)
      (input " color_scheme = %d " value));
    st.volume <- clamp 0.0 1.0 (input " volume = %f " value);
    st.mute <- (0 <> input " mute = %d " value);
    let current = String.trim (input " play = %[\x20-\xff]" value) in
    st.current <- if current = "" then None else Some (make_track current);
    let seek = clamp 0.0 1.0 (input " seek = %f " value) in
    st.timemode <-
      if input " timemode = %d " value = 0 then `Elapse else `Remain;
    st.shuffled <- (0 <> input " shuffle = %d " value);
    if st.shuffled then State.shuffle st (Some st.playlist_pos);
    st.repeat <-
      (match input " repeat = %d " value with 1 -> `One | 2 -> `All | _ -> `None);
    st.loop <-
      (match input " loop = %f, %f " pair with
      | t1, _t2 when t1 < 0.0 -> `None
      | t1, t2 when t2 < 0.0 -> `A t1
      | t1, t2 -> `AB (t1, max t1 t2)
      );

    let len = Array.length st.playlist - 1 in
    st.playlist_pos <- clamp 0 (len - 1) (input " play_pos = %d " value);
    if st.current = None && len > 0 then
      st.current <- Some st.playlist.(st.playlist_pos);
    if st.current <> None then
      State.switch_track st (Option.get st.current) false;
    State.seek_track st seek;

    st.playlist_scroll <- clamp 0 (len - 1) (input " play_scroll = %d " value);
    st.playlist_open <- (0 <> input " play_open = %d " value);
    (* TODO: 83 = playlist_min; use constant *)
    st.playlist_height <- max 83 (input " play_height = %d " value);
  );
  State.update_summary st;
  Storage.load config_file (fun file ->
    let input fmt = fscanf file fmt in
    if input " [%s@]" value <> config_header then failwith "load_config";
    st.exec_tag <- String.trim (input " exec_tag = %[\x20-\xff]" value);
    st.exec_tag_max_len <- max 0 (input " exec_tag_max_len = %d " value);
  );
  ok st
