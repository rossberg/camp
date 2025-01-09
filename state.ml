(* Program State *)

open Audio_file

type t =
{
  ui : Ui.t;
  control : Control.t;
  playlist : Playlist.t;
  library : Library.t;
  config : Config.t;
}


(* Constructor *)

let make ui audio db =
  {
    ui;
    control = Control.make audio;
    playlist = Playlist.make ();
    library = Library.make db;
    config = Config.make ();
  }


(* Validation *)

let log_file = "error.log"

let dumped_before = ref None
let dump = ref (fun _ -> assert false)

let check msg b = if b then [] else [msg]

let ok st =
  match
    Control.ok st.control @
    Playlist.ok st.playlist @
    Library.ok st.library @
    Config.ok st.config @
    check "playlist empty when no current track"
      (st.control.current <> None || st.playlist.tracks = [||]);
  with
  | errors when errors <> [] && !dumped_before <> Some st ->
    let tm = Unix.(localtime (time ())) in
    let pre = Printf.sprintf
      "%04d-%02d-%02d %02d:%02d:%02d Invariant violated: "
      (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
      tm.tm_hour tm.tm_min tm.tm_sec
    in
    let s = String.concat "\n" (List.map ((^) pre) errors) ^ "\n" ^ !dump st in
    Out_channel.output_string stderr s;
    if !dumped_before = None then Storage.save log_file ignore;  (* clear log *)
    Storage.append log_file (fun file -> Out_channel.output_string file s);
    Out_channel.flush_all ();
    dumped_before := Some st;
  | _ -> ()


(* Persistance *)

let playlist_file = "playlist.m3u"
let state_file = "state.conf"
let config_file = "config.conf"

let state_header = App.name ^ " state"
let config_header = App.name ^ " config"


let string_of_playlist (tracks : Track.t array) =
  List.init (Array.length tracks) (fun i ->
    let time = int_of_float tracks.(i).time in
    let info =
      if time = 0 then None else Some M3u.{time; title = tracks.(i).name} in
    M3u.{path = tracks.(i).path; info}
  ) |> M3u.make_ext

let playlist_of_string s =
  Array.map (fun (item : M3u.item) ->
    match item.info with
    | None -> Track.make item.path
    | Some info -> Track.make_predet item.path info.title (float info.time)
  ) (Array.of_list (M3u.parse_ext s))

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


let to_string st =
  let buf = Buffer.create 1024 in
  let output fmt  = Printf.bprintf buf fmt in
  output "[%s]\n" state_header;
  let x, y = Api.Window.pos (Ui.window st.ui) in
  output "win_pos = %d, %d\n" x y;
  output "palette = %d\n" (Ui.get_palette st.ui);
  output "volume = %.2f\n" st.control.volume;
  output "mute = %d\n" (Bool.to_int st.control.mute);
  output "play = %s\n" (match st.control.current with Some s -> s.path | None -> "");
  let length = Api.Audio.length st.control.audio st.control.sound in
  let played = Api.Audio.played st.control.audio st.control.sound in
  output "seek = %.4f\n" (if length > 0.0 then played /. length else 0.0);
  output "timemode = %d\n" (Bool.to_int (st.control.timemode = `Remain));
  output "shuffle = %d\n" (Bool.to_int (st.playlist.shuffle <> None));
  output "repeat = %d\n"
    (match st.control.repeat with
    | `None -> 0
    | `One -> 1
    | `All -> 2
    );
  let a, b =
    match st.control.loop with
    | `None -> -1.0, -1.0
    | `A t1 -> t1, -1.0
    | `AB tt -> tt
  in
  output "loop = %.4f, %.4f\n" a b;
  output "play_pos = %d\n" (Option.value st.playlist.pos ~default: (-1));
  output "play_scroll = %d\n" st.playlist.scroll;
  output "play_open = %d\n" (Bool.to_int st.playlist.shown);
  output "play_height = %d\n" st.playlist.height;
  output "lib_open = %d\n" (Bool.to_int st.library.shown);
  output "lib_side = %d\n" (Bool.to_int (st.library.side = `Right));
  output "lib_width = %d\n" st.library.width;
  output "lib_browser_width = %d\n" st.library.browser_width;
  Buffer.contents buf

let opt f = function
  | None -> ""
  | Some i -> f i

let opt_int = opt string_of_int
let opt_int_pair = opt (fun (i, j) -> string_of_int i ^ ", " ^ string_of_int j)

let _ = dump := fun st ->
  let buf = Buffer.create 1024 in
  let pr fmt = Printf.bprintf buf fmt in
  pr "%s" (to_string st);
  pr "play_rows = %d\n" st.playlist.rows;
  pr "play_length = %d\n" (Array.length st.playlist.tracks);
  pr "play_selected = %d" (Playlist.IntSet.cardinal st.playlist.selected);
  if st.playlist.selected <> Playlist.IntSet.empty then
    pr " (%d-%d)"
      (Playlist.IntSet.min_elt st.playlist.selected)
      (Playlist.IntSet.max_elt st.playlist.selected);
  pr "\n";
  
  pr "play_sel_range = %s\n" (opt_int_pair st.playlist.sel_range);
  pr "play_total = %.2f, %d\n" (fst st.playlist.total) (snd st.playlist.total);
  pr "play_total_selected = %.2f, %d\n"
    (fst st.playlist.total_selected) (snd st.playlist.total_selected);
  pr "play_undo_length = %d\n" (List.length !(st.playlist.undos));
  pr "play_redo_length = %d\n" (List.length !(st.playlist.redos));
  pr "%!";
  Buffer.contents buf


let save st =
  Storage.save state_file (fun file ->
    Out_channel.output_string file (to_string st)
  );
  save_playlist st.playlist.tracks


let value = Fun.id
let bool x = x <> 0
let num l h x = max l (min h x)
let pair x y = x, y
let num_pair lx ly hx hy x y = num lx hx x, num ly hy y
let num_opt l h x = if x < 0 then None else Some (num l h x)

let fscanf file =
  match In_channel.input_line file with
  | None -> raise End_of_file
  | Some s -> Scanf.sscanf s

let load st =
  Storage.load state_file (fun file ->
    let win = Ui.window st.ui in
    let input fmt = fscanf file fmt in
    if input " [%s@]" value <> state_header then failwith "load_state";
    let sx, sy = Api.Window.min_pos win in
    let sw, sh = Api.Window.max_size win in
    let x, y = input " win_pos = %d , %d " (num_pair sx sy (sx + sw - 20) (sy + sh - 20)) in
    Api.Window.set_pos win x y;

    let playlist = load_playlist () in
    let len = Array.length playlist in

    Ui.set_palette st.ui
      (input " palette = %d " (num 0 (Ui.num_palette st.ui - 1)));
    st.control.volume <- input " volume = %f " (num 0.0 1.0);
    st.control.mute <- input " mute = %d " bool;
    let current = input " play = %[\x20-\xff]" String.trim in
    st.control.current <-
      if current = "" then None else Some (Track.make current);
    let seek = input " seek = %f " (num 0.0 1.0) in
    st.control.timemode <-
      if input " timemode = %d " bool then `Remain else `Elapse;
    let shuffle = input " shuffle = %d " bool in
    st.control.repeat <-
      (match input " repeat = %d " value with
      | 1 -> `One
      | 2 -> `All
      | _ -> `None
      );
    st.control.loop <-
      (match input " loop = %f, %f " pair with
      | t1, _t2 when t1 < 0.0 -> `None
      | t1, t2 when t2 < 0.0 -> `A t1
      | t1, t2 -> `AB (t1, max t1 t2)
      );

    st.playlist.tracks <- playlist;
    st.playlist.pos <- input " play_pos = %d " (num_opt 0 (len - 1));
    if st.control.current = None && len > 0 then
      st.control.current <- Playlist.current_opt st.playlist;
    if st.control.current <> None then
      Control.switch st.control (Option.get st.control.current) false;
    Control.seek st.control seek;

    if shuffle then Playlist.shuffle st.playlist st.playlist.pos;

    st.playlist.scroll <- input " play_scroll = %d " (num 0 (len - 1));
    st.playlist.shown <- input " play_open = %d " bool;
    (* TODO: 83 = playlist_min, 160 = control_h; use constants *)
    st.playlist.height <- input " play_height = %d " (num 83 (sh - 160));
    st.library.shown <- input " lib_open = %d " bool;
    st.library.side <- if input " lib_side = %d " bool then `Right else `Left;
    (* TODO: 400 = library_min, 360 = control_w; use constants *)
    st.library.width <- input " lib_width = %d " (num 400 (sw - 360));
    (* TODO: 40 = browser_min, 60 = browser_min + 2*margin; use constants *)
    st.library.browser_width <- input " lib_width = %d " (num 40 (st.library.width - 60));
  );
  Playlist.update_total st.playlist;
  Storage.load config_file (fun file ->
    let input fmt = fscanf file fmt in
    if input " [%s@]" value <> config_header then failwith "load_config";
    st.config.delay_track_update <- input " delay_track_update = %f "
      (num 1.0 Float.infinity);
    st.config.exec_tag <- input " exec_tag = %[\x20-\xff]" String.trim;
    st.config.exec_tag_max_len <- input " exec_tag_max_len = %d "
      (num 0 max_int);
  );
  ok st
