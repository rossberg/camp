(* Program State *)

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

let dumped_before = ref None
let to_string_fwd = ref (fun _ -> assert false)

let check msg b = if b then [] else [msg]

let rec ok st =
  match
    Control.ok st.control @
    Playlist.ok st.playlist @
    Library.ok st.library @
    Config.ok st.config @
    check "playlist empty when no current track"
      (st.control.current <> None || st.playlist.table.entries = [||]) @
    check "at most one selection"
      (not (Playlist.has_selection st.playlist &&
        Library.has_selection st.library)) @
    []
  with
  | errors when errors <> [] ->
    dump st (List.map ((^) "Invariant violated: ") errors)
  | exception exn ->
    dump st ["Exception during validation: " ^ Printexc.to_string exn ^ "\n" ^
      Printexc.get_backtrace ()]
  | _ -> ()

and dump st errors =
  let msgs = errors @ ["State:\n" ^ !to_string_fwd st] in
  if !dumped_before <> Some msgs then
  (
    if !dumped_before = None then Storage.log_clear ();
    List.iter Storage.log msgs;
    dumped_before := Some msgs;
  )


(* Persistance *)

let state_file = "state.conf"
let config_file = "config.conf"

let state_header = App.name ^ " state"
let config_header = App.name ^ " config"


let ui_to_string st =
  let buf = Buffer.create 1024 in
  let output fmt  = Printf.bprintf buf fmt in
  let x, y = Api.Window.pos (Ui.window st.ui) in
  output "win_pos = %d, %d\n" x y;
  output "palette = %d\n" (Ui.get_palette st.ui);
  Buffer.contents buf

let save_ui st file =
  Out_channel.output_string file (ui_to_string st)


let to_string' st =
  Control.to_string st.control ^
  Playlist.to_string st.playlist ^
  Library.to_string st.library ^
  ui_to_string st

let to_string st =
  to_string' st ^ Config.to_string st.config

let _ = to_string_fwd := to_string

let save st =
  Playlist.save_playlist st.playlist;
  Storage.save state_file (fun file ->
    Printf.fprintf file "[%s]\n" state_header;
    Control.save st.control file;
    Playlist.save st.playlist file;
    Library.save st.library file;
    save_ui st file;
  )


let fscanf file =
  match In_channel.input_line file with
  | None -> raise End_of_file
  | Some s -> Scanf.sscanf s

let value = Fun.id
let num l h x = max l (min h x)
let num_pair lx ly hx hy x y = num lx hx x, num ly hy y

let load_ui st file =  (* assumes playlist and library already loaded *)
  let win = Ui.window st.ui in
  let input fmt = fscanf file fmt in
  let sx, sy = Api.Window.min_pos win in
  let sw, sh = Api.Window.max_size win in
  let x, y =
    input " win_pos = %d , %d " (num_pair sx sy (sx + sw - 20) (sy + sh - 20)) in
  Api.Window.set_pos win x y;
  let pal = input " palette = %d " (num 0 (Ui.num_palette st.ui - 1)) in
  Ui.set_palette st.ui pal;
  (* TODO: 83 = playlist_min, 160 = control_h; use constants *)
  st.playlist.height <- num 83 (sh - 160) st.playlist.height;
  (* TODO: 400 = library_min, 360 = control_w; use constants *)
  st.library.width <- num 400 (sw - 360) st.library.width;
  (* TODO: 40 = browser_min, 60 = browser_min + 2*margin; use constants *)
  st.library.browser_width <-
    num 40 (st.library.width - 60) st.library.browser_width

let load st =
  Storage.load config_file (fun file ->
    let input fmt = fscanf file fmt in
    if input " [%s@]" value <> config_header then failwith "load_config";
    Config.load st.config file;
  );

  Library.load_roots st.library;
  Playlist.load_playlist st.playlist;

  Storage.load state_file (fun file ->
    let input fmt = fscanf file fmt in
    if input " [%s@]" value <> state_header then failwith "load_state";

    Control.load st.control file;
    Playlist.load st.playlist file;
    Library.load st.library file;
    load_ui st file;
  );

  if st.control.current = None && Playlist.length st.playlist > 0 then
  (
    st.control.current <- Table.current_opt st.playlist.table;
    Control.switch st.control (Option.get st.control.current) false;
  );

  ok st
