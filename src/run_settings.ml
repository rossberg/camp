(* Playlist UI *)

type state = State.t


(* Helper *)

let clamp min max v =
  if v < min then min else
  if v > max then max else
  v


(*
Display:
  Time: Played | Remaining
  Visualisation: Cover | Turntable | Spectrum | Wave | Oscilloscope | None
  Spectrum Bands: N

Color Palette: Blue | Opal | Fluor | Mint | Green | Amber | White

Text:
  Label Size: N
  Button Label Size: N
  List Size: N
  List Padding: N
  List Gutter: N
  SDF Rendering: []

Covers:
  Grid Size: N
  Grid Padding: N
  Disable Loading: []

Layout:
  Scaling: 1x | 2x | 3x | 4x
  Control Size: N
  Margin: N
  Scrollbar Width: N
  Reflection Radius: N

Playlist:
  Show Column Headers: []

Library:
  Side: Left | Right

External Programs:
  Tagging Program: _
*)

(*
type setting = {name : string; item : setting_item}
and setting_item =
  | Section of setting list
  | Flag of bool * (bool -> unit)
  | Choice of string list * int * (int -> unit)
  | Number of int * (int -> unit) 
  | Text of Edit.t
*)


(* Runner *)

let run (st : state) =
  let geo = st.geometry in
  let set = st.settings in

  Layout.settings_pane geo;

  (* Scrollbar *)

  let _, _, _, page_h = Ui.dim geo.ui (Layout.settings_scrollbar_area geo) in
  let set_h = Layout.settings_h geo in
  let coeff = float (max 1 page_h) /. float (max 1 set_h) /. 4.0 in
  let _, wdy = Layout.settings_wheel geo in
  let ext = if set_h = 0 then 1.0 else min 1.0 (float page_h /. float set_h) in
  let pos = if set_h = 0 then 0.0 else float set.vscroll /. float set_h in
(*Printf.printf "scroll=%d pos=%.2f/ext=%.2f page_h=%d/set_h=%d\n%!" set.vscroll pos ext page_h set_h;*)
  let pos' = Layout.settings_scrollbar geo pos ext -. coeff *. wdy in
  set.vscroll <- clamp 0 (max 0 (set_h - page_h))
    (int_of_float (Float.round (pos' *. float set_h)));
(*Printf.printf "pos'=%.2f scroll'=%d\n%!" pos' set.vscroll;*)

  (* Display *)

  Layout.sec_display_label geo set.vscroll;
  Layout.sec_time_label geo set.vscroll;

  Layout.time_elapse_indicator geo set.vscroll (st.control.timemode = `Elapse);
  Layout.time_elapse_label geo set.vscroll;
  if Layout.time_elapse_button geo set.vscroll then
    st.control.timemode <- `Elapse;

  Layout.time_remain_indicator geo set.vscroll (st.control.timemode = `Remain);
  Layout.time_remain_label geo set.vscroll;
  if Layout.time_remain_button geo set.vscroll then
    st.control.timemode <- `Remain;

  (* Color *)

  Layout.sec_color_label geo set.vscroll;

  (* Text *)

  Layout.sec_text_label geo set.vscroll;
