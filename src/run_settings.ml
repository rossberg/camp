(* Playlist UI *)

type state = State.t


(* Helper *)

let clamp min max v =
  if v < min then min else
  if v > max then max else
  v


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
Printf.printf "scroll=%d pos=%.2f/ext=%.2f page_h=%d/set_h=%d\n%!" set.vscroll pos ext page_h set_h;
  let pos' = Layout.settings_scrollbar geo pos ext -. coeff *. wdy in
  set.vscroll <- clamp 0 (max 0 (set_h - page_h))
    (int_of_float (Float.round (pos' *. float set_h)));
Printf.printf "pos'=%.2f scroll'=%d\n%!" pos' set.vscroll;

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
