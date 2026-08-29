(* Run Popup UI *)

(* Runners *)

val run : State.t -> unit

(* Initiate Menus *)

val cover : State.t -> Popup.cover -> unit

val command_menu : State.t -> (Ui.menu_entry * (unit -> unit)) iarray -> unit
val header_menu :
  State.t ->
  ('c, 'd) Table.t ->
  ([< Data.any_attr_ex > `Custom] as 'a) Library.view ->
  ('x, 'b) Data.kind ->
  int -> 'a list -> 'a list ->
  (unit -> unit) option ->
  unit
