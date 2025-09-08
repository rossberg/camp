(* Run Menu UI *)

(* Runner *)

val run : State.t -> unit
val run_popup : State.t -> unit

(* Initiate Menus *)

val command_menu : State.t -> (Ui.menu_entry * (unit -> unit)) array -> unit
val header_menu :
  State.t -> ([< Data.any_attr] as 'a) Library.view ->
  int -> 'a list -> 'a list -> unit
