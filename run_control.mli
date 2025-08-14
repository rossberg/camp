(* Run Control UI *)

val quit : State.t -> unit
val resize_text : State.t -> int -> unit
val resize_grid : State.t -> int -> unit

val run : State.t -> unit
val run_toggle_panes : State.t -> unit
