(* Run Control UI *)

(* Runner *)

val run : State.t -> unit
val run_toggle_panel : State.t -> unit

(* Commands *)

val quit : State.t -> unit

val resize_text : State.t -> int -> unit
val resize_grid : State.t -> int -> unit
