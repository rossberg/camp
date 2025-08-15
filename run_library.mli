(* Library UI *)

type state = State.t

(* Runner *)

val run : State.t -> unit

(* Drag & Drop *)

val drag_on_browser : State.t -> unit
val drop_on_browser : State.t -> Data.track array -> unit
