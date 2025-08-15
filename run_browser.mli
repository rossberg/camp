(* Library Browser UI *)

type state = State.t

(* Drag & Drop *)

val drag_on_browser : State.t -> unit
val drop_on_browser : State.t -> Data.track array -> unit
