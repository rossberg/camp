(* Generic Handling of Editable Track View UI *)

type table = (Data.track, Ui.cached) Table.t

(* Runner *)

val run_edit_panel : State.t -> unit

(* Views *)

type view

val playlist_view : State.t -> view
val library_view : State.t -> view

(* Initiate Edit Menu *)

val list_menu : State.t -> view -> unit
val edit_menu : State.t -> view -> int option -> unit

(* Commands *)

val rescan : State.t -> Data.track array -> unit
val tag : State.t -> Data.track array -> bool -> unit

val rescan_avail : State.t -> view -> bool
val tag_avail : State.t -> view -> bool

(* Drag & Drop *)

val drag_on_playlist : State.t -> unit
val drag_on_library : State.t -> unit

val drop_on_playlist : State.t -> Data.track array -> unit
val drop_on_library : State.t -> Data.track array -> unit

val library_mouse : State.t -> Layout.t -> table -> int option
