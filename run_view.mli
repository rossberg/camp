(* Generic Handling of Editable Track View UI *)

type table = (Data.track, Ui.cached) Table.t

(* Runner *)

val run_edit_panel : State.t -> unit

(* Views *)

type view

val artists_view : State.t -> view
val albums_view : State.t -> view
val tracks_view : State.t -> view
val playlist_view : State.t -> view

(* Initiate Edit Menu *)

val list_menu : State.t -> view -> unit
val edit_menu : State.t -> view -> int option -> unit

(* Drag & Drop *)

val drag_on_playlist : State.t -> unit
val drag_on_tracks : State.t -> unit

val drop_on_playlist : State.t -> Data.track array -> unit
val drop_on_tracks : State.t -> Data.track array -> unit

val external_drop_on_playlist : State.t -> unit
val external_drop_on_tracks : State.t -> unit

val set_drop_cursor : State.t -> unit
