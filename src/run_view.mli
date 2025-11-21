(* Generic Handling of Editable Track View UI *)

open Audio_file

type path = File.path
type table = (Data.track, Ui.cached) Table.t


(* Runner *)

val run_edit_panel : State.t -> unit


(* Spinner *)

val spin : State.t -> string
val spin_changed : State.t -> bool


(* Views *)

type view

val artists_view : State.t -> view
val albums_view : State.t -> view
val tracks_view : State.t -> view
val playlist_view : State.t -> view


(* Initiate Edit Menu *)

val list_menu : State.t -> view -> string list -> unit
val edit_menu : State.t -> view -> string list -> int option -> unit


(* Drag & Drop *)

val drag_on_playlist : State.t -> unit
val drag_on_tracks : State.t -> unit

val drop_on_playlist : State.t -> Data.track array -> unit
val drop_on_tracks : State.t -> Data.track array -> unit

val external_drop_on_playlist : State.t -> unit
val external_drop_on_tracks : State.t -> unit

val set_drop_cursor : State.t -> unit

val queue_on_playlist : State.t -> Data.track array -> bool (* replace *) -> unit
val external_queue_on_playlist : State.t -> path list -> bool (* replace *) -> unit


(* Playlist Modification *)

val local_dir : State.t -> Library.dir -> unit
val relative_dir : State.t -> Library.dir -> unit
val resolve_dir : State.t -> Library.dir -> unit
val repair_dir : State.t -> Library.dir -> unit
