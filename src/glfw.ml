external get_cursor_pos : unit -> int * int =
  "glfw_get_cursor_pos"
external get_window_pos : unit -> int * int =
  "glfw_get_window_pos"
external get_window_size : unit -> int * int =
  "glfw_get_window_size"
external set_window_rect : int -> int -> int -> int -> unit =
  "glfw_set_window_rect"
external get_monitor_workarea : int -> int * int * int * int =
  "glfw_get_monitor_workarea"
external drag_window : unit -> unit =
  "glfw_drag_window"
external set_window_refresh_callback : (unit -> unit) -> unit =
  "glfw_set_window_refresh_callback"
external set_window_pos_callback : (int -> int -> unit) -> unit =
  "glfw_set_window_pos_callback"
external set_window_size_callback : (int -> int -> unit) -> unit =
  "glfw_set_window_size_callback"
