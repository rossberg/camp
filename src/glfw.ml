external get_cursor_pos : unit -> int * int = "glfw_get_cursor_pos"
external get_window_pos : unit -> int * int = "glfw_get_window_pos"
external get_window_size : unit -> int * int = "glfw_get_window_size"
external get_framebuffer_size : unit -> int * int = "glfw_get_framebuffer_size"

external get_monitor_workarea : int -> int * int * int * int =
  "glfw_get_monitor_workarea"

external set_window_rect : int -> int -> int -> int -> unit =
  "glfw_set_window_rect"
external set_window_size_limits : int -> int -> int -> int -> unit =
  "glfw_set_window_size_limits"
external set_window_aspect_ratio : int -> int -> unit =
  "glfw_set_window_aspect_ratio"

let left = 1
let top = 2
let right = 4
let bottom = 8
let topleft = top lor left
let topright = top lor right
let bottomleft = bottom lor left
let bottomright = bottom lor right

external drag_window : unit -> bool = "glfw_drag_window"
external resize_window : int -> bool = "glfw_resize_window"

external set_window_refresh_callback : (unit -> unit) -> unit =
  "glfw_set_window_refresh_callback"
external set_window_pos_callback : (int -> int -> unit) -> unit =
  "glfw_set_window_pos_callback"
external set_window_size_callback : (int -> int -> unit) -> unit =
  "glfw_set_window_size_callback"
external set_framebuffer_size_callback : (int -> int -> unit) -> unit =
  "glfw_set_framebuffer_size_callback"
