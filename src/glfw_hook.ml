external glfw_install : unit -> bool = "glfw_install"

let install (f : int -> int -> unit) =
  let _f' () =
Printf.eprintf "[f']\n%!";
  Raylib.(f (get_screen_width ()) (get_screen_height ())) in
  Callback.register "glfw_on_size" f;
(*  Callback.register "glfw_on_refresh" f';*)
  glfw_install ()

external get_monitor_workarea : int -> int * int * int * int =
  "glfw_get_monitor_workarea"
