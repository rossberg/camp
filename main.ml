let pic = In_channel.(with_open_bin "pic.jpg" input_all)

let callback _ _ = ()

let rec run callback =  (* pass callback to keep it alive *)
  for _ = 0 to 3 do
    let image = Raylib.load_image_from_memory ".jpg" pic (String.length pic) in
    let texture = Raylib.load_texture_from_image image in
    Gc.finalise Raylib.unload_texture texture;  (* [1] *)
    Raylib.unload_image image                   (* [2] *)
  done;
  Unix.sleepf 0.02;
  run callback

let rec pressure () =
  Unix.sleepf 0.1;
  Printf.printf "[allocating memory]\n%!";
  ignore (Buffer.create 0x1_0000_0000);
  pressure ()

let _main =
  Gc.(set {(get ()) with space_overhead = 20});  (* GC more often *)
  Raylib.(set_trace_log_level TraceLogLevel.All);
  Raylib.init_audio_device ();                             (* [3] *)
  Raylib.init_window 100 100 "Crash test";
  Raylib_callbacks.attach_audio_mixed_processor callback;  (* [4] *)
  ignore (Domain.spawn pressure);
  run callback
