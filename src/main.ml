(* Main Program *)

open Audio_file

let args = Arg.align
[
  "-help", Arg.Unit ignore, "";
  "--debug-perf", Arg.Set App.debug_perf, "\tLog execution times";
  "--debug-strict", Arg.Set App.debug_strict, "\tAbort on invariant violation";
  "--debug-layout", Arg.Set App.debug_layout, "\tPrint window layout";
]

let _main =
  try
    Printexc.record_backtrace true;
    let paths = ref [] in
    Arg.parse args (fun path -> paths := path :: !paths) "";
    (* Configure GC very aggressive to avoid giga bytes of memory usage *)
    Gc.(set {(get ()) with space_overhead = 20});
    (* Trigger GC compaction if worthwhile *)
    Domain.spawn (fun () ->
      Unix.sleepf 3600.0;  (* roughly once a minute, assuming 60 fps *)
      let gc = Gc.quick_stat () in
      if gc.free_words > gc.live_words then Gc.compact ()
    ) |> ignore;
    Run.start (List.rev !paths);
  with exn ->
    Storage.log_exn "internal" exn "";
    Stdlib.exit 2
