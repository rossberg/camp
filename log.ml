(* Log state *)

type entry = Ui.color * Ui.cell array

type 'cache t =
{
  table : (entry, 'cache) Table.t;
  columns : Ui.column array;
  mutable heading : Ui.heading option;
  mutable info : string;
  mutable cancel : bool;
  mutable completed : bool;
  mutable on_completion : 'cache t -> unit;
}


(* Constructor *)

let make heading columns on_completion =
  {
    table = Table.make 0;
    columns;
    heading;
    info = "";
    cancel = false;
    completed = false;
    on_completion;
  }


(* Manipulation *)

let length log = Table.length log.table

let insert log i entries =
  Mutex.protect log.table.mutex (fun () -> Table.insert log.table i entries)

let append log entries = insert log (length log) entries


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]
let check_opt opt f = Option.value (Option.map f opt) ~default: []

let ok log =
  check_opt log.heading (fun (headers, _) ->
    check "consistent headers" (Array.length headers = Array.length log.columns)
  ) @
  check "consistent table"
    (Array.for_all (fun (_, cells) ->
      Array.length cells = Array.length log.columns
    ) log.table.entries) @
  []
