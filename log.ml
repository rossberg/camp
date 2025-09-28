(* Log state *)

type 'cache t =
{
  table : (Ui.color * Ui.cell array, 'cache) Table.t;
  columns : Ui.column array;
  mutable heading : Ui.heading option;
  mutable info : string;
  mutable completed : bool;
  mutable on_completion : 'cache t -> [`Ok | `Cancel] -> unit;
}


(* Constructor *)

let make heading columns on_completion =
  {
    table = Table.make 0;
    columns;
    heading;
    info = "";
    completed = false;
    on_completion;
  }


let add log entries =
  Table.insert log.table (Table.length log.table) entries


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
