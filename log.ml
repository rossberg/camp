(* Log state *)

type entry = Ui.color * Ui.cell iarray

type 'cache t =
{
  table : (entry, 'cache) Table.t;
  mutable columns : Ui.column iarray;
  mutable heading : Ui.heading option;
  mutable info : string;
  mutable cancel : bool;
  mutable completed : bool;
  mutable on_completion : 'cache t -> unit;
  mutable on_menu : 'cache t -> int option * int option -> unit;
}


(* Constructor *)

let make heading columns on_completion on_menu =
  {
    table = Table.make 0;
    columns;
    heading;
    info = "";
    cancel = false;
    completed = false;
    on_completion;
    on_menu;
  }


(* Manipulation *)

let length log = Table.length log.table

let insert log i entries =
  Mutex.protect log.table.mutex (fun () -> Table.insert log.table i entries)

let append log entries = insert log (length log) entries

let text log i j =
  match Iarray.get (snd log.table.entries.(i)) j with
  | `Text s -> s
  | `Image _ -> raise (Invalid_argument "Log.text")

let complete log =
  let on_completion = log.on_completion in
  log.on_completion <- ignore;
  on_completion log


(* Validation *)

type error = string

let check msg b = if b then [] else [msg]
let check_opt opt f = Option.value (Option.map f opt) ~default: []

let ok log =
  check_opt log.heading (fun (headers, _) ->
    check "consistent headers"
      (Iarray.length headers = Iarray.length log.columns)
  ) @
  check "consistent table"
    (Array.for_all (fun (_, cells) ->
      Iarray.length cells = Iarray.length log.columns
    ) log.table.entries) @
  []
