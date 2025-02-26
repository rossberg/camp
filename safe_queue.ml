type 'a t = {queue : 'a Queue.t; mutex : Mutex.t; nonempty : Condition.t}

let create () =
  { queue = Queue.create();
    mutex = Mutex.create();
    nonempty = Condition.create()
  }

let add x q =
  Mutex.protect q.mutex (fun () ->
    let was_empty = Queue.is_empty q.queue in
    Queue.add x q.queue;
    if was_empty then Condition.broadcast q.nonempty
  )

let take q =
  Mutex.protect q.mutex (fun () ->
    while Queue.is_empty q.queue do Condition.wait q.nonempty q.mutex done;
    Queue.take q.queue
  )

let clear q =
  Mutex.protect q.mutex (fun () ->
    Queue.clear q.queue
  )
