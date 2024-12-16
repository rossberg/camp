type 'a t = {queue : 'a Queue.t; mutex : Mutex.t; nonempty : Condition.t}

let create () =
  { queue = Queue.create();
    mutex = Mutex.create();
    nonempty = Condition.create()
  }

let add x q =
  Mutex.lock q.mutex;
  let was_empty = Queue.is_empty q.queue in
  Queue.add x q.queue;
  if was_empty then Condition.broadcast q.nonempty;
  Mutex.unlock q.mutex

let take q =
  Mutex.lock q.mutex;
  while Queue.is_empty q.queue do Condition.wait q.nonempty q.mutex done;
  let x = Queue.take q.queue in
  Mutex.unlock q.mutex;
  x
