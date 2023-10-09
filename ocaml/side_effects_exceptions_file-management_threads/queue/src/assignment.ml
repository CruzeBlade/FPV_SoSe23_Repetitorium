open Thread
open Event

module type Queue = sig
  exception NoElementLeft

  type 'a queue

  val create_queue : unit -> 'a queue
  val enqueue : 'a queue -> 'a -> unit
  val dequeue_opt : 'a queue -> 'a option

  (* Throws a 'NoElementLeft' exception if no element is left *)
  val dequeue : 'a queue -> 'a

  (* deletes every element*)
  val empty : 'a queue -> unit

  (* reverses the order of the elements *)
  val reverse : 'a queue -> unit

  (* opposite of create_queue *)
  val discard_queue : 'a queue -> unit
end

module MyQueue : Queue = struct
  exception NoElementLeft

  type 'a req =
    | Enqueue of 'a
    | Dequeue of ('a, exn) result channel
    | Empty
    | Reverse
    | Discard

  type 'a queue = 'a req channel

  let create_queue () =
    let req_channel = new_channel () in
    let rec serve state =
      match sync (receive req_channel) with
      | Enqueue n -> serve (state @ [ n ])
      | Dequeue res -> (
          try
            if List.length state = 0 then (
              sync (send res (Error NoElementLeft));
              serve state)
            else (
              sync (send res (Ok (List.hd state)));
              serve (List.tl state))
          with e ->
            sync (send res (Error e));
            serve state)
      | Empty -> serve []
      | Reverse -> serve (List.rev state)
      | Discard -> ()
    in
    let _ = create serve [] in
    req_channel

  let enqueue q n = sync (send q (Enqueue n))

  let dequeue_opt q =
    let res = new_channel () in
    sync (send q (Dequeue res));
    match sync (receive res) with
    | Ok v -> Some v
    | Error NoElementLeft -> None
    | Error e -> raise e

  (* Throws a 'NoElementLeft' exception if no element is left *)
  let dequeue q =
    let res = new_channel () in
    sync (send q (Dequeue res));
    match sync (receive res) with Ok v -> v | Error e -> raise e

  (* deletes every element*)
  let empty q = sync (send q Empty)

  (* reverses the order of the elements *)
  let reverse q = sync (send q Reverse)

  (* opposite of create_queue *)
  let discard_queue q = sync (send q Discard)
end
