open LibCollection

module type Map = sig
  type ('k, 'v) t

  val empty : ('k, 'v) t
  val get_opt : 'k -> ('k, 'v) t -> 'v option
  val set : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  val delete : 'k -> ('k, 'v) t -> ('k, 'v) t
  val contains : 'k -> ('k, 'v) t -> bool
end

module TODO_Map = struct
  type ('k, 'v) t = unit (* TODO *)

  let empty = ()
  let get_opt key map = failwith "TODO"
  let set key value map = failwith "TODO"
  let delete key map = failwith "TODO"
  let contains key map = failwith "TODO"
end

module MapFromCollection (C : Collection) : Map = struct
  type ('k, 'v) t = ('k * 'v) C.t

  let empty = C.empty

  let get_opt key map =
    match C.find_opt (fun (k, v) -> k = key) map with
    | None -> None
    | Some (k, v) -> Some v

  let set key value map = C.insert (key, value) map
  let delete key map = C.filter (fun (k, v) -> k <> key) map

  let contains key map =
    match get_opt key map with None -> false | Some _ -> true
end
