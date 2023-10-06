open LibFoldable

module type Collection = sig
  include Foldable

  val size : 'a t -> int
  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val delete : 'a -> 'a t -> 'a t
  val find_opt : ('a -> bool) -> 'a t -> 'a option
  val iter : ('a -> unit) -> 'a t -> unit
end

module TODO_Collection : Collection = struct
  include TODO_Foldable

  let size c = failwith "TODO"
  let map f c = failwith "TODO"
  let filter pred c = failwith "TODO"
  let delete v c = failwith "TODO"
  let find_opt pred c = failwith "TODO"
  let iter f c = failwith "TODO"
end

module CollectionFromFoldable (F : Foldable) : Collection = struct
  include F

  let size c = F.fold_left (fun acc _ -> acc + 1) 0 c
  let map f c = F.fold_left (fun acc i -> F.insert (f i) acc) F.empty c

  let filter pred c =
    F.fold_left (fun acc i -> if pred i then F.insert i acc else acc) F.empty c

  let delete v c = filter (( <> ) v) c

  let find_opt pred c =
    F.fold_left
      (fun acc i -> if pred i && Option.is_none acc then Some i else acc)
      None c

  let iter f c = F.fold_left (fun _ i -> f i) () c
end
