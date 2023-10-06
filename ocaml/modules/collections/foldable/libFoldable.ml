module type Foldable = sig
  type 'a t

  val empty : 'a t
  val insert : 'a -> 'a t -> 'a t
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
end

module Foldable : Foldable = struct
  type 'a t = 'a list

  let empty = []
  let insert value foldable = value :: foldable

  let rec fold_left f acc foldable =
    match foldable with [] -> acc | x :: xs -> fold_left f (f acc x) xs
end
