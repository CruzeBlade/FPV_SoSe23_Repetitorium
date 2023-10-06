open LibFoldable
open LibCollection

module ListFoldable : Foldable = struct
  type 'a t = 'a list

  let empty = []
  let insert value foldable = value :: foldable

  let rec fold_left f acc foldable =
    match foldable with [] -> acc | x :: xs -> fold_left f (f acc x) xs
end

module ListCollection = CollectionFromFoldable (ListFoldable)
