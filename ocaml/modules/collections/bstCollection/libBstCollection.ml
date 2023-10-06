open LibFoldable
open LibCollection

type 'a bst = Leaf | Node of 'a bst * 'a * 'a bst

module BstFoldable : Foldable = struct
  type 'a t = 'a bst

  let empty = Leaf

  let rec insert value foldable =
    match foldable with
    | Leaf -> Node (Leaf, value, Leaf)
    | Node (l, v, r) ->
        if v = value then Node (l, v, r)
        else if v < value then Node (insert value l, v, r)
        else Node (l, v, insert value r)

  let rec fold_left f acc foldable =
    match foldable with
    | Leaf -> acc
    | Node (l, v, r) ->
        let lres = fold_left f acc l in
        let cacc = f lres v in
        fold_left f cacc r
end

module BstCollection = CollectionFromFoldable (BstFoldable)
