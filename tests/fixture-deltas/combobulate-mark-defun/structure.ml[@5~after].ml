(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 227) (2 outline 255) (3 outline 310) (4 outline 379) (5 outline 466) (6 outline 505) (7 outline 574) (8 outline 592)); -*- *)
open Format

exception Parse_failure of string * int

type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

let rec depth t =
  match t with
  | Leaf -> 0
  | Node (l, _, r) -> 1 + max (depth l) (depth r)

module type ORDERED = sig


  include Set.OrderedType
end

class counter init =
  object
    val mutable n = init

    method bump = n <- n + 1
  end
