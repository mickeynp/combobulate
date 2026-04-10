(* -*- combobulate-test-point-overlays: ((1 outline 180) (2 outline 259) (3 outline 281) (4 outline 305)); eval: (combobulate-test-fixture-mode t); -*- *)
module Positive : sig
  type t = private int (* Hierarchy navigation should go from sig to type t *)
  val make : int -> t
  val to_int : t -> int
  val add : t -> t -> t
end = struct
  type t = int
  let make i = if i > 0 then i else invalid_arg "make"
  let to_int x = x
  let add x y = x + y
end
