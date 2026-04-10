(* -*- combobulate-test-point-overlays: ((1 outline 345) (2 outline 360) (3 outline 415) (4 outline 434)); eval: (combobulate-test-fixture-mode t); -*- *)
module Positive : sig
  type t = private int
  val make : int -> t
  val to_int : t -> int
  val add : t -> t -> t
end = struct (* Hierarchy navigation should go from struct to type t *)
  type t = int
  let make i = if i > 0 then i else invalid_arg "make"
  let to_int x = x
  let add x y = x + y
end
