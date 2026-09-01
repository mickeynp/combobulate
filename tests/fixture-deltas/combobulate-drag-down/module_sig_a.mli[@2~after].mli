(* -*- combobulate-test-point-overlays: ((1 outline 180) (2 outline 203) (3 outline 225) (4 outline 249)); eval: (combobulate-test-fixture-mode t); -*- *)
module Positive : sig
  type t = private int
  val to_int : t -> int
  val make : int -> t
  val add : t -> t -> t
end
