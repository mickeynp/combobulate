(* -*- combobulate-test-point-overlays: ((1 outline 136) (2 outline 146)); eval: (combobulate-test-fixture-mode t); -*- *)
module type ORDERED = sig
  type t = A | B | C | D
  val compare : t -> t -> int
end
