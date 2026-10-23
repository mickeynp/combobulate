(* -*- combobulate-test-point-overlays: ((1 outline 227) (2 outline 246) (3 outline 273) (4 outline 279)); eval: (combobulate-test-fixture-mode t); -*- *)
module type ORDERED = sig
  type t

  val compare : t -> t -> int
end

module type MAKE =
functor
  (M : ORDERED)
-> sig
  type key = M.t

  val create : key -> int
end
