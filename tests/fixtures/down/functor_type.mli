(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 243) (2 outline 255) (3 outline 262) (4 outline 289) (5 outline 295)); -*- *)
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
