(* -*- combobulate-test-point-overlays: ((1 outline 323) (2 outline 330) (3 outline 334) (4 outline 375) (5 outline 381) (6 outline 388) (7 outline 396) (8 outline 406) (9 outline 411)); eval: (combobulate-test-fixture-mode t); -*- *)
module type ORDERED = sig
  type t = A | B | C | D

  val compare : t -> t -> int
end

module F :
functor
  (M : ORDERED) (N : ORDERED)
-> sig
  module P :
    sig
      type t
    end
end
