(* -*- combobulate-test-point-overlays: ((1 outline 323) (2 outline 330) (3 outline 334) (4 outline 375) (5 outline 384) (6 outline 391) (7 outline 399) (8 outline 412) (9 outline 417)); eval: (combobulate-test-fixture-mode t); -*- *)
module type ORDERED = sig
  type t = A | B | C | D

  val compare : t -> t -> int
end

module F =
functor
  (M : ORDERED) (N : ORDERED)
-> struct
  module P =
    struct
      type t
    end
end
