(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 193) (2 outline 226) (3 outline 263) (4 outline 297) (5 outline 364)); -*- *)
module Inner : sig
  val scale : int -> int -> int

  val triple : int list -> int list

  module Deeper : sig
    val sum_squares : int list -> int
  end

  val add_pair : int list -> int

  val combined : int list -> int
end
