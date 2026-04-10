(* -*- combobulate-test-point-overlays: ((1 outline 148) (2 outline 159) (3 outline 308)); eval: (combobulate-test-fixture-mode t); -*- *)

module Positive : sig  (* sibling navigation should go to struct *)
  type t = private int
  val make : int -> t
  val to_int : t -> int
  val add : t -> t -> t
end = struct
  type t = int
  let make i = if i > 0 then i else invalid_arg "make"
  let to_int x = x
  let add x y = x + y
end

(* Module for mathematical operations *)
module Math = struct
  let square x = x * x
  let cube x = x * x * x
  let double x = x + x
  let half x = x / 2

  module Constants = struct
    let pi = 3.14159
    let e = 2.71828
    let golden_ratio = 1.61803
  end
end
