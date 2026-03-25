(* -*- combobulate-test-point-overlays: ((1 outline 181) (2 outline 204) (3 outline 280) (4 outline 304)); eval: (combobulate-test-fixture-mode t); -*- *)
module Positive : sig 
  type t = private int
  val make : int -> t     (* sibling navigation should go to val to_int  *)
  val to_int : t -> int
  val add : t -> t -> t
end = struct
  type t = int
  let make i = if i > 0 then i else invalid_arg "make"
  let to_int x = x
  let add x y = x + y
end
