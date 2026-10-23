(* -*- combobulate-test-point-overlays: ((1 outline 215) (2 outline 253) (3 outline 276) (4 outline 290) (5 outline 334) (6 outline 395) (7 outline 412)); eval: (combobulate-test-fixture-mode t); -*- *)
open Format

external id : 'a -> 'a = "%identity"

exception E of string

type t = int

class type c = object
  method m : int
end

module type S = sig
  val f : t -> int

  include Other
end

let g x = x + 1
