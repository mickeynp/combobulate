(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 204) (2 outline 217) (3 outline 255) (4 outline 278) (5 outline 292) (6 outline 336) (7 outline 397)); -*- *)
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

val g : t -> t
