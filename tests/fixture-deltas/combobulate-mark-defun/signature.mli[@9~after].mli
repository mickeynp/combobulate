(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 243) (2 outline 271) (3 outline 310) (4 outline 370) (5 outline 408) (6 outline 450) (7 outline 497) (8 outline 556) (9 outline 575)); -*- *)
open Format

exception Parse_failure of string * int

external identity : 'a -> 'a = "%identity"

type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

val depth : 'a tree -> int

module Parsed : sig
  val starts_with_prefix : string -> bool

  include Set.OrderedType
end

class type counter =
  object
    val n : int


  end
