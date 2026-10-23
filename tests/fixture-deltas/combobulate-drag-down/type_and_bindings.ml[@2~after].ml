(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 145) (2 outline 200) (3 outline 226)); -*- *)
type expr =
  | Int of int
  | Bin of expr * op * expr

and stmt =
  | Assign of string * expr
  | Print of expr

and op =
  | Add
  | Mul
