(* -*- combobulate-test-point-overlays: ((1 outline 159) (2 outline 185) (3 outline 229)); eval: (combobulate-test-fixture-mode t); -*- *)

type _ term =
  | Int : int -> int term
  | Add : (int term * int term) -> int term
  | Bool : bool -> bool term
