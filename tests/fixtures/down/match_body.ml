(* -*- combobulate-test-point-overlays: ((1 outline 158) (2 outline 171)); eval: (combobulate-test-fixture-mode t); -*- *)
let number x =
  match x with
  | 1 | 2 | 3 -> false
  | _ -> true
