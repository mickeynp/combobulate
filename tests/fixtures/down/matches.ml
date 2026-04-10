(* -*- combobulate-test-point-overlays: ((1 outline 160) (2 outline 200)); eval: (combobulate-test-fixture-mode t); -*- *)
let is_positive_even = function
  | x when x > 0 && x mod 2 = 0 -> true
  | _ -> false
