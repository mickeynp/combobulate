(* -*- combobulate-test-point-overlays: ((1 outline 156) (2 outline 160) (3 outline 179) (4 outline 192)); eval: (combobulate-test-fixture-mode t); -*- *)
let is_positive_even = function
  | x when x > 0 && x mod 2 = 0 -> true
  | _ -> false
