(* -*- combobulate-test-point-overlays: ((1 outline 175) (2 outline 202) (3 outline 230) (4 outline 260)); eval: (combobulate-test-fixture-mode t); -*- *)
let classify n =
  if n < 0 then "negative"
  else if n = 0 then "zero"
  else if n < 10 then "small"
  else "large"
