(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 139) (2 outline 166)); -*- *)
let sign n =
  if n > 0 then "positive"
  else if n < 0 then "negative"
  else "zero"
