(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 148) (2 outline 206) (3 outline 265)); -*- *)
let rec is_even n =
  if n = 0 then true else is_odd (n - 1)

and is_odd n =
  if n = 0 then false else is_even (n - 1)

and describe n =
  if is_even n then "even" else "odd"
