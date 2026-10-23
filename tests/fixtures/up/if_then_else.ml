(* -*- combobulate-test-point-overlays: ((1 outline 172) (2 outline 176) (3 outline 188) (4 outline 206) (5 outline 215)); eval: (combobulate-test-fixture-mode t); -*- *)
let parse n =
  if n = 0xffffffff then
    (DWARF64, n)
  else (DWARF32, n)
