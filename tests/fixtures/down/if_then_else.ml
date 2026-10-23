(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 156) (2 outline 174) (3 outline 183)); -*- *)
let parse n =
  if n = 0xffffffff then
    (DWARF64, n)
  else (DWARF32, n)
