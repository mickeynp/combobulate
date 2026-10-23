(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 188) (2 outline 208) (3 outline 233) (4 outline 253) (5 outline 280) (6 outline 300)); -*- *)
[@@@warning "-32"]

let unused_helper x = x



let used_helper y = y + 1

let annotated z = z [@@warning "-32"]
