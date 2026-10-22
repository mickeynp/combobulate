(* -*- combobulate-test-point-overlays: ((1 outline 204) (2 outline 211) (3 outline 218)); eval: (combobulate-test-fixture-mode t); -*- *)
type point = { x : int; y : int; z : int }

let origin () =
  { x = 0; z = 2; y = 1 }
