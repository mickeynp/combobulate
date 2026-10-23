(* -*- combobulate-test-point-overlays: ((1 outline 172) (2 outline 177) (3 outline 187) (4 outline 189) (5 outline 200)); eval: (combobulate-test-fixture-mode t); -*- *)
type shape =
  [ `Circle of float
  | `Rectangle of float * float
  | `Point
  ]
