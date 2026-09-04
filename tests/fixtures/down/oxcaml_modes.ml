(* -*- combobulate-test-point-overlays: ((1 outline 124) (2 outline 167)); eval: (combobulate-test-fixture-mode t); -*- *)
let use_temp ~(f : int ref -> 'a) : 'a =
  let tmp = stack_ (ref 0) in
  f tmp
