(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 170) (2 outline 209) (3 outline 257)); -*- *)
let next_float lo hi seed =
  let copy = Random.State.copy seed in
  let v = Random.State.float copy (hi -. lo) in
  (lo +. v, copy)
