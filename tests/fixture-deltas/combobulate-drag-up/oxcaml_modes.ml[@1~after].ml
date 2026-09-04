(* -*- combobulate-test-point-overlays: ((1 outline 156) (2 outline 247) (3 outline 291) (4 outline 334)); eval: (combobulate-test-fixture-mode t); -*- *)
let use_temp ~(f : int ref -> 'a) : 'a =
  let tmp = stack_ (ref 0) in
  f tmp [@nontail]

let make_pair a b = enclave_ stack_ (a, b)

let[@zero_alloc] fast_clamp (x : int) = x

let[@inline always] inlined_square x = x * x
