(* -*- combobulate-test-point-overlays: ((1 outline 1) (2 outline 357) (3 outline 366) (4 outline 376) (5 outline 384) (6 outline 391) (7 outline 396) (8 outline 409) (9 outline 424) (10 outline 430) (11 outline 439) (12 outline 444) (13 outline 450) (14 outline 453) (15 outline 460) (16 outline 467)); eval: (combobulate-test-fixture-mode t); -*- *)
let describe x =
  let label = String.trim x in
  match label with
  | "" -> None
  | s -> Some (s, String.length s)
