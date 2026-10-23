(* -*- combobulate-test-point-overlays: ((1 outline 1) (2 outline 421) (3 outline 425) (4 outline 434) (5 outline 440) (6 outline 444) (7 outline 452) (8 outline 459) (9 outline 464) (10 outline 471) (11 outline 477) (12 outline 492) (13 outline 498) (14 outline 507) (15 outline 512) (16 outline 517) (17 outline 518) (18 outline 521) (19 outline 528) (20 outline 535)); eval: (combobulate-test-fixture-mode t); -*- *)
let describe x =
  let label = String.trim x in
  match label with
  | "" -> None
  | s -> Some (s, String.length s)
