(* -*- combobulate-test-point-overlays: ((1 outline 253) (2 outline 284) (3 outline 312) (4 outline 338) (5 outline 363) (6 outline 449) (7 outline 548) (8 outline 577) (9 outline 586) (10 outline 596)); eval: (combobulate-test-fixture-mode t); -*- *)
let with_structure_payload = 1 [@payload let x = 10 in x]

let with_type_payload = 1 [@payload: int -> bool]

let with_pattern_payload = function
  | Some x [@payload? Some y] -> x
  | None -> 0

let with_pattern_and_guard = function
  | Some x [@payload? Some y when y > 0] -> x
  | None -> 0

let multiple_attributes = 42 [@first] [@second] [@third]
