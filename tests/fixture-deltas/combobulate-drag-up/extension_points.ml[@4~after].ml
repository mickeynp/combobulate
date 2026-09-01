(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 204) (2 outline 263) (3 outline 314) (4 outline 400) (5 outline 499) (6 outline 557) (7 outline 603)); -*- *)
let with_structure_payload = 1 [@payload let x = 10 in x]

let with_type_payload = 1 [@payload: int -> bool]

let with_pattern_and_guard = function
  | Some x [@payload? Some y when y > 0] -> x
  | None -> 0

let with_pattern_payload = function
  | Some x [@payload? Some y] -> x
  | None -> 0

let multiple_attributes = 42 [@first] [@second] [@third]

let[@inline always] inlined_square x = x * x

let[@inline always] inlined_add a b = a + b
