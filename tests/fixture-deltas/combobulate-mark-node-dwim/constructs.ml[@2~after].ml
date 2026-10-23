(* -*- combobulate-test-point-overlays: ((1 outline 195) (2 outline 243) (3 outline 258) (4 outline 338) (5 outline 372)); eval: (combobulate-test-fixture-mode t); -*- *)
let describe x =
  let label = String.trim x in
  match label with
  |  -> None
  | s -> Some (s, String.length s)

type config = { verbose : bool; retries : int }

module M = struct
  let a = 1
end
