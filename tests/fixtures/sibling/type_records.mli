(* -*- combobulate-test-point-overlays: ((1 outline 180) (2 outline 191) (3 outline 207) (4 outline 231)); eval: (combobulate-test-fixture-mode t); -*- *)
type user_profile = {
  id: int;
  name: string;
  email: string option;
  mutable last_login: float;
}
