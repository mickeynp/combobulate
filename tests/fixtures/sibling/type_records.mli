(* -*- combobulate-test-point-overlays: ((1 outline 181) (2 outline 192) (3 outline 208) (4 outline 232)); eval: (combobulate-test-fixture-mode t); -*- *)

type user_profile = {
  id: int;
  name: string;
  email: string option;
  mutable last_login: float;
}
