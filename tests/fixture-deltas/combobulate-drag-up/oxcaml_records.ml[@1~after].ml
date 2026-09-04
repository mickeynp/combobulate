(* -*- combobulate-test-point-overlays: ((1 outline 124) (2 outline 203)); eval: (combobulate-test-fixture-mode t); -*- *)
type point = {
  label : string;
  count : int;
  x : float#;
  y : float#;
}

type 'a wrapper = {
  global_ value : 'a;
  tag : int;
}
