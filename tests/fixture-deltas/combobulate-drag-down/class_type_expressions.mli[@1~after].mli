(* -*- combobulate-test-point-overlays: ((1 outline 185) (2 outline 209) (3 outline 241)); eval: (combobulate-test-fixture-mode t); -*- *)

class type ['a] basic_widget =
  object
    method set_id : int -> unit
    method get_id : int
    method draw : 'a -> unit
  end
