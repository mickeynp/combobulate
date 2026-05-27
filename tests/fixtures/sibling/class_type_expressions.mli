(* -*- combobulate-test-point-overlays: ((1 outline 184) (2 outline 208) (3 outline 240)); eval: (combobulate-test-fixture-mode t); -*- *)
class type ['a] basic_widget =
  object
    method get_id : int
    method set_id : int -> unit
    method draw : 'a -> unit
  end
