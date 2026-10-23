(* -*- combobulate-test-point-overlays: ((1 outline 172) (2 outline 183) (3 outline 205) (4 outline 216) (5 outline 223)); eval: (combobulate-test-fixture-mode t); -*- *)
class type ['a] basic_widget =
  object
    method get_id : int
    method set_id : int -> unit
  end
