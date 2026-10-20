(* -*- combobulate-test-point-overlays: ((1 outline 156) (2 outline 189) (3 outline 200) (4 outline 207)); eval: (combobulate-test-fixture-mode t); -*- *)
class type ['a] basic_widget =
  object
    method get_id : int
    method set_id : int -> unit
  end
