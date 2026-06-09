(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 156) (2 outline 172) (3 outline 189) (4 outline 200)); -*- *)
class type ['a] basic_widget =
  object
    method get_id : int
    method set_id : int -> unit
  end
