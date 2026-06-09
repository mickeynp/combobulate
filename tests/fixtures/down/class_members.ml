(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 156) (2 outline 170) (3 outline 196) (4 outline 207)); -*- *)
class virtual file_handler filename =
  object
    val mutable file_descr = None
    method open_file = 1
  end
