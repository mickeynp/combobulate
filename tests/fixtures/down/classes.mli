(* -*- combobulate-test-point-overlays: ((1 outline 208) (2 outline 216)); eval: (combobulate-test-fixture-mode t); -*- *)
class counter : object
  val mutable count : int
  method increment : unit
  method reset : unit
  method get : int
end
