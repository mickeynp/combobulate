(* -*- combobulate-test-point-overlays: ((1 outline 165) (2 outline 173)); eval: (combobulate-test-fixture-mode t); -*- *)
class counter =
  object
    val mutable count = 0

    method increment =
      count <- count + 1

    method reset =
      count <- 0

    method get =
      count
  end
