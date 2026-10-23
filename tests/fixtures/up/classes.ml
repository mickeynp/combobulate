(* -*- combobulate-test-point-overlays: ((1 outline 156) (2 outline 162) (3 outline 174) (4 outline 185)); eval: (combobulate-test-fixture-mode t); -*- *)
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
