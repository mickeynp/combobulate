(* -*- combobulate-test-point-overlays: ((1 outline 156) (2 outline 180) (3 outline 202) (4 outline 228)); eval: (combobulate-test-fixture-mode t); -*- *)
val create : unit -> t

val is_empty : t -> bool

val free : t -> unit

val size : t -> int
