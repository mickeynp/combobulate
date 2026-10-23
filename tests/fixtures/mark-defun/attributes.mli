(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 188) (2 outline 208) (3 outline 240) (4 outline 260) (5 outline 290) (6 outline 317)); -*- *)
[@@@warning "-32"]

val unused_helper : int -> int

[@@@warning "+32"]

val used_helper : int -> int

val annotated : int -> int [@@warning "-32"]
