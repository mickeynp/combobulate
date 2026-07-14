(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 181) (2 outline 188) (3 outline 197) (4 outline 216)); -*- *)
(** Inverse direction *)
module Parsed : sig
  (** doc *)
  val starts_with_prefix : string -> bool

  (** doc *)
  val parse : string -> int option
end
