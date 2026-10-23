(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 156) (2 outline 163) (3 outline 181) (4 outline 187)); -*- *)
module Make (S : Spec) : sig
  val sequential :
    ?config:Property.config -> ?seq_len:int -> unit -> Property.property
  (** Sequential state machine property. Generates command sequences, executes
      them, and checks postconditions at each step. *)
end
