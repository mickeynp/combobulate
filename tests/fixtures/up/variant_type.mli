(* -*- combobulate-test-point-overlays: ((1 outline 226) (2 outline 231) (3 outline 249) (4 outline 251) (5 outline 271)); eval: (combobulate-test-fixture-mode t); -*- *)
(** A path item represents a single lexical scope. *)
type 'cu path_item =
  | Compilation_unit of 'cu  (** A compilation unit *)
  | Inline_marker
  | Module of string
  | Class of string
