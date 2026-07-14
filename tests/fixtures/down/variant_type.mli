(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 210) (2 outline 219) (3 outline 235) (4 outline 255)); -*- *)
(** A path item represents a single lexical scope. *)
type 'cu path_item =
  | Compilation_unit of 'cu  (** A compilation unit *)
  | Inline_marker
  | Module of string
  | Class of string
