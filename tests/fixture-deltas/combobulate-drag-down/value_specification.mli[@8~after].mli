(** A simple integer value.  -*- combobulate-test-point-overlays: ((1 outline 296) (2 outline 355) (3 outline 415) (4 outline 475) (5 outline 550) (6 outline 609) (7 outline 659) (8 outline 720) (9 outline 780) (10 outline 846) (11 outline 922)); eval: (combobulate-test-fixture-mode t); -*- *)
val simple_integer : int

(** A 32-bit integer literal. *)
val int32_literal : int32

(** A 64-bit integer literal. *)
val int64_literal : int64

(** A native integer literal. *)
val nativeint_literal : nativeint

(** An integer with a custom suffix. *)
val integer_with_suffix : int

(** A character literal. *)
val char_literal : char

(** A string literal. *)
val string_literal : string

(** A quoted string literal. *)
val float_literal : float

(** A float literal. *)
val quoted_string_literal : string

(** A float in scientific notation. *)
val scientific_notation_float : float

(** A float with a custom suffix. *)
val float_with_suffix : float
