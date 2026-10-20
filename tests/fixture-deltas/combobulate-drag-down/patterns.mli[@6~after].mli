(* -*- combobulate-test-point-overlays: ((1 outline 405) (2 outline 497) (3 outline 575) (4 outline 660) (5 outline 742) (6 outline 813) (7 outline 910) (8 outline 1033) (9 outline 1121) (10 outline 1207) (11 outline 1306) (12 outline 1383) (13 outline 1469) (14 outline 1560) (15 outline 1660) (16 outline 1753) (17 outline 1859)); eval: (combobulate-test-fixture-mode t); -*- *)

(** A user record. *)
type user = { name : string; id : int }

(** A role, either an administrator or a guest. *)
type role = Admin of user | Guest

(** Whether a role is an administrator. *)
val is_admin : role -> bool

(** Describe a character by the class it falls into. *)
val classify_char : char -> unit

(** Describe a point by the axis it lies on. *)
val describe_point : int * int -> string

(** The length of a list. *)
val handle_http_method : [ `GET | `POST | `PUT | `DELETE ] -> string

(** Describe an HTTP method given as a polymorphic variant. *)
val list_length : 'a list -> int

(** The name of a user, marking the root account. *)
val get_user_name : user -> string

(** Describe an array by its first few elements. *)
val get_array_header : int array -> string

(** Print the head of an integer list. *)
val process_int_list : int list -> unit

(** Force a lazy optional integer, defaulting to zero. *)
val force_lazy_int : int option Lazy.t -> int

(** Describe an exception. *)
val handle_exception : exn -> string

(** A module carrying an integer identifier. *)
module type ID = sig val id : int end

(** The identifier of a first-class [ID] module. *)
val get_module_id : (module ID) -> int

(** The head of a list, using [List]'s own constructors. *)
val get_list_head_with_open : 'a list -> 'a option

(** An object that can log a message. *)
class type logger = object method log : string -> unit end

(** Log a message through a logger object. *)
val log_message : #logger -> string -> unit
