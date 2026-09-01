(** A network response aliased from [result].  -*- combobulate-test-point-overlays: ((1 outline 265) (2 outline 379) (3 outline 479) (4 outline 581) (5 outline 680) (6 outline 811) (7 outline 994) (8 outline 1084)); eval: (combobulate-test-fixture-mode t); -*- *)
type ('data, 'error) network_response = ('data, 'error) result

(** A date module with a float representation. *)
module Date : sig
  type t = float
end

(** An extensible object type with port and host fields. *)
type extensible_config = < port : int; host : string >

(** A base widget class with an id method. *)
class type db_connection = object
  method query : string -> string array array
end

(** A class type for database connections. *)
class base_widget : object
  method id : string
end

(** A polymorphic variant type for shapes. *)
type shape =
  [ `Circle of float
  | `Rectangle of float * float ]

(** [zip_with_value x ys] pairs [x] with every element of [ys].
    Universally quantified over ['a] and ['b]. *)
val zip_with_value : 'a -> 'b list -> ('a * 'b) list

(** A key-value store signature. *)
module type KV_STORE = sig
  type key
  type t
  val get : t -> key -> string option
end
