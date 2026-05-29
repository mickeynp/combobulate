(** A generic key-value store signature.  -*- combobulate-test-point-overlays: ((1 outline 230) (2 outline 469) (3 outline 895) (4 outline 1076) (5 outline 1177) (6 outline 1264)); eval: (combobulate-test-fixture-mode t); -*- *)
module type KEY_VALUE_STORE = sig
  type key
  type t

  val connect : string -> t
  val get : t -> key -> string option
  val set : t -> key -> string -> unit
end

(** A database connection signature with utilities and error handling. *)
module type DATABASE_CONNECTION = sig
  val connection_pool_size : int

  external get_driver_version : unit -> string = "caml_get_db_driver_version"

  type 'a query_result =
    | Row of 'a
    | Error of string

  and row_id = int

  exception Connection_failed of string

  module Utils : sig
    val normalize_query : string -> string
  end
end

(** A functor signature that builds a cache from a database connection. *)
module type MAKE_CACHE =
  functor (Db : DATABASE_CONNECTION) -> sig
    val get_user : id:int -> string
  end

(** A concrete module — exposed so callers can use it as a value. *)
module My_db : sig
  val version : int
end

(** The module type of [My_db], derived structurally. *)
module type DB_TYPE = module type of My_db

(** An alias for [DATABASE_CONNECTION]. *)
module type CONNECTION_ALIAS = DATABASE_CONNECTION
