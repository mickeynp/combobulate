(* -*- combobulate-test-point-overlays: ((1 outline 189) (2 outline 353) (3 outline 688) (4 outline 802) (5 outline 845) (6 outline 889)); eval: (combobulate-test-fixture-mode t); -*- *)

module type KEY_VALUE_STORE = sig
  type key
  type t
  val connect : string -> t
  val get : t -> key -> string option
  val set : t -> key -> string -> unit
end

module type DATABASE_CONNECTION = sig

  val connection_pool_size : int
  external get_driver_version : unit -> string = "caml_get_db_driver_version"

  type 'a query_result = Row of 'a | Error of string
  and row_id = int

  exception Connection_failed of string

  module Utils : sig val normalize_query : string -> string end

end

module type MAKE_CACHE =
  functor (Db : DATABASE_CONNECTION) ->
  sig
    val get_user : id:int -> string
  end

module My_db = struct let version = 1 end

module type DB_TYPE = module type of My_db

module type CONNECTION_ALIAS = DATABASE_CONNECTION
