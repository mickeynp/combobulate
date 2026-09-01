(* -*- combobulate-test-point-overlays: ((1 outline 191) (2 outline 263) (3 outline 315) (4 outline 389) (5 outline 1069) (6 outline 1187)); eval: (combobulate-test-fixture-mode t); -*- *)

module Core_utils = struct
  let normalize_query s = String.trim s
end

module Redis = struct
  type connection = true
end

module Http = struct
  module Request = struct
    type t = ..
  end
end

module Redis_client = struct

  open Core_utils

  type connection = Redis.connection
  type 'a query_result = Success of 'a | Failure of exn
  type Http.Request.t += Redis_command of string

  exception Connection_failed of string
  exception Query_failed of { query: string; reason: string }

  external get_redis_version_major : unit -> int = "caml_redis_major_version"

  let default_port = 6379

  module type LOGGER = sig
    val log : string -> unit
  end

  include (val (failwith "Unimplemented") : LOGGER)

  class type redis_connection_obj = object method send: string -> unit end
  class default_connection = object
    method send cmd = print_endline cmd
  end

end

module Safe_redis_client = (Redis_client : sig
  exception Connection_failed of string
  val default_port : int
end)

module Redis = Redis_client

