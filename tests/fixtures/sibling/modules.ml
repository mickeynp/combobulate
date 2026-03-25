(* -*- combobulate-test-point-overlays: ((1 outline 503) (2 outline 522) (3 outline 559) (4 outline 615) (5 outline 665) (6 outline 705) (7 outline 923) (8 outline 950) (9 outline 1013) (10 outline 1066) (11 outline 1144)); eval: (combobulate-test-fixture-mode t); -*- *)
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

  (* the external keyword is behaving unexpectedly in this context *)
  (* external get_redis_version_major : unit -> int = "caml_redis_major_version" *)

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
