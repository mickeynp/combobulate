(* -*- combobulate-test-point-overlays: ((1 outline 356) (2 outline 489) (3 outline 620) (4 outline 727) (5 outline 838) (6 outline 947) (7 outline 1004) (8 outline 1129) (9 outline 1212) (10 outline 1372)); eval: (combobulate-test-fixture-mode t); -*- *)

module Redis_client : sig

  (** The Redis connection type, aliased from [Redis.connection]. *)
  type connection = Redis.connection

  (** The result of a query — either a success with a value or a failure with an exception. *)
  type 'a query_result =
    | Success of 'a
    | Failure of exn

  (** Extends [Http.Request.t] with a Redis command variant. *)
  type Http.Request.t += Redis_command of string

  (** Raised when a connection cannot be established. *)
  exception Connection_failed of string

  (** Raised when a query fails, with the query string and reason. *)
  exception Query_failed of {
    query : string;
    reason : string;
  }

  (** The default Redis port. *)
  val default_port : int

  (** A logger module type. *)
  module type LOGGER = sig
    val log : string -> unit
  end

  (** The log function included from the LOGGER signature. *)
  val log : string -> unit

  (** The class type for a Redis connection object. *)
  class default_connection : object
    method send : string -> unit
  end

  (** A default connection implementation that prints commands to stdout. *)
  class type redis_connection_obj = object
    method send : string -> unit
  end

end

