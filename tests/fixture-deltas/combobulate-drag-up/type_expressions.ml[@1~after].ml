(* -*- combobulate-test-point-overlays: ((1 outline 220) (2 outline 284) (3 outline 325) (4 outline 382) (5 outline 433) (6 outline 516) (7 outline 581) (8 outline 688)); eval: (combobulate-test-fixture-mode t); -*- *)
type ('data, 'error) network_response = ('data, 'error) result

module Date = struct type t = float end

type extensible_config = < port : int; host : string; >

class base_widget = object method id = "base" end

class type db_connection = object method query : string -> string array array end

type shape = [ `Circle of float | `Rectangle of float * float ]

let zip_with_value : 'a 'b. 'a -> 'b list -> ('a * 'b) list =
  fun x ys -> List.map (fun y -> (x, y)) ys

module type KV_STORE = sig
  type key
  type t
  val get : t -> key -> string option
end
