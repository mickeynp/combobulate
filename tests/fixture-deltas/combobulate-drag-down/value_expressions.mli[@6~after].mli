(** Approximation of pi.  -*- combobulate-test-point-overlays: ((1 outline 859) (2 outline 896) (3 outline 939) (4 outline 1015) (5 outline 1096) (6 outline 1185) (7 outline 1263) (8 outline 1327) (9 outline 1403) (10 outline 1507) (11 outline 1626) (12 outline 1773) (13 outline 1860) (14 outline 1909) (15 outline 1949) (16 outline 1971) (17 outline 1994) (18 outline 2022) (19 outline 2053) (20 outline 2126) (21 outline 2181) (22 outline 2202) (23 outline 2227) (24 outline 2250) (25 outline 2346) (26 outline 2428) (27 outline 2512) (28 outline 2604) (29 outline 2636) (30 outline 2703) (31 outline 2777) (32 outline 2855) (33 outline 2948) (34 outline 2974) (35 outline 3054) (36 outline 3161) (37 outline 3245) (38 outline 3362) (39 outline 3406) (40 outline 3420) (41 outline 3475) (42 outline 3596)); eval: (combobulate-test-fixture-mode t); -*- *)
val pi : float

(** Two times pi. *)
val two_pi : float

(** Math constants. *)
module Math : sig
  val e : float
end

(** Euler's number from [Math.e]. *)
val euler : float

(** [area_of_circle radius] computes the area of a circle. *)
val area_of_circle : float -> float

(** [factorial n] computes the factorial of [n]. *)
val is_zero : int -> bool

(** [is_zero n] returns [true] if [n] is zero. *)
val factorial : int -> int

(** Increments an integer by one. *)
val increment : int -> int

(** [greet ~name] returns a greeting string. *)
val greet : name:string -> string

(** [get_config ?port ()] returns the port, defaulting to [8080]. *)
val get_config : ?port:int -> unit -> int

(** [number_to_string n] converts small integers to their English names. *)
val number_to_string : int -> string

(** [get_env_var var] returns the value of the environment variable [var],
    or ["default"] if not set. *)
val get_env_var : string -> string

(** A 3-tuple representing a point in 3D space. *)
val point : int * int * int

(** A role type. *)
type role =
  | Admin
  | Guest of int

val admin_role : role
val guest_role : role

val an_option : int option

val http_ok : [> `Ok of int ]

val http_get : [> `GET ]

(** A user record with a mutable age field. *)
type user = {
  name : string;
  mutable age : int;
}

val new_user : user

val alice_name : string

val older_user : user

val primes : int array

(** [sign n] returns [1], [-1], or [0] depending on the sign of [n]. *)
val sign : int -> int

(** [countdown n] prints integers from [n] down to [1]. *)
val countdown : int -> unit

(** [print_upto n] prints integers from [1] to [n]. *)
val print_upto : int -> unit

(** [print_downto n] prints integers from [n] down to [0]. *)
val print_downto : int -> unit

val an_int : int

(** An anonymous object with a single method. *)
val an_obj : < x : int >

(** [an_obj] coerced to an open object type. *)
val coerced_obj : < x : int; .. >

(** A counter class with mutable state. *)
class counter : object
  val mutable count : int
  method get : int
  method inc : unit
end

val my_counter : counter

val custom_obj : < value : int >

(** A module type for identifiable values. *)
module type ID = sig
  val id : string
end

(** A locally defined module packed as a first-class value. *)
val local_module : (module ID)

(** A lazy computation that produces an integer. *)
val lazy_computation : int Lazy.t

(** [make_id_gen ()] creates a fresh hashtable for mapping values to integers. *)
val make_id_gen : unit -> (module sig end)

val len : int
val len_block : int

(** Monadic bind for [option]. *)
val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option

(** [add_options opt_a opt_b] adds two optional integers. *)
val add_options : int option -> int option -> int option
