(* ========== Polymorphic Variants ========== *)

(* Simple polymorphic variant *)
type color = [ `Red | `Green | `Blue | `RGB of int * int * int ]

(* Polymorphic variant with inheritance *)
type basic_color = [ `Red | `Green | `Blue ]
type extended_color = [ basic_color | `Yellow | `Orange ]

(* Open polymorphic variant *)
val color_to_string : [> `Red | `Green | `Blue ] -> string

(* Closed polymorphic variant *)
val string_to_color : string -> [< `Red | `Green | `Blue | `Unknown ] option

(* ========== Class Types ========== *)

(* Basic class type *)
class type point_type = object
  method x : int
  method y : int
  method move : int -> int -> unit
end

(* Class type with inheritance *)
class type shape_type = object
  method area : float
  method perimeter : float
end

class type colored_shape_type = object
  inherit shape_type
  method color : color
  method set_color : color -> unit
end

(* Class type with instance variables *)
class type counter_type = object
  val mutable count : int
  method increment : unit
  method get_count : int
end

(* ========== Include Statements ========== *)

(* Include a module signature *)
module type COMPARABLE = sig
  type t
  val compare : t -> t -> int
end

module type PRINTABLE = sig
  type t
  val to_string : t -> string
end

module type COMPARABLE_PRINTABLE = sig
  include COMPARABLE
  include PRINTABLE with type t := t
end

(* Include with constraints *)
module type MAP_LIKE = sig
  type key
  type 'a t
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a option
end

(* ========== Attributes and Extensions ========== *)

(* Deprecated value *)
val old_function : int -> int [@@deprecated "Use new_function instead"]

(* OCaml attribute *)
val inline_me : int -> int [@@inline]

(* External with attributes *)
external get_time : unit -> float = "caml_sys_time" [@@noalloc]

(* Type with attributes *)
type message =
  | Info of string
  | Warning of string [@warn_on_literal_pattern]
  | Error of string

(* Alert attribute *)
val experimental_feature : unit -> unit [@@alert experimental "This feature is experimental"]

(* ========== Original Examples ========== *)

module Positive : sig
  type t = private int

  val make : int -> t
  val to_int : t -> int
  val add : t -> t -> t
end

module Math : sig
  val square : int -> int
  val cube : int -> int
  val double : int -> int
  val half : int -> int

  module Constants : sig
    val pi : float
    val e : float
    val golden_ratio : float
  end
end

module Stack : sig
  type 'a t

  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> ('a * 'a t) option
  val size : 'a t -> int
end

module StringOps : sig
  val uppercase : string -> string
  val lowercase : string -> string
  val reverse : string -> string
  val concat_with : string -> string list -> string
end

module Operations : sig
  val creer : 'a -> 'a
  val detruire : 'a -> unit
  val repeter : int -> ('a -> 'a) -> 'a -> 'a

  module Mathematiques : sig
    val carre : int -> int
    val racine_carree : int -> float
    val perimetre_cercle : float -> float
    val ete : int -> int
    val hiver : int -> int
  end

  module Chaines : sig
    val longueur : string -> int
    val premiere_lettre : string -> char option
    val derniere_lettre : string -> char option
    val resume : string -> string
  end
end

module Francais : sig
  val prenom : string
  val age : int
  val ville : string

  module Numeros : sig
    val zero : int
    val numero_un : int
    val numero_prefere : int
    val systeme_decimal : int
  end

  module Evenements : sig
    type evenement = { nom : string; annee : int; saison : string }

    val creer_evenement : string -> int -> string -> evenement
    val annee_evenement : evenement -> int
    val nom_evenement : evenement -> string
  end
end

module Collections : sig
  module List : sig
    module Ops : sig
      val take : int -> 'a list -> 'a list
      val drop : int -> 'a list -> 'a list
      val split_at : int -> 'a list -> 'a list * 'a list
    end

    module Fold : sig
      val sum : int list -> int
      val product : int list -> int
      val max_elem : int list -> int
      val min_elem : int list -> int
    end

    module Transform : sig
      val squares : int list -> int list
      val cubes : int list -> int list
      val doubles : int list -> int list
      val increments : int list -> int list
    end
  end

  module Array : sig
    module Create : sig
      val range : int -> int -> int array
      val constant : int -> 'a -> 'a array
      val fibonacci : int -> int array
    end

    module Stats : sig
      val sum : int array -> int
      val mean : int array -> float
      val max : int array -> int
      val min : int array -> int
    end
  end

  module Tree : sig
    type 'a t = Leaf | Node of 'a * 'a t * 'a t

    module Build : sig
      val leaf : 'a t
      val node : 'a -> 'a t -> 'a t -> 'a t
      val singleton : 'a -> 'a t
    end

    module Ops : sig
      val size : 'a t -> int
      val height : 'a t -> int
      val map : ('a -> 'b) -> 'a t -> 'b t
      val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
    end
  end
end

module DataStructures : sig
  module Linear : sig
    module Queue : sig
      type 'a t = { front : 'a list; back : 'a list }

      val empty : 'a t
      val enqueue : 'a -> 'a t -> 'a t
      val dequeue : 'a t -> ('a * 'a t) option
      val size : 'a t -> int
    end

    module Deque : sig
      type 'a t = 'a list * 'a list

      val empty : 'a list * 'b list
      val push_front : 'a -> 'a list * 'b -> 'a list * 'b
      val push_back : 'a -> 'b * 'a list -> 'b * 'a list
      val pop_front : 'a list * 'a list -> ('a * ('a list * 'a list)) option
      val pop_back : 'a list * 'a list -> ('a * ('a list * 'a list)) option
    end
  end

  module Associative : sig
    module HashMap : sig
      type ('k, 'v) t = ('k * 'v) list array

      val create : int -> 'a list array
      val hash_function : 'a -> int -> int
      val add : 'a -> 'b -> ('a * 'b) list array -> ('a * 'b) list array
      val find : 'a -> ('a * 'b) list array -> 'b option
    end

    module Trie : sig
      type t = { is_end : bool; children : (char * t) list }

      val empty : t
      val insert : char list -> t -> t
      val search : char list -> t -> bool
    end
  end
end

module Functional : sig
  module Combinators : sig
    val id : 'a -> 'a
    val const : 'a -> 'b -> 'a
    val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

    module Compose : sig
      val ( <| ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
      val ( |> ) : 'a -> ('a -> 'b) -> 'b
      val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
      val ( << ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
    end
  end

  module Curry : sig
    val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
    val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
    val curry3 : ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    val uncurry3 : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd
  end

  module Partial : sig
    val apply_first : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
    val apply_second : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
    val compose_left : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
    val compose_right : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
  end
end

module Mathematics : sig
  module Arithmetic : sig
    module Basic : sig
      val add : int -> int -> int
      val sub : int -> int -> int
      val mul : int -> int -> int
      val div : int -> int -> int option
    end

    module Advanced : sig
      val gcd : int -> int -> int
      val lcm : int -> int -> int
      val factorial : int -> int
      val fibonacci : int -> int
    end
  end

  module Algebra : sig
    module Polynomial : sig
      type t = float list

      val eval : float list -> float -> float
      val add : float list -> float list -> float list
      val scale : float -> float list -> float list
    end

    module Matrix : sig
      type t = float array array

      val create : int -> int -> 'a -> 'a array array
      val get : 'a array array -> int -> int -> 'a
      val set : 'a array array -> int -> int -> 'a -> unit
      val dimensions : 'a array array -> int * int
    end
  end

  module Statistics : sig
    module Descriptive : sig
      val mean : float list -> float
      val variance : float list -> float
      val std_dev : float list -> float
    end

    module Correlation : sig
      val covariance : float list -> float list -> float
      val correlation : float list -> float list -> float
    end
  end

  module Statistiques : sig
    val mediane : float list -> float
    val etendue : float list -> float
    val quartiles : float list -> float * float * float
  end

  module Geometrie : sig
    module Cercle : sig
      val perimetre : float -> float
      val aire : float -> float
      val diametre : float -> float
    end

    module Triangle : sig
      val perimetre : float -> float -> float -> float
      val aire : float -> float -> float
      val est_equilateral : 'a -> 'a -> 'a -> bool
    end

    module Carre : sig
      val perimetre : float -> float
      val aire : float -> float
      val diagonale : float -> float
    end
  end
end

module type ORDERED = sig
  type t

  val compare : t -> t -> int
end

module type SORTABLE = sig
  type elem

  val sort : elem list -> elem list
end

module MakeSort : (Ord : ORDERED) -> sig
  type elem = Ord.t

  val sort : elem list -> elem list
end

module IntOrder : sig
  type t = int

  val compare : 'a -> 'a -> int
end

module StringOrder : sig
  type t = string

  val compare : string -> string -> int
end

module IntSort : sig
  type elem = int

  val sort : elem list -> elem list
end

module StringSort : sig
  type elem = string

  val sort : elem list -> elem list
end

module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
  val to_string : t -> string
end

module type SET = sig
  type elem
  type t

  val empty : t
  val add : elem -> t -> t
  val mem : elem -> t -> bool
  val to_list : t -> elem list
end

module MakeSet : (C : COMPARABLE) -> sig
  type elem = C.t
  type t

  val empty : t
  val add : elem -> t -> t
  val mem : elem -> t -> bool
  val to_list : t -> elem list
end

module IntComparable : sig
  type t = int

  val compare : 'a -> 'a -> int
  val to_string : int -> string
end

module IntSet : sig
  type elem = int
  type t = MakeSet(IntComparable).t

  val empty : t
  val add : elem -> t -> t
  val mem : elem -> t -> bool
  val to_list : t -> elem list
end

val make_adder : int -> int -> int
val make_multiplier : int -> int -> int
val make_power : int -> int -> int
val apply_twice : ('a -> 'a) -> 'a -> 'a
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val apply_n_times : int -> ('a -> 'a) -> 'a -> 'a
val map_pair : ('a -> 'b) -> 'a * 'a -> 'b * 'b
val apply_to_list : ('a -> 'b) list -> 'a -> 'b list
val filter_and_map : ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b list
val creer_additionneur : int -> int -> int
val creer_multiplicateur : int -> int -> int
val appliquer_repete : int -> ('a -> 'a) -> 'a -> 'a
val composer_fonctions : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val transformer_paire : ('a -> 'b) -> 'a * 'a -> 'b * 'b
val filtrer_et_transformer : ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b list
val appliquer_a_tous : ('a -> 'b) -> 'a list -> 'b list
val reduire : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val add : int -> int -> int
val add_five : int -> int
val add_ten : int -> int
val multiply : int -> int -> int
val double : int -> int
val triple : int -> int
val quadruple : int -> int
val power : int -> int -> int
val square_via_power : int -> int
val cube_via_power : int -> int
val fold_sum : int list -> int
val fold_product : int list -> int
val fold_max : int list -> int
val map_square : int list -> int list
val map_double : int list -> int list
val map_negate : int list -> int list
val filter_positive : int list -> int list
val filter_even : int list -> int list
val filter_odd : int list -> int list
val math_ops : (int -> int) list
val transform_list : int list -> int list
val process_pairs : (int * int) list -> int list
val nested_functions : int -> int -> int -> int
val apply_op : string -> int -> int -> int

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module OptionMonad : sig
  type 'a t = 'a option

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module ListMonad : sig
  type 'a t = 'a list

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

val pipeline_example : unit -> int

module CustomIndexing : sig
  type int_array = int array

  val ( .%() ) : 'a array -> int -> 'a
  val ( .%()<- ) : 'a array -> int -> 'a -> unit

  type text = string

  val ( .%[] ) : string -> int -> char
  val ( .%[]<- ) : string -> int -> char -> string

  type ('k, 'v) map = ('k * 'v) list

  val ( .%{} ) : ('a * 'b) list -> 'a -> 'b option
  val ( .%{}<- ) : ('a * 'b) list -> 'a -> 'b -> ('a * 'b) list

  type 'a circular_buffer = { data : 'a array; size : int }

  val ( .@() ) : 'a circular_buffer -> int -> 'a
end

module LetPunning : sig
  type person = { name : string; age : int; city : string }
  type coordinates = { x : float; y : float; z : float }

  val make_person : string -> int -> string -> person
  val make_coords : float -> float -> float -> coordinates
  val get_person_info : person -> string
  val distance_from_origin : coordinates -> float

  type address = { street : string; number : int; postal_code : string }
  type employee = { name : string; employee_id : int; address : address }

  val make_address : string -> int -> string -> address
  val make_employee : string -> int -> address -> employee
  val process_employee : employee -> string
end

module OptionOps : sig
  val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
  val ( >>| ) : 'a option -> ('a -> 'b) -> 'b option
  val return : 'a -> 'a option
  val safe_div : int -> int -> int option
  val safe_sqrt : float -> float option
  val safe_log : float -> float option
  val complex_calculation : int -> int -> int -> float option
  val simple_calc : int -> int option
end

module ResultOps : sig
  type ('a, 'e) result = ('a, 'e) result

  val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
  val ( >>| ) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result
  val return : 'a -> ('a, 'b) result
  val parse_int : string -> (int, string) result
  val validate_positive : int -> (int, string) result
  val validate_range : int -> int -> int -> (int, string) result
  val parse_and_validate : string -> (int, string) result
end

module ListOps : sig
  val ( >>= ) : 'a list -> ('a -> 'b list) -> 'b list
  val ( >>| ) : 'a list -> ('a -> 'b) -> 'b list
  val return : 'a -> 'a list
  val choices : int list
  val computation : (int * int * int) list
  val cartesian_product : 'a list -> 'b list -> ('a * 'b) list
end

module StateOps : sig
  type ('s, 'a) state = 's -> 'a * 's

  val ( >>= ) : ('a -> 'b * 'c) -> ('b -> 'c -> 'd) -> 'a -> 'd
  val return : 'a -> 'b -> 'a * 'b
  val get : 'a -> 'a * 'a
  val put : 'a -> 'b -> unit * 'a
  val modify : ('a -> 'b) -> 'a -> unit * 'b
  val run_state : ('a -> 'b) -> 'a -> 'b
  val increment : int -> unit * int
  val decrement : int -> unit * int
  val get_count : 'a -> 'a * 'a
  val counter_computation : int -> (int * int) * int
end

module WriterOps : sig
  type ('w, 'a) writer = 'a * 'w list

  val ( >>= ) : 'a * 'b list -> ('a -> 'c * 'b list) -> 'c * 'b list
  val return : 'a -> 'a * 'b list
  val tell : 'a -> unit * 'a list
  val run_writer : 'a * 'b -> 'a * 'b
  val logged_add : int -> int -> int * string list
  val logged_multiply : int -> int -> int * string list
  val computation : int -> int -> int * string list
end

module MonadicSyntax : sig
  module Option : sig
    val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option
    val ( and* ) : 'a option -> 'b option -> ('a * 'b) option
    val example : int -> int -> float option
    val parallel_example : int -> int -> int -> int option
  end

  module Result : sig
    val ( let* ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
    val parse_and_process : string -> (int, string) result
  end

  module List : sig
    val ( let* ) : 'a list -> ('a -> 'b list) -> 'b list
    val combinations : (int * int) list
  end
end
