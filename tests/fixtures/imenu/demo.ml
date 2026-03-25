(* ========== Polymorphic Variants ========== *)

(* Simple polymorphic variant *)
type color = [ `Red | `Green | `Blue | `RGB of int * int * int ]

type color_2 = [ `Red | `Green | `Blue | `RGB of int * string * bool ]

(* Polymorphic variant with inheritance *)
type basic_color = [ `Red | `Green | `Blue ]
type extended_color = [ basic_color | `Yellow | `Orange ]

(* Open polymorphic variant *)
let color_to_string : [> `Red | `Green | `Blue ] -> string = function
  | `Red -> "red"
  | `Green -> "green"
  | `Blue -> "blue"
  | _ -> "unknown"

(* Closed polymorphic variant *)
let string_to_color s : [< `Red | `Green | `Blue | `Unknown ] option =
  match s with
  | "red" -> Some `Red
  | "green" -> Some `Green
  | "blue" -> Some `Blue
  | _ -> Some `Unknown

(* Pattern matching with polymorphic variants *)
let color_brightness = function
  | `Red -> 0.299
  | `Green -> 0.587
  | `Blue -> 0.114
  | `RGB (r, g, b) ->
      (float_of_int r *. 0.299 +.
       float_of_int g *. 0.587 +.
       float_of_int b *. 0.114) /. 255.0

(* ========== Class Types and Implementations ========== *)

(* Basic class implementation matching point_type *)
class point initial_x initial_y = object
  val mutable x = initial_x
  val mutable y = initial_y
  method x = x
  method y = y
  method move dx dy =
    x <- x + dx;
    y <- y + dy
end

(* Shape classes *)
class virtual shape = object
  method virtual area : float
  method virtual perimeter : float
end

class circle radius = object
  inherit shape
  method area = 3.14159 *. radius *. radius
  method perimeter = 2.0 *. 3.14159 *. radius
end

class rectangle width height = object
  inherit shape
  method area = width *. height
  method perimeter = 2.0 *. (width +. height)
end

(* Colored shape *)
class colored_circle radius initial_color = object
  inherit circle radius
  val mutable current_color = initial_color
  method color = current_color
  method set_color c = current_color <- c
end

(* Counter with instance variables *)
class counter = object
  val mutable count = 0
  method increment = count <- count + 1
  method get_count = count
end

(* ========== Include Statements ========== *)

(* Module types for inclusion *)
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

(* Module implementing the combined interface *)
module IntComparablePrintable = struct
  type t = int
  let compare = compare
  let to_string = string_of_int
end

(* Using include in module implementation *)
module ExtendedInt = struct
  include IntComparablePrintable
  let add x y = x + y
  let multiply x y = x * y
end

(* ========== Attributes and Extensions ========== *)

(* Deprecated function *)
let old_function x = x + 1 [@@deprecated "Use new_function instead"]

(* New replacement function *)
let new_function x = x + 1

(* Inline function *)
let inline_me x = x * 2 [@@inline]

(* External with attributes *)
external get_time : unit -> float = "caml_sys_time" [@@noalloc]

(* Type with attributes on constructors *)
type message =
  | Info of string
  | Warning of string [@warn_on_literal_pattern]
  | Error of string

(* Alert attribute *)
let experimental_feature () =
  print_endline "This is experimental" [@@alert experimental "This feature is experimental"]

(* Function with multiple attributes *)
let optimized_function x =
  x * x * x
  [@@inline]
  [@@specialise]

(* GADT Example *)
type _ expression =
  | Int : int -> int expression
  | Bool : bool -> bool expression
  | Add : int expression * int expression -> int expression
  | Eq : 'a expression * 'a expression -> bool expression

let rec eval : type a. a expression -> a = function
  | Int n -> n
  | Bool b -> b
  | Add (e1, e2) -> eval e1 + eval e2
  | Eq (e1, e2) -> eval e1 = eval e2

(* First-class module example *)
module type SHOW = sig
  type t
  val show : t -> string
end

let int_show = (module struct
  type t = int
  let show = string_of_int
end : SHOW with type t = int)

let show_value (type a) (module S : SHOW with type t = a) (x : a) =
  S.show x

(* ========== Original Module Examples ========== *)

(* Simple module with signature *)
module Positive : sig
  type t = private int
  val make : int -> t
  val to_int : t -> int
  val add : t -> t -> t
end = struct
  type t = int
  let make i = if i > 0 then i else invalid_arg "make"
  let to_int x = x
  let add x y = x + y
end

(* Module for mathematical operations *)
module Math = struct            (* TODO hierarchy navigation should skip visiting struct and go directly
                                        to let/type bindings *)
  let all x = x * x + x - x / x
  let square x = x * x
  let cube x = x * x * x
  let double x = x + x
  let half x = x / 2

  module Constants = struct
    let pi = 3.14159            (* TODO sibling navigation should move between this let binding and the following let bindings. *)
    let e = 2.71828
    let golden_ratio = 1.61803
  end
end

(* Module with abstract type *)
module Stack : sig
  type 'a t
  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> ('a * 'a t) option
  val size : 'a t -> int
end = struct
  type 'a t = 'a list
  let empty = []
  let push x s = x :: s
  let pop = function
    | [] -> None
    | x :: xs -> Some (x, xs)
  let size = List.length
end

(* Module for string operations *)
module StringOps = struct
  let uppercase s = String.uppercase_ascii s
  let lowercase s = String.lowercase_ascii s
  let reverse s =               (* TODO hierarchy navigation from argument s goes to fun i skipping the let binding. It should go to the let binding. *)
    let len = String.length s in
    String.init len (fun i -> s.[len - 1 - i]) (* TODO sibling navigation to previous sibling should go to "let len" *)
  let concat_with sep strs = String.concat sep strs
end

(* Module with French Latin-1 characters *)
module Operations = struct
  let creer x = x
  let detruire _ = ()
  let repeter n f x =
    let rec aux acc = function
      | 0 -> acc
      | n -> aux (f acc) (n - 1)
    in aux x n

  module Mathematiques = struct
    let carre x = x * x
    let racine_carree x = sqrt (float_of_int x)
    let perimetre_cercle rayon = 2.0 *. 3.14159 *. rayon
    let ete x = x + 10  (* ete = summer *)
    let hiver x = x - 10  (* hiver = winter *)
  end

  module Chaines = struct
    let longueur s = String.length s
    let premiere_lettre s = if String.length s > 0 then Some s.[0] else None
    let derniere_lettre s =
      let len = String.length s in
      if len > 0 then Some s.[len - 1] else None
    let resume s = if String.length s > 10 then String.sub s 0 10 else s
  end
end

(* Module avec caracteres accentues *)
module Francais = struct
  let prenom = "Francois"
  let age = 25
  let ville = "Montreal"

  module Numeros = struct
    let zero = 0
    let numero_un = 1
    let numero_prefere = 42
    let systeme_decimal = 10
  end

  module Evenements = struct
    type evenement = {
      nom: string;
      annee: int;
      saison: string;
    }

    let creer_evenement nom annee saison =
      { nom; annee; saison }

    let annee_evenement evt = evt.annee
    let nom_evenement evt = evt.nom
  end
end

(* Nested module hierarchy for collections *)
module Collections = struct
  module List = struct
    module Ops = struct
      let rec take n lst =
        match n, lst with
        | 0, _ | _, [] -> []
        | n, x :: xs -> x :: take (n - 1) xs

      let rec drop n lst =
        match n, lst with
        | 0, _ -> lst
        | _, [] -> []
        | n, _ :: xs -> drop (n - 1) xs

      let split_at n lst = (take n lst, drop n lst)
    end

    module Fold = struct
      let sum = List.fold_left (+) 0
      let product = List.fold_left ( * ) 1
      let max_elem lst = List.fold_left max min_int lst
      let min_elem lst = List.fold_left min max_int lst
    end

    module Transform = struct
      let squares = List.map (fun x -> x * x)
      let cubes = List.map (fun x -> x * x * x)
      let doubles = List.map (fun x -> x * 2)
      let increments = List.map (fun x -> x + 1)
    end
  end

  module Array = struct
    module Create = struct
      let range start stop =
        Array.init (stop - start) (fun i -> start + i)

      let constant n value =
        Array.make n value

      let fibonacci n =
        let arr = Array.make n 0 in (* TODO sibling navigation between let,if,for, done, arr doesn't work here. *)
        if n > 0 then arr.(0) <- 0;
        if n > 1 then arr.(1) <- 1;
        for i = 2 to n - 1 do
          arr.(i) <- arr.(i-1) + arr.(i-2)
        done;
        arr
    end

    module Stats = struct
      let sum arr = Array.fold_left (+) 0 arr
      let mean arr =
        let len = Array.length arr in
        if len = 0 then 0.0
        else float_of_int (sum arr) /. float_of_int len

      let max arr = Array.fold_left max min_int arr
      let min arr = Array.fold_left min max_int arr
    end
  end

  module Tree = struct
    type 'a t =                 (* TODO hierarchy navigation between type and constructor declaration should skip the variant declaration. Then sibling navigation between constructor declarations works correctly. *)
      | Leaf
      | Node of 'a * 'a t * 'a t

    module Build = struct
      let leaf = Leaf
      let node v l r = Node (v, l, r)
      let singleton v = Node (v, Leaf, Leaf)
    end

    module Ops = struct
      let rec size = function
        | Leaf -> 0
        | Node (_, l, r) -> 1 + size l + size r

      let rec height = function
        | Leaf -> 0
        | Node (_, l, r) -> 1 + max (height l) (height r)

      let rec map f = function
        | Leaf -> Leaf
        | Node (v, l, r) -> Node (f v, map f l, map f r)

      let rec fold f acc = function
        | Leaf -> acc
        | Node (v, l, r) ->
            let acc' = fold f acc l in
            let acc'' = f acc' v in
            fold f acc'' r
    end
  end
end

(* Deeply nested module for data structures *)
module DataStructures = struct
  module Linear = struct
    module Queue = struct
      type 'a t = {
        front: 'a list;
        back: 'a list;
      }

      let empty = { front = []; back = [] }

      let enqueue x q = { q with back = x :: q.back }

      let dequeue q =
        match q.front with
        | x :: xs -> Some (x, { q with front = xs })
        | [] ->
            match List.rev q.back with
            | [] -> None
            | x :: xs -> Some (x, { front = xs; back = [] })

      let size q = List.length q.front + List.length q.back
    end

    module Deque = struct
      type 'a t = 'a list * 'a list

      let empty = ([], [])

      let push_front x (front, back) = (x :: front, back)
      let push_back x (front, back) = (front, x :: back)

      let pop_front = function
        | (x :: xs, back) -> Some (x, (xs, back))
        | ([], back) ->
            match List.rev back with
            | [] -> None
            | x :: xs -> Some (x, (xs, []))

      let pop_back = function
        | (front, x :: xs) -> Some (x, (front, xs))
        | (front, []) ->
            match List.rev front with
            | [] -> None
            | x :: xs -> Some (x, ([], xs))
    end
  end

  module Associative = struct
    module HashMap = struct
      type ('k, 'v) t = ('k * 'v) list array

      let create size = Array.make size []

      let hash_function key size =
        (Hashtbl.hash key) mod size

      let add key value map =
        let size = Array.length map in
        let idx = hash_function key size in
        let bucket = map.(idx) in
        let new_bucket = (key, value) :: List.filter (fun (k, _) -> k <> key) bucket in
        map.(idx) <- new_bucket;
        map

      let find key map =
        let size = Array.length map in
        let idx = hash_function key size in
        List.assoc_opt key map.(idx)
    end

    module Trie = struct
      type t = {
        is_end: bool;
        children: (char * t) list;
      }

      let empty = { is_end = false; children = [] }

      let rec insert word trie =
        match word with
        | [] -> { trie with is_end = true }
        | c :: cs ->
            let child =
              match List.assoc_opt c trie.children with
              | Some node -> insert cs node
              | None -> insert cs empty
            in
            let new_children = (c, child) :: List.filter (fun (ch, _) -> ch <> c) trie.children in
            { trie with children = new_children }

      let rec search word trie =
        match word with
        | [] -> trie.is_end
        | c :: cs ->
            match List.assoc_opt c trie.children with
            | None -> false
            | Some child -> search cs child
    end
  end
end

(* Module for functional programming utilities *)
module Functional = struct
  module Combinators = struct
    let id x = x
    let const x _ = x
    let flip f x y = f y x

    module Compose = struct
      let (<|) f g x = f (g x)
      let (|>) x f = f x
      let (>>) f g x = g (f x)
      let (<<) f g x = f (g x)
    end
  end

  module Curry = struct
    let curry f x y = f (x, y)
    let uncurry f (x, y) = f x y

    let curry3 f x y z = f (x, y, z)
    let uncurry3 f (x, y, z) = f x y z
  end

  module Partial = struct
    let apply_first f x = fun y -> f x y
    let apply_second f y = fun x -> f x y

    let compose_left f g = fun x -> f (g x)
    let compose_right f g = fun x -> g (f x)
  end
end

(* Module for mathematical operations with nested modules *)
module Mathematics = struct
  module Arithmetic = struct
    module Basic = struct
      let add x y = x + y
      let sub x y = x - y
      let mul x y = x * y
      let div x y = if y <> 0 then Some (x / y) else None
    end

    module Advanced = struct
      let rec gcd a b =
        if b = 0 then a else gcd b (a mod b)

      let lcm a b =
        (a * b) / gcd a b

      let rec factorial = function
        | 0 | 1 -> 1
        | n -> n * factorial (n - 1)

      let rec fibonacci = function
        | 0 -> 0
        | 1 -> 1
        | n -> fibonacci (n - 1) + fibonacci (n - 2)
    end
  end

  module Algebra = struct
    module Polynomial = struct
      type t = float list

      let eval coeffs x =
        List.fold_left (fun acc c -> acc *. x +. c) 0.0 (List.rev coeffs)

      let add p1 p2 =
        let rec aux acc l1 l2 =
          match l1, l2 with
          | [], [] -> List.rev acc
          | x :: xs, [] | [], x :: xs -> aux (x :: acc) xs []
          | x :: xs, y :: ys -> aux ((x +. y) :: acc) xs ys
        in aux [] p1 p2

      let scale s p = List.map (fun c -> s *. c) p
    end

    module Matrix = struct
      type t = float array array

      let create rows cols init =
        Array.make_matrix rows cols init

      let get m i j = m.(i).(j)
      let set m i j v = m.(i).(j) <- v

      let dimensions m =
        (Array.length m, if Array.length m > 0 then Array.length m.(0) else 0)
    end
  end

  module Statistics = struct
    module Descriptive = struct
      let mean lst =
        let sum = List.fold_left (+.) 0.0 lst in
        let count = float_of_int (List.length lst) in
        sum /. count

      let variance lst =
        let m = mean lst in
        let squared_diffs = List.map (fun x -> (x -. m) ** 2.0) lst in
        mean squared_diffs

      let std_dev lst =
        sqrt (variance lst)
    end

    module Correlation = struct
      let covariance xs ys =
        let mean_x = Descriptive.mean xs in
        let mean_y = Descriptive.mean ys in
        let pairs = List.combine xs ys in
        let prods = List.map (fun (x, y) -> (x -. mean_x) *. (y -. mean_y)) pairs in
        Descriptive.mean prods

      let correlation xs ys =
        let cov = covariance xs ys in
        let std_x = Descriptive.std_dev xs in
        let std_y = Descriptive.std_dev ys in
        cov /. (std_x *. std_y)
    end
  end

  (* Statistiques en francais *)
  module Statistiques = struct
    let mediane lst =
      let sorted = List.sort compare lst in
      let len = List.length sorted in
      if len = 0 then 0.0
      else if len mod 2 = 1 then
        List.nth sorted (len / 2)
      else
        let mid1 = List.nth sorted (len / 2 - 1) in
        let mid2 = List.nth sorted (len / 2) in
        (mid1 +. mid2) /. 2.0

    let etendue lst =
      if lst = [] then 0.0
      else
        let sorted = List.sort compare lst in
        List.nth sorted (List.length sorted - 1) -. List.hd sorted

    let quartiles lst =
      let sorted = List.sort compare lst in
      let len = List.length sorted in
      if len < 4 then (0.0, 0.0, 0.0)
      else
        let q1 = List.nth sorted (len / 4) in
        let q2 = List.nth sorted (len / 2) in
        let q3 = List.nth sorted (3 * len / 4) in
        (q1, q2, q3)
  end

  (* Geometrie avec caracteres accentues *)
  module Geometrie = struct
    module Cercle = struct
      let perimetre rayon = 2.0 *. 3.14159 *. rayon
      let aire rayon = 3.14159 *. rayon *. rayon
      let diametre rayon = 2.0 *. rayon
    end

    module Triangle = struct
      let perimetre cote1 cote2 cote3 = cote1 +. cote2 +. cote3
      let aire base hauteur = 0.5 *. base *. hauteur
      let est_equilateral a b c = a = b && b = c
    end

    module Carre = struct
      let perimetre cote = 4.0 *. cote
      let aire cote = cote *. cote
      let diagonale cote = cote *. sqrt 2.0
    end
  end
end

(* ========== Functor Examples ========== *)

(* Functor: Module parameterized by another module *)
module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module type SORTABLE = sig
  type elem
  val sort : elem list -> elem list
end

module MakeSort (Ord : ORDERED) : SORTABLE with type elem = Ord.t = struct
  type elem = Ord.t

  let rec insert x = function
    | [] -> [x]
    | y :: ys ->
        if Ord.compare x y <= 0 then x :: y :: ys
        else y :: insert x ys

  let rec sort = function
    | [] -> []
    | x :: xs -> insert x (sort xs)
end

(* Create instances of the functor *)
module IntOrder = struct
  type t = int
  let compare = compare
end

module StringOrder = struct
  type t = string
  let compare = String.compare
end

module IntSort = MakeSort(IntOrder) (* TODO hierarchy navigation up from IntOrder skips MakeSort, it should visit it like it does for navigating down. *)
module StringSort = MakeSort(StringOrder)

(* Another functor for creating collections *)
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

module MakeSet (C : COMPARABLE) : SET with type elem = C.t = struct
  type elem = C.t
  type t = elem list

  let empty = []

  let rec add x = function
    | [] -> [x]
    | y :: ys ->
        match C.compare x y with
        | 0 -> y :: ys
        | n when n < 0 -> x :: y :: ys
        | _ -> y :: add x ys

  let rec mem x = function      (* TODO Navigating down from function goes to ] rather than [] *)
    | [] -> false
    | y :: ys ->
        match C.compare x y with
        | 0 -> true             (* TODO Navigating between match_case doesn't work here *)
        | n when n < 0 -> false
        | _ -> mem x ys

  let to_list s = s
end

module IntComparable = struct
  type t = int
  let compare = compare
  let to_string = string_of_int
end

module IntSet = MakeSet(IntComparable)

(* ========== First-Class Functions ========== *)

(* Functions that return functions *)
let make_adder n = fun x -> x + n

let make_multiplier n = fun x -> x * n

let make_power n = fun x ->
  let rec power acc = function
    | 0 -> acc
    | n -> power (acc * x) (n - 1)
  in power 1 n

(* Functions that take functions as arguments *)
let apply_twice f x = f (f x)

let compose f g x = f (g x)

let apply_n_times n f x =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (f acc) (n - 1)
  in aux x n

(* Higher-order functions *)
let map_pair f (x, y) = (f x, f y)

let apply_to_list fs x = List.map (fun f -> f x) fs

let filter_and_map pred f lst =
  List.map f (List.filter pred lst)

(* Fonctions de premiere classe avec caracteres francais *)
let creer_additionneur n = fun x -> x + n

let creer_multiplicateur n = fun x -> x * n

let appliquer_repete n f x =
  let rec aux acc = function
    | 0 -> acc
    | i -> aux (f acc) (i - 1)
  in aux x n

let composer_fonctions f g x = f (g x)

(* Fonctions d'ordre superieur *)
let transformer_paire f (x, y) = (f x, f y)

let filtrer_et_transformer predicat transformeur liste =
  List.map transformeur (List.filter predicat liste)

let appliquer_a_tous operation liste =
  List.map operation liste

let reduire combineur initial liste =
  List.fold_left combineur initial liste

(* ========== Partially Applied Functions ========== *)

(* Create partially applied functions *)
let add x y = x + y
let add_five = add 5
let add_ten = add 10

let multiply x y = x * y
let double = multiply 2
let triple = multiply 3
let quadruple = multiply 4

let power base exp =
  let rec pow acc = function
    | 0 -> acc
    | n -> pow (acc * base) (n - 1)
  in pow 1 exp

let square_via_power = power 2
let cube_via_power = power 3

(* More complex partial application *)
let fold_sum = List.fold_left (+) 0
let fold_product = List.fold_left ( * ) 1
let fold_max = List.fold_left max min_int

let map_square = List.map (fun x -> x * x)
let map_double = List.map (fun x -> x * 2)
let map_negate = List.map (fun x -> -x)

let filter_positive = List.filter (fun x -> x > 0)
let filter_even = List.filter (fun x -> x mod 2 = 0)
let filter_odd = List.filter (fun x -> x mod 2 <> 0)

(* ========== Anonymous Functions ========== *)

(* List of anonymous functions *)
let math_ops = [
  (fun x -> x + 1);
  (fun x -> x * 2);
  (fun x -> x * x);
  (fun x -> x - 10);
]

(* Using anonymous functions with higher-order functions *)
let transform_list lst =
  lst
  |> List.map (fun x -> x * 2)
  |> List.filter (fun x -> x > 10)
  |> List.map (fun x -> x + 5)

let process_pairs pairs =
  List.map (fun (x, y) -> x + y) pairs

let nested_functions x =
  (fun y -> (fun z -> x + y + z))

(* Anonymous functions in pattern matching *)
let apply_op op x y =
  match op with
  | "add" -> (fun a b -> a + b) x y
  | "mul" -> (fun a b -> a * b) x y
  | "sub" -> (fun a b -> a - b) x y
  | _ -> 0

(* ========== Combined Examples ========== *)

(* Combining modules, functors, and higher-order functions *)
module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module OptionMonad : MONAD with type 'a t = 'a option = struct
  type 'a t = 'a option
  let return x = Some x
  let bind m f =
    match m with
    | None -> None
    | Some x -> f x
end

module ListMonad : MONAD with type 'a t = 'a list = struct
  type 'a t = 'a list
  let return x = [x]
  let bind m f = List.concat (List.map f m)
end

(* Complex example combining all concepts *)
let pipeline_example () =
  let add_partial = (+) 10 in
  let multiply_partial = ( * ) 3 in

  let pipeline =
    [1; 2; 3; 4; 5]
    |> List.map add_partial
    |> List.filter (fun x -> x mod 2 = 0)
    |> List.map multiply_partial
    |> List.fold_left (+) 0
  in
  pipeline

(* ========== Extended Indexing Operators ========== *)

(* Custom indexing operators for different data structures *)
module CustomIndexing = struct
  (* Array-like indexing with .() *)
  type int_array = int array

  let ( .%() ) arr i = Array.get arr i
  let ( .%()<- ) arr i v = Array.set arr i v

  (* String indexing with .[] *)
  type text = string

  let (.%[]) str i = String.get str i
  let (.%[]<-) str i c =
    let bytes = Bytes.of_string str in
    Bytes.set bytes i c;
    Bytes.to_string bytes

  (* Map-like indexing with .{} *)
  type ('k, 'v) map = ('k * 'v) list

  let (.%{}) map key = List.assoc_opt key map
  let (.%{}<-) map key value = (key, value) :: List.remove_assoc key map


  (* Custom collection with .@() *)
  type 'a circular_buffer = {
    data: 'a array;
    size: int;
  }

  let (.@()) buf i =
    let actual_i = i mod buf.size in
    buf.data.(actual_i)
end

(* ========== Let Punning Examples ========== *)

(* Let punning allows omitting the pattern when it matches the expression *)
module LetPunning = struct
  (* Record type for demonstrating punning *)
  type person = {
    name: string;
    age: int;
    city: string;
  }

  type coordinates = {
    x: float;
    y: float;
    z: float;
  }

  (* Function using let punning in record construction *)
  let make_person name age city =
    { name; age; city }  (* equivalent to { name = name; age = age; city = city } *)

  let make_coords x y z =
    { x; y; z }

  (* Pattern matching with punning *)
  let get_person_info { name; age; city } =
    Printf.sprintf "%s is %d years old and lives in %s" name age city

  let distance_from_origin { x; y; z } =
    sqrt (x *. x +. y *. y +. z *. z)

  (* Nested record with punning *)
  type address = {
    street: string;
    number: int;
    postal_code: string;
  }

  type employee = {
    name: string;
    employee_id: int;
    address: address;
  }

  let make_address street number postal_code =
    { street; number; postal_code }

  let make_employee name employee_id address =
    { name; employee_id; address }

  (* Using punning in let bindings *)
  let process_employee emp =
    let { name; employee_id; address } = emp in
    let { street; number; postal_code } = address in
    Printf.sprintf "Employee %s (#%d) lives at %d %s, %s"
      name employee_id number street postal_code
end

(* ========== Monadic Bind (>>=) Examples ========== *)

(* Option monad with bind operator *)
module OptionOps = struct
  let (>>=) opt f =
    match opt with
    | None -> None
    | Some x -> f x

  let (>>|) opt f =
    match opt with
    | None -> None
    | Some x -> Some (f x)

  let return x = Some x

  (* Example: safe division chain *)
  let safe_div x y =
    if y = 0 then None else Some (x / y)

  let safe_sqrt x =
    if x < 0.0 then None else Some (sqrt x)

  let safe_log x =
    if x <= 0.0 then None else Some (log x)

  (* Chaining operations with >>= *)
  let complex_calculation x y z =
    safe_div x y >>= fun result1 ->
    safe_sqrt (float_of_int result1) >>= fun result2 ->
    safe_log result2 >>= fun result3 ->
    return (result3 *. float_of_int z)

  (* Using >>| for mapping *)
  let simple_calc x =
    Some x >>| (fun n -> n * 2) >>| (fun n -> n + 10)
end

(* Result monad with bind operator *)
module ResultOps = struct
  type ('a, 'e) result = ('a, 'e) Result.t

  let (>>=) res f =
    match res with
    | Error e -> Error e
    | Ok x -> f x

  let (>>|) res f =
    match res with
    | Error e -> Error e
    | Ok x -> Ok (f x)

  let return x = Ok x

  (* Example: parsing and validation *)
  let parse_int s =
    try Ok (int_of_string s)
    with Failure _ -> Error ("Failed to parse: " ^ s)

  let validate_positive x =
    if x > 0 then Ok x
    else Error "Number must be positive"

  let validate_range min max x =
    if x >= min && x <= max then Ok x
    else Error (Printf.sprintf "Number must be between %d and %d" min max)

  (* Chaining with >>= *)
  let parse_and_validate s =
    parse_int s >>= fun num ->
    validate_positive num >>= fun pos_num ->
    validate_range 1 100 pos_num >>= fun valid_num ->
    return (valid_num * 2)
end

(* List monad with bind operator *)
module ListOps = struct
  let (>>=) lst f = List.concat_map f lst
  let (>>|) lst f = List.map f lst
  let return x = [x]

  (* Example: non-deterministic computation *)
  let choices = [1; 2; 3]

  let computation =
    choices >>= fun x ->
    [x; x * 2] >>= fun y ->
    [y + 1; y + 2] >>= fun z ->
    return (x, y, z)

  (* Cartesian product using bind *)
  let cartesian_product xs ys =
    xs >>= fun x ->
    ys >>= fun y ->
    return (x, y)
end

(* State monad with bind operator *)
module StateOps = struct
  type ('s, 'a) state = 's -> ('a * 's)

  let (>>=) m f = fun s ->
    let (a, s') = m s in
    f a s'

  let return x = fun s -> (x, s)

  let get = fun s -> (s, s)
  let put s' = fun _ -> ((), s')
  let modify f = fun s -> ((), f s)

  let run_state m s = m s

  (* Example: counter *)
  let increment = modify ((+) 1)
  let decrement = modify (fun x -> x - 1)
  let get_count = get

  let counter_computation =
    increment >>= fun () ->
    increment >>= fun () ->
    get_count >>= fun count1 ->
    increment >>= fun () ->
    get_count >>= fun count2 ->
    return (count1, count2)
end

(* Custom monad: Writer for logging *)
module WriterOps = struct
  type ('w, 'a) writer = 'a * 'w list

  let (>>=) (a, log) f =
    let (b, log2) = f a in
    (b, log @ log2)

  let return x = (x, [])

  let tell msg = ((), [msg])

  let run_writer (a, log) = (a, log)

  (* Example: computation with logging *)
  let logged_add x y =
    tell (Printf.sprintf "Adding %d + %d" x y) >>= fun () ->
    return (x + y)

  let logged_multiply x y =
    tell (Printf.sprintf "Multiplying %d * %d" x y) >>= fun () ->
    return (x * y)

  let computation x y =
    logged_add x y >>= fun sum ->
    logged_multiply sum 2 >>= fun result ->
    tell "Computation finished" >>= fun () ->
    return result
end

(* Combining monadic operations with let* syntax (OCaml 4.08+) *)
module MonadicSyntax = struct
  (* Option monad *)
  module Option = struct
    let ( let* ) = OptionOps.(>>=)
    let ( and* ) opt1 opt2 =
      match opt1, opt2 with
      | Some x, Some y -> Some (x, y)
      | _ -> None

    let example x y =
      let* a = OptionOps.safe_div x y in
      let* b = OptionOps.safe_sqrt (float_of_int a) in
      Some (b *. 2.0)

    let parallel_example x y z =
      let* a = Some x
      and* b = Some y
      and* c = Some z in
      Some (a + b + c)
  end

  (* Result monad *)
  module Result = struct
    let ( let* ) = ResultOps.(>>=)

    let parse_and_process s =
      let* num = ResultOps.parse_int s in
      let* validated = ResultOps.validate_positive num in
      Ok (validated * 3)
  end

  (* List monad *)
  module List = struct
    let ( let* ) = ListOps.(>>=)

    let combinations =
      let* x = [1; 2; 3] in
      let* y = [10; 20] in
      [(x, y)]
  end
end

(* ========== Main Test Section ========== *)

let () =
  (* Test Positive module *)
  let p1 = Positive.make 5 in
  let p2 = Positive.make 10 in
  let sum = Positive.add p1 p2 in
  let _ = Positive.to_int sum in

  (* Test Math module *)
  let _ = Math.square 5 in
  let _ = Math.cube 3 in
  let _ = Math.Constants.pi in

  (* Test Stack module *)
  let s = Stack.empty in
  let s = Stack.push 1 s in
  let s = Stack.push 2 s in
  let s = Stack.push 3 s in
  let _ = Stack.size s in

  (* Test nested Collections modules *)
  let test_list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in
  let _ = Collections.List.Ops.take 5 test_list in
  let _ = Collections.List.Ops.drop 3 test_list in
  let (_first_half, _second_half) = Collections.List.Ops.split_at 5 test_list in

  let _ = Collections.List.Fold.sum test_list in
  let _ = Collections.List.Fold.product [1; 2; 3; 4] in
  let _ = Collections.List.Fold.max_elem test_list in

  let _ = Collections.List.Transform.squares test_list in
  let _ = Collections.List.Transform.cubes [1; 2; 3] in
  let _ = Collections.List.Transform.doubles test_list in

  (* Test Collections.Array nested modules *)
  let arr = Collections.Array.Create.range 0 10 in
  let _ = Collections.Array.Create.constant 5 42 in
  let _fib_arr = Collections.Array.Create.fibonacci 10 in

  let _ = Collections.Array.Stats.sum arr in
  let _ = Collections.Array.Stats.mean arr in
  let _ = Collections.Array.Stats.max arr in

  (* Test Collections.Tree nested modules *)
  let tree = Collections.Tree.Build.node 5
    (Collections.Tree.Build.node 3
      (Collections.Tree.Build.singleton 1)
      (Collections.Tree.Build.singleton 4))
    (Collections.Tree.Build.node 8
      (Collections.Tree.Build.singleton 6)
      (Collections.Tree.Build.singleton 10)) in

  let _ = Collections.Tree.Ops.size tree in
  let _ = Collections.Tree.Ops.height tree in
  let _doubled_tree = Collections.Tree.Ops.map (fun x -> x * 2) tree in
  let tree_sum = Collections.Tree.Ops.fold (+) 0 tree in

  (* Test DataStructures.Linear modules *)
  let q = DataStructures.Linear.Queue.empty in
  let q = DataStructures.Linear.Queue.enqueue 1 q in
  let q = DataStructures.Linear.Queue.enqueue 2 q in
  let q = DataStructures.Linear.Queue.enqueue 3 q in
  let _ = DataStructures.Linear.Queue.size q in
  let _result_q = DataStructures.Linear.Queue.dequeue q in

  let dq = DataStructures.Linear.Deque.empty in
  let dq = DataStructures.Linear.Deque.push_front 1 dq in
  let dq = DataStructures.Linear.Deque.push_back 2 dq in
  let dq = DataStructures.Linear.Deque.push_front 0 dq in
  let _ = DataStructures.Linear.Deque.pop_front dq in

  (* Test DataStructures.Associative modules *)
  let hmap = DataStructures.Associative.HashMap.create 10 in
  let hmap = DataStructures.Associative.HashMap.add "key1" 100 hmap in
  let hmap = DataStructures.Associative.HashMap.add "key2" 200 hmap in
  let _ = DataStructures.Associative.HashMap.find "key1" hmap in

  let trie = DataStructures.Associative.Trie.empty in
  let trie = DataStructures.Associative.Trie.insert ['h'; 'e'; 'l'; 'l'; 'o'] trie in
  let trie = DataStructures.Associative.Trie.insert ['h'; 'e'; 'l'; 'p'] trie in
  let _ = DataStructures.Associative.Trie.search ['h'; 'e'; 'l'; 'l'; 'o'] trie in
  let _ = DataStructures.Associative.Trie.search ['h'; 'i'] trie in

  (* Test Functional.Combinators modules *)
  let _ = Functional.Combinators.id 42 in
  let const_5 = Functional.Combinators.const 5 in
  let _ = const_5 "anything" in
  let flipped_sub = Functional.Combinators.flip (-) in
  let _ = flipped_sub 3 10 in

  let open Functional.Combinators.Compose in
  let add1 = (+) 1 in
  let mul2 = ( * ) 2 in
  let composed = add1 << mul2 in
  let _ = composed 5 in

  (* Test Functional.Curry *)
  let add_tuple (x, y) = x + y in
  let curried_add = Functional.Curry.curry add_tuple in
  let _ = curried_add 3 4 in

  let curried_mul x y = x * y in
  let uncurried_mul = Functional.Curry.uncurry curried_mul in
  let _ = uncurried_mul (5, 6) in

  (* Test Functional.Partial *)
  let add_fn x y = x + y in
  let add_10 = Functional.Partial.apply_first add_fn 10 in
  let _ = add_10 5 in

  (* Test Mathematics.Arithmetic modules *)
  let _ = Mathematics.Arithmetic.Basic.add 5 3 in
  let _ = Mathematics.Arithmetic.Basic.mul 4 7 in
  let _ = Mathematics.Arithmetic.Basic.div 10 3 in

  let _ = Mathematics.Arithmetic.Advanced.gcd 48 18 in
  let _ = Mathematics.Arithmetic.Advanced.lcm 12 15 in
  let _ = Mathematics.Arithmetic.Advanced.factorial 5 in
  let _ = Mathematics.Arithmetic.Advanced.fibonacci 10 in

  (* Test Mathematics.Algebra modules *)
  let poly = [1.0; 2.0; 3.0] in
  let _ = Mathematics.Algebra.Polynomial.eval poly 2.0 in
  let poly2 = [2.0; 3.0; 1.0] in
  let _ = Mathematics.Algebra.Polynomial.add poly poly2 in
  let _ = Mathematics.Algebra.Polynomial.scale 2.0 poly in

  let matrix = Mathematics.Algebra.Matrix.create 3 3 0.0 in
  Mathematics.Algebra.Matrix.set matrix 0 0 1.0;
  Mathematics.Algebra.Matrix.set matrix 1 1 2.0;
  let _ = Mathematics.Algebra.Matrix.get matrix 0 0 in
  let _ = Mathematics.Algebra.Matrix.dimensions matrix in

  (* Test Mathematics.Statistics modules *)
  let data = [1.0; 2.0; 3.0; 4.0; 5.0] in
  let _ = Mathematics.Statistics.Descriptive.mean data in
  let _ = Mathematics.Statistics.Descriptive.variance data in
  let _ = Mathematics.Statistics.Descriptive.std_dev data in

  let x_data = [1.0; 2.0; 3.0; 4.0; 5.0] in
  let y_data = [2.0; 4.0; 6.0; 8.0; 10.0] in
  let _ = Mathematics.Statistics.Correlation.covariance x_data y_data in
  let _ = Mathematics.Statistics.Correlation.correlation x_data y_data in

  (* Test French modules with Latin-1 characters *)
  let _ = Operations.creer 42 in
  let _ = Operations.detruire () in
  let _ = Operations.repeter 3 (fun x -> x + 1) 0 in

  let _ = Operations.Mathematiques.carre 5 in
  let _ = Operations.Mathematiques.racine_carree 16 in
  let _ = Operations.Mathematiques.perimetre_cercle 5.0 in
  let _ = Operations.Mathematiques.ete 20 in
  let _ = Operations.Mathematiques.hiver 30 in

  let _ = Operations.Chaines.longueur "bonjour" in
  let _ = Operations.Chaines.premiere_lettre "hello" in
  let _ = Operations.Chaines.derniere_lettre "world" in
  let _ = Operations.Chaines.resume "this is a very long string" in

  let _ = Francais.prenom in
  let _ = Francais.age in
  let _ = Francais.ville in
  let _ = Francais.Numeros.zero in
  let _ = Francais.Numeros.numero_prefere in

  let evt = Francais.Evenements.creer_evenement "Noel" 2024 "hiver" in
  let _ = Francais.Evenements.annee_evenement evt in
  let _ = Francais.Evenements.nom_evenement evt in

  (* Test French statistics functions *)
  let donnees = [5.0; 2.0; 8.0; 1.0; 9.0; 3.0; 7.0] in
  let _ = Mathematics.Statistiques.mediane donnees in
  let _ = Mathematics.Statistiques.etendue donnees in
  let (_q1, _q2, _q3) = Mathematics.Statistiques.quartiles donnees in

  (* Test French geometry functions *)
  let _ = Mathematics.Geometrie.Cercle.perimetre 5.0 in
  let _ = Mathematics.Geometrie.Cercle.aire 5.0 in
  let _ = Mathematics.Geometrie.Cercle.diametre 5.0 in

  let _ = Mathematics.Geometrie.Triangle.perimetre 3.0 4.0 5.0 in
  let _ = Mathematics.Geometrie.Triangle.aire 4.0 3.0 in
  let _ = Mathematics.Geometrie.Triangle.est_equilateral 5.0 5.0 5.0 in

  let _ = Mathematics.Geometrie.Carre.perimetre 4.0 in
  let _ = Mathematics.Geometrie.Carre.aire 4.0 in
  let _ = Mathematics.Geometrie.Carre.diagonale 4.0 in

  (* Test French first-class functions *)
  let additionneur = creer_additionneur 10 in
  let _ = additionneur 5 in

  let multiplicateur = creer_multiplicateur 3 in
  let _ = multiplicateur 7 in

  let _ = appliquer_repete 3 (fun x -> x * 2) 1 in
  let _ = composer_fonctions (fun x -> x * 2) (fun x -> x + 1) 5 in

  let _ = transformer_paire (fun x -> x * x) (3, 4) in
  let _ = filtrer_et_transformer (fun x -> x > 2) (fun x -> x * 2) [1; 2; 3; 4; 5] in
  let _ = appliquer_a_tous (fun x -> x + 1) [1; 2; 3] in
  let _ = reduire (+) 0 [1; 2; 3; 4; 5] in

  (* Test functors *)
  let int_list = [5; 2; 8; 1; 9] in
  let sorted_ints = IntSort.sort int_list in

  let str_list = ["zebra"; "apple"; "mango"; "banana"] in
  let _sorted_strs = StringSort.sort str_list in

  (* Test IntSet functor *)
  let set = IntSet.empty in
  let set = IntSet.add 5 set in
  let set = IntSet.add 2 set in
  let set = IntSet.add 8 set in
  let _ = IntSet.mem 5 set in

  (* Test first-class functions *)
  let adder = make_adder 10 in
  let _ = adder 5 in

  let multiplier = make_multiplier 3 in
  let _ = multiplier 7 in

  let _ = apply_twice (fun x -> x + 1) 5 in
  let _ = compose (fun x -> x * 2) (fun x -> x + 1) 5 in

  (* Test partially applied functions *)
  let _ = add_five 10 in
  let _ = double 7 in
  let _ = triple 4 in

  let numbers = [1; 2; 3; 4; 5] in
  let _ = fold_sum numbers in
  let _ = fold_product numbers in

  let _ = map_square numbers in
  let _ = filter_positive [-2; -1; 0; 1; 2] in
  let _ = filter_even [1; 2; 3; 4; 5; 6] in

  (* Test anonymous functions *)
  let _ = List.map (fun x -> x * x) numbers in
  let _ = List.filter (fun x -> x > 3) numbers in
  let _ = transform_list [1; 2; 3; 4; 5; 6; 7; 8] in

  let pairs = [(1, 2); (3, 4); (5, 6)] in
  let _ = process_pairs pairs in

  let curried = nested_functions 1 in
  let _ = curried 2 3 in

  (* Test combined example *)
  let result = pipeline_example () in

  (* Final computation combining everything *)
  let final_result =
    sorted_ints
    |> List.map (fun x -> x * 2)
    |> List.filter (fun x -> x > 5)
    |> List.fold_left (+) result
    |> (+) tree_sum
  in

  Printf.printf "Final result: %d\n" final_result;

  (* ========== Test Extended Indexing Operators ========== *)

  (* Test array indexing with .%() *)
  let arr = [| 10; 20; 30; 40; 50 |] in
  let open CustomIndexing in
  let _ = arr.%(2) in
  arr.%(1) <- 99;
  let _ = arr.(1) in

  (* Test string indexing with .%[] *)
  let str = "hello" in
  let _ = str.%[0] in
  let _ = str.%[0] <- 'H' in

  (* Test map indexing with .%{} *)
  let map = [("a", 1); ("b", 2); ("c", 3)] in
  let _ = map.%{"b"} in
  let map2 = map.%{"d"} <- 4 in
  let _ = map2.%{"d"} in

  (* Test circular buffer indexing with .@() *)
  let cbuf = { data = [| 1; 2; 3; 4; 5 |]; size = 5 } in
  let _ = cbuf.@(7) in  (* wraps around to index 2 *)
  let _ = cbuf.@(12) in  (* wraps around to index 2 *)

  (* ========== Test Let Punning ========== *)

  (* Test basic record punning *)
  let name = "Alice" in
  let age = 30 in
  let city = "Paris" in
  let person = LetPunning.make_person name age city in
  let _ = LetPunning.get_person_info person in

  (* Test coordinate punning *)
  let x = 3.0 in
  let y = 4.0 in
  let z = 0.0 in
  let coords = LetPunning.make_coords x y z in
  let _ = LetPunning.distance_from_origin coords in

  (* Test nested record punning *)
  let street = "Main St" in
  let number = 123 in
  let postal_code = "12345" in
  let addr = LetPunning.make_address street number postal_code in

  let employee_id = 42 in
  let emp = LetPunning.make_employee name employee_id addr in
  let _ = LetPunning.process_employee emp in

  (* Test pattern matching with punning *)
  let LetPunning.{ name = pname; age = page; city = _ } = person in
  let _ = Printf.sprintf "%s is %d" pname page in

  (* ========== Test Monadic Bind (>>=) ========== *)

  (* Test Option monad *)
  let opt_result = OptionOps.complex_calculation 100 5 3 in
  let _ = match opt_result with
    | Some v -> Printf.sprintf "Result: %f" v
    | None -> "Failed" in

  let simple_opt = OptionOps.simple_calc 5 in
  let _ = simple_opt in

  (* Test Option monad failure case *)
  let opt_fail = OptionOps.safe_div 10 0 in
  let _ = opt_fail in

  (* Test Result monad *)
  let res_ok = ResultOps.parse_and_validate "42" in
  let _ = match res_ok with
    | Ok v -> Printf.sprintf "Validated: %d" v
    | Error e -> e in

  let res_err = ResultOps.parse_and_validate "150" in
  let _ = match res_err with
    | Ok v -> Printf.sprintf "Value: %d" v
    | Error e -> e in

  let res_parse_err = ResultOps.parse_and_validate "not-a-number" in
  let _ = res_parse_err in

  (* Test List monad *)
  let list_result = ListOps.computation in
  let _ = List.length list_result in

  let cartesian = ListOps.cartesian_product [1; 2; 3] ['a'; 'b'] in
  let _ = List.length cartesian in

  (* Test State monad *)
  let (count_result, final_state) = StateOps.run_state StateOps.counter_computation 0 in
  let _ = Printf.sprintf "Counts: (%d, %d), Final state: %d"
    (fst count_result) (snd count_result) final_state in

  (* Test Writer monad *)
  let (writer_result, log) = WriterOps.run_writer (WriterOps.computation 5 3) in
  let _ = Printf.sprintf "Result: %d" writer_result in
  let _ = String.concat "; " log in

  (* Test let* syntax *)
  let monadic_opt = MonadicSyntax.Option.example 20 4 in
  let _ = monadic_opt in

  let parallel_opt = MonadicSyntax.Option.parallel_example 1 2 3 in
  let _ = parallel_opt in

  let monadic_result = MonadicSyntax.Result.parse_and_process "25" in
  let _ = monadic_result in

  let monadic_list = MonadicSyntax.List.combinations in
  let _ = List.length monadic_list in

  Printf.printf "All extended examples tested successfully!\n"
