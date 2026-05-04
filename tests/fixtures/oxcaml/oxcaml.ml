(* ============================================================
   OxCaml sample code
   Demonstrates: stack allocation, unboxed types, modes,
   parallelism, comprehensions, uniqueness, templates, misc.
   ============================================================ *)
open! Core

[@@@warning "-32-34-69"]

(* ----------------------------------------------------------
   1. STACK ALLOCATION
   Values marked local live on the stack — no GC pressure.
   ---------------------------------------------------------- *)

(* local parameter — caller may pass stack-allocated values;
   the function promises not to store the argument anywhere *)
let use_temp ~(f : int ref @ local -> 'a) : 'a =
  let tmp @ local = stack_ (ref 0) in
  f tmp [@nontail]

(* local-returning function — result lives in the caller's region *)
let make_pair a b = exclave_ stack_ (a, b)

(* global_ field — always heap-allocated even if the record is on the stack *)
type 'a wrapper = { global_ value : 'a; tag : int }

(* ----------------------------------------------------------
   2. UNBOXED TYPES
   float#, int64#, etc. are stored without pointers.
   Mixed records combine boxed and unboxed fields.
   ---------------------------------------------------------- *)

(* unboxed float — no allocation, passed in FP register *)

type t : float32 = float32#

let fast_square (x : t) : t = Float32_u.mul x x

(* unboxed constants use a # prefix *)
let pi : float# = #3.14159265358979
let big : int64# = #9_000_000_000L

(* mixed record — label/count are boxed, x/y are unboxed float# *)
type point = {
  label : string;
  count : int;
  x : float#; (* stored flat — no pointer *)
  y : float#;
}

let distance (a : point) (b : point) : float# =
  let dx = Float_u.sub a.x b.x in
  let dy = Float_u.sub a.y b.y in
  Float_u.sqrt (Float_u.add (Float_u.( * ) dx dx) (Float_u.( * ) dy dy))

(* unboxed tuple — #(...) syntax, elements passed in separate registers *)
let sincos (theta : float#) : #(float# * float#) =
  #(Float_u.sin theta, Float_u.cos theta)

(* array of unboxed elements — packed, no pointers *)
let zeros : float# array = [| #0.0; #0.0; #0.0 |]

(* ----------------------------------------------------------
   3. MODES
   Modes are deep properties tracked alongside types.
   portable/contended enforce data-race freedom.
   unique enforces single ownership.
   ---------------------------------------------------------- *)

(* In a .mli you can write [@@ portable] at the top to make
   every val in the file portable by default.

   val name   : t @ contended -> string  @@ portable
     — safe because name reads an immutable field
   val rename : t -> string -> unit      @@ portable
     — requires uncontended (default), so no parallel access
*)

module Arena : sig
  type t

  val create : unit -> t @ unique
  val free : t @ unique -> unit (* consumes — no use after this *)
end = struct
  type t = { mutable buf : bytes }

  let create () = { buf = Bytes.create 4096 }
  let free _t = ()
end

let arena_example () =
  let arena @ unique = Arena.create () in
  Arena.free arena

(* ----------------------------------------------------------
   4. KINDS
   ---------------------------------------------------------- *)

module T : sig @@ portable
  type ('a : value mod contended portable) t : value mod contended portable
  type 'a r : immutable_data with 'a

  val create : unit -> 'a t
  val pop : 'a t -> 'a option
  val push : 'a t -> 'a -> unit
  val is_empty : 'a t -> 'a r
  val equal_to : 'a t -> 'a r -> bool
  val size : 'a t -> int
end = struct
  type ('a : value mod contended portable) t = 'a list Atomic.t
  type 'a r = Empty | Not_empty

  let create (type a : value mod contended portable) () : a t = Atomic.make []

  let pop (stack : ('a : value mod contended portable) t) : 'a option =
    let rec loop () =
      let xs = Atomic.get stack in
      match xs with
      | [] -> None
      | x :: xs' ->
          if
            phys_equal
              (Atomic.compare_exchange stack ~if_phys_equal_to:xs
                 ~replace_with:xs')
              xs
          then Some x
          else loop ()
    in
    loop ()

  let push : type (a : value mod contended portable). a t -> a -> unit =
   fun stack x ->
    let rec loop () =
      let xs = Atomic.get stack in
      let xs' = x :: xs in
      if
        phys_equal
          (Atomic.compare_exchange stack ~if_phys_equal_to:xs ~replace_with:xs')
          xs
      then ()
      else loop ()
    in
    loop ()

  let is_empty t = match Atomic.get t with [] -> Empty | _ :: _ -> Not_empty

  let equal_to (type (a : value mod contended portable) b) (t : a t) (r : b r) :
      bool =
    match (Atomic.get t, r) with
    | [], Empty -> true
    | _ :: _, Not_empty -> true
    | _ -> false

  type sint = int

  let size t : sint as (_ : immediate) = List.length (Atomic.get t)
end

(* ----------------------------------------------------------
   5. COMPREHENSIONS
   List and array comprehensions with range iterators,
   parallel binding (and), and when-guards.
   ---------------------------------------------------------- *)

(* Pythagorean triples up to n *)
let triples n =
  [
    (a, b, c)
    for a = 1 to n
    for b = a to n
    for c = b to n
    when (a * a) + (b * b) = c * c
  ]

(* Cartesian product — parallel 'and' enables fixed-size pre-allocation *)
let combos =
  [|
    Printf.sprintf "%s %s" color shape
    for color in [| "red"; "blue"; "green" |]
    and shape in [| "circle"; "square" |]
  |]
(* → [|"red circle";"blue circle";"green circle";
        "red square";"blue square";"green square"|] *)

(* flatten nested arrays *)
let flatten (xss : 'a array array) : 'a array =
  [| x for xs in xss for x in xs |]

(* squares of even numbers up to n *)
let squares_of_evens n = [ x * x for x = 1 to n when x mod 2 = 0 ]

(* reciprocals in descending order, skipping multiples of 3 *)
let reciprocals =
  [ 1.0 /. Float.of_int x for x = 10 downto 1 when x mod 3 <> 0 ]

(* immutable array comprehension — [: :] syntax produces iarray *)
let even_iarray : int iarray = [: x for x = 1 to 20 when x mod 2 = 0 :]

(* ----------------------------------------------------------
   7. TEMPLATES (ppx_template)
   Generates monomorphized copies for each mode/kind combo.
   ---------------------------------------------------------- *)

(* identity polymorphic over locality mode *)
let%template[@mode m = (global, local)] id : 'a. 'a @ m -> 'a @ m = fun x -> x
(* generates:
     val id        : 'a -> 'a
     val id__local : 'a @ local -> 'a @ local *)

(* kind-polymorphic identity — works for value and unboxed pairs *)
let%template[@kind k = (value, value & value)] id_any : ('a : k). 'a -> 'a =
 fun x -> x

(* portable functor — generates portable and nonportable versions *)
module%template.portable Make (S : Comparable.S) = struct
  let min a b = if S.compare a b <= 0 then a else b
  let max a b = if S.compare a b >= 0 then a else b
end

(* floating default — applies the mode to all items in the block *)
module type List_ops = sig
  [%%template:
  [@@@mode.default m = (global, local)]

  val first : 'a list @ m -> 'a option @ m
  val second : 'a list @ m -> 'a option @ m]
end

(* ----------------------------------------------------------
   8. MISCELLANEOUS EXTENSIONS
   ---------------------------------------------------------- *)

(* labeled tuples — names are part of the type *)
let origin : x:float * y:float * z:float = (~x:0.0, ~y:0.0, ~z:0.0)

let translate ~dx ~dy ~dz (~x:ox, ~y:oy, ~z:oz) =
  (~x:(ox +. dx), ~y:(oy +. dy), ~z:(oz +. dz))

(* immutable arrays (iarray) — safe to read from contended values *)
let primes : int iarray = Iarray.of_list [ 2; 3; 5; 7; 11; 13 ]
let prime_sum = Iarray.fold primes ~init:0 ~f:( + ) (* = 41 *)

(* zero_alloc — static proof that this function never heap-allocates *)
let[@zero_alloc] fast_clamp (x : int) ~lo ~hi =
  if x < lo then lo else if x > hi then hi else x

(* or_null — unboxed option, no pointer overhead vs. 'a option *)
let safe_div (a : int) (b : int) : int or_null =
  if b = 0 then Null else This (a / b)

let () =
  match safe_div 10 3 with
  | Null -> print_endline "division by zero"
  | This n -> Printf.printf "result: %d\n" n

module type S = sig
  module type S = sig
    type t
  end

  module M : S

  (* module strengthening extension *)
  module type SS = S with M
end

(* small number extension *)
let a : float32 = 1.0s
let a_unboxed : float32# = #1.0s
let b : int16 = 42S
let b_unboxed : int16# = #42S
let c : int8 = 42s
let c_unboxed : int8# = #42s
let d_unboxed : char# = #'a'
let e_unboxed : char# = #'\123'
let f_unboxed : char# = #'\o123'