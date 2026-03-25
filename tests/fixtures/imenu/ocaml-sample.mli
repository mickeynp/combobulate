(* Test OCaml interface imenu *)

val my_function : int -> int

val another_value : int

type my_type =
  | Foo
  | Bar of int

module MyModule : sig
  val inner_function : int -> int
end

class my_class :
  object
    method get_value : int
  end

exception MyException of string

module type MyModuleType = sig
  type t
  val operation : string -> string
  val double : int -> int
end
