(* Test OCaml imenu *)

let my_function x = x + 1

let another_value = 42

type my_type =
  | Foo
  | Bar of int

module MyModule = struct
  let inner_function y = y * 2
end

class my_class =
  object
    method get_value = 10
  end

exception MyException of string

external my_external : int -> int = "native_function"
