(* -*- combobulate-test-point-overlays: ((1 outline 361) (2 outline 375) (3 outline 400) (4 outline 440) (5 outline 483) (6 outline 548) (7 outline 601) (8 outline 712) (9 outline 800) (10 outline 879) (11 outline 988) (12 outline 1007) (13 outline 1079) (14 outline 1122) (15 outline 1149) (16 outline 1204)); eval: (combobulate-test-fixture-mode t); -*- *)

type user_id

type file_handle = int 

type 'a cache = ('a, float) Hashtbl.t 

type 'a internal_state = private 'a list 

type ('a, 'b) constrained_pair = 'a * 'b constraint 'a = string

type http_status = Success | NotFound | ServerError

type expression =
  | Const of int
  | Add of expression * expression
  | Multiply of expression * expression

type ui_event =
  | MouseClick of { x: int; y: int }
  | KeyPress of { key_code: int }

type _ value =
  | Int : int -> int value
  | String : string -> string value

type user_profile = {
  id: user_id;
  name: string;
  email: string option;
  mutable last_login: float;
}

type command = ..

type command +=
  | Login of { user: string; pass: string }
  | Logout

type command +=
  | SendMessage of string

exception Timeout_expired

exception Api_error of { code: int; message: string }

exception Old_error_name = Not_found
