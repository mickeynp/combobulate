(** An abstract user identifier — representation hidden from callers.  -*- combobulate-test-point-overlays: ((1 outline 433) (2 outline 495) (3 outline 577) (4 outline 745) (5 outline 859) (6 outline 959) (7 outline 1064) (8 outline 1212) (9 outline 1335) (10 outline 1473) (11 outline 1631) (12 outline 1682) (13 outline 1783) (14 outline 1882) (15 outline 1972) (16 outline 2084)); eval: (combobulate-test-fixture-mode t); -*- *)
type user_id

(** A file handle represented as an integer. *)
type file_handle = int

(** A cache mapping values of type ['a] to timestamps. *)
type 'a cache = ('a, float) Hashtbl.t

(** An internal state that is a private list of ['a].
    Callers can inspect but not construct values of this type directly. *)
type 'a internal_state = private 'a list

(** A pair constrained so that the first element must be a [string]. *)
type ('a, 'b) constrained_pair = 'a * 'b constraint 'a = string

(** HTTP response status codes. *)
type http_status =
  | Success
  | NotFound
  | ServerError

(** A simple arithmetic expression tree. *)
type expression =
  | Const of int
  | Add of expression * expression
  | Multiply of expression * expression

(** A UI event with named fields. *)
type ui_event =
  | MouseClick of { x : int; y : int }
  | KeyPress of { key_code : int }

(** A GADT for typed values. *)
type _ value =
  | Int : int -> int value
  | String : string -> string value

(** A user profile with a mutable last login timestamp. *)
type user_profile = {
  id : user_id;
  name : string;
  email : string option;
  mutable last_login : float;
}

(** An open type for extensible commands. *)
type command = ..

(** Authentication commands. *)
type command +=
  | Login of { user : string; pass : string }
  | Logout

(** Messaging commands. *)
type command +=
  | SendMessage of string

(** Raised when an operation exceeds its time limit. *)
exception Timeout_expired

(** Raised when an API call fails, with a code and message. *)
exception Api_error of {
  code : int;
  message : string;
}

(** An alias for [Not_found] under a new name. *)
exception Old_error_name = Not_found
