type value = Bool of bool | Integer of int | String of string | Nil

let string_of_value = function
  | Bool v -> string_of_bool v
  | Integer v -> string_of_int v
  | String v -> v
  | Nil -> "nil"

type ast =
  | Literal of value
  | Var of string
  | VarAssign of string * ast
  | Conditional of ast * ast * ast
  | Binop of (value -> value -> value) * ast * ast
  | Seq of ast list

let typefail msg = failwith ("TypeError: " ^ msg)