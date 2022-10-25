type value =
  | Bool of bool
  | Integer of int
  | String of string
  | Array of value list
  | Nil

let rec string_of_value = function
  | Bool v -> string_of_bool v
  | Integer v -> string_of_int v
  | String v -> v
  | Nil -> "nil"
  | Array l -> "[" ^ (List.map string_of_value l |> String.concat ", ") ^ "]"

type ast =
  | Literal of value
  | ArrayDecl of ast list
  | Var of string
  | VarAssign of string * ast
  | Conditional of ast * ast * ast
  | WhileLoop of ast * ast
  | Binop of (value -> value -> value) * ast * ast
  | Seq of ast list

let typefail msg = failwith ("TypeError: " ^ msg)
