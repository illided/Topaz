type ruby_literal =
  | BoolL
  | IntegerL
  | StringL
  | NilL

type ast =
  | Literal of ruby_literal * string
  | ArrayDecl of ast list
  | Var of string
  | VarAssign of string * ast
  | Conditional of ast * ast * ast
  | WhileLoop of ast * ast
  | Binop of string * ast * ast
  | Seq of ast list
  | Indexing of ast * ast

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

let value_of_literal (lit_t: ruby_literal) (s: string) = 
  match lit_t with
  | BoolL -> Bool (bool_of_string s)
  | IntegerL -> Integer (int_of_string s)
  | StringL -> String (s)
  | NilL -> Nil

let typefail msg = failwith ("TypeError: " ^ msg)
