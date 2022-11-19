type ruby_literal = BoolL | IntegerL | StringL | NilL

(** Ast types used in parsing*)
type ast =
  | Literal of ruby_literal * string
      (** Literal [literal_type literal_as_string] *)
  | ArrayDecl of ast list  (** ArrayDecl [initial_content] *)
  | Var of string  (** Var [var_name] *)
  | VarAssign of string * ast  (** VarAssign [var_name new_value] *)
  | Conditional of ast * ast * ast
      (** Conditional [condition then_branch else_branch] *)
  | WhileLoop of ast * ast  (** WhileLoop [condition body] *)
  | Binop of string * ast * ast  (** Binop [op left right] *)
  | Seq of ast list  (** Seq [expressions] *)
  | Indexing of ast * ast  (** Indexing [box index] *)
  | FuncDeclaration of string * string list * ast
      (** FunctionDeclaration [name param_names body]*)
  | Invocation of ast * ast list  (** Invocation [name param_values] *)

(** Data types used in runtime *)
type value =
  | Bool of bool  (** Bool [value]*)
  | Integer of int  (** Integer [value]*)
  | String of string  (** String [value]*)
  | Array of value list  (** Array [value_list]*)
  | Function of string * string list * (value list -> value)
      (** Function [name param_list body]*)
  | Nil

let rec string_of_value = function
  | Bool v -> string_of_bool v
  | Integer v -> string_of_int v
  | String v -> v
  | Nil -> "nil"
  | Array l -> "[" ^ (List.map string_of_value l |> String.concat ", ") ^ "]"
  | Function (name, params, _) -> name ^ "(" ^ String.concat ", " params ^ ")"

let value_of_literal (lit_t : ruby_literal) (s : string) =
  match lit_t with
  | BoolL -> Bool (bool_of_string s)
  | IntegerL -> Integer (int_of_string s)
  | StringL -> String s
  | NilL -> Nil

let typefail msg = failwith ("TypeError: " ^ msg)
