open Angstrom
open Lexer
open Types
open BuiltinOps

let integer_v = integer_t >>| fun i -> Integer (int_of_string i)
let true_v = true_t *> return (Bool true)
let false_v = false_t *> return (Bool false)
let bool_v = true_v <|> false_v
let string_v = ruby_string >>| fun s -> String s
let literal = choice [ integer_v; bool_v; string_v ] >>| fun l -> Literal l
let wr_binop p = p >>| fun op l r -> Binop (match_binop op, l, r)
let binop_v = wr_binop binops
let asoc0 = wr_binop asoc0_t
let asoc1 = wr_binop asoc1_t
let asoc2 = wr_binop asoc2_t
let var_cal = identifier_t >>| fun s -> Var s

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init

let parens p = token "(" *> p <* token ")"
let maybe p = p <|> return ""

let seq_of_expr =
  fix (fun seq_of_expr ->
      let expr =
        fix (fun expr ->
            (* --- Var assn --- *)
            let assn =
              identifier_t >>= fun i ->
              token "=" *> expr >>| fun var_val -> VarAssign (i, var_val)
            in
            (* --- While --- *)
            let while_loop =
              token "while" *> expr >>= fun cond ->
              maybe (token "do") *> seq_of_expr <* token "end" >>= fun body ->
              return (WhileLoop (cond, body))
            in
            (* --- Conditional ---*)
            let conditional =
              token "if" *> maybe new_lines *> expr
              <* maybe new_lines
              >>= (fun condition ->
                    token "then" *> maybe new_lines *> seq_of_expr
                    <* maybe new_lines
                    >>= fun thenB ->
                    option
                      (Conditional (condition, thenB, Literal Nil))
                      ( string "else" *> maybe new_lines *> seq_of_expr
                      <* maybe new_lines
                      >>= fun elseB ->
                        return (Conditional (condition, thenB, elseB)) ))
              <* token "end"
            in
            (* --- Array declaration --- *)
            let array_t = token "[" *> sep_by (token ",") expr <* token "]" in
            let array_v = array_t >>| fun arr -> ArrayDecl arr in
            (* --- Indexing --- *)
            let index_p =
              choice [var_cal; (string_v >>| fun s -> Literal s); array_v; parens expr]
              >>= fun box ->
              (token "[" *> expr <* token "]") >>| fun ind -> Indexing (box, ind)
            in
            (* --- Binops ---*)
            let factor =
              parens expr <|> literal <|> var_cal <|> conditional <|> array_v
            in
            let asoc0_p = chainl1 factor asoc0 in
            let asoc1_p = chainl1 asoc0_p asoc1 in
            let asoc2_p = chainl1 asoc1_p asoc2 in
            (* --- Expr definition --- *)
            choice [index_p; assn; asoc2_p; parens expr; while_loop; array_v])
      in
      maybe new_lines *> sep_by expr_separator expr <* maybe new_lines
      >>| fun s -> Seq s)

let build (str : string) : ast =
  match parse_string ~consume:All seq_of_expr str with
  | Ok v -> v
  | Error msg -> failwith msg
