open Types

let tree_to_json (t : ast) (max_n : int) =
  let rec helper s_t i =
    if i >= max_n then "\"<DEPTH_LIMIT>\""
    else
      match s_t with
      | Literal v -> "{\"Literal\": \"" ^ string_of_value v ^ "\"}"
      | Var n -> "{\"Variable\": \"" ^ n ^ "\'}"
      | VarAssign (n, v) ->
          "{\"VarAssn\": {" ^ "\"Name\": \"" ^ n ^ "\", \"Value\": "
          ^ helper v (i + 1)
          ^ "}}"
      | Binop (_, l, r) ->
          "{\"Binop\": {\"left\": "
          ^ helper l (i + 1)
          ^ ", \"right\": "
          ^ helper r (i + 1)
          ^ "}}"
      | Conditional (c, t, e) ->
          "{\"Conditional\": {\"Condition\": "
          ^ helper c (i + 1)
          ^ ", \"Then\": "
          ^ helper t (i + 1)
          ^ ", \"Else\": "
          ^ helper e (i + 1)
          ^ "}}"
      | Seq t_lst ->
          "{\"Seq\": ["
          ^ String.concat ", " (List.map (fun ex -> helper ex (i + 1)) t_lst)
          ^ "]}"
  in
  helper t 0
