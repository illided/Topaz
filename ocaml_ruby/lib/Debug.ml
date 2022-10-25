open Types

let quoted s = "\"" ^ s ^ "\""

let pairs_to_json (p : (string * string) list) : string =
  List.map (fun (n, c) -> "\"" ^ n ^ "\": " ^ c) p |> String.concat ","
  |> fun s -> "{" ^ s ^ "}"

let wrap_with_name (n : string) (c : string) : string = pairs_to_json [ (n, c) ]
let list_to_json (l : string list) : string = "[" ^ String.concat ", " l ^ "]"

let tree_to_json (max_n : int) (t : ast) =
  let rec helper s_t i =
    if i >= max_n then quoted "<DEPTH_LIMIT>"
    else
      match s_t with
      | Literal v -> wrap_with_name "Literal" (quoted (string_of_value v))
      | Var n -> wrap_with_name "Variable" (quoted n)
      | VarAssign (n, v) ->
          pairs_to_json [ ("Name", quoted n); ("Value", helper v (i + 1)) ]
          |> wrap_with_name "VarAssn"
      | Binop (_, l, r) ->
          pairs_to_json
            [ ("Left", helper l (i + 1)); ("Right", helper r (i + 1)) ]
          |> wrap_with_name "Binop"
      | Conditional (c, t, e) ->
          pairs_to_json
            [
              ("Condition", helper c (i + 1));
              ("Then", helper t (i + 1));
              ("Else", helper e (i + 1));
            ]
          |> wrap_with_name "Conditional"
      | Seq t_lst ->
          let seq_as_list =
            List.map (fun ex -> helper ex (i + 1)) t_lst |> list_to_json
          in
          wrap_with_name "Seq" seq_as_list
      | WhileLoop (cond, body) ->
          pairs_to_json
            [
              ("Condition", helper cond (i + 1)); ("Body", helper body (i + 1));
            ]
          |> wrap_with_name "WhileLoop"
      | ArrayDecl lst ->
          let lst_as_json =
            List.map (fun el -> helper el (i + 1)) lst |> list_to_json
          in
          wrap_with_name "ArrayDecl" lst_as_json
  in
  helper t 0
