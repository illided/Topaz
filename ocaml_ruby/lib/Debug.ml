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
      | Literal (_, v) -> wrap_with_name "Literal" (quoted v)
      | Var n -> wrap_with_name "Variable" (quoted n)
      | VarAssign (n, v) ->
          pairs_to_json [ ("Name", quoted n); ("Value", helper v (i + 1)) ]
          |> wrap_with_name "VarAssn"
      | Binop (op, l, r) ->
          pairs_to_json
            [
              ("Op", quoted op);
              ("Left", helper l (i + 1));
              ("Right", helper r (i + 1));
            ]
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
      | Indexing (box, ind) ->
          pairs_to_json
            [ ("Box", helper box (i + 1)); ("Index", helper ind (i + 1)) ]
          |> wrap_with_name "Indexing"
      | FuncDeclaration (name, params, body) ->
          pairs_to_json
            [
              ("Name", quoted name);
              ("Params", params |> List.map quoted |> list_to_json);
              ("Body", helper body (i + 1));
            ]
          |> wrap_with_name "FuncDeclaration"
      | Invocation (inv_box, params) ->
          pairs_to_json
            [
              ("InvBox", helper inv_box (i + 1));
              ( "Params",
                params |> List.map (fun a -> helper a (i + 1)) |> list_to_json
              );
            ]
  in

  helper t 0
