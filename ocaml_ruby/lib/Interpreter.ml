open Types
open BuiltinOps
open Environment

let rec eval (st : State.storage) (code : ast) : value * State.storage =
  match code with
  | Literal v -> (v, st)
  | Var n -> (State.get_variable st n, st)
  | VarAssign (i, v) ->
      let var_value, st = eval st v in
      let new_state = State.set_variable st i var_value in
      (var_value, new_state)
  | Binop (op, l, r) ->
      let l_v, st = eval st l in
      let r_v, st = eval st r in
      (op l_v r_v, st)
  | Conditional (cond, thenB, elseB) ->
      let cond_v, st = eval st cond in
      eval st (conditional cond_v thenB elseB)
  | Seq lst ->
      List.fold_left
        (fun (_, betw_exp_st) el -> eval betw_exp_st el)
        (Nil, st) lst
  | WhileLoop (cond, body) ->
      let rec iteration s =
        let c_v, n_st = eval s cond in
        match c_v with
        | Bool v when v ->
            let _, n_st = eval n_st body in
            iteration n_st
        | Bool v when not v -> n_st
        | _ -> typefail "While loop expected bool as condition"
      in
      (Nil, iteration st)
  | ArrayDecl lst ->
      let values, new_st =
        List.fold_left
          (fun (acc, betw_exp_st) el ->
            match eval betw_exp_st el with s_v, s_s -> (s_v :: acc, s_s))
          ([], st) lst
      in
      (Array (List.rev values), new_st)

let run (code : ast) = fst (eval (State.create ()) code)
