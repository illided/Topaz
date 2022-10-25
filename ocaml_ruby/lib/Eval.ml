let run_expr s = s |> Parser.build |> Interpreter.run |> Types.string_of_value
let test_eval prog exp = String.equal (run_expr prog) exp

let%test "integer" = test_eval "123" "123"
let%test "plus" = test_eval "1 + 1" "2"
let%test "minus" = test_eval "1 - 1" "0"
let%test "multiply" = test_eval "4 * 4" "16"
let%test "division" = test_eval "42 / 6" "7"
let%test "binop ws around" = test_eval " 1 + 1 " "2"
(* let%test "binop new lines around" = test_eval "\n1+1\n" "2" *)
let%test "multiple binops" = test_eval "1 + 2 * 3" "7"
let%test "binops with brackets" = test_eval "(1 + 4) * (2 + 3) / 5" "5"
let%test "bool and" = test_eval "true && false" "false"
let%test "bool or" = test_eval "true || false" "true"
let%test "int comp eq" = test_eval "6 == 7" "false"
let%test "int comp neq" = test_eval "6 != 7" "true"
let%test "simple string" = test_eval "\"hello\"" "hello"
let%test "repeated string" = test_eval "\"hello\"*3" "hellohellohello"
let%test "string comparison" = test_eval "\"hello\" == \"hello\"" "true"
let%test "simple conditional" = test_eval "if true then 10 else 7 end" "10"

let%test "expr in condition" =
  test_eval "if 10 + 3 == 13 then 10 else 7 end" "10"

let%test "no else branch in conditional" =
  test_eval "if false then 10 end" "nil"

let%test "multiple expr sep by newline" = test_eval "1 + 1\n2 + 2\n3 + 3" "6"
let%test "multiple expr sep by semicolumn" = test_eval "1 + 1;2 + 2;3 + 3" "6"
let%test "multiple expr with random ws" = test_eval "1 + 1; 2 + 2\n 5 + 5" "10"

let%test "conditional with multuple expressions" =
  test_eval "if true then 1 + 2; 2 + 3; 5 + 5 end" "10"

let%test "sum of conditionals" =
  test_eval "if true then 10 end + if true then 5 end" "15"

let%test "condition with gr and nl" = test_eval "if 3 > 2 then 6 end" "6"
let%test "variable assign" = test_eval "u = 2 + 2" "4"
let%test "variable assign and call" = test_eval "x = 10; 2 + 2; x" "10"
let%test "multiple variables and call" = test_eval "x = 10; y = 7; x + y" "17"
let%test "bool variables" = test_eval "x = true; y = false; x && y" "false"

let%test "string variables" =
  test_eval "x = \"hello \"; y = \"world\"; x + y" "hello world"

let%test "variable from condition" =
  test_eval "x = false; y = if x then 13 else 10 end; y" "10"
