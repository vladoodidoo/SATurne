open Utils
open FromString

(**
    Testing fromString
 **)


let%expect_test _ =
    let () = printFormula (fromString "A + B") in
    [%expect{|
      formula = {
      literals = A B
      clauses =
      A B
    |}]

let%expect_test _ =
    let () = printFormula (fromString "A + B, C + !D") in
    [%expect{|
      formula = {
      literals = A B C !D
      clauses =
      A B
      C !D
    
    |}]

let%expect_test _ = 
    let () = printFormula (fromString "A, !A, B, C") in
    [%expect{|
      formula = {
      literals = A !A B C
      clauses =
      A
      !A
      B
      C

    |}]

let%expect_test _ =
    let () = printFormula (fromString "A + B + C + D") in
    [%expect{|
      formula = {
      literals = A B C D
      clauses =
      A B C D

    |}]

let%expect_test _ = 
    let () = printFormula (fromString "A,,B") in
    [%expect{|
      formula = {
      literals = A B
      clauses =
      A

      B |}]

(**
    Testing getUnitClause 
**)

let%expect_test _ = 
    let formula = fromString "A + B, C, D + E,,F" in
    let () = match getUnitClause formula with
        Some clause -> printLitSet clause
        |None -> print_endline "None"
    in
    [%expect{| C |}] 

let%expect_test _ = 
    let formula = fromString "A + B, D, C, D + E,,F" in
    let () = match getUnitClause formula with
        Some clause -> printLitSet clause
        |None -> print_endline "None"
    in
    [%expect{| D |}] 

let%expect_test _ = 
    let formula = fromString "A + B,, D + E,,F" in
    let () = match getUnitClause formula with
        Some clause -> printLitSet clause
        |None -> print_endline "None"
    in
    [%expect{| |}] 

(**
    Testing IncludesEmptyClause
 **)

let print_bool v = 
    if v then 
        print_string "true"
    else
        print_string "false"

let%expect_test _ =
    let formula = fromString "E,!E + R,,L" in
    let () = print_bool (includesEmptyClause formula) in
    [%expect{| true |}]

let%expect_test _ =
    let formula = fromString ",A + B, C" in
    let () = print_bool (includesEmptyClause formula) in
    [%expect{| true |}]

let%expect_test _ =
    let formula = fromString "A + B" in
    let () = print_bool (includesEmptyClause formula) in
    [%expect{| false |}]
