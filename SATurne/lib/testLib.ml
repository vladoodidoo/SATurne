open Utils
open FromString
open Dpll
open Datastructures
open FormulaType

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

let%expect_test _ = 
    let formula = fromString "A + B, D + E" in
    let () = match getUnitClause formula with
        Some clause -> printLitSet clause
        |None -> print_endline "None"
    in
    [%expect{| None |}] 

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

(**
    Testing getMonotoneLitreal
**)

let%expect_test _ =
    let formula = fromString "A, !B, B + C, !A" in
    let () = match getMonotoneLiteral formula with
        Some literal -> printLiteral literal
        |None -> print_endline "None"
    in
    [%expect{| C |}]

let%expect_test _ =
    let formula = fromString "!C + A, !B, B + C, !A" in
    let () = match getMonotoneLiteral formula with
        Some literal -> printLiteral literal
        |None -> print_endline "None"
    in
    [%expect{| None |}]
    
let%expect_test _ =
    let formula = fromString "S, !B, B + !R, !A" in
    let () = match getMonotoneLiteral formula with
        Some literal -> printLiteral literal
        |None -> print_endline "None"
    in
    [%expect{| !R |}]
    (*Alphabetical order... if this test fails it does not mean the
     provided implementation is not right*)

(**
    Testing setVar
 **)

let isSolved formula trueLits =
    let _eltInTrueList elt = 
        match LitSet.find_opt elt trueLits with
            Some _ -> true
            |_ -> false
    in
    List.for_all (fun clause -> LitSet.exists _eltInTrueList clause)
        formula.clauses 

let%expect_test _ = 
    let formula = fromString "A + B, !A + C + D + !B, B + E, !A, E" in
    let formula = setVar formula ({name= "A"; neg= false}) in
    let () = printFormula formula in
    [%expect{|
      formula = {
      literals = B !B C D E
      clauses =
      !B C D
      B E

      E |}]

let%expect_test _ = 
    let formula = fromString "A + B, !A + C + D + !B, B + E, !A, E" in
    let formula = setVar formula ({name = "A"; neg = true}) in
    let () = printFormula formula in
    [%expect{|
      formula = {
      literals = B !B C D E
      clauses =
      B
      B E
      E |}]

let%expect_test _ = 
    let formula = fromString "A + B, !A" in
    let res, lits = dpll formula in
    let () = print_bool (res = (isSolved formula lits)) in
    [%expect{| true |}]

let%expect_test _ = 
    let formula = fromString "A, !A" in
    let res, _ = dpll formula in
    let () = print_bool res in
    [%expect{| false |}]

let%expect_test _ =
    let formula = fromString "A + B, !A + B + E, !E + B, !B + !A, !E" in
    let res, lits = dpll formula in
    let () = print_bool (res = (isSolved formula lits)) in
    [%expect{| true |}]

let%expect_test _ =
    let formula = fromString "A + B + C, !C + !A + B" in
    let res, lits = dpll formula in
    let () = print_bool (res = (isSolved formula lits)) in
    [%expect{| true |}]
