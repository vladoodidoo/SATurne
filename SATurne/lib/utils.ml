open Types
open Datastructures
open FormulaType

let getUnitClause formula = 
    let rec _getUnitClause = function 
        [] -> None
        |e::_ when ((LitSet.cardinal e) <= 1) -> Some e
        |_::r -> _getUnitClause r
    in
    _getUnitClause formula.clauses

let includesEmptyClause formula =
    let rec _includesEmptyClause = function
        [] -> false
        |e::r -> (LitSet.is_empty e) || (_includesEmptyClause r)
    in
    _includesEmptyClause formula.clauses

let getMonotoneLiteral formula =
    let predicat elt = 
        let elt = {elt with neg = not elt.neg} in           
        match LitSet.find_opt elt formula.literals with
            Some _ -> false
            |_ -> true
    in
    LitSet.find_first_opt predicat formula.literals

let printLiteral lit = 
    let () = 
        if lit.neg then
            print_string "!"
        else
            ()
    in
    print_string lit.name

let printLitSet set =
    let rec _handleListForm = function
        [] -> ()
        |e::r -> (printLiteral e; print_string " "; _handleListForm r)
    in 
    let () = _handleListForm (LitSet.elements set) in
    print_string "\n"

let printFormula formula =
    let rec _printSetList = function
        [] -> ()
        |set::r -> (printLitSet set; _printSetList r)
    in
    let () = print_string "formula = {\nliterals = " in
    let () = printLitSet formula.literals in
    let () = print_string "clauses =\n" in
    _printSetList formula.clauses

let litSetHas literal set = 
    match LitSet.find_opt literal set with
        Some _ -> true
        |_ -> false

