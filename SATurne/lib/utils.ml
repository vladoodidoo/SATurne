open Types
open Datastructures
open FormulaType

let getUnitClause formula = 
    let rec _getUnitClause = function 
        [] -> None
        |e::r when ((LitSet.cardinal e) <= 1) -> Some e
        |_::r -> _getUnitClause r
    in
    _getUnitClause formula.clauses

let rec includesEmptyClause formula =
    let rec _includesEmptyClause = function
        [] -> false
        |e::r -> (LitSet.is_empty e) || (_includesEmptyClause r)
    in
    _includesEmptyClause formula.clauses

let rec getMonotoneLiteral formula =
    let predicat elt = 
        let elt = {elt with neg = not elt.neg} in           
        match LitSet.find_opt elt formula.literals with
            Some _ -> false
            |_ -> true
    in
    LitSet.find_first_opt predicat formula.literals

