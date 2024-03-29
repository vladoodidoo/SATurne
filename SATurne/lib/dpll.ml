open Datastructures
open FormulaType
open Utils
open Types

let setVar formula var = 
    let reverse = {var with neg = not var.neg} in
    let rec _rmAllClauses = function
        [] -> []
        |clause::r when (litSetHas var clause) -> _rmAllClauses r
        |clause::r -> clause::(_rmAllClauses r)
    in
    let _deleteNegFromClauses clauses =
        List.map (fun elt -> LitSet.remove reverse elt) clauses
    in
    let _removeFromLiterals literals =
        LitSet.filter (fun elt -> elt <> reverse && elt <> var) literals
    in
    {clauses = _deleteNegFromClauses (_rmAllClauses formula.clauses);
        literals = _removeFromLiterals formula.literals}

let rec unitClausePropagation formula = 
    match getUnitClause formula with
        Some _ when includesEmptyClause formula -> formula
        |Some clause -> unitClausePropagation (setVar formula 
                            (List.nth (LitSet.elements clause) 0))
        |None -> formula

(**
let subsumption forumula =
**) 

let rec monotoneLiteralFixing formula = 
    match getMonotoneLiteral formula with
        Some literal -> monotoneLiteralFixing (setVar formula literal)
        |None -> formula

let dpKernel formula = 
    monotoneLiteralFixing (unitClausePropagation formula)

let choice formula = 
    List.hd (LitSet.elements formula.literals)
    (*TODO: Implement heuristic*)

let dpll formula =
    let rec dpll_ formula literals = 
        match formula with
            _ when includesEmptyClause formula -> false, literals
            |_ when LitSet.is_empty formula.literals -> true, literals
            |_ -> begin
                    let var = choice formula in 
                    let rev = {var with neg = not var.neg} in
                    let sat, lits = dpll_ (setVar formula var) 
                        (LitSet.add var literals) in
                    if sat then
                        true, lits
                    else
                        dpll_ (setVar formula rev) (LitSet.add rev literals)
                end
    in  
    dpll_ formula LitSet.empty
