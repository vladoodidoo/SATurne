open Types
open Datastructures
open FormulaType

(**
    String format: A + B + C, !A + D, V + !C
    ! -> neg
 **)

let fromString str =
    let _handleLit litStr = 
        let litStr = String.trim litStr in
        if litStr.[0] = '!' then
            {name = (String.sub litStr 1 1); neg = true} 
        else
            {name = litStr; neg = false}
    in
    let _handleSetPart set e =
        let e = _handleLit e in
        LitSet.add e set
    in
    let _handleSub e =
        let e = String.trim e in
        if String.length e = 0 then
            LitSet.empty
        else
            List.fold_left _handleSetPart LitSet.empty (String.split_on_char '+' e)     
    in
    let rec _handleClause set = function
        [] -> set
        |e::r -> _handleClause (LitSet.add e set) r
    in
    let rec _createLitList set = function
        [] -> set
        |e::r -> _createLitList (_handleClause set (LitSet.elements e)) r
    in
    let formulaList = List.map _handleSub (String.split_on_char ',' str) in
    {literals = _createLitList LitSet.empty formulaList;
        clauses = formulaList}
