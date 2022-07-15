open Datastructures
open FormulaType

let setVar formula var = 
    let rec _setVar var tail =  function 
        | [] -> tail
        | e::r -> _setVar var ((LitSet.remove var e)::tail) r
    in
    {formula with clauses = (_setVar var [] formula.clauses)}

(**
let unitClausePropagation formula = 

let subsumption forumula =

let monotoneLiteralFixing forumula = 

let dpKernel formula = 
**) 
