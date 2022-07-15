open Types

let compareLiteral x y = 
    if x.name <> y.name then
        String.compare x.name y.name
    else
        Bool.compare x.neg y.neg

module LitSet = Set.Make(
    struct
        type t = literal
        let compare = compareLiteral
    end
)
