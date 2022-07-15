open Types

module Clause : Set.S with type elt = literal 

type formula = {
    length: int;
    clauses: Clause.t
}
