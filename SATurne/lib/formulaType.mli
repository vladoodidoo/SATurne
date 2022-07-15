open Types
open Datastructures

type formula = {
    literals: LitSet.t;
    clauses: LitSet.t list
}
