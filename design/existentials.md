# existential types are hard to type check
There are two primary criteria to ensure that it isn't possible to "leak" existential types (store them in containers):

## The scope that opens the existential type can't return it
This is easy enough, check that the result type of the scope that opens the existential type doesn't contain the existential type.

## Unification variables from outside the opening scope can't be unified with types 
Ideas:
1. Don't allow unifying with variables outside of the current scope
2. Before entering a scope, create a list of unification variables. At some point, check that none of these unification variables contain a type that includes the existential type
3. Don't worry about it for now :)

In an ideal world we would:
- Have an expression level `open()` built-in function, that takes an existential and skolomizes it
- This type can't be named (since its been skolomized), so the only other type it can be unified with is one from the same AST node but at a different time (ie a for loop that repeatedly `open`s existentials from a list and tries to use functions from one on types from another)
  - This in theory needs some kind of unification guard to prevent, but that seems like a later concern
