# Ast Redesign
Some concerns with the current AST design:

## Concerns
### Confusing `Expr::Op` nodes
It isn't clear when a node should belong to `Expr::Op` and when it should be its own kind of node.

### Difficult to traverse subexprs / subtypes
There isn't an easy mechanism to say "for all subexprs" or "for all subtypes" do this.

### Lots of string allocations
All identifiers are stored as freshly allocated `String`s
Options include:
1. Interning
2. Storing `&'src str`
3. Small string optimization

3 is definitely the easiest to implement right now.

### Lots of node allocations
Every node has its own heap allocation, a flat array representation would be better.

## Solutions
### Unified AST Node Struct
Instead of making the AST an enum, have it store its sub-exprs in a single vec.
