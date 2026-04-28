# bidir type checking
The original draft of the compiler used a full constraint driven HM solver. More advanced features of Khaki like rank-n types and existential types need bidir checking.

## theory 
### functions
We simply infer all functions to have the universally quantified type `fn[T...](args...): Type`

### calls
1. Infer the function type
2. Instantiate it with unification variables
3. Check the args
4. Solve the resulting constraints
5. Apply the substitution to the function and its args
6. Discard all constraints

This approach has the following tradeoffs:
- Can't type functions of the form `fn[T](args..): Type[T]`, where `T` cannot be infered by args
  - This seems fine, there are some cases where this would require type annotations but I'm okay with that
- Minimizes the scope of constraints
  - This is definitely a good thing

### existential types
We have an inner expression of type `(Int, fn(Int): Void)` and want to turn it in to `any[T] (T, fn(T): Void)`
- Note that we will disallow type variables that escape from the `any` (ie `any[T] (T, fn(T): T)` wouldn't be allowed)
1. Sub `T = unif1`, obtaining `(unif1, fn(unif1): Void)`
2. Check the inner expression against this type
3. Solve the resulting constraints
4. Take the resulting solved unifs and build a `Expr::Any` with the solved unif types
5. Discard all constraints

#### subnote
Do we want the subexpressions of the `any` type to have their correct types (ie `Int`, `fn(Int): Void`), or their substituted types (ie `T`, `fn(T): Void`). Let's consider from several perspectives:
**Parsing:** makes no difference.
**Type checking:** once we've type checked the Expr::Any, we should never need to inspect the types of its subexpressions, so it doesn't matter.
**Lowering:** when lowering, we lower the subexpression of the `any` as normal, and then place it in a tuple packed with the witness table for `T`
  - this requires us to know what `T` is, which we should store in the `any`, and we obtain from the unification variable we used.
**Emitting:** follows from lowering.

### existential type elimination?
No special elimination is needed.

## algorithm
We originally built up a substitution mapping at a single point after inferring the entire function body. This is a potentially O(N^2) operation (I think?), and doing it potentially O(N) times is not ideal. Instead, we will use a union find (possibly the ena crate?).

### checking
1. If we have an `Expr::Any` and a `TypeKind::Any`, then:
- substitute the opaque type with a unification variable
- check the subexpression against this unification variable
2. Fall back on inference, and unify the expected type with the actual type

### inference
1. If we have a `Expr::Var`, look it up in the context
2. If we have a `Expr::Call`, infer the function, requiring its type to be a polymoprhic function
- instantiate the polymorphic function type with unif vars
- check the argument types

Note that this scheme can still use our pre-existing setup for numerical inference: assign each literal a unification var, make all integer ops of type `(t, t) -> t`, and then ensure that all the integer unification variables are unified to integers or not unified at all in the end.
