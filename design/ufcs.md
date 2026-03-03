# Method vs Field Disambiguation
Differentiate accessing a field vs calling a method.
- make method calls always have parens and field accesses never have parens
- field function pointers can be called with parens

## Method Call Syntax
- We want to support calling methods that take the object by value or ptr
- When we see a `object.method(args...)`, we either:
1. if `object` is known to be `Named` or `Ptr[Named]`, we resolve immediately
2. if `object` is `?unif` or `*?unif`, emit a constraint
