# Syntax
Khaki should have an **LL** syntax that can be parsed with **chumsky parser combinators**.

## Type Syntax
- `any[t] <type>`
- `fn[<generics>](<type>*) -> <type>`
- primitives like `Int`, `Void`, `T*`, etc
- named types have upper case names, generics have lower case names
- named types can optionally be prefixed with a path

## Value Syntax
- normal operators (+, -, *, /, >, <, >=, etc)
- pointer dereference (postfix *)
- if statements (statements as expressions): `if <expr> { <a> } else {<b>}` or `if <expr> then <a> else <b>`, optionally without an else in both cases
- variables: `<var-name>`
- function call: `<expr>(<expr>*)`
- struct pack: `StructName(<expr>*)`, optionally with a path
- functions: `function_name(<expr>*)`, optionally with a path
- fields: `struct_value.field`

This raises three concerns:
1. How to tell between `MyType*` and `my_value*`:
- Solution is simple: the parser always needs to know if it is parsing a type or a value

2. How to tell between `my_ptr*` and `my_int * my_int`
- Look ahead to see if there is a token that starts a new expression after the *

3. How to tell between `<path>StructName(<expr>*)` and `<path>function_name(<expr>*)` and `struct_value.field`
- Capitalized module names would actually solve a lot of problems, but this seems weird: we're not writing Haskell
- Using :: for path seperation helps here, now we just have to distinguish between `my::best::module::StructName(<expr>*)` and `my::best::module::function_name(<expr>*)`
