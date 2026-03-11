# operators
As of writing the language doesn't have any operators (+, <, etc), you always specify the operation using function calls that internally reference builtins: `Int.add` is an actual function:
```
struct Int {
  func add(x: Int, y: Int): Int = <builtin int_add>(x, y)
}
```
I've been thinking about whether this is a good approach moving forward, or whether it should change when we add operators.
There are two reasonable approaches that I see:
1. We desugar `x + y` into `x.add(y)`, which uses our method syntax to dispatch which `add` is called based soley on the type of `x`
2. We keep `x + y` as it is while typechecking and then emit <builtin int add>(x, y) directly when lowering

While either seems reasonable, and 1 initially seems desirable for ease of implementation, I think that 2 is the only valid option. Option 1 requires the type of `x` to be known before typing the call, as `add` dispatches off of `x`. This means `let x: I32 = 1 + 2` is ambiguous, which seems bad.

