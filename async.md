# async-await / coroutine compilation model
We use the same technique as Rust (async state machines), but with less sophistication in terms of safety.

```
cor foo(): Int {
  yield;
  5
}

cor bar(): Int =
  Int.add(1, foo()!) // automatically awaits foo

fn main(): Unit {
  let bar_cor: @bar = bar(); // use spawn to do a call that isn't automatically awaited
}
```

Each async function `foo` compiles down to a struct `@foo` which holds the state of the cor invocation, and a function `@foo.poll`.
```
cor foo(): Int {
  yield;
  5
}
```
Gets turned into:
```
struct @foo {
  fn poll(foo: Ptr[@foo], result: Ptr[Int]): Unit;
}
```

## semantics of cors
**cor** marks a function as being asynchronous; it can `yield`, and allows direct calls to other `cor`s
**yield** yields execution from a `cor`; if you want to yield a specific value, you can pass in an outptr to the function
directly **call**ing a `cor` provides you a `@struct_name.cor_name` struct
using **@cor!** awaits a `cor`.
using **@cor.poll** on a `@cor` directly allow you to run a `cor` until it yields or returns
