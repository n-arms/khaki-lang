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

# on lvalues
concern:
```
cor f(): Int = {
  let x = 5;
  let y = &x;
  yield;
  y*
}
```
this is valid depending on where `y` points.
1. If `y = &my_cor->x`, then everything is fine, we can deref `y` over a yield point.
2. On the other hand, if we generate:

```
int x = my_cor->x;
int *y = &x;
yield;
return *y;
```

then the saved value of y will point to the stack local variable x (saving pointers into the stack over yields is illegal), as opposed to the cor slot.

## solutions
### current compilation approach (unsound)
1. create a stack local for each slot
2. copy over the values of saved slots
3. run the code

### new approach (sound!)
1. slots are either stack local (which means they provably never persist over yields), or saved
2. stack local slots get `alloca`s like normal
3. saved slots are always refered to directly through the cor struct (ie they are GEPs, not allocas)

How do we determine if a stack slot never persists over a yield?
- It clearly persists if it is directly used over a yield (ie `let x = 5; yield; x`)
- It has the potenial to persist if a reference is taken, stored over a yield, then used (ie `let x = 5; let y = &x; yield; *y`)
  - The problem is that without full data-flow analysis we can't tell when the reference will be used
  - Therefore assume that all variables that are referenced have the potential to persist
