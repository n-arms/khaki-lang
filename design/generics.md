# generics
Insead of the the original design using monomorphization, Khaki now uses a style of generic implementation similar to Swift: generics are represented as unsized types, and information is passed about them at runtime in the form of a witness table. In Khaki, witness tables just hold the alignment and size, as opposed to Swift where these tables hold a v-table of reference-counting operations.

Since generics are reified into runtime arguments to the function (eg `fn id[T](x: T): T` has two arguments: `T: WitnessTable`, and `x: *Unsized`), generic functions can't be specialized in a non-call context:

```
fn id[T](x: T): T = x

let f: I32 -> I32 = id; // not valid
```

In order to make this work, we would need to set `f = |x| id(i32WitnessTable, x)` (using Rust closure syntax), but Khaki doesn't have closures. This is potentially limitting for a number of applications, including an async scheduler. The following:

```
fn spawn[Cor](cor: Cor, poll: (*Cor, *()) -> Bool) {
  let cor_ptr = Arena.push(&cors, cor);
  Vec.push(&tasks, Task(cor_ptr.cast(), erased_poll[Cor]));
}

fn erased_poll[Cor](cor: Void*, poll: (*Void, ()) -> Bool) {
  let concrete_poll: (Cor, *()) -> Bool = poll.cast();
  concrete_poll(cor.cast());
}
```

despite being a necessary core of an async runtime, is not possible in above setup because `erased_poll` would need to close over `Cor`.

## solution: existential types
The solution is simple: allow closure over witness tables via existential types. This has the added bonus of making support for closures really easy. We can represent a task as `any[T] (T, (*T, *()) -> Bool)`. This is possible because dynamic-sized types are the default in Khaki. Although this is clearly a powerful tool, it doesn't solve the problem of how to compile functions that close over their generic types.

I think this boils down to the question: what type should `fn id[T](x: T): T = x` have?
Some options:
1. It doesn't have a type, it only exists in a calling position.
2. It has the type `fn[T](T): T`, which can only be specialized to particular types in the calling position.
3. It has the type `any[T] fn(T): T` -> this doesn't make any sense, `any` means "exists some", we want "for all"
  - We introduce an `all` keyword, and do `all[T] fn(T): T`

All of these are valid, but which are:
- Powerful
- Convenient
- Easy to implement

### Option 1
This is really simple but not unreasonable. We only let non-generic functions to be used in non-call positions. This is technically still sufficient to represent tasks, as all tasks are going to be concrete `fn(*Cor, *()): Bool` for some concrete type `Cor`, and so you could trivially coerce the function into a task of the shape `any[T] (T, fn(*T, *()): Bool)`

### Option 2
This seems worth exploring. Function types are determined by the number of generics of the underlying function, eg `fn id[T](x: T): T = x` would have type `id: fn[T](T): T`, whereas `fn id[A, B](x: A): A = x` would have type `id: fn[A, B](A): A`. This is more powerful in that it lets you write generic vtables. Eg:

```
trait Annotater {
  fn annotate<T>(&self, t: T): Annotated<T>
}
```

would map to: `any[A] (A, fn[T](*A, T): Annotated[T])`.

#### Type Inference
**Function type introduction:** Given `fn id[T](x: T): T = x`, we infer `f: fn[T](T): T`.
**Function type elimination:** Given `f: fn[T](T): T` and `x: Type1`, `f(x)` becomes `f[Type1](x): Type1`
