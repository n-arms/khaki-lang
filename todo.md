# Features
I think none of these require changes to type system? Automated struct getters/setters/constructors require auto-gen tooling though.

## Integers
- Basic operations like add, sub, comparisons, etc

## Booleans
- Boolean operations like and, or, etc

## Structs
- Automatically generate getters, setters, field pointer projectors, constructors

### For loop design
- should take a `cor (): T` and produce the `T`
- the yielded values should be pulled out with an outparam
```
cor twice(): Unit = { yield; yield }

for twice() {
  Fmt.print("Hello");
}
```

```
cor range(high: Int, out: Ptr[Int]): Unit = {
  let mut i = 0;
  while (Int.less_than(i, high)) {
    Ptr.set(out, i);
    yield;
    set i = Int.add(i, 1);
  }
}

let mut i = 0;
for range(10, &i) {
  Fmt.print("Hello %d", i);
}

// or, using outparams:

for range(10, out i) {
  Fmt.print("Hello %d", i);
}

// or, using outparams + piping:

for out i -> range(10) {
  Fmt.print("Hello %d", i);
}
```

## Outparams
## Piping

