# Scheduler Library
- Store all the coroutine structs
  - Even if the number of coroutines is known, each struct is dynamically sized
  - We're going to need a mechanism to mimic dynamic allocation
    - Bump allocator out of an `Array[I8, N]`
- Store a list of (pointer to cor struct, pointer to poll function) pairs (plus metadata about which tasks are blocked)
  - To keep uniformity, cor structs are `Ptr[Unit]`, poll functions are `func(Ptr[Unit])`

# Language Features

## Integers
- Basic operations like add, sub, comparisons, etc

## Booleans
- Boolean operations like and, or, etc

## Arrays / Slices
- Slice type (`Slice[t]`)
- `[]` operator to index into the slice (can be part of an lvalue chain)
- Slices are stack alocated (aren't valid outside the scope they were originally declared)
- Declared with a slice literal
- When lowering, we need an inline array type (ie no indirections)
  - This is necessary because coroutines need to store the value 

## For loop design
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

