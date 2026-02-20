# Khaki Lang
Khaki is a novel language and LLVM-based compiler.
- **Highly embeddable:** Khaki is designed to run in extremely resource-constrained environments like Cortex-M microcontrollers. The compiler uses LLVM to support a variety of targets, and tends to produce small binaries.
- **Native coroutines:** Khaki has native support for co-operative multitasking via coroutines. You'll never need an RTOS, and your tasks are never pre-empted.
- **Generic types:** Khaki promotes reuse via generic structs and functions. You can define a generic `Pair[key, value]` type once and reuse it forever.

## Compiler Design
The Khaki compiler is based on a modern compiler architecture:
- Lexing and parsing using [Logos](https://github.com/maciejhirsz/logos) and [Chumsky](https://github.com/zesterer/chumsky)
- An abstract syntax tree that carries span information for good type errors
- Unification-based Hindley-Milner type inference
- A graph-of-basic-blocks IR that makes it easy to desugar coroutines and target LLVM
- A backend that emits textual LLVM to be ingested by llc or clang

## Examples
```
struct Main {
  cor range(up_to: Int, out: Ptr[Int]): Unit {
    let i = 0;
    while i < up_to {
      Ptr.store(out, i);
      yield;
      set i = 1.add(i);
    }
  }

  func main(): Unit {
    let i = 0;
    let r = range(10, &i);
    let result = {};
    while (Main_range.poll(&r, &result)) {
      Int.print(i);
    }
  }
}
```
