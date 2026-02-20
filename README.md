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
