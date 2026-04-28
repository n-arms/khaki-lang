# module design
Khaki's design of generics is such that the code can be split up and compiled as seperately as desired. Khaki has the idea of a **module**, which is similar to that of Rust: it's a file, or a specially marked group of functions.

## compiling a module
Compiling modules follows a `map -> reduce -> map -> reduce` pipeline in order to exploit as much parallelism as possible. There is a comparable amount of seperability between modules to C.

1. (map) Modules are parsed and type definitions / function signatures are extracted.
2. (reduce) Type definitions and function signatures are exchanged between modules.
3. (map) Modules are type checked, lowered, emitted to LLVM, and compiled to seperate object files.
4. (reduce) The object files are linked.
