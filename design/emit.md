# emitting llvm ir
- dynamic allocations for DLAs is difficult
- slots that don't need to be saved to a cor correspond to allocas
- slots that do need to be saved to a cor correspond to a GEP in the `cor` struct

## dynamic alloca for DLAs
- `Witness::Static` slots are great: we can allocate them in the function prefix, and they will hopefully get SROAed into registers
- all the types with `Witness::Dynamic` witnesses need to be allocated with dynamic `alloca`s
  - these can't get SROAed so it is less important where we place them
  - we also can't necessarily calculate the witness for all our dynamic slots in the function prefix, as some of them might depend on intermediate results of the function
  - we need some way to indicate what scope a slot is declared in (particularly for `if` expressions, where this is 100% needed)

## named types
Problem: a lot of types (anything with a generic) can't be defined by a LLVM struct type (hence the decision to keep everything behind an opaque pointer in LLVM IR). Solution: avoid loading values from slots as much as possible.
1. Value::Slot - can be implemented with a memcpy
2. Value::Load - requires us to get the pointer in the slot, but since we know its a pointer, we can specify its type
3. Value::Call - requires that we load the function (which has a known size), but everything else stays in its slot
4. Value::Op(Op::Arth(Arith::Add)) - load the arguments as integers, add them, and return the result to a slot

This illuminates a pattern: we generally only need to load a type when it is one of a few known core types:
- ptr
- int
- function
- etc
