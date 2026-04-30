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

## if we are emitting outside a cor
- all the work is already done, the correct types should be used already

## if we are emitting in a cor
- direct calls should work like normal
- calls to `poll` should work like normal

### await
`await state_machine result_val then_branch`
becomes
```
while (state_machine.poll(&result_val) == YIELD) return YIELD;
goto then_branch;
```

### yield
`yield then_branch`
becomes
```
return YIELD;
goto then_branch;
```
