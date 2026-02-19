# emitting llvm ir
- slots that don't need to be saved to a cor correspond to allocas
- slots that do need to be saved to a cor correspond to a GEP in the `cor` struct

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
