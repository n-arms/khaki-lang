> [!warning]
> This is out of date: Khaki's lowering process is decided upon and (mostly) works. The chosen approach resembles point 3.
# lowering
There really isn't a reason to have `Await` or `Yield` endings, or to have any references to coroutines in the IR. The reason we have it right now is because it seems difficult to lower without any await or yields in a single pass. This seems really unideal though - now that I'm having to rewrite the LLVM backend, having to deal with all of the edge cases of coroutines is unpleasant.

## lowering cors in one pass
The problem: you can't know what slots need to be stored/loaded from cors without first having slots, which don't exist before lowering. In particular, we only know what slots need to be persisted over slots when they are used over an await point, by which time it is too late to have stored them. Slots that are needed to be stored:
1. assume all slots can be persisted and rely on the optimizer to elide unecessary ones
- even if LLVM would be good at removing redundant slot loads/stores (which I'm not even sure it would at this point), the slots would still be allocated in the cor struct, which is unnecssary overhead
2. keep a stack of expressions we are currently in, and use a clever convservative heuristic approximation to determine what values need to be saved
- this would probably work fine but it seems unpleasant to maintain
3. keep `End::Await` and `End::Yield` and have an IR -> IR pass that does the desugaring befre we emit LLVM IR
- this isn't as satisfying because the IR isn't initially ignorant to cors, but it seems to strike a good balance
4. maintain the status quo: eliminate `End::Yield` and `End::Await` while eliminating LLVM IR
- this is pretty clearly a mixing of concerns and is currently proving to be unmaintainable
