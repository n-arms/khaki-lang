# ir design
Some questions:
1. Can we get rid of types during lowering and just maintain witness tables?
2. How are we going to emit LLVM IR if everything is passed on the stack?
  - This is actually a really good question, I don't know :(

Let's start with 1:
