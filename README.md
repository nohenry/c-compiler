# C Compiler
Well, it's kinda in the name
## Purpose
This is a modern c compiler built from scratch. The goal is to create a small compiler focused on build speed and ease-of-use.
This comes at the cost of runtime speed, but this is fine for development builds in order to speed up the development process.
\
\
This is also a project I created to test out data oriented concepts and parallelization in a compiler. I had also never
implemented a code generation backend from scratch so this does that too.

## Building
The compiler can be built with the following command
```bash
zig build
```
Then run the compiler
```bash
./zig-out/cp main.c -c -o main.o
```
The command line interface is more or less the same as that of clang or gcc.

## Build System
###### *Not Implemented*
The compiler will have an integrated build interface to make building complex (or even simple) projects easy.
Build scripts will be written in c itself, allowing for the ability to imperatively dictate build steps.
This will also lead to faster compile times as the compiler can see multiple translation units at once,
letting it reuse certain aspects, and cleverly parallelize compilation.

## Targets
###### No LLVM
Currently, the only supported output target is AArch64 on macOS.
The code generation backend is handwritten providing, a fast (yet dumb) output of machine code.
LLVM is not used as it would break the goal of a fast compiler. That said, LLVM will be implemented
in the future for use in release builds where slow, but optimized compilation is desired.
