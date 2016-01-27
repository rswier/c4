c5 - C in four functions + AST + back-end code generator
========================================================
An exercise in minimalism.

This branch extends **c4.c** by adding:

   * Abstract Syntax Tree creation
   * Back-end code generator function: **gen()**
   * Standard ordering of function parameters on stack
   * Native x86 version: **c5x86.c**
   * Various optimizations
  
Try the following:

    gcc -o c5 c5.c  (you may need the -m32 option on 64bit machines)
    ./c5 hello.c
    ./c5 -s hello.c
    
    ./c5 c5.c hello.c
    ./c5 c5.c c5.c hello.c
