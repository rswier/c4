c4 - C in four functions
========================

An exercise in minimalism.

This branch adds structures (struct) along with the dot (.) and arrow (->) operators.
It is very silly to add this level of complexity while keeping it just four functions.
But it had to be done :-)

Try the following:

    gcc -o c4 c4.c  (you may need the -m32 option on 64bit machines)
    ./c4 hello.c
    ./c4 -s hello.c
    
    ./c4 c4.c hello.c
    ./c4 c4.c c4.c hello.c

