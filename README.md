c4 - C in four functions
========================

An exercise in minimalism.

Try the following under GNU/Linux:

    gcc -o c4 c4.c
    ./c4 hello.c
    ./c4 -s hello.c
    
    ./c4 c4.c hello.c
    ./c4 c4.c c4.c hello.c


Try the following under Microsoft Windows/Visual Studio 2017 in the "x86 Native Tools Command Prompt for VS 2017" or "x64_x86 Cross Tools Command Prompt for VS 2017":

    cl c4.c
    c4 hello.c
    c4 -s hello.c
    
    c4 c4.c hello.c
    c4 c4.c c4.c hello.c

