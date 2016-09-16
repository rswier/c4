#!/bin/sh
#Program:
#   This program shows c4 build steps
#History
#2016/09/16  vnail First release
PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin:~/bin
export PATH

ARCH=$(arch)

if [ "${ARCH}" == "x86_64" ]; then
 echo "64bit machine"
 gcc -m32 -o c4 c4.c
else
 echo "32bit machine"
 gcc -o c4 c4.c
fi

test -e c4 && echo " use gcc to build c4" || echo "failed to output c4"
 
