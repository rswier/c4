all: c4

.PHONY: test

test: c4
	./c4 c4.c hello.c
	
