all: c4

.PHONY: test clean

test: c4
	./c4 -s hello.c
	./c4 -d hello.c
	./c4 c4.c hello.c
	./c4 c4.c c4.c hello.c
	
clean:
	rm ./c4
