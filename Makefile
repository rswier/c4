CC=gcc

c4: c4.c
	${CC} -o c4 -m32 ./c4.c

clean:
	rm -rf ./c4

test: c4
	./c4 hello.c
	./c4 -s hello.c