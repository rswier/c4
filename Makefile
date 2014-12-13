CFLAGS = -m32

test: c4
	./c4 hello.c
	./c4 -s hello.c
	./c4 c4.c hello.c
	./c4 c4.c c4.c hello.c

clean:
	rm -f c4
