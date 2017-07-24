libanansi-c.so: anansi-c.o
	gcc -shared -o libanansi-c.so anansi-c.o

anansi-c.o: main.c data.h
	gcc -fPIC -c main.c -o anansi-c.o

tags: main.c data.h log.h index.h
	ctags -R .

main: main.c data.h log.h index.h tags
	gcc -g -o main -O0 main.c -Wunused -Wreturn-type -Wall
