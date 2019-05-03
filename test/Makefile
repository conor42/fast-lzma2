OBJ=file_test.o

CFLAGS:=-Wall -O1 -pthread
CC:=gcc

file_test : $(OBJ)
	$(CC) -pthread -o file_test $(OBJ) -lfast-lzma2

clean:
	rm -f file_test $(OBJ)
