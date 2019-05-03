SRC = $(wildcard ../*.c) datagen.c fuzzer.c
OBJ = $(SRC:.c=.o)
DEP = $(OBJ:.o=.d)

CFLAGS:=-Wall -O2 -pthread
CC:=gcc

ASFLAGS:=

UNAME_P := $(shell uname -p)
ifeq ($(UNAME_P),x86_64)
	ASFLAGS+=-D MS_x64_CALL=0
	CFLAGS+=-DLZMA2_DEC_OPT
	OBJ+=../lzma_dec_x86_64.o
endif

fuzzer : $(OBJ)
	$(CC) -pthread -o fuzzer $(OBJ)

-include $(DEP)

%.d: %.c
	@$(CC) $(CFLAGS) $< -MM -MT $(@:.d=.o) >$@

clean:
	rm -f fuzzer $(OBJ) ($DEP)