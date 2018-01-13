CFILES := ../../error_private.c \
../../fastpos_table.c \
../../fl2_common.c \
../../fl2_compress.c \
../../lzma2_enc.c \
../../pool.c \
../../radix_bitpack.c \
../../radix_mf.c \
../../radix_struct.c \
../../range_enc.c \
../../threading.c \
../bench.c \
../datagen.c

PROG := bench
CFLAGS := -Wall -O3 -DFL2_MULTITHREAD
LDFLAGS :=

# -MMD generates dependencies while compiling
CFLAGS += -MMD
CC := x86_64-w64-mingw32-gcc

OBJFILES := $(CFILES:.c=.o)
DEPFILES := $(CFILES:.c=.d)

$(PROG) : $(OBJFILES)
	$(LINK.o) $(LDFLAGS) -o $@ $^

clean :
	rm -f $(PROG) $(OBJFILES) $(DEPFILES)

-include $(DEPFILES)