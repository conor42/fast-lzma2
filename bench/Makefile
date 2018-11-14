objects = ../fl2_common.o \
../fl2_compress.o \
../fl2_decompress.o \
../fl2_error_private.o \
../fl2_pool.o \
../fl2_threading.o \
../lzma2_dec.o \
../lzma2_enc.o \
../radix_bitpack.o \
../radix_mf.o \
../radix_struct.o \
../range_enc.o \
../xxhash.o \
../tests/datagen.o \
bench.o

CFLAGS := -Wall -O3 -pthread
CC := gcc

bench : $(objects)
	$(CC) -pthread -o bench.exe $(objects) -lm

fl2_common.o : ../fast-lzma2.h ../fl2_error_private.h ../fl2_internal.h
fl2_compress.o : ../fast-lzma2.h ../fl2_internal.h ../mem.h ../util.h ../fl2_compress_internal.h ../fl2_threading.h ../fl2_pool.h ../radix_mf.h ../lzma2_enc.h
fl2_decompress.o : ../fast-lzma2.h ../fl2_internal.h ../mem.h ../util.h ../lzma2_dec.h ../xxhash.h
fl2_error_private.o : ../fl2_error_private.h
fl2_pool.o : ../fl2_pool.h ../fl2_internal.h
fl2_threading.o : ../fl2_threading.h
lzma2_dec.o : ../lzma2_dec.h ../fl2_internal.h
lzma2_enc.o : ../fl2_internal.h ../mem.h ../lzma2_enc.h ../fl2_compress_internal.h ../radix_mf.h ../range_enc.h ../count.h
radix_bitpack.o : ../fast-lzma2.h ../mem.h ../fl2_threading.h ../fl2_internal.h ../radix_internal.h ../radix_engine.h
radix_mf.o : ../fast-lzma2.h ../mem.h ../fl2_internal.h ../radix_internal.h
radix_struct.o : ../fast-lzma2.h ../mem.h ../fl2_threading.h ../fl2_internal.h ../radix_internal.h ../radix_engine.h
range_enc.o : ../fl2_internal.h ../mem.h ../range_enc.h
xxhash.o : ../xxhash.h
bench.o : ../fast-lzma2.h ../mem.h ../util.h ../tests/datagen.h
