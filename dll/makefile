objects = ..\error_private.o \
..\fl2_common.o \
..\fl2_compress.o \
..\fl2_decompress.o \
..\lzma2_dec.o \
..\lzma2_enc.o \
..\pool.o \
..\radix_bitpack.o \
..\radix_mf.o \
..\radix_struct.o \
..\range_enc.o \
..\threading.o \
..\xxhash.o

CFLAGS := -Wall -O3 -DFL2_MULTITHREAD -DFL2_DLL_EXPORT=1 -pthread
CC := gcc

libflzma2-x64 : $(objects)
	$(CC) -shared -pthread -o libflzma2-x64.dll $(objects)

error_private.o : ..\error_private.h
fl2_common.o : ..\fast-lzma2.h ..\error_private.h ..\fl2_internal.h
fl2_compress.o : ..\fast-lzma2.h ..\fl2_internal.h ..\mem.h ..\util.h ..\fl2_compress_internal.h ..\threading.h ..\pool.h ..\radix_mf.h ..\lzma2_enc.h
fl2_decompress.o : ..\fast-lzma2.h ..\fl2_internal.h ..\mem.h ..\util.h ..\lzma2_dec.h ..\xxhash.h
lzma2_dec.o : ..\lzma2_dec.h ..\fl2_internal.h
lzma2_enc.o : ..\fl2_internal.h ..\mem.h ..\lzma2_enc.h ..\fl2_compress_internal.h ..\radix_mf.h ..\range_enc.h ..\count.h
pool.o : ..\pool.h ..\fl2_internal.h
radix_bitpack.o : ..\fast-lzma2.h ..\mem.h ..\threading.h ..\fl2_internal.h ..\radix_internal.h ..\radix_engine.h
radix_mf.o : ..\fast-lzma2.h ..\mem.h ..\fl2_internal.h ..\radix_internal.h
radix_struct.o : ..\fast-lzma2.h ..\mem.h ..\threading.h ..\fl2_internal.h ..\radix_internal.h ..\radix_engine.h
range_enc.o : ..\fl2_internal.h ..\mem.h ..\range_enc.h
threading.o : ..\threading.h
xxhash.o : ..\xxhash.h
