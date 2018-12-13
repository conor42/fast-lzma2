objects = ../error_private.o \
../dict_buffer.o \
../fl2_common.o \
../fl2_compress.o \
../fl2_decompress.o \
../fl2_pool.o \
../fl2_threading.o \
../lzma2_dec.o \
../lzma2_enc.o \
../radix_bitpack.o \
../radix_mf.o \
../radix_struct.o \
../range_enc.o \
../util.o \
../xxhash.o \
datagen.o \
fuzzer.o

CFLAGS := -Wall -O3 -pthread
CC := gcc

ASFLAGS :=

x86_64 := 0

exe_ext :=

ifeq ($(OS),Windows_NT)
	exe_ext :=.exe
ifeq ($(PROCESSOR_ARCHITECTURE),AMD64)
    ASFLAGS += -D MS_x64_CALL=1
	x86_64 := 1
endif
else
	UNAME_P := $(shell uname -p)
ifeq ($(UNAME_P),x86_64)
    ASFLAGS += -D MS_x64_CALL=0
	x86_64 := 1
endif
endif

ifeq ($(x86_64),1)
	CFLAGS += -DLZMA2_DEC_OPT
	objects += ../LzmaDecOpt.o
endif

fuzzer : $(objects)
	$(CC) -pthread -o fuzzer$(exe_ext) $(objects) -lm

dict_buffer.o : ../dict_buffer.h ../fl2_internal.h ../mem.h ../data_block.h
error_private.o : ../error_private.h
fl2_common.o : ../fast-lzma2.h ../error_private.h ../fl2_internal.h
fl2_compress.o : ../dict_buffer.h ../fast-lzma2.h ../fl2_internal.h ../mem.h ../util.h ../fl2_compress_internal.h ../fl2_threading.h ../fl2_pool.h ../radix_mf.h ../lzma2_enc.h
fl2_decompress.o : ../fast-lzma2.h ../fl2_internal.h ../mem.h ../util.h ../lzma2_dec.h ../xxhash.h
fl2_pool.o : ../fl2_pool.h ../fl2_internal.h
fl2_threading.o : ../fl2_threading.h
lzma2_dec.o : ../lzma2_dec.h ../fl2_internal.h
lzma2_enc.o : ../fl2_internal.h ../mem.h ../lzma2_enc.h ../fl2_compress_internal.h ../radix_get.h ../radix_mf.h ../range_enc.h ../count.h
radix_bitpack.o : ../fast-lzma2.h ../mem.h ../fl2_threading.h ../fl2_internal.h ../radix_internal.h ../radix_engine.h
radix_mf.o : ../fast-lzma2.h ../mem.h ../fl2_internal.h ../radix_internal.h
radix_struct.o : ../fast-lzma2.h ../mem.h ../fl2_threading.h ../fl2_internal.h ../radix_internal.h ../radix_engine.h
range_enc.o : ../fl2_internal.h ../mem.h ../range_enc.h
util.o : ../util.h
xxhash.o : ../xxhash.h
fuzzer.o : ../fast-lzma2.h ../fl2_errors.h ../mem.h ../xxhash.h datagen.h
