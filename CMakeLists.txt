cmake_minimum_required (VERSION 3.10)

option(BUILD_SHARED_LIBS "Build the shared library" ON)

include_directories("${CMAKE_CURRENT_SOURCE_DIR}")
include_directories("${CMAKE_CURRENT_SOURCE_DIR}/tests")

add_library(flzma2
  error_private.c
  fl2_compress.c
  lzma2_dec.c
  pool.c
  radix_mf.c
  range_enc.c
  xxhash.c
  fl2_common.c
  fl2_decompress.c
  lzma2_enc.c
  radix_bitpack.c
  radix_struct.c
  threading.c)

add_executable(fuzzer tests/fuzzer.c tests/datagen.c)
target_link_libraries(fuzzer flzma2)

add_executable(bench bench/bench.c)
target_link_libraries(bench flzma2)
