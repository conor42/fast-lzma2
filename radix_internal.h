/*
* Copyright (c) 2018, Conor McCarthy
* All rights reserved.
*
* This source code is licensed under both the BSD-style license (found in the
* LICENSE file in the root directory of this source tree) and the GPLv2 (found
* in the COPYING file in the root directory of this source tree).
* You may select, at your option, one of the above-listed licenses.
*/

#ifndef RADIX_INTERNAL_H
#define RADIX_INTERNAL_H

#include "atomic.h"
#include "radix_mf.h"

#if defined (__cplusplus)
extern "C" {
#endif

#define DICTIONARY_LOG_MIN 12U
#define DICTIONARY_LOG_MAX_64 30U
#define DICTIONARY_LOG_MAX_32 27U
#define DEFAULT_BUFFER_LOG 8U
#define DEFAULT_BLOCK_OVERLAP 2U
#define DEFAULT_SEARCH_DEPTH 32U
#define DEFAULT_DIVIDEANDCONQUER 1
#define MAX_REPEAT 32
#define RADIX16_TABLE_SIZE (1UL << 16)
#define RADIX8_TABLE_SIZE (1UL << 8)
#define STACK_SIZE (RADIX16_TABLE_SIZE * 3)
#define MAX_BRUTE_FORCE_LIST_SIZE 5
#define BUFFER_LINK_MASK 0xFFFFFFU
#define MATCH_BUFFER_OVERLAP 6
#define BITPACK_MAX_LENGTH 63UL
#define STRUCTURED_MAX_LENGTH 255UL

#define RADIX_LINK_BITS 26
#define RADIX_LINK_MASK ((1UL << RADIX_LINK_BITS) - 1)
#define RADIX_NULL_LINK 0xFFFFFFFFUL

#define UNIT_BITS 2
#define UNIT_MASK ((1UL << UNIT_BITS) - 1)

typedef struct
{
    U32 head;
    U32 count;
} RMF_tableHead;

typedef struct
{
    U32 from;
    BYTE chars[4];
    U32 next;
} RMF_buildMatch;

typedef struct
{
    U32 prev_index;
    U32 list_count;
} RMF_listTail;

typedef struct
{
    U32 links[1 << UNIT_BITS];
    BYTE lengths[1 << UNIT_BITS];
} RMF_unit;

typedef struct
{
    unsigned max_len;
    U32* table;
    size_t match_buffer_size;
    size_t match_buffer_limit;
    RMF_listTail tails_8[RADIX8_TABLE_SIZE];
    RMF_tableHead stack[STACK_SIZE];
    RMF_listTail tails_16[RADIX16_TABLE_SIZE];
    RMF_buildMatch match_buffer[1];
} RMF_builder;

struct FL2_matchTable_s
{
    FL2_atomic st_index;
    long end_index;
    int isStruct;
    int allocStruct;
    unsigned thread_count;
    RMF_parameters params;
    RMF_builder** builders;
    U32 stack[RADIX16_TABLE_SIZE];
    RMF_tableHead list_heads[RADIX16_TABLE_SIZE];
    U32 table[1];
};

void RMF_bitpackInit(struct FL2_matchTable_s* tbl, const void* data, size_t start, size_t end);
void RMF_bitpackInitComplete(struct FL2_matchTable_s* tbl, const void* data, size_t start, size_t end);
void RMF_structuredInit(struct FL2_matchTable_s* tbl, const void* data, size_t start, size_t end);
long RMF_findEndHeadIndex(RMF_tableHead* head_table, size_t end, size_t block_size, unsigned thread_count);
void RMF_bitpackBuildTable(struct FL2_matchTable_s* const tbl,
    unsigned job,
    unsigned multiThread,
    const void* srcStart,
    size_t const block_start,
    size_t const block_size);
void RMF_structuredBuildTable(struct FL2_matchTable_s* const tbl,
    unsigned job,
    unsigned multiThread,
    const void* srcStart,
    size_t const block_start,
    size_t const block_size);
void RMF_recurseListChunk(RMF_builder* const tbl,
    const BYTE* const data_block,
    size_t const block_start,
    BYTE depth,
    BYTE max_depth,
    U32 list_count,
    size_t stack_base);
int RMF_bitpackIntegrityCheck(const struct FL2_matchTable_s* tbl, const BYTE* data, size_t index, size_t end, unsigned max_depth);
int RMF_structuredIntegrityCheck(const struct FL2_matchTable_s* tbl, const BYTE* data, size_t index, size_t end, unsigned max_depth);
void RMF_bitpackLimitLengths(struct FL2_matchTable_s* tbl, size_t index);
void RMF_structuredLimitLengths(struct FL2_matchTable_s* tbl, size_t index);
BYTE* RMF_bitpackAsOutputBuffer(struct FL2_matchTable_s* tbl, size_t index);
BYTE* RMF_structuredAsOutputBuffer(struct FL2_matchTable_s* tbl, size_t index);
size_t RMF_bitpackExtendMatch(const BYTE* const data,
    const U32* const table,
    ptrdiff_t const start_index,
    ptrdiff_t limit,
    U32 const link,
    size_t const length);
size_t RMF_structuredExtendMatch(const BYTE* const data,
    const U32* const table,
    ptrdiff_t const start_index,
    ptrdiff_t limit,
    U32 const link,
    size_t const length);
size_t RMF_bitpackGetMatch(struct FL2_matchTable_s* tbl,
    const BYTE* const data,
    size_t index,
    size_t limit,
    unsigned max_depth,
    size_t* offsetPtr);
size_t RMF_structuredGetMatch(struct FL2_matchTable_s* tbl,
    const BYTE* const data,
    size_t index,
    size_t limit,
    unsigned max_depth,
    size_t* offsetPtr);

#if defined (__cplusplus)
}
#endif

#endif /* RADIX_INTERNAL_H */