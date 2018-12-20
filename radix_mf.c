/*
* Copyright (c) 2018, Conor McCarthy
* All rights reserved.
*
* This source code is licensed under both the BSD-style license (found in the
* LICENSE file in the root directory of this source tree) and the GPLv2 (found
* in the COPYING file in the root directory of this source tree).
* You may select, at your option, one of the above-listed licenses.
*/

#include <stddef.h>     /* size_t, ptrdiff_t */
#include <stdlib.h>     /* malloc, free */
#include "fast-lzma2.h"
#include "mem.h"          /* U32, U64, MEM_64bits */
#include "fl2_internal.h"
#include "radix_internal.h"

#ifdef __GNUC__
#  pragma GCC diagnostic ignored "-Wmaybe-uninitialized" /* warning: 'rpt_head_next' may be used uninitialized in this function */
#elif defined(_MSC_VER)
#  pragma warning(disable : 4701)  /* disable: C4701: potentially uninitialized local variable */
#endif

#define MIN_MATCH_BUFFER_SIZE 256U /* min buffer size at least FL2_SEARCH_DEPTH_MAX + 2 for bounded build */
#define MAX_MATCH_BUFFER_SIZE (1UL << 24) /* max buffer size constrained by 24-bit link values */

#define REPEAT_CHECK_TABLE ((1 << 1) | (1 << 2) | (1 << 4) | (1 << 8) | (1UL << 16) | (1ULL << 32))

static void RMF_initTailTable(RMF_builder* const tbl)
{
    for (size_t i = 0; i < RADIX8_TABLE_SIZE; i += 2) {
        tbl->tails_8[i].prevIndex = RADIX_NULL_LINK;
        tbl->tails_8[i + 1].prevIndex = RADIX_NULL_LINK;
    }
    for (size_t i = 0; i < RADIX16_TABLE_SIZE; i += 2) {
        tbl->tails_16[i].prevIndex = RADIX_NULL_LINK;
        tbl->tails_16[i + 1].prevIndex = RADIX_NULL_LINK;
    }
}

static RMF_builder* RMF_createBuilder(size_t matchBufferSize)
{
    matchBufferSize = MIN(matchBufferSize, MAX_MATCH_BUFFER_SIZE);
    matchBufferSize = MAX(matchBufferSize, MIN_MATCH_BUFFER_SIZE);

    RMF_builder* const builder = (RMF_builder*)malloc(
        sizeof(RMF_builder) + (matchBufferSize - 1) * sizeof(RMF_buildMatch));

    if (builder == NULL)
        return NULL;

    builder->matchBufferSize = matchBufferSize;
    builder->matchBufferLimit = matchBufferSize;

    RMF_initTailTable(builder);

    return builder;
}

static void RMF_freeBuilderTable(RMF_builder** const builders, unsigned const size)
{
    if (builders == NULL)
        return;

    for (unsigned i = 0; i < size; ++i)
        free(builders[i]);

    free(builders);
}

static RMF_builder** RMF_createBuilderTable(U32* const matchTable, size_t const matchBufferSize, unsigned const max_len, unsigned const size)
{
    DEBUGLOG(3, "RMF_createBuilderTable : matchBufferSize %u, builders %u", (U32)matchBufferSize, size);

    RMF_builder** const builders = (RMF_builder**)malloc(size * sizeof(RMF_builder*));

    if (builders == NULL)
        return NULL;

    for (unsigned i = 0; i < size; ++i)
        builders[i] = NULL;

    for (unsigned i = 0; i < size; ++i) {
        builders[i] = RMF_createBuilder(matchBufferSize);
        if (builders[i] == NULL) {
            RMF_freeBuilderTable(builders, i);
            return NULL;
        }
        builders[i]->table = matchTable;
        builders[i]->max_len = max_len;
    }
    return builders;
}

static int RMF_isStruct(size_t const dictionary_size, unsigned const depth)
{
    return dictionary_size > ((size_t)1 << RADIX_LINK_BITS);
}

static int RMF_isStructParam(const RMF_parameters* const params)
{
    return RMF_isStruct(params->dictionary_size, params->depth);
}

/** RMF_clampParams() :
*  make param values within valid range.
*  @return : valid RMF_parameters */
static RMF_parameters RMF_clampParams(RMF_parameters params)
{
#   define CLAMP(val,min,max) {      \
        if (val<(min)) val=(min);        \
        else if (val>(max)) val=(max);   \
    }
    CLAMP(params.dictionary_size, DICTIONARY_SIZE_MIN, MEM_64bits() ? DICTIONARY_SIZE_MAX_64 : DICTIONARY_SIZE_MAX_32);
    CLAMP(params.match_buffer_log, FL2_BUFFER_SIZE_LOG_MIN, FL2_BUFFER_SIZE_LOG_MAX);
    if (params.overlap_fraction > FL2_BLOCK_OVERLAP_MAX)
        params.overlap_fraction = FL2_BLOCK_OVERLAP_MAX;
    CLAMP(params.depth, FL2_SEARCH_DEPTH_MIN, FL2_SEARCH_DEPTH_MAX);
    return params;
#   undef CLAMP
}

static size_t RMF_applyParameters_internal(FL2_matchTable* const tbl, const RMF_parameters* const params)
{
    int const isStruct = RMF_isStructParam(params);
    size_t const dictionary_size = tbl->params.dictionary_size;
    /* dictionary is allocated with the struct and is immutable */
    if (params->dictionary_size > tbl->params.dictionary_size
        || (params->dictionary_size == tbl->params.dictionary_size && isStruct > tbl->allocStruct))
        return FL2_ERROR(parameter_unsupported);

    size_t const matchBufferSize = params->dictionary_size >> params->match_buffer_log;
    tbl->params = *params;
    tbl->params.dictionary_size = dictionary_size;
    tbl->isStruct = isStruct;
    if (tbl->builders == NULL
        || matchBufferSize > tbl->builders[0]->matchBufferSize)
    {
        RMF_freeBuilderTable(tbl->builders, tbl->threadCount);
        tbl->builders = RMF_createBuilderTable(tbl->table, matchBufferSize, tbl->isStruct ? STRUCTURED_MAX_LENGTH : BITPACK_MAX_LENGTH, tbl->threadCount);
        if (tbl->builders == NULL) {
            return FL2_ERROR(memory_allocation);
        }
    }
    else {
        for (unsigned i = 0; i < tbl->threadCount; ++i) {
            tbl->builders[i]->matchBufferLimit = matchBufferSize;
            tbl->builders[i]->max_len = tbl->isStruct ? STRUCTURED_MAX_LENGTH : BITPACK_MAX_LENGTH;
        }
    }
    return 0;
}

static void RMF_reduceDict(RMF_parameters* const params, size_t const dict_reduce)
{
    if (dict_reduce)
        for (size_t dict_size = params->dictionary_size; dict_size > DICTIONARY_SIZE_MIN && (dict_size >> 1) >= dict_reduce; dict_size >>= 1) {
            /* Use unchanged match buffer size for reduced dict */
            params->match_buffer_log = MAX(params->match_buffer_log - 1, FL2_BUFFER_SIZE_LOG_MIN);
        }
}

static void RMF_initListHeads(FL2_matchTable* const tbl)
{
    for (size_t i = 0; i < RADIX16_TABLE_SIZE; i += 2) {
        tbl->listHeads[i].head = RADIX_NULL_LINK;
        tbl->listHeads[i].count = 0;
        tbl->listHeads[i + 1].head = RADIX_NULL_LINK;
        tbl->listHeads[i + 1].count = 0;
    }
}

FL2_matchTable* RMF_createMatchTable(const RMF_parameters* const p, size_t const dict_reduce, unsigned const threadCount)
{
    RMF_parameters params = RMF_clampParams(*p);
    RMF_reduceDict(&params, dict_reduce);

    int const isStruct = RMF_isStructParam(&params);
    size_t dictionary_size = params.dictionary_size;

    DEBUGLOG(3, "RMF_createMatchTable : isStruct %d, dict %u", isStruct, (U32)dictionary_size);

    size_t const table_bytes = isStruct ? ((dictionary_size + 3U) / 4U) * sizeof(RMF_unit)
		: dictionary_size * sizeof(U32);
    FL2_matchTable* const tbl = (FL2_matchTable*)malloc(
        sizeof(FL2_matchTable) + table_bytes - sizeof(U32));
    if (!tbl) return NULL;

    tbl->isStruct = isStruct;
    tbl->allocStruct = isStruct;
    tbl->threadCount = threadCount + !threadCount;
    tbl->params = params;
    tbl->builders = NULL;

    RMF_applyParameters_internal(tbl, &params);

    RMF_initListHeads(tbl);

    RMF_initProgress(tbl);
    
    return tbl;
}

void RMF_freeMatchTable(FL2_matchTable* const tbl)
{
    if (tbl == NULL)
        return;

    DEBUGLOG(3, "RMF_freeMatchTable");

    RMF_freeBuilderTable(tbl->builders, tbl->threadCount);
    free(tbl);
}

BYTE RMF_compatibleParameters(const FL2_matchTable* const tbl, const RMF_parameters * const p, size_t const dict_reduce)
{
    RMF_parameters params = RMF_clampParams(*p);
    RMF_reduceDict(&params, dict_reduce);
    return tbl->params.dictionary_size > params.dictionary_size
        || (tbl->params.dictionary_size == params.dictionary_size && tbl->allocStruct >= RMF_isStructParam(&params));
}

size_t RMF_applyParameters(FL2_matchTable* const tbl, const RMF_parameters* const p, size_t const dict_reduce)
{
    RMF_parameters params = RMF_clampParams(*p);
    RMF_reduceDict(&params, dict_reduce);
    return RMF_applyParameters_internal(tbl, &params);
}

size_t RMF_threadCount(const FL2_matchTable* const tbl)
{
    return tbl->threadCount;
}

void RMF_initProgress(FL2_matchTable * const tbl)
{
    if (tbl != NULL)
        tbl->progress = 0;
}

size_t RMF_initTable(FL2_matchTable* const tbl, const void* const data, size_t const start, size_t const end)
{
    DEBUGLOG(5, "RMF_initTable : start %u, size %u", (U32)start, (U32)end);

    tbl->st_index = ATOMIC_INITIAL_VALUE;

    if (tbl->isStruct)
        return RMF_structuredInit(tbl, data, start, end);
    else
        return RMF_bitpackInit(tbl, data, start, end);
}

static void RMF_handleRepeat(RMF_buildMatch* const match_buffer,
    const BYTE* const data_block,
    size_t const next,
    U32 count,
    U32 const rpt_len,
    U32 const depth,
    U32 const max_len)
{
    size_t index = next;
    U32 length = depth + rpt_len;

    const BYTE* const data = data_block + match_buffer[index].from;
    const BYTE* const data_2 = data - rpt_len;

    while (data[length] == data_2[length] && length < max_len)
        ++length;

    for (; length <= max_len && count; --count) {
        size_t next_i = match_buffer[index].next & 0xFFFFFF;
        match_buffer[index].next = (U32)next_i | (length << 24);
        length += rpt_len;
        index = next_i;
    }
    for (; count; --count) {
        size_t next_i = match_buffer[index].next & 0xFFFFFF;
        match_buffer[index].next = (U32)next_i | (max_len << 24);
        index = next_i;
    }
}

typedef struct
{
    size_t index;
    const BYTE* data_src;
	union src_data_u src;
} BruteForceMatch;

static void RMF_bruteForceBuffered(RMF_builder* const tbl,
    const BYTE* const data_block,
    size_t const block_start,
    size_t index,
    size_t const listCount,
    size_t const slot,
    size_t const depth,
    size_t const max_depth)
{
    BruteForceMatch buffer[MAX_BRUTE_FORCE_LIST_SIZE + 1];
    const BYTE* const data_src = data_block + depth;
    size_t const limit = max_depth - depth;
    const BYTE* const start = data_src + block_start;
    size_t i = 0;
    for (;;) {
        buffer[i].index = index;
        buffer[i].data_src = data_src + tbl->match_buffer[index].from;
        buffer[i].src.u32 = tbl->match_buffer[index].src.u32;

        if (++i >= listCount)
            break;

        index = tbl->match_buffer[index].next & 0xFFFFFF;
    }
    i = 0;
    do {
        size_t longest = 0;
        size_t j = i + 1;
        size_t longest_index = j;
        const BYTE* const data = buffer[i].data_src;
        do {
            size_t len_test = slot;
            while (len_test < 4 && buffer[i].src.chars[len_test] == buffer[j].src.chars[len_test] && len_test - slot < limit) {
                ++len_test;
            }
            len_test -= slot;
            if (len_test) {
                const BYTE* data_2 = buffer[j].data_src;
                while (data[len_test] == data_2[len_test] && len_test < limit) {
                    ++len_test;
                }
            }
            if (len_test > longest) {
                longest_index = j;
                longest = len_test;
                if (len_test >= limit) {
                    break;
                }
            }
        } while (++j < listCount);
        if (longest > 0) {
            index = buffer[i].index;
            tbl->match_buffer[index].next = (U32)(buffer[longest_index].index | ((depth + longest) << 24));
        }
        ++i;
    } while (i < listCount - 1 && buffer[i].data_src >= start);
}

FORCE_INLINE_TEMPLATE
void RMF_recurseListChunk_generic(RMF_builder* const tbl,
    const BYTE* const data_block,
    size_t const block_start,
    BYTE depth,
    BYTE const max_depth,
    U32 listCount,
    size_t const stack_base)
{
    /* Create an offset data buffer pointer for reading the next bytes */
    BYTE const base_depth = depth;
    size_t st_index = stack_base;
    size_t index = 0;
    ++depth;
    /* The last element is done separately and won't be copied back at the end */
    --listCount;
    do {
        size_t const radix_8 = tbl->match_buffer[index].src.chars[0];
        /* Seen this char before? */
        U32 const prev = tbl->tails_8[radix_8].prevIndex;
        if (prev != RADIX_NULL_LINK) {
            ++tbl->tails_8[radix_8].listCount;
            /* Link the previous occurrence to this one and record the new length */
            tbl->match_buffer[prev].next = (U32)index | ((U32)depth << 24);
        }
        else {
            tbl->tails_8[radix_8].listCount = 1;
            /* Add the new sub list to the stack */
            tbl->stack[st_index].head = (U32)index;
            /* This will be converted to a count at the end */
            tbl->stack[st_index].count = (U32)radix_8;
            ++st_index;
        }
        tbl->tails_8[radix_8].prevIndex = (U32)index;
        ++index;
    } while (index < listCount);

    {   /* Do the last element */
        size_t const radix_8 = tbl->match_buffer[index].src.chars[0];
        /* Nothing to do if there was no previous */
        U32 const prev = tbl->tails_8[radix_8].prevIndex;
        if (prev != RADIX_NULL_LINK) {
            ++tbl->tails_8[radix_8].listCount;
            tbl->match_buffer[prev].next = (U32)index | ((U32)depth << 24);
        }
    }
    /* Convert radix values on the stack to counts and reset any used tail slots */
    for (size_t j = stack_base; j < st_index; ++j) {
        tbl->tails_8[tbl->stack[j].count].prevIndex = RADIX_NULL_LINK;
        tbl->stack[j].count = (U32)tbl->tails_8[tbl->stack[j].count].listCount;
    }
    while (st_index > stack_base) {
        /* Pop an item off the stack */
        --st_index;
        listCount = tbl->stack[st_index].count;
        if (listCount < 2) {
            /* Nothing to match with */
            continue;
        }
        index = tbl->stack[st_index].head;
        size_t link = tbl->match_buffer[index].from;
        if (link < block_start) {
            /* Chain starts in the overlap region which is already encoded */
            continue;
        }
        /* Check stack space. The first comparison is unnecessary but it's a constant so should be faster */
        if (st_index > STACK_SIZE - RADIX8_TABLE_SIZE
            && st_index > STACK_SIZE - listCount)
        {
            /* Stack may not be able to fit all possible new items. This is very rare. */
            continue;
        }
        depth = tbl->match_buffer[index].next >> 24;
        size_t slot = (depth - base_depth) & 3;
        if (listCount <= MAX_BRUTE_FORCE_LIST_SIZE) {
            /* Quicker to use brute force, each string compared with all previous strings */
            RMF_bruteForceBuffered(tbl,
                data_block,
                block_start,
                index,
                listCount,
                slot,
                depth,
                max_depth);
            continue;
        }
        /* check for repeats at depth 4,8,16,32 etc */
        U32 const test = max_depth != 6 && ((depth & 3) == 0) && ((REPEAT_CHECK_TABLE >> ((depth >> 2) & 31)) & 1) && (max_depth >= depth + (depth >> 1));
        ++depth;
        /* Update the offset data buffer pointer */
        const BYTE* const data_src = data_block + depth;
        /* Last pass is done separately */
        if (!test && depth < max_depth) {
            size_t const prev_st_index = st_index;
            /* Last element done separately */
            --listCount;
            /* slot is the char cache index. If 3 then chars need to be loaded. */
            if (slot == 3 && max_depth != 6) do {
                size_t const radix_8 = tbl->match_buffer[index].src.chars[3];
                size_t const next_index = tbl->match_buffer[index].next & BUFFER_LINK_MASK;
                /* Pre-load the next link and data bytes to avoid waiting for RAM access */
                tbl->match_buffer[index].src.u32 = MEM_read32(data_src + link);
                size_t const next_link = tbl->match_buffer[next_index].from;
                U32 const prev = tbl->tails_8[radix_8].prevIndex;
                tbl->tails_8[radix_8].prevIndex = (U32)index;
                if (prev != RADIX_NULL_LINK) {
                    ++tbl->tails_8[radix_8].listCount;
                    tbl->match_buffer[prev].next = (U32)index | ((U32)depth << 24);
                }
                else {
                    tbl->tails_8[radix_8].listCount = 1;
                    tbl->stack[st_index].head = (U32)index;
                    tbl->stack[st_index].count = (U32)radix_8;
                    ++st_index;
                }
                index = next_index;
                link = next_link;
            } while (--listCount != 0);
            else do {
                size_t const radix_8 = tbl->match_buffer[index].src.chars[slot];
                size_t const next_index = tbl->match_buffer[index].next & BUFFER_LINK_MASK;
                /* Pre-load the next link to avoid waiting for RAM access */
                size_t const next_link = tbl->match_buffer[next_index].from;
                U32 const prev = tbl->tails_8[radix_8].prevIndex;
                tbl->tails_8[radix_8].prevIndex = (U32)index;
                if (prev != RADIX_NULL_LINK) {
                    ++tbl->tails_8[radix_8].listCount;
                    tbl->match_buffer[prev].next = (U32)index | ((U32)depth << 24);
                }
                else {
                    tbl->tails_8[radix_8].listCount = 1;
                    tbl->stack[st_index].head = (U32)index;
                    tbl->stack[st_index].count = (U32)radix_8;
                    ++st_index;
                }
                index = next_index;
                link = next_link;
            } while (--listCount != 0);

            size_t const radix_8 = tbl->match_buffer[index].src.chars[slot];
            U32 const prev = tbl->tails_8[radix_8].prevIndex;
            if (prev != RADIX_NULL_LINK) {
                if (slot == 3) {
                    tbl->match_buffer[index].src.u32 = MEM_read32(data_src + link);
                }
                ++tbl->tails_8[radix_8].listCount;
                tbl->match_buffer[prev].next = (U32)index | ((U32)depth << 24);
            }
            for (size_t j = prev_st_index; j < st_index; ++j) {
                tbl->tails_8[tbl->stack[j].count].prevIndex = RADIX_NULL_LINK;
                tbl->stack[j].count = (U32)tbl->tails_8[tbl->stack[j].count].listCount;
            }
        }
        else if (test) {
            S32 rpt = -1;
            size_t rpt_head_next;
            U32 rpt_dist = 0;
            size_t const prev_st_index = st_index;
            U32 const rpt_depth = depth - 1;
            /* Last element done separately */
            --listCount;
            do {
                size_t const radix_8 = tbl->match_buffer[index].src.chars[slot];
                size_t const next_index = tbl->match_buffer[index].next & BUFFER_LINK_MASK;
                size_t const next_link = tbl->match_buffer[next_index].from;
                if ((link - next_link) > rpt_depth) {
                    if (rpt > 0) {
                        RMF_handleRepeat(tbl->match_buffer, data_block, rpt_head_next, rpt, rpt_dist, rpt_depth, tbl->max_len);
                    }
                    rpt = -1;
                    U32 const prev = tbl->tails_8[radix_8].prevIndex;
                    tbl->tails_8[radix_8].prevIndex = (U32)index;
                    if (prev != RADIX_NULL_LINK) {
                        ++tbl->tails_8[radix_8].listCount;
                        tbl->match_buffer[prev].next = (U32)index | ((U32)depth << 24);
                    }
                    else {
                        tbl->tails_8[radix_8].listCount = 1;
                        tbl->stack[st_index].head = (U32)index;
                        tbl->stack[st_index].count = (U32)radix_8;
                        ++st_index;
                    }
                    index = next_index;
                    link = next_link;
                }
                else {
                    U32 const dist = (U32)(link - next_link);
                    if (rpt < 0 || dist != rpt_dist) {
                        if (rpt > 0) {
                            RMF_handleRepeat(tbl->match_buffer, data_block, rpt_head_next, rpt, rpt_dist, rpt_depth, tbl->max_len);
                        }
                        rpt = 0;
                        rpt_head_next = next_index;
                        rpt_dist = dist;
                        U32 const prev = tbl->tails_8[radix_8].prevIndex;
                        tbl->tails_8[radix_8].prevIndex = (U32)index;
                        if (prev != RADIX_NULL_LINK) {
                            ++tbl->tails_8[radix_8].listCount;
                            tbl->match_buffer[prev].next = (U32)index | ((U32)depth << 24);
                        }
                        else {
                            tbl->tails_8[radix_8].listCount = 1;
                            tbl->stack[st_index].head = (U32)index;
                            tbl->stack[st_index].count = (U32)radix_8;
                            ++st_index;
                        }
                    }
                    else {
                        ++rpt;
                    }
                    index = next_index;
                    link = next_link;
                }
            } while (--listCount != 0);

            if (rpt > 0)
                RMF_handleRepeat(tbl->match_buffer, data_block, rpt_head_next, rpt, rpt_dist, rpt_depth, tbl->max_len);

            size_t const radix_8 = tbl->match_buffer[index].src.chars[slot];
            U32 const prev = tbl->tails_8[radix_8].prevIndex;
            if (prev != RADIX_NULL_LINK) {
                if (slot == 3) {
                    tbl->match_buffer[index].src.u32 = MEM_read32(data_src + link);
                }
                ++tbl->tails_8[radix_8].listCount;
                tbl->match_buffer[prev].next = (U32)index | ((U32)depth << 24);
            }
            for (size_t j = prev_st_index; j < st_index; ++j) {
                tbl->tails_8[tbl->stack[j].count].prevIndex = RADIX_NULL_LINK;
                tbl->stack[j].count = (U32)tbl->tails_8[tbl->stack[j].count].listCount;
            }
        }
        else {
            size_t const prev_st_index = st_index;
            /* The last pass at max_depth */
            do {
                size_t const radix_8 = tbl->match_buffer[index].src.chars[slot];
                size_t const next_index = tbl->match_buffer[index].next & BUFFER_LINK_MASK;
                /* Pre-load the next link. */
                /* The last element in tbl->match_buffer is circular so this is never an access violation. */
                size_t const next_link = tbl->match_buffer[next_index].from;
                U32 const prev = tbl->tails_8[radix_8].prevIndex;
                tbl->tails_8[radix_8].prevIndex = (U32)index;
                if (prev != RADIX_NULL_LINK) {
                    tbl->match_buffer[prev].next = (U32)index | ((U32)depth << 24);
                }
                else {
                    tbl->stack[st_index].count = (U32)radix_8;
                    ++st_index;
                }
                index = next_index;
                link = next_link;
            } while (--listCount != 0);
            for (size_t j = prev_st_index; j < st_index; ++j) {
                tbl->tails_8[tbl->stack[j].count].prevIndex = RADIX_NULL_LINK;
            }
            st_index = prev_st_index;
        }
    }
}

void RMF_recurseListChunk(RMF_builder* const tbl,
    const BYTE* const data_block,
    size_t const block_start,
    BYTE const depth,
    BYTE const max_depth,
    U32 const listCount,
    size_t const stack_base)
{
    if (max_depth > 6)
        RMF_recurseListChunk_generic(tbl, data_block, block_start, depth, max_depth, listCount, stack_base);
    else
        RMF_recurseListChunk_generic(tbl, data_block, block_start, depth, 6, listCount, stack_base);
}

/* Iterate the head table concurrently with other threads, and recurse each list until max_depth is reached */
int RMF_buildTable(FL2_matchTable* const tbl,
	size_t const job,
    unsigned const multi_thread,
    FL2_dataBlock const block)
{
    DEBUGLOG(5, "RMF_buildTable : thread %u", (U32)job);

    if (tbl->isStruct)
        RMF_structuredBuildTable(tbl, job, multi_thread, block);
    else
        RMF_bitpackBuildTable(tbl, job, multi_thread, block);

    if (job == 0 && tbl->st_index >= RADIX_CANCEL_INDEX) {
        RMF_initListHeads(tbl);
        return 1;
    }
    return 0;
}

void RMF_cancelBuild(FL2_matchTable * const tbl)
{
    if(tbl != NULL)
        FL2_atomic_add(tbl->st_index, RADIX_CANCEL_INDEX - ATOMIC_INITIAL_VALUE);
}

void RMF_resetIncompleteBuild(FL2_matchTable * const tbl)
{
    RMF_initListHeads(tbl);
}

int RMF_integrityCheck(const FL2_matchTable* const tbl, const BYTE* const data, size_t const index, size_t const end, unsigned const max_depth)
{
    if (tbl->isStruct)
        return RMF_structuredIntegrityCheck(tbl, data, index, end, max_depth);
    else
        return RMF_bitpackIntegrityCheck(tbl, data, index, end, max_depth);
}

void RMF_limitLengths(FL2_matchTable* const tbl, size_t const index)
{
    if (tbl->isStruct)
        RMF_structuredLimitLengths(tbl, index);
    else
        RMF_bitpackLimitLengths(tbl, index);
}

BYTE* RMF_getTableAsOutputBuffer(FL2_matchTable* const tbl, size_t const index)
{
    if (tbl->isStruct)
        return RMF_structuredAsOutputBuffer(tbl, index);
    else
        return RMF_bitpackAsOutputBuffer(tbl, index);
}

size_t RMF_memoryUsage(size_t const dict_size, unsigned const buffer_log, unsigned const depth, unsigned const threadCount)
{
    size_t size = (size_t)(4U + RMF_isStruct(dict_size, depth)) * dict_size;
    size_t const buf_size = dict_size >> buffer_log;
    size += ((buf_size - 1) * sizeof(RMF_buildMatch) + sizeof(RMF_builder)) * threadCount;
    return size;
}
