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
#include "fast-lzma2.h"
#include "mem.h"          /* U32, U64, MEM_64bits */
#include "fl2_internal.h"
#include "radix_internal.h"

#ifdef __GNUC__
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized" /* warning: 'rpt_head_next' may be used uninitialized in this function */
#elif defined(_MSC_VER)
#  pragma warning(disable : 4701)  /* disable: C4701: potentially uninitialized local variable */
#endif

#define MIN_MATCH_BUFFER_SIZE 256 /* min buffer size at least FL2_SEARCH_DEPTH_MAX + 2 for bounded build */
#define MAX_MATCH_BUFFER_SIZE (1UL << 24) /* max buffer size constrained by 24-bit link values */

#define REPEAT_CHECK_TABLE ((1 << 1) | (1 << 2) | (1 << 4) | (1 << 8) | (1 << 16) | (1ULL << 32))

void RMF_initTailTable(RMF_builder* tbl)
{
    for (size_t i = 0; i < RADIX8_TABLE_SIZE; i += 2) {
        tbl->tails_8[i].prev_index = RADIX_NULL_LINK;
        tbl->tails_8[i + 1].prev_index = RADIX_NULL_LINK;
    }
    for (size_t i = 0; i < RADIX16_TABLE_SIZE; i += 2) {
        tbl->tails_16[i].prev_index = RADIX_NULL_LINK;
        tbl->tails_16[i + 1].prev_index = RADIX_NULL_LINK;
    }
}

static RMF_builder* RMF_createBuilder(U32* matchTable, size_t match_buffer_size)
{
    match_buffer_size = MIN(match_buffer_size, MAX_MATCH_BUFFER_SIZE);
    match_buffer_size = MAX(match_buffer_size, MIN_MATCH_BUFFER_SIZE);

    {   RMF_builder* const builder = (RMF_builder*)malloc(
            sizeof(RMF_builder) + (match_buffer_size - 1) * sizeof(RMF_buildMatch));
        builder->table = matchTable;
        builder->match_buffer_size = match_buffer_size;
        builder->match_buffer_limit = match_buffer_size;
        RMF_initTailTable(builder);
        return builder;
    }
}

static void RMF_freeBuilderTable(RMF_builder** builders, unsigned size)
{
    if (builders == NULL)
        return;
    for (unsigned i = 0; i < size; ++i) {
        free(builders[i]);
    }
    free(builders);
}

static RMF_builder** RMF_createBuilderTable(U32* matchTable, size_t match_buffer_size, unsigned max_len, unsigned size)
{
    DEBUGLOG(3, "RMF_createBuilderTable : match_buffer_size %u, builders %u", (U32)match_buffer_size, size);
    RMF_builder** builders = (RMF_builder**)malloc(size * sizeof(RMF_builder*));
    if (builders == NULL)
        return NULL;
    for (unsigned i = 0; i < size; ++i) {
        builders[i] = RMF_createBuilder(matchTable, match_buffer_size);
        if (builders[i] == NULL) {
            RMF_freeBuilderTable(builders, i);
            return NULL;
        }
        builders[i]->max_len = max_len;
    }
    return builders;
}

static int RMF_isStruct(RMF_parameters* params)
{
    return params->dictionary_log > RADIX_LINK_BITS || params->depth > BITPACK_MAX_LENGTH;
}

/** RMF_clampCParams() :
*  make CParam values within valid range.
*  @return : valid CParams */
static RMF_parameters RMF_clampParams(RMF_parameters params)
{
#   define CLAMP(val,min,max) {      \
        if (val<(min)) val=(min);        \
        else if (val>(max)) val=(max);   \
    }
    CLAMP(params.dictionary_log, DICTIONARY_LOG_MIN, MEM_64bits() ? DICTIONARY_LOG_MAX_64 : DICTIONARY_LOG_MAX_32);
    CLAMP(params.match_buffer_log, FL2_BUFFER_SIZE_LOG_MIN, FL2_BUFFER_SIZE_LOG_MAX);
    CLAMP(params.overlap_fraction, FL2_BLOCK_OVERLAP_MIN, FL2_BLOCK_OVERLAP_MAX);
    CLAMP(params.depth, FL2_SEARCH_DEPTH_MIN, FL2_SEARCH_DEPTH_MAX);
    return params;
}

static size_t RMF_applyParameters_internal(FL2_matchTable* tbl, RMF_parameters* params)
{
    int isStruct = RMF_isStruct(params);
    unsigned dictionary_log = tbl->params.dictionary_log;
    /* dictionary is allocated with the struct and is immutable */
    if (params->dictionary_log > tbl->params.dictionary_log
        || (params->dictionary_log == tbl->params.dictionary_log && isStruct > tbl->allocStruct))
        return FL2_ERROR(parameter_unsupported);

    {   size_t match_buffer_size = (size_t)1 << (params->dictionary_log - params->match_buffer_log);
        tbl->params = *params;
        tbl->params.dictionary_log = dictionary_log;
        tbl->isStruct = isStruct;
        if (tbl->builders == NULL
            || match_buffer_size > tbl->builders[0]->match_buffer_size)
        {
            RMF_freeBuilderTable(tbl->builders, tbl->thread_count);
            tbl->builders = RMF_createBuilderTable(tbl->table, match_buffer_size, tbl->isStruct ? STRUCTURED_MAX_LENGTH : BITPACK_MAX_LENGTH, tbl->thread_count);
            if (tbl->builders == NULL) {
                free(tbl);
                return FL2_ERROR(memory_allocation);
            }
        }
        else {
            for (unsigned i = 0; i < tbl->thread_count; ++i) {
                tbl->builders[i]->match_buffer_limit = match_buffer_size;
                tbl->builders[i]->max_len = tbl->isStruct ? STRUCTURED_MAX_LENGTH : BITPACK_MAX_LENGTH;
            }
        }
    }
    return 0;
}

static void RMF_reduceDict(RMF_parameters* params, size_t dict_reduce)
{
    if (dict_reduce)
        while (params->dictionary_log > DICTIONARY_LOG_MIN && (size_t)1 << (params->dictionary_log - 1) >= dict_reduce) {
            --params->dictionary_log;
            params->match_buffer_log = MAX(params->match_buffer_log - 1, FL2_BUFFER_SIZE_LOG_MIN);
        }
}

FL2_matchTable* RMF_createMatchTable(RMF_parameters* p, size_t dict_reduce, unsigned thread_count)
{
    int isStruct;
    size_t dictionary_size;
    RMF_parameters params = RMF_clampParams(*p);

    RMF_reduceDict(&params, dict_reduce);
    isStruct = RMF_isStruct(&params);
    dictionary_size = (size_t)1 << params.dictionary_log;

    DEBUGLOG(3, "RMF_createMatchTable : isStruct %d, dict %u", isStruct, (U32)dictionary_size);

    FL2_matchTable* const tbl = (FL2_matchTable*)malloc(
        sizeof(FL2_matchTable) + (isStruct ? dictionary_size * sizeof(RMF_unit) : dictionary_size * sizeof(U32)) - sizeof(U32));
    if (!tbl) return NULL;

    tbl->isStruct = isStruct;
    tbl->allocStruct = isStruct;
    tbl->thread_count = thread_count;
    tbl->params = params;
    tbl->params.match_buffer_log = 0;
    tbl->builders = NULL;

    RMF_applyParameters_internal(tbl, &params);

    for (size_t i = 0; i < RADIX16_TABLE_SIZE; i += 2) {
        tbl->list_heads[i].head = RADIX_NULL_LINK;
        tbl->list_heads[i].count = 0;
        tbl->list_heads[i + 1].head = RADIX_NULL_LINK;
        tbl->list_heads[i + 1].count = 0;
    }
    return tbl;
}

void RMF_freeMatchTable(FL2_matchTable* tbl)
{
    if (tbl == NULL)
        return;
    DEBUGLOG(3, "RMF_freeMatchTable");
    RMF_freeBuilderTable(tbl->builders, tbl->thread_count);
    free(tbl);
}

BYTE RMF_compatibleParameters(FL2_matchTable* tbl, RMF_parameters * p, size_t dict_reduce)
{
    RMF_parameters params = RMF_clampParams(*p);
    RMF_reduceDict(&params, dict_reduce);
    return tbl->params.dictionary_log > params.dictionary_log
        || (tbl->params.dictionary_log == params.dictionary_log && tbl->allocStruct >= RMF_isStruct(&params));
}

size_t RMF_applyParameters(FL2_matchTable* tbl, RMF_parameters* p, size_t dict_reduce)
{
    RMF_parameters params = RMF_clampParams(*p);
    RMF_reduceDict(&params, dict_reduce);
    return RMF_applyParameters_internal(tbl, &params);
}

size_t RMF_threadCount(const FL2_matchTable * tbl)
{
    return tbl->thread_count;
}

void RMF_initTable(FL2_matchTable* tbl, const void* data, size_t start, size_t end)
{
    DEBUGLOG(5, "RMF_initTable : start %u, size %u", (U32)start, (U32)end);
    if (tbl->isStruct) {
        RMF_structuredInit(tbl, data, start, end);
    }
    else {
        RMF_bitpackInit(tbl, data, start, end);
    }
}

static void HandleRepeat(RMF_buildMatch* match_buffer,
    const BYTE* const data_block,
    size_t next,
    U32 count,
    U32 rpt_len,
    U32 depth,
    U32 max_len)
{
    size_t index = next;
    size_t next_i;
    U32 length = depth + rpt_len;
    const BYTE* const data = data_block + match_buffer[index].from;
    const BYTE* const data_2 = data - rpt_len;
    while (data[length] == data_2[length] && length < max_len)
        ++length;
    for (; length <= max_len && count; --count) {
        next_i = match_buffer[index].next & 0xFFFFFF;
        match_buffer[index].next = (U32)next_i | (length << 24);
        length += rpt_len;
        index = next_i;
    }
    for (; count; --count) {
        next_i = match_buffer[index].next & 0xFFFFFF;
        match_buffer[index].next = (U32)next_i | (max_len << 24);
        index = next_i;
    }
}

typedef struct
{
    size_t index;
    const BYTE* data_src;
    BYTE chars[4];
} BruteForceMatch;

static void BruteForceBuffered(RMF_builder* const tbl,
    const BYTE* const data_block,
    size_t const block_start,
    size_t index,
    size_t list_count,
    size_t slot,
    size_t depth,
    size_t max_depth)
{
    BruteForceMatch buffer[MAX_BRUTE_FORCE_LIST_SIZE + 1];
    const BYTE* data_src = data_block + depth;
    size_t limit = max_depth - depth;
    const BYTE* start = data_src + block_start;
    size_t i = 0;
    for (;;) {
        buffer[i].index = index;
        buffer[i].data_src = data_src + tbl->match_buffer[index].from;
        *(U32*)buffer[i].chars = *(U32*)tbl->match_buffer[index].chars;
        if (++i >= list_count) {
            break;
        }
        index = tbl->match_buffer[index].next & 0xFFFFFF;
    }
    i = 0;
    do {
        size_t longest = 0;
        size_t j = i + 1;
        size_t longest_index = j;
        const BYTE* data = buffer[i].data_src;
        do {
            size_t len_test = slot;
            while (len_test < 4 && buffer[i].chars[len_test] == buffer[j].chars[len_test] && len_test - slot < limit) {
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
        } while (++j < list_count);
        if (longest > 0) {
            index = buffer[i].index;
            tbl->match_buffer[index].next = (U32)(buffer[longest_index].index | ((depth + longest) << 24));
        }
        ++i;
    } while (i < list_count - 1 && buffer[i].data_src >= start);
}

/* Copy the list into a buffer and recurse it there. This decreases cache misses and allows */
/* data characters to be loaded every second pass and stored for use in the next 2 passes */
FORCE_INLINE_TEMPLATE
void RMF_recurseListChunk_generic(RMF_builder* const tbl,
    const BYTE* const data_block,
    size_t const block_start,
    BYTE depth,
    BYTE const max_depth,
    U32 list_count,
    size_t stack_base)
{
    /* Create an offset data buffer pointer for reading the next bytes */
    const BYTE base_depth = depth;
    size_t st_index = stack_base;
    size_t index = 0;
    U32 prev;
    ++depth;
    /* The last element is done separately and won't be copied back at the end */
    --list_count;
    do {
        size_t radix_8 = tbl->match_buffer[index].chars[0];
        /* Seen this char before? */
        const U32 prev = tbl->tails_8[radix_8].prev_index;
        if (prev != RADIX_NULL_LINK) {
            ++tbl->tails_8[radix_8].list_count;
            /* Link the previous occurrence to this one and record the new length */
            tbl->match_buffer[prev].next = (U32)index | ((U32)depth << 24);
        }
        else {
            tbl->tails_8[radix_8].list_count = 1;
            /* Add the new sub list to the stack */
            tbl->stack[st_index].head = (U32)index;
            /* This will be converted to a count at the end */
            tbl->stack[st_index].count = (U32)radix_8;
            ++st_index;
        }
        tbl->tails_8[radix_8].prev_index = (U32)index;
        ++index;
    } while (index < list_count);

    {   /* Do the last element */
        size_t radix_8 = tbl->match_buffer[index].chars[0];
        /* Nothing to do if there was no previous */
        prev = tbl->tails_8[radix_8].prev_index;
        if (prev != RADIX_NULL_LINK) {
            ++tbl->tails_8[radix_8].list_count;
            tbl->match_buffer[prev].next = (U32)index | ((U32)depth << 24);
        }
    }
    /* Convert radix values on the stack to counts */
    for (size_t j = stack_base; j < st_index; ++j) {
        tbl->tails_8[tbl->stack[j].count].prev_index = RADIX_NULL_LINK;
        tbl->stack[j].count = (U32)tbl->tails_8[tbl->stack[j].count].list_count;
    }
    while (st_index > stack_base) {
        const BYTE* data_src;
        size_t link;
        size_t slot;
        U32 test;

        /* Pop an item off the stack */
        --st_index;
        list_count = tbl->stack[st_index].count;
        if (list_count < 2) {
            /* Nothing to match with */
            continue;
        }
        index = tbl->stack[st_index].head;
        link = tbl->match_buffer[index].from;
        if (link < block_start) {
            /* Chain starts in the overlap region which is already encoded */
            continue;
        }
        /* Check stack space. The first comparison is unnecessary but it's a constant so should be faster */
        if (st_index > STACK_SIZE - RADIX8_TABLE_SIZE
            && st_index > STACK_SIZE - list_count)
        {
            /* Stack may not be able to fit all possible new items. This is very rare. */
            continue;
        }
        depth = tbl->match_buffer[index].next >> 24;
        slot = (depth - base_depth) & 3;
        if (list_count <= MAX_BRUTE_FORCE_LIST_SIZE) {
            /* Quicker to use brute force, each string compared with all previous strings */
            BruteForceBuffered(tbl,
                data_block,
                block_start,
                index,
                list_count,
                slot,
                depth,
                max_depth);
            continue;
        }
        /* check for repeats at depth 4,8,16,32 etc */
        test = max_depth != 6 && ((depth & 3) == 0) && ((REPEAT_CHECK_TABLE >> ((depth >> 2) & 31)) & 1) && (max_depth >= depth + (depth >> 1));
        ++depth;
        /* Update the offset data buffer pointer */
        data_src = data_block + depth;
        /* Last pass is done separately */
        if (!test && depth < max_depth) {
            size_t prev_st_index = st_index;
            /* Last element done separately */
            --list_count;
            /* slot is the char cache index. If 3 then chars need to be loaded. */
            if (slot == 3 && max_depth != 6) do {
                const size_t radix_8 = tbl->match_buffer[index].chars[3];
                const size_t next_index = tbl->match_buffer[index].next & BUFFER_LINK_MASK;
                /* Pre-load the next link and data bytes to avoid waiting for RAM access */
                if (depth < max_depth - 2) {
                    *(U32*)tbl->match_buffer[index].chars = MEM_read32(data_src + link);
                }
                else {
                    *(U16*)tbl->match_buffer[index].chars = MEM_read16(data_src + link);
                }
                const size_t next_link = tbl->match_buffer[next_index].from;
                prev = tbl->tails_8[radix_8].prev_index;
                if (prev!=RADIX_NULL_LINK) {
                    ++tbl->tails_8[radix_8].list_count;
                    tbl->match_buffer[prev].next = (U32)index | ((U32)depth << 24);
                }
                else {
                    tbl->tails_8[radix_8].list_count = 1;
                    tbl->stack[st_index].head = (U32)index;
                    tbl->stack[st_index].count = (U32)radix_8;
                    ++st_index;
                }
                tbl->tails_8[radix_8].prev_index = (U32)index;
                index = next_index;
                link = next_link;
            } while (--list_count != 0);
            else do {
                const size_t radix_8 = tbl->match_buffer[index].chars[slot];
                const size_t next_index = tbl->match_buffer[index].next & BUFFER_LINK_MASK;
                /* Pre-load the next link to avoid waiting for RAM access */
                const size_t next_link = tbl->match_buffer[next_index].from;
                prev = tbl->tails_8[radix_8].prev_index;
                if (prev != RADIX_NULL_LINK) {
                    ++tbl->tails_8[radix_8].list_count;
                    tbl->match_buffer[prev].next = (U32)index | ((U32)depth << 24);
                }
                else {
                    tbl->tails_8[radix_8].list_count = 1;
                    tbl->stack[st_index].head = (U32)index;
                    tbl->stack[st_index].count = (U32)radix_8;
                    ++st_index;
                }
                tbl->tails_8[radix_8].prev_index = (U32)index;
                index = next_index;
                link = next_link;
            } while (--list_count != 0);

            {   const size_t radix_8 = tbl->match_buffer[index].chars[slot];
                prev = tbl->tails_8[radix_8].prev_index;
                if (prev != RADIX_NULL_LINK) {
                    if (slot == 3) {
                        *(U32*)tbl->match_buffer[index].chars = MEM_read32(data_src + link);
                    }
                    ++tbl->tails_8[radix_8].list_count;
                    tbl->match_buffer[prev].next = (U32)index | ((U32)depth << 24);
                }
            }
            for (size_t j = prev_st_index; j < st_index; ++j) {
                tbl->tails_8[tbl->stack[j].count].prev_index = RADIX_NULL_LINK;
                tbl->stack[j].count = (U32)tbl->tails_8[tbl->stack[j].count].list_count;
            }
        }
        else if (test) {
            S32 rpt = -1;
            size_t rpt_head_next;
            U32 rpt_dist = 0;
            size_t prev_st_index = st_index;
            U32 rpt_depth = depth - 1;
            /* Last element done separately */
            --list_count;
            do {
                const size_t radix_8 = tbl->match_buffer[index].chars[slot];
                const size_t next_index = tbl->match_buffer[index].next & BUFFER_LINK_MASK;
                const size_t next_link = tbl->match_buffer[next_index].from;
                if ((link - next_link) > rpt_depth) {
                    if (rpt > 0) {
                        HandleRepeat(tbl->match_buffer, data_block, rpt_head_next, rpt, rpt_dist, rpt_depth, tbl->max_len);
                    }
                    rpt = -1;
                    prev = tbl->tails_8[radix_8].prev_index;
                    if (prev != RADIX_NULL_LINK) {
                        ++tbl->tails_8[radix_8].list_count;
                        tbl->match_buffer[prev].next = (U32)index | ((U32)depth << 24);
                    }
                    else {
                        tbl->tails_8[radix_8].list_count = 1;
                        tbl->stack[st_index].head = (U32)index;
                        tbl->stack[st_index].count = (U32)radix_8;
                        ++st_index;
                    }
                    tbl->tails_8[radix_8].prev_index = (U32)index;
                    index = next_index;
                    link = next_link;
                }
                else {
                    U32 dist = (U32)(link - next_link);
                    if (rpt < 0 || dist != rpt_dist) {
                        if (rpt > 0) {
                            HandleRepeat(tbl->match_buffer, data_block, rpt_head_next, rpt, rpt_dist, rpt_depth, tbl->max_len);
                        }
                        rpt = 0;
                        rpt_head_next = next_index;
                        rpt_dist = dist;
                        prev = tbl->tails_8[radix_8].prev_index;
                        if (prev != RADIX_NULL_LINK) {
                            ++tbl->tails_8[radix_8].list_count;
                            tbl->match_buffer[prev].next = (U32)index | ((U32)depth << 24);
                        }
                        else {
                            tbl->tails_8[radix_8].list_count = 1;
                            tbl->stack[st_index].head = (U32)index;
                            tbl->stack[st_index].count = (U32)radix_8;
                            ++st_index;
                        }
                        tbl->tails_8[radix_8].prev_index = (U32)index;
                    }
                    else {
                        ++rpt;
                    }
                    index = next_index;
                    link = next_link;
                }
            } while (--list_count != 0);
            if (rpt > 0) {
                HandleRepeat(tbl->match_buffer, data_block, rpt_head_next, rpt, rpt_dist, rpt_depth, tbl->max_len);
            }

            {   const size_t radix_8 = tbl->match_buffer[index].chars[slot];
                prev = tbl->tails_8[radix_8].prev_index;
                if (prev != RADIX_NULL_LINK) {
                    if (slot == 3) {
                        *(U32*)tbl->match_buffer[index].chars = MEM_read32(data_src + link);
                    }
                    ++tbl->tails_8[radix_8].list_count;
                    tbl->match_buffer[prev].next = (U32)index | ((U32)depth << 24);
                }
            }
            for (size_t j = prev_st_index; j < st_index; ++j) {
                tbl->tails_8[tbl->stack[j].count].prev_index = RADIX_NULL_LINK;
                tbl->stack[j].count = (U32)tbl->tails_8[tbl->stack[j].count].list_count;
            }
        }
        else {
            size_t prev_st_index = st_index;
            /* The last pass at max_depth */
            do {
                const size_t radix_8 = tbl->match_buffer[index].chars[slot];
                const size_t next_index = tbl->match_buffer[index].next & BUFFER_LINK_MASK;
                /* Pre-load the next link. */
                /* The last element in tbl->match_buffer is circular so this is never an access violation. */
                const size_t next_link = tbl->match_buffer[next_index].from;
                prev = tbl->tails_8[radix_8].prev_index;
                if (prev != RADIX_NULL_LINK) {
                    tbl->match_buffer[prev].next = (U32)index | ((U32)depth << 24);
                }
                else {
                    tbl->stack[st_index].count = (U32)radix_8;
                    ++st_index;
                }
                tbl->tails_8[radix_8].prev_index = (U32)index;
                index = next_index;
                link = next_link;
            } while (--list_count != 0);
            for (size_t j = prev_st_index; j < st_index; ++j) {
                tbl->tails_8[tbl->stack[j].count].prev_index = RADIX_NULL_LINK;
            }
            st_index = prev_st_index;
        }
    }
}

void RMF_recurseListChunk(RMF_builder* const tbl,
    const BYTE* const data_block,
    size_t const block_start,
    BYTE depth,
    BYTE max_depth,
    U32 list_count,
    size_t stack_base)
{
    if (max_depth > 6) {
        RMF_recurseListChunk_generic(tbl, data_block, block_start, depth, max_depth, list_count, stack_base);
    }
    else {
        RMF_recurseListChunk_generic(tbl, data_block, block_start, depth, 6, list_count, stack_base);
    }
}

/* Iterate the head table concurrently with other threads, and recurse each list until max_depth is reached */
void RMF_buildTable(FL2_matchTable* const tbl,
    unsigned job,
    unsigned multi_thread,
    const void* src,
    size_t const block_start,
    size_t const block_size)
{
    DEBUGLOG(5, "RMF_buildTable : thread %u", job);
    if (tbl->isStruct) {
        RMF_structuredBuildTable(tbl, job, multi_thread, src, block_start, block_size);
    }
    else {
        RMF_bitpackBuildTable(tbl, job, multi_thread, src, block_start, block_size);
    }
}

int RMF_integrityCheck(const FL2_matchTable* tbl, const BYTE* data, size_t index, size_t end, unsigned max_depth)
{
    if (tbl->isStruct) {
        return RMF_structuredIntegrityCheck(tbl, data, index, end, max_depth);
    }
    else {
        return RMF_bitpackIntegrityCheck(tbl, data, index, end, max_depth);
    }
}

size_t RMF_getMatch(FL2_matchTable* tbl,
    const BYTE* const data,
    size_t index,
    size_t limit,
    unsigned max_depth,
    size_t* offsetPtr)
{
    if (tbl->isStruct) {
        return RMF_structuredGetMatch(tbl, data, index, limit, max_depth, offsetPtr);
    }
    else {
        return RMF_bitpackGetMatch(tbl, data, index, limit, max_depth, offsetPtr);
    }
}

void RMF_limitLengths(FL2_matchTable* tbl, size_t index)
{
    if (tbl->isStruct) {
        RMF_structuredLimitLengths(tbl, index);
    }
    else {
        RMF_bitpackLimitLengths(tbl, index);
    }
}

BYTE* RMF_getTableAsOutputBuffer(FL2_matchTable* const tbl, size_t index)
{
    if (tbl->isStruct) {
        return RMF_structuredAsOutputBuffer(tbl, index);
    }
    else {
        return RMF_bitpackAsOutputBuffer(tbl, index);
    }
}
