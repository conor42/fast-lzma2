/*
* Copyright (c) 2018, Conor McCarthy
* All rights reserved.
*
* This source code is licensed under both the BSD-style license (found in the
* LICENSE file in the root directory of this source tree) and the GPLv2 (found
* in the COPYING file in the root directory of this source tree).
* You may select, at your option, one of the above-listed licenses.
*/

#ifndef RADIX_MF_H
#define RADIX_MF_H

#if defined (__cplusplus)
extern "C" {
#endif

typedef struct FL2_matchTable_s FL2_matchTable;

#define OVERLAP_FROM_DICT_LOG(d, o) (((size_t)1 << ((d) - 4)) * (o))

#define RMF_MIN_BYTES_PER_THREAD 1024

typedef struct
{
    unsigned dictionary_log;
    unsigned match_buffer_log;
    unsigned overlap_fraction;
    unsigned divide_and_conquer;
    unsigned depth;
#ifdef RMF_REFERENCE
    unsigned use_ref_mf;
#endif
} RMF_parameters;

FL2_matchTable* RMF_createMatchTable(RMF_parameters* params, size_t dict_reduce, unsigned thread_count);
void RMF_freeMatchTable(FL2_matchTable* tbl);
BYTE RMF_compatibleParameters(FL2_matchTable* tbl, RMF_parameters* params, size_t dict_reduce);
size_t RMF_applyParameters(FL2_matchTable* tbl, RMF_parameters* params, size_t dict_reduce);
size_t RMF_threadCount(const FL2_matchTable * tbl);
void RMF_initTable(FL2_matchTable* tbl, const void* data, size_t start, size_t end);
void RMF_buildTable(FL2_matchTable* const tbl,
    unsigned job,
    unsigned multi_thread,
    const void* src,
    size_t const block_start,
    size_t const block_size);
int RMF_integrityCheck(const FL2_matchTable* tbl, const BYTE* data, size_t index, size_t end, unsigned max_depth);
void RMF_limitLengths(FL2_matchTable* tbl, size_t index);
BYTE* RMF_getTableAsOutputBuffer(FL2_matchTable* const tbl, size_t index);

#if defined (__cplusplus)
}
#endif

#endif /* RADIX_MF_H */