/*
* Copyright (c) 2018, Conor McCarthy
* All rights reserved.
* Parts based on zstd_compress.c copyright Yann Collet
*
* This source code is licensed under both the BSD-style license (found in the
* LICENSE file in the root directory of this source tree) and the GPLv2 (found
* in the COPYING file in the root directory of this source tree).
* You may select, at your option, one of the above-listed licenses.
*/

#include <string.h>
#include "fast-lzma2.h"
#include "fl2_internal.h"
#include "platform.h"
#include "mem.h"
#include "util.h"
#include "fl2_compress_internal.h"
#include "fl2_threading.h"
#include "fl2_pool.h"
#include "radix_mf.h"
#include "lzma2_enc.h"

#define FL2_MAX_LOOPS 10U

/*-=====  Pre-defined compression levels  =====-*/

#define MB *(1U<<20)

#define FL2_MAX_HIGH_CLEVEL 9

#ifdef FL2_XZ_BUILD

#define FL2_CLEVEL_DEFAULT  6
#define FL2_MAX_CLEVEL      9

static const FL2_compressionParameters FL2_defaultCParameters[FL2_MAX_CLEVEL + 1] = {
    { 0,0,0,0,0,0,0,0,0 },
    { 1 MB, 1, 7, 0, 6, 32, 1, 4, FL2_fast }, /* 1 */
    { 2 MB, 2, 7, 0, 14, 32, 1, 4, FL2_fast }, /* 2 */
    { 2 MB, 2, 7, 0, 14, 40, 1, 4, FL2_opt }, /* 3 */
    { 4 MB, 2, 7, 0, 26, 40, 1, 4, FL2_opt }, /* 4 */
    { 16 MB, 2, 8, 0, 42, 48, 1, 4, FL2_opt }, /* 5 */
    { 16 MB, 2, 9, 0, 42, 48, 1, 4, FL2_ultra }, /* 6 */
    { 32 MB, 2, 10, 0, 50, 64, 1, 4, FL2_ultra }, /* 7 */
    { 64 MB, 2, 11, 1, 60, 96, 1, 3, FL2_ultra }, /* 8 */
    { 64 MB, 3, 12, 2, 126, 160, 1, 3, FL2_ultra }, /* 9 */
};

#elif defined(FL2_7ZIP_BUILD)

#define FL2_CLEVEL_DEFAULT  5
#define FL2_MAX_CLEVEL      9

static const FL2_compressionParameters FL2_defaultCParameters[FL2_MAX_CLEVEL + 1] = {
    { 0,0,0,0,0,0,0,0,0 },
    { 1 MB, 1, 7, 0, 6, 32, 1, 4, FL2_fast }, /* 1 */
    { 1 MB, 2, 7, 0, 12, 32, 1, 4, FL2_fast }, /* 2 */
    { 2 MB, 2, 7, 0, 16, 32, 1, 4, FL2_fast }, /* 3 */
    { 1 MB, 2, 7, 0, 16, 32, 1, 4, FL2_opt }, /* 4 */
    { 16 MB, 2, 9, 0, 40, 48, 1, 4, FL2_ultra }, /* 5 */
    { 32 MB, 2, 10, 0, 48, 64, 1, 4, FL2_ultra }, /* 6 */
    { 64 MB, 2, 11, 1, 60, 96, 1, 3, FL2_ultra }, /* 7 */
    { 128 MB, 2, 12, 2, 128, 128, 1, 2, FL2_ultra }, /* 8 */
    { 128 MB, 3, 14, 3, 252, 160, 0, 2, FL2_ultra } /* 9 */
};

#else

#define FL2_CLEVEL_DEFAULT   9
#define FL2_MAX_CLEVEL      12

static const FL2_compressionParameters FL2_defaultCParameters[FL2_MAX_CLEVEL + 1] = {
    { 0,0,0,0,0,0,0,0,0 },
    { 1 MB, 1, 7, 0, 6, 32, 1, 4, FL2_fast }, /* 1 */
    { 1 MB, 2, 7, 0, 12, 32, 1, 4, FL2_fast }, /* 2 */
    { 2 MB, 2, 7, 0, 14, 32, 1, 4, FL2_fast }, /* 3 */
    { 1 MB, 2, 7, 0, 12, 32, 1, 4, FL2_opt }, /* 4 */
    { 2 MB, 2, 7, 0, 14, 40, 1, 4, FL2_opt }, /* 5 */
    { 4 MB, 2, 7, 0, 26, 40, 1, 4, FL2_opt }, /* 6 */
    { 8 MB, 2, 8, 0, 42, 48, 1, 4, FL2_opt }, /* 7 */
    { 16 MB, 2, 9, 0, 42, 48, 1, 4, FL2_ultra }, /* 8 */
    { 32 MB, 2, 10, 0, 50, 64, 1, 4, FL2_ultra }, /* 9 */
    { 64 MB, 2, 11, 1, 60, 64, 1, 3, FL2_ultra }, /* 10 */
    { 128 MB, 2, 12, 2, 126, 96, 1, 2, FL2_ultra }, /* 11 */
    { 256 MB, 2, 14, 3, 254, 160, 1, 2, FL2_ultra } /* 12 */
};

#endif

static const FL2_compressionParameters FL2_highCParameters[FL2_MAX_HIGH_CLEVEL + 1] = {
    { 0,0,0,0,0,0,0,0,0 },
    { 1 MB, 3, 9, 2, 254, 273, 0, 4, FL2_ultra }, /* 1 */
    { 2 MB, 3, 10, 2, 254, 273, 0, 4, FL2_ultra }, /* 2 */
    { 4 MB, 3, 11, 2, 254, 273, 0, 4, FL2_ultra }, /* 3 */
    { 8 MB, 3, 12, 2, 254, 273, 0, 4, FL2_ultra }, /* 4 */
    { 16 MB, 3, 13, 3, 254, 273, 0, 4, FL2_ultra }, /* 5 */
    { 32 MB, 3, 14, 3, 254, 273, 0, 4, FL2_ultra }, /* 6 */
    { 64 MB, 3, 14, 4, 254, 273, 0, 4, FL2_ultra }, /* 7 */
    { 128 MB, 3, 14, 4, 254, 273, 0, 4, FL2_ultra }, /* 8 */
    { 256 MB, 3, 14, 5, 254, 273, 0, 3, FL2_ultra } /* 9 */
};

#undef MB

FL2LIB_API int FL2LIB_CALL FL2_maxCLevel(void)
{
    return FL2_MAX_CLEVEL;
}

FL2LIB_API int FL2LIB_CALL FL2_maxHighCLevel(void)
{
    return FL2_MAX_HIGH_CLEVEL;
}

static void FL2_fillParameters(FL2_CCtx* const cctx, const FL2_compressionParameters* const params)
{
    FL2_lzma2Parameters* const cParams = &cctx->params.cParams;
    cParams->lc = 3;
    cParams->lp = 0;
    cParams->pb = 2;
    cParams->fast_length = params->fastLength;
    cParams->match_cycles = 1U << params->searchLog;
    cParams->strategy = params->strategy;
    cParams->second_dict_bits = params->chainLog;

    RMF_parameters* const rParams = &cctx->params.rParams;
    rParams->dictionary_size = MIN(params->dictionarySize, FL2_DICTSIZE_MAX); /* allows for reduced dict in 32-bit version */
    rParams->match_buffer_log = RMF_BUFFER_LOG_BASE - params->bufferLog;
    rParams->overlap_fraction = params->overlapFraction;
    rParams->divide_and_conquer = params->divideAndConquer;
    rParams->depth = params->searchDepth;
}

static FL2_CCtx* FL2_createCCtx_internal(unsigned nbThreads, int const dualBuffer)
{
    nbThreads = FL2_checkNbThreads(nbThreads);

    DEBUGLOG(3, "FL2_createCCtxMt : %u threads", nbThreads);

    FL2_CCtx* const cctx = malloc(sizeof(FL2_CCtx) + (nbThreads - 1) * sizeof(FL2_job));
    if (cctx == NULL)
        return NULL;

    cctx->jobCount = nbThreads;
    for (unsigned u = 0; u < nbThreads; ++u)
        cctx->jobs[u].enc = NULL;

    cctx->params.highCompression = 0;
#ifndef NO_XXHASH
    cctx->params.doXXH = 1;
#endif
    cctx->params.omitProp = 0;

#ifdef RMF_REFERENCE
    cctx->params.rParams.use_ref_mf = 0;
#endif

    cctx->matchTable = NULL;

    cctx->compressThread = NULL;

#ifndef FL2_SINGLETHREAD
    cctx->factory = FL2POOL_create(nbThreads - 1);
    if (nbThreads > 1 && cctx->factory == NULL) {
        FL2_freeCCtx(cctx);
        return NULL;
    }
    if (dualBuffer) {
      cctx->compressThread = FL2POOL_create(1);
      if (cctx->compressThread == NULL)
        return NULL;
    }
#endif

    for (unsigned u = 0; u < nbThreads; ++u) {
        cctx->jobs[u].enc = LZMA2_createECtx();
        if (cctx->jobs[u].enc == NULL) {
            FL2_freeCCtx(cctx);
            return NULL;
        }
        cctx->jobs[u].cctx = cctx;
    }
    cctx->dictMax = 0;
    cctx->blockTotal = 0;
    cctx->streamTotal = 0;
    cctx->outThread = 0;
    cctx->outPos = 0;
    cctx->asyncRes = 0;
    cctx->threadCount = 0;
    cctx->timeout = 0;

    DICT_construct(&cctx->buf, dualBuffer);

    cctx->endMarked = 0;
    cctx->wroteProp = 0;
    cctx->lockParams = 0;

    FL2_CCtx_setParameter(cctx, FL2_p_compressionLevel, FL2_CLEVEL_DEFAULT);
    cctx->params.cParams.reset_interval = 4;

    return cctx;
}

FL2LIB_API FL2_CCtx* FL2LIB_CALL FL2_createCCtx(void)
{
    return FL2_createCCtx_internal(1, 0);
}

FL2LIB_API FL2_CCtx* FL2LIB_CALL FL2_createCCtxMt(unsigned nbThreads)
{
    return FL2_createCCtx_internal(nbThreads, 0);
}

FL2LIB_API void FL2LIB_CALL FL2_freeCCtx(FL2_CCtx* cctx)
{
    if (cctx == NULL) 
        return;

    DEBUGLOG(3, "FL2_freeCCtx : %u threads", cctx->jobCount);

    DICT_destruct(&cctx->buf);

    for (unsigned u = 0; u < cctx->jobCount; ++u) {
        LZMA2_freeECtx(cctx->jobs[u].enc);
    }

#ifndef FL2_SINGLETHREAD
    FL2POOL_free(cctx->factory);
#endif
    FL2POOL_free(cctx->compressThread);

    RMF_freeMatchTable(cctx->matchTable);
    free(cctx);
}

FL2LIB_API unsigned FL2LIB_CALL FL2_getCCtxThreadCount(const FL2_CCtx* cctx)
{
    return cctx->jobCount;
}

/* FL2_buildRadixTable() : FL2POOL_function type */
static void FL2_buildRadixTable(void* const jobDescription, ptrdiff_t const n)
{
    FL2_CCtx* const cctx = (FL2_CCtx*)jobDescription;

    RMF_buildTable(cctx->matchTable, n, 1, cctx->curBlock);
}

/* FL2_compressRadixChunk() : FL2POOL_function type */
static void FL2_compressRadixChunk(void* const jobDescription, ptrdiff_t const n)
{
    FL2_CCtx* const cctx = (FL2_CCtx*)jobDescription;

    cctx->jobs[n].cSize = LZMA2_encode(cctx->jobs[n].enc, cctx->matchTable,
        cctx->jobs[n].block,
        &cctx->params.cParams,
        -1,
        &cctx->encProgress, &cctx->canceled);
}

static int FL2_initEncoders(FL2_CCtx* const cctx)
{
    for(unsigned u = 0; u < cctx->jobCount; ++u) {
        if (LZMA2_hashAlloc(cctx->jobs[u].enc, &cctx->params.cParams) != 0)
            return 1;
    }
    return 0;
}

static void FL2_initProgress(FL2_CCtx* const cctx)
{
    RMF_initProgress(cctx->matchTable);
    cctx->encProgress = 0;
    cctx->canceled = 0;
}

static size_t FL2_compressCurBlock_blocking(FL2_CCtx* const cctx, int const streamProp)
{
    size_t const encodeSize = (cctx->curBlock.end - cctx->curBlock.start);
#ifndef FL2_SINGLETHREAD
    size_t mfThreads = cctx->curBlock.end / RMF_MIN_BYTES_PER_THREAD;
    size_t nbThreads = MIN(cctx->jobCount, encodeSize / ENC_MIN_BYTES_PER_THREAD);
    nbThreads += !nbThreads;
#else
    size_t mfThreads = 1;
    size_t nbThreads = 1;
#endif

    DEBUGLOG(5, "FL2_compressCurBlock : %u threads, %u start, %u bytes", (U32)nbThreads, (U32)cctx->curBlock.start, (U32)encodeSize);

    size_t sliceStart = cctx->curBlock.start;
    size_t const sliceSize = encodeSize / nbThreads;
    cctx->jobs[0].block.data = cctx->curBlock.data;
    cctx->jobs[0].block.start = sliceStart;
    cctx->jobs[0].block.end = sliceStart + sliceSize;

    for (size_t u = 1; u < nbThreads; ++u) {
        sliceStart += sliceSize;
        cctx->jobs[u].block.data = cctx->curBlock.data;
        cctx->jobs[u].block.start = sliceStart;
        cctx->jobs[u].block.end = sliceStart + sliceSize;
    }
    cctx->jobs[nbThreads - 1].block.end = cctx->curBlock.end;

    /* initialize to length 2 */
    cctx->matchTable->progress = RMF_initTable(cctx->matchTable, cctx->curBlock.data, cctx->curBlock.start, cctx->curBlock.end);

    if (cctx->canceled) {
        RMF_resetIncompleteBuild(cctx->matchTable);
        return FL2_ERROR(canceled);
    }

#ifndef FL2_SINGLETHREAD

    mfThreads = MIN(RMF_threadCount(cctx->matchTable), mfThreads);
    FL2POOL_addRange(cctx->factory, FL2_buildRadixTable, cctx, 1, mfThreads);

#endif

    int err = RMF_buildTable(cctx->matchTable, 0, mfThreads > 1, cctx->curBlock);

#ifndef FL2_SINGLETHREAD

    FL2POOL_waitAll(cctx->factory, 0);

    if (err)
        return FL2_ERROR(canceled);

#ifdef RMF_CHECK_INTEGRITY
    err = RMF_integrityCheck(cctx->matchTable, cctx->curBlock.data, cctx->curBlock.start, cctx->curBlock.end, cctx->params.rParams.depth);
    if (err)
        return FL2_ERROR(internal);
#endif

    FL2POOL_addRange(cctx->factory, FL2_compressRadixChunk, cctx, 1, nbThreads);

    cctx->jobs[0].cSize = LZMA2_encode(cctx->jobs[0].enc, cctx->matchTable, cctx->jobs[0].block, &cctx->params.cParams, streamProp, &cctx->encProgress, &cctx->canceled);

    FL2POOL_waitAll(cctx->factory, 0);

#else /* FL2_SINGLETHREAD */

    if (err)
        return FL2_ERROR(canceled);

#ifdef RMF_CHECK_INTEGRITY
    err = RMF_integrityCheck(cctx->matchTable, cctx->curBlock.data, cctx->curBlock.start, cctx->curBlock.end, cctx->params.rParams.depth);
    if (err)
        return FL2_ERROR(internal);
#endif
    cctx->jobs[0].cSize = LZMA2_encode(cctx->jobs[0].enc, cctx->matchTable, cctx->jobs[0].block, &cctx->params.cParams, streamProp, &cctx->encProgress, &cctx->canceled);

#endif

    for (size_t u = 0; u < nbThreads; ++u)
        if (FL2_isError(cctx->jobs[u].cSize))
            return cctx->jobs[u].cSize;

    cctx->threadCount = nbThreads;

    return FL2_error_no_error;
}

/* FL2_compressCurBlock_async() : FL2POOL_function type */
static void FL2_compressCurBlock_async(void* const jobDescription, ptrdiff_t const n)
{
    FL2_CCtx* const cctx = (FL2_CCtx*)jobDescription;

    cctx->asyncRes = FL2_compressCurBlock_blocking(cctx, (int)n);
}

static size_t FL2_compressCurBlock(FL2_CCtx* const cctx, int const streamProp)
{
    FL2_initProgress(cctx);

    if (cctx->curBlock.start == cctx->curBlock.end)
        return FL2_error_no_error;

    /* update largest dict size used */
    cctx->dictMax = MAX(cctx->dictMax, cctx->curBlock.end);

    cctx->blockTotal += cctx->curBlock.end - cctx->curBlock.start;

    cctx->outThread = 0;
    cctx->threadCount = 0;
    cctx->outPos = 0;

    U32 rmfWeight = ZSTD_highbit32((U32)cctx->curBlock.end);
    U32 depthWeight = 2 + (cctx->params.rParams.depth >= 12) + (cctx->params.rParams.depth >= 28);
    U32 encWeight;

    if (rmfWeight >= 20) {
        rmfWeight = depthWeight * (rmfWeight - 10) + (rmfWeight - 19) * 12;
        if (cctx->params.cParams.strategy == 0)
            encWeight = 20;
        else if (cctx->params.cParams.strategy == 1)
            encWeight = 50;
        else
            encWeight = 60 + cctx->params.cParams.second_dict_bits + ZSTD_highbit32(cctx->params.cParams.fast_length) * 3U;
        rmfWeight = (rmfWeight << 4) / (rmfWeight + encWeight);
        encWeight = 16 - rmfWeight;
    }
    else {
        rmfWeight = 8;
        encWeight = 8;
    }

    cctx->rmfWeight = rmfWeight;
    cctx->encWeight = encWeight;

    if(cctx->compressThread != NULL)
        FL2POOL_add(cctx->compressThread, FL2_compressCurBlock_async, cctx, streamProp);
    else
        cctx->asyncRes = FL2_compressCurBlock_blocking(cctx, streamProp);

    return cctx->asyncRes;
}

static BYTE FL2_getProp(FL2_CCtx* const cctx, size_t const dictionarySize)
{
#ifndef NO_XXHASH
    return LZMA2_getDictSizeProp(dictionarySize) | (BYTE)((cctx->params.doXXH != 0) << FL2_PROP_HASH_BIT);
#else
    (void)cctx;
    return LZMA2_getDictSizeProp(dictionarySize);
#endif
}

static void FL2_preBeginFrame(FL2_CCtx* const cctx, size_t const dictReduce)
{
    /* Free unsuitable match table before reallocating anything else */
    if (cctx->matchTable && !RMF_compatibleParameters(cctx->matchTable, &cctx->params.rParams, dictReduce)) {
        RMF_freeMatchTable(cctx->matchTable);
        cctx->matchTable = NULL;
    }
}

static size_t FL2_beginFrame(FL2_CCtx* const cctx, size_t const dictReduce)
{
    if (FL2_initEncoders(cctx) != 0) /* Create hash objects together, leaving the (large) match table last */
        return FL2_ERROR(memory_allocation);

    if (!cctx->matchTable) {
        cctx->matchTable = RMF_createMatchTable(&cctx->params.rParams, dictReduce, cctx->jobCount);
        if (cctx->matchTable == NULL)
            return FL2_ERROR(memory_allocation);
    }
    else {
        DEBUGLOG(5, "Have compatible match table");
        RMF_applyParameters(cctx->matchTable, &cctx->params.rParams, dictReduce);
    }

    cctx->dictMax = 0;
    cctx->blockTotal = 0;
    cctx->streamTotal = 0;
    cctx->encProgress = 0;
    RMF_initProgress(cctx->matchTable);
    cctx->asyncRes = 0;
    cctx->outThread = 0;
    cctx->threadCount = 0;
    cctx->outPos = 0;
    cctx->curBlock.start = 0;
    cctx->curBlock.end = 0;
    cctx->lockParams = 1;

    return FL2_error_no_error;
}

static void FL2_endFrame(FL2_CCtx* const cctx)
{
    cctx->dictMax = 0;
    cctx->asyncRes = 0;
    cctx->lockParams = 0;
}

static size_t FL2_compressBlock(FL2_CCtx* const cctx,
    const void* const src, size_t srcSize,
    void* const dst, size_t dstCapacity)
{
    if (srcSize == 0)
        return 0;

    BYTE* dstBuf = dst;
    size_t const dictionarySize = cctx->params.rParams.dictionary_size;
    size_t const blockOverlap = OVERLAP_FROM_DICT_SIZE(dictionarySize, cctx->params.rParams.overlap_fraction);
    int streamProp = cctx->params.omitProp ? -1 : FL2_getProp(cctx, MIN(srcSize, dictionarySize));

    cctx->curBlock.data = src;
    cctx->curBlock.start = 0;

    do {
        cctx->curBlock.end = cctx->curBlock.start + MIN(srcSize, dictionarySize - cctx->curBlock.start);

        CHECK_F(FL2_compressCurBlock(cctx, streamProp));

        streamProp = -1;

        for (size_t u = 0; u < cctx->threadCount; ++u) {
            DEBUGLOG(5, "Write thread %u : %u bytes", (U32)u, (U32)cctx->jobs[u].cSize);

            if (dstCapacity < cctx->jobs[u].cSize) 
                return FL2_ERROR(dstSize_tooSmall);

            const BYTE* const outBuf = RMF_getTableAsOutputBuffer(cctx->matchTable, cctx->jobs[u].block.start);
            memcpy(dstBuf, outBuf, cctx->jobs[u].cSize);

            dstBuf += cctx->jobs[u].cSize;
            dstCapacity -= cctx->jobs[u].cSize;
        }
        srcSize -= cctx->curBlock.end - cctx->curBlock.start;
        if (cctx->params.cParams.reset_interval
            && cctx->blockTotal + MIN(cctx->curBlock.end - blockOverlap, srcSize) > dictionarySize * cctx->params.cParams.reset_interval) {
            /* periodically reset the dictionary for mt decompression */
            cctx->curBlock.start = 0;
            cctx->blockTotal = 0;
        }
        else {
            cctx->curBlock.start = blockOverlap;
        }
        cctx->curBlock.data += cctx->curBlock.end - cctx->curBlock.start;
    } while (srcSize != 0);
    return dstBuf - (const BYTE*)dst;
}

FL2LIB_API size_t FL2LIB_CALL FL2_compressCCtx(FL2_CCtx* cctx,
    void* dst, size_t dstCapacity,
    const void* src, size_t srcSize,
    int compressionLevel)
{
    if (dstCapacity < 2U - cctx->params.omitProp) /* empty LZMA2 stream is byte sequence {0, 0} */
        return FL2_ERROR(dstSize_tooSmall);

    if (compressionLevel > 0)
        FL2_CCtx_setParameter(cctx, FL2_p_compressionLevel, compressionLevel);

    DEBUGLOG(4, "FL2_compressCCtx : level %u, %u src => %u avail", cctx->params.compressionLevel, (U32)srcSize, (U32)dstCapacity);

    /* No async compression for in-memory function */
    FL2POOL_free(cctx->compressThread);
    cctx->compressThread = NULL;
    cctx->timeout = 0;

    FL2_preBeginFrame(cctx, srcSize);
    CHECK_F(FL2_beginFrame(cctx, srcSize));

    size_t const cSize = FL2_compressBlock(cctx, src, srcSize, dst, dstCapacity);

    if (FL2_isError(cSize))
        return cSize;

    BYTE* dstBuf = dst;
    BYTE* const end = dstBuf + dstCapacity;

    dstBuf += cSize;
    if(dstBuf >= end)
        return FL2_ERROR(dstSize_tooSmall);

    if (cSize == 0)
        *dstBuf++ = FL2_getProp(cctx, 0);

    *dstBuf++ = LZMA2_END_MARKER;

#ifndef NO_XXHASH
    if (cctx->params.doXXH && !cctx->params.omitProp) {
        XXH32_canonical_t canonical;
        DEBUGLOG(5, "Writing hash");
        if(end - dstBuf < XXHASH_SIZEOF)
            return FL2_ERROR(dstSize_tooSmall);
        XXH32_canonicalFromHash(&canonical, XXH32(src, srcSize, 0));
        memcpy(dstBuf, &canonical, XXHASH_SIZEOF);
        dstBuf += XXHASH_SIZEOF;
    }
#endif
    
    FL2_endFrame(cctx);

    return dstBuf - (BYTE*)dst;
}

FL2LIB_API size_t FL2LIB_CALL FL2_compressMt(void* dst, size_t dstCapacity,
    const void* src, size_t srcSize,
    int compressionLevel,
    unsigned nbThreads)
{
    FL2_CCtx* const cctx = FL2_createCCtxMt(nbThreads);
    if (cctx == NULL)
        return FL2_ERROR(memory_allocation);

    size_t const cSize = FL2_compressCCtx(cctx, dst, dstCapacity, src, srcSize, compressionLevel);

    FL2_freeCCtx(cctx);

    return cSize;
}

FL2LIB_API size_t FL2LIB_CALL FL2_compress(void* dst, size_t dstCapacity,
    const void* src, size_t srcSize,
    int compressionLevel)
{
    return FL2_compressMt(dst, dstCapacity, src, srcSize, compressionLevel, 1);
}

FL2LIB_API BYTE FL2LIB_CALL FL2_getCCtxDictProp(FL2_CCtx* cctx)
{
    return LZMA2_getDictSizeProp(cctx->dictMax ? cctx->dictMax : cctx->params.rParams.dictionary_size);
}

#define MAXCHECK(val,max) do {            \
    if ((val)>(max)) {     \
        return FL2_ERROR(parameter_outOfBound);  \
}   } while(0)

#define CLAMPCHECK(val,min,max) do {            \
    if (((val)<(min)) | ((val)>(max))) {     \
        return FL2_ERROR(parameter_outOfBound);  \
}   } while(0)


FL2LIB_API size_t FL2LIB_CALL FL2_CCtx_setParameter(FL2_CCtx* cctx, FL2_cParameter param, size_t value)
{
    if (cctx->lockParams
        && param != FL2_p_literalCtxBits && param != FL2_p_literalPosBits && param != FL2_p_posBits)
        return FL2_ERROR(stage_wrong);

    switch (param)
    {
    case FL2_p_compressionLevel:
        if (cctx->params.highCompression) {
            CLAMPCHECK(value, 1, FL2_MAX_HIGH_CLEVEL);
            FL2_fillParameters(cctx, &FL2_highCParameters[value]);
        }
        else {
            CLAMPCHECK(value, 1, FL2_MAX_CLEVEL);
            FL2_fillParameters(cctx, &FL2_defaultCParameters[value]);
        }
        cctx->params.compressionLevel = (unsigned)value;
        break;

    case FL2_p_highCompression:
        cctx->params.highCompression = value != 0;
        FL2_CCtx_setParameter(cctx, FL2_p_compressionLevel, cctx->params.compressionLevel);
        break;

    case FL2_p_dictionaryLog:
        CLAMPCHECK(value, FL2_DICTLOG_MIN, FL2_DICTLOG_MAX);
        cctx->params.rParams.dictionary_size = (size_t)1 << value;
        break;

    case FL2_p_dictionarySize:
        CLAMPCHECK(value, FL2_DICTSIZE_MIN, FL2_DICTSIZE_MAX);
        cctx->params.rParams.dictionary_size = value;
        break;

    case FL2_p_overlapFraction:
        MAXCHECK(value, FL2_BLOCK_OVERLAP_MAX);
        cctx->params.rParams.overlap_fraction = (unsigned)value;
        break;

    case FL2_p_resetInterval:
        if (value != 0)
            CLAMPCHECK(value, FL2_BLOCK_MUL_MIN, FL2_BLOCK_MUL_MAX);
        cctx->params.cParams.reset_interval = (unsigned)value;
        break;

    case FL2_p_bufferLog:
        MAXCHECK(value, RMF_BUFFER_LOG_BASE - FL2_BUFFER_SIZE_LOG_MIN);
        cctx->params.rParams.match_buffer_log = RMF_BUFFER_LOG_BASE - (unsigned)value;
        break;

    case FL2_p_chainLog:
        CLAMPCHECK(value, FL2_CHAINLOG_MIN, FL2_CHAINLOG_MAX);
        cctx->params.cParams.second_dict_bits = (unsigned)value;
        break;

    case FL2_p_searchLog:
        MAXCHECK(value, FL2_SEARCHLOG_MAX);
        cctx->params.cParams.match_cycles = 1U << value;
        break;

    case FL2_p_searchDepth:
        CLAMPCHECK(value, FL2_SEARCH_DEPTH_MIN, FL2_SEARCH_DEPTH_MAX);
        cctx->params.rParams.depth = (unsigned)value;
        break;

    case FL2_p_fastLength:
        CLAMPCHECK(value, FL2_FASTLENGTH_MIN, FL2_FASTLENGTH_MAX);
        cctx->params.cParams.fast_length = (unsigned)value;
        break;

    case FL2_p_divideAndConquer:
        cctx->params.rParams.divide_and_conquer = (unsigned)value;
        break;

    case FL2_p_strategy:
        MAXCHECK(value, (unsigned)FL2_ultra);
        cctx->params.cParams.strategy = (FL2_strategy)value;
        break;

        /* lc, lp, pb can be changed between encoder chunks.
         * Set lc and lp even if lc+lp > 4 to allow sequential setting.
         * If lc+lp is still >4 when encoding begins, lc will be reduced. */
    case FL2_p_literalCtxBits:
        MAXCHECK(value, FL2_LC_MAX);
        cctx->params.cParams.lc = (unsigned)value;
        if (value + cctx->params.cParams.lp > FL2_LCLP_MAX)
            return FL2_ERROR(lclpMax_exceeded);
        break;

    case FL2_p_literalPosBits:
        MAXCHECK(value, FL2_LP_MAX);
        cctx->params.cParams.lp = (unsigned)value;
        if (cctx->params.cParams.lc + value > FL2_LCLP_MAX)
            return FL2_ERROR(lclpMax_exceeded);
        break;

    case FL2_p_posBits:
        MAXCHECK(value, FL2_PB_MAX);
        cctx->params.cParams.pb = (unsigned)value;
        break;

#ifndef NO_XXHASH
    case FL2_p_doXXHash:
        cctx->params.doXXH = value != 0;
        break;
#endif

    case FL2_p_omitProperties:
        cctx->params.omitProp = value != 0;
        break;
#ifdef RMF_REFERENCE
    case FL2_p_useReferenceMF:
        cctx->params.rParams.use_ref_mf = value != 0;
        break;
#endif
    default: return FL2_ERROR(parameter_unsupported);
    }
    return value;
}

FL2LIB_API size_t FL2LIB_CALL FL2_CCtx_getParameter(FL2_CCtx* cctx, FL2_cParameter param)
{
    switch (param)
    {
    case FL2_p_compressionLevel:
        return cctx->params.compressionLevel;

    case FL2_p_highCompression:
        return cctx->params.highCompression;

    case FL2_p_dictionaryLog: {
        size_t dictLog = FL2_DICTLOG_MIN;
        while (((size_t)1 << dictLog) < cctx->params.rParams.dictionary_size)
            ++dictLog;
        return dictLog;
    }

    case FL2_p_dictionarySize:
        return cctx->params.rParams.dictionary_size;

    case FL2_p_overlapFraction:
        return cctx->params.rParams.overlap_fraction;

    case FL2_p_resetInterval:
        return cctx->params.cParams.reset_interval;

    case FL2_p_bufferLog:
        return RMF_BUFFER_LOG_BASE - cctx->params.rParams.match_buffer_log;

    case FL2_p_chainLog:
        return cctx->params.cParams.second_dict_bits;

    case FL2_p_searchLog:
        return ZSTD_highbit32(cctx->params.cParams.match_cycles);

    case FL2_p_literalCtxBits:
        return cctx->params.cParams.lc;

    case FL2_p_literalPosBits:
        return cctx->params.cParams.lp;

    case FL2_p_posBits:
        return cctx->params.cParams.pb;

    case FL2_p_searchDepth:
        return cctx->params.rParams.depth;

    case FL2_p_fastLength:
        return cctx->params.cParams.fast_length;

    case FL2_p_divideAndConquer:
        return cctx->params.rParams.divide_and_conquer;

    case FL2_p_strategy:
        return (size_t)cctx->params.cParams.strategy;

#ifndef NO_XXHASH
    case FL2_p_doXXHash:
        return cctx->params.doXXH;
#endif

    case FL2_p_omitProperties:
        return cctx->params.omitProp;
#ifdef RMF_REFERENCE
    case FL2_p_useReferenceMF:
        return cctx->params.rParams.use_ref_mf;
#endif
    default: return FL2_ERROR(parameter_unsupported);
    }
}

FL2LIB_API size_t FL2LIB_CALL FL2_CStream_setParameter(FL2_CStream* fcs, FL2_cParameter param, size_t value)
{
    return FL2_CCtx_setParameter(fcs, param, value);
}

FL2LIB_API size_t FL2LIB_CALL FL2_CStream_getParameter(FL2_CStream* fcs, FL2_cParameter param)
{
    return FL2_CCtx_getParameter(fcs, param);
}

FL2LIB_API FL2_CStream* FL2LIB_CALL FL2_createCStream(void)
{
    return FL2_createCCtx_internal(1, 0);
}

FL2LIB_API FL2_CStream* FL2LIB_CALL FL2_createCStreamMt(unsigned nbThreads, int dualBuffer)
{
    return FL2_createCCtx_internal(nbThreads, dualBuffer);
}

FL2LIB_API void FL2LIB_CALL FL2_freeCStream(FL2_CStream * fcs)
{
    FL2_freeCCtx(fcs);
}

FL2LIB_API size_t FL2LIB_CALL FL2_initCStream(FL2_CStream* fcs, int compressionLevel)
{
    DEBUGLOG(4, "FL2_initCStream level %d", compressionLevel);

    fcs->endMarked = 0;
    fcs->wroteProp = 0;
    fcs->loopCount = 0;

    FL2_CCtx_setParameter(fcs, FL2_p_compressionLevel, compressionLevel);

    DICT_buffer *const buf = &fcs->buf;
    size_t const dictSize = fcs->params.rParams.dictionary_size;

    /* Free unsuitable objects before reallocating anything new */
    if (DICT_size(buf) < dictSize)
        DICT_destruct(buf);

    FL2_preBeginFrame(fcs, 0);

#ifdef NO_XXHASH
    int const doHash = 0;
#else
    int const doHash = (fcs->params.doXXH && !fcs->params.omitProp);
#endif
    if (DICT_init(buf, dictSize, doHash) != 0)
        return FL2_ERROR(memory_allocation);

    CHECK_F(FL2_beginFrame(fcs, 0));

    return 0;
}

FL2LIB_API size_t FL2LIB_CALL FL2_setCStreamTimeout(FL2_CStream * fcs, unsigned timeout)
{
    if (timeout != 0) {
        if (fcs->compressThread == NULL) {
            fcs->compressThread = FL2POOL_create(1);
            if (fcs->compressThread == NULL)
                return FL2_ERROR(memory_allocation);
        }
    }
    else if (!DICT_async(&fcs->buf) && fcs->dictMax == 0) {
        /* Only free the thread if not dual buffering and compression not underway */
        FL2POOL_free(fcs->compressThread);
        fcs->compressThread = NULL;
    }
    fcs->timeout = timeout;

    return FL2_error_no_error;
}

static size_t FL2_compressStream_internal(FL2_CStream* const fcs, int const ending)
{
    CHECK_F(FL2_waitStream(fcs));

    DICT_buffer *const buf = &fcs->buf;

    /* no compression can occur while compressed output exists */
    if (fcs->outThread == fcs->threadCount && DICT_hasUnprocessed(buf)) {
        fcs->streamTotal += fcs->curBlock.end - fcs->curBlock.start;

        DICT_getBlock(buf, &fcs->curBlock);

        int streamProp = -1;

        if (!fcs->wroteProp && !fcs->params.omitProp) {
            size_t dictionarySize = ending ? MAX(fcs->dictMax, fcs->curBlock.end)
                : fcs->params.rParams.dictionary_size;
            streamProp = FL2_getProp(fcs, dictionarySize);
            DEBUGLOG(4, "Writing property byte : 0x%X", streamProp);
            fcs->wroteProp = 1;
        }

        CHECK_F(FL2_compressCurBlock(fcs, streamProp));
    }
    return FL2_error_no_error;
}

static void FL2_copyCStreamOutput(FL2_CStream* fcs, FL2_outBuffer *output)
{
    for (; fcs->outThread < fcs->threadCount; ++fcs->outThread) {
        const BYTE* const outBuf = RMF_getTableAsOutputBuffer(fcs->matchTable, fcs->jobs[fcs->outThread].block.start) + fcs->outPos;
        BYTE* const dstBuf = (BYTE*)output->dst + output->pos;
        size_t const dstCapacity = output->size - output->pos;
        size_t toWrite = fcs->jobs[fcs->outThread].cSize;

        toWrite = MIN(toWrite - fcs->outPos, dstCapacity);

        DEBUGLOG(5, "CStream : writing %u bytes", (U32)toWrite);

        memcpy(dstBuf, outBuf, toWrite);
        fcs->outPos += toWrite;
        output->pos += toWrite;

        if (fcs->outPos < fcs->jobs[fcs->outThread].cSize)
            break;

        fcs->outPos = 0;
    }
}

static size_t FL2_compressStream_input(FL2_CStream* fcs, FL2_inBuffer* input)
{
    CHECK_F(fcs->asyncRes);

    DICT_buffer * const buf = &fcs->buf;
    size_t const blockOverlap = OVERLAP_FROM_DICT_SIZE(fcs->params.rParams.dictionary_size, fcs->params.rParams.overlap_fraction);

    while (input->pos < input->size) {
        /* read input until the buffer(s) are full */
        size_t overlap = blockOverlap;
        if (!DICT_hasUnprocessed(buf)
            && fcs->params.cParams.reset_interval != 0
            && fcs->blockTotal + (fcs->params.rParams.dictionary_size - overlap)
            > (fcs->params.rParams.dictionary_size * fcs->params.cParams.reset_interval))
        {
            /* periodically reset the dictionary for mt decompression */
            overlap = 0;
            fcs->blockTotal = 0;
        }
        if (DICT_needShift(buf, overlap)) {
            /* cannot shift single dict during compression */
            if(!DICT_async(buf))
                CHECK_F(FL2_waitStream(fcs));
            DICT_shift(buf, overlap);
        }
        
        CHECK_F(fcs->asyncRes);

        DICT_put(buf, input);
        
        if (!DICT_availSpace(buf)) {
            /* break if the compressor is not available */
            if (fcs->outThread < fcs->threadCount)
                break;

            CHECK_F(FL2_compressStream_internal(fcs, 0));
        }

        CHECK_F(fcs->asyncRes);
    }

    return FL2_error_no_error;
}

static size_t FL2_loopCheck(FL2_CStream* fcs, int unchanged)
{
    if (unchanged) {
        ++fcs->loopCount;
        if (fcs->loopCount > FL2_MAX_LOOPS)
            return FL2_ERROR(buffer);
    }
    else {
        fcs->loopCount = 0;
    }
    return FL2_error_no_error;
}

FL2LIB_API size_t FL2LIB_CALL FL2_compressStream(FL2_CStream* fcs, FL2_outBuffer *output, FL2_inBuffer* input)
{
    if (!fcs->lockParams)
        return FL2_ERROR(init_missing);

    size_t const prevIn = input->pos;
    size_t const prevOut = (output != NULL) ? output->pos : 0;

    if (output != NULL && fcs->outThread < fcs->threadCount)
        FL2_copyCStreamOutput(fcs, output);

    CHECK_F(FL2_compressStream_input(fcs, input));

    if(output != NULL && fcs->outThread < fcs->threadCount)
        FL2_copyCStreamOutput(fcs, output);

    CHECK_F(FL2_loopCheck(fcs, prevIn == input->pos && (output == NULL || prevOut == output->pos)));

    return fcs->outThread == fcs->threadCount;
}

FL2LIB_API size_t FL2LIB_CALL FL2_getDictionaryBuffer(FL2_CStream * fcs, FL2_outBuffer * dict)
{
    if (!fcs->lockParams)
        return FL2_ERROR(init_missing);

    CHECK_F(fcs->asyncRes);

    size_t const blockOverlap = OVERLAP_FROM_DICT_SIZE(fcs->params.rParams.dictionary_size, fcs->params.rParams.overlap_fraction);

    if (DICT_needShift(&fcs->buf, blockOverlap) && !DICT_async(&fcs->buf))
        CHECK_F(FL2_waitStream(fcs));

    DICT_get(&fcs->buf, blockOverlap, dict);

    return FL2_error_no_error;
}

FL2LIB_API size_t FL2LIB_CALL FL2_updateDictionary(FL2_CStream * fcs, size_t addedSize)
{
    if (DICT_update(&fcs->buf, addedSize))
        CHECK_F(FL2_compressStream_internal(fcs, 0));

    return fcs->outThread == fcs->threadCount;
}

FL2LIB_API unsigned long long FL2LIB_CALL FL2_getCStreamProgress(const FL2_CStream * fcs)
{
    U64 const encodeSize = fcs->curBlock.end - fcs->curBlock.start;

    if (fcs->encProgress == 0 && fcs->curBlock.end != 0)
        return fcs->streamTotal + ((fcs->matchTable->progress * encodeSize / fcs->curBlock.end * fcs->rmfWeight) >> 4);

    return fcs->streamTotal + ((fcs->rmfWeight * encodeSize) >> 4) + ((fcs->encProgress * fcs->encWeight) >> 4);
}

FL2LIB_API size_t FL2LIB_CALL FL2_waitStream(FL2_CStream * fcs)
{
    if (FL2POOL_waitAll(fcs->compressThread, fcs->timeout) != 0)
        return FL2_ERROR(timedOut);
    CHECK_F(fcs->asyncRes);
    return fcs->outThread < fcs->threadCount;
}

FL2LIB_API void FL2LIB_CALL FL2_cancelOperation(FL2_CStream *fcs)
{
    if (fcs->compressThread != NULL) {
        fcs->canceled = 1;

        RMF_cancelBuild(fcs->matchTable);
        FL2POOL_waitAll(fcs->compressThread, 0);

        fcs->canceled = 0;
    }

    FL2_endFrame(fcs);
}

FL2LIB_API size_t FL2LIB_CALL FL2_remainingOutputSize(const FL2_CStream* fcs)
{
    CHECK_F(fcs->asyncRes);

    size_t cSize = 0;
    for (size_t u = fcs->outThread; u < fcs->threadCount; ++u)
        cSize += fcs->jobs[u].cSize;

    return cSize;
}

FL2LIB_API size_t FL2LIB_CALL FL2_getNextCStreamBuffer(FL2_CStream* fcs, FL2_inBuffer* cbuf)
{
    cbuf->src = NULL;
    cbuf->pos = 0;
    cbuf->size = 0;

    CHECK_F(FL2_waitStream(fcs));

    if (fcs->outThread < fcs->threadCount) {
        cbuf->src = RMF_getTableAsOutputBuffer(fcs->matchTable, fcs->jobs[fcs->outThread].block.start) + fcs->outPos;
        cbuf->size = fcs->jobs[fcs->outThread].cSize - fcs->outPos;
        ++fcs->outThread;
        fcs->outPos = 0;
    }
    return cbuf->size;
}

static void FL2_writeEnd(FL2_CStream* const fcs)
{
    size_t thread = fcs->threadCount - 1;
    if (fcs->outThread == fcs->threadCount) {
        fcs->outThread = 0; 
        fcs->threadCount = 1;
        fcs->jobs[0].cSize = 0;
        thread = 0;
    }
    BYTE *const dst = RMF_getTableAsOutputBuffer(fcs->matchTable, fcs->jobs[thread].block.start)
        + fcs->jobs[thread].cSize;

    size_t pos = 0;

    if (!fcs->wroteProp && !fcs->params.omitProp) {
        /* no compression occurred */
        dst[pos] = FL2_getProp(fcs, 0);
        DEBUGLOG(4, "Writing property byte : 0x%X", dst[pos]);
        ++pos;
        fcs->wroteProp = 1;
    }

    DEBUGLOG(4, "Writing end marker");
    dst[pos++] = LZMA2_END_MARKER;

#ifndef NO_XXHASH
    if (fcs->params.doXXH && !fcs->params.omitProp) {
        XXH32_canonical_t canonical;

        XXH32_canonicalFromHash(&canonical, DICT_getDigest(&fcs->buf));
        DEBUGLOG(4, "Writing XXH32");
        memcpy(dst + pos, &canonical, XXHASH_SIZEOF);

        pos += XXHASH_SIZEOF;
    }
#endif
    fcs->jobs[thread].cSize += pos;
    fcs->endMarked = 1;

    FL2_endFrame(fcs);
}

static size_t FL2_flushStream_internal(FL2_CStream* fcs, int const ending)
{
    CHECK_F(fcs->asyncRes);

    DEBUGLOG(4, "FL2_flushStream_internal : %u to compress, %u to write",
        (U32)(fcs->buf.end - fcs->buf.start),
        (U32)FL2_remainingOutputSize(fcs));

    CHECK_F(FL2_compressStream_internal(fcs, ending));

    return fcs->outThread < fcs->threadCount;
}

FL2LIB_API size_t FL2LIB_CALL FL2_flushStream(FL2_CStream* fcs, FL2_outBuffer *output)
{
    if (!fcs->lockParams)
        return FL2_ERROR(init_missing);

    size_t const prevOut = (output != NULL) ? output->pos : 0;

    if (output != NULL && fcs->outThread < fcs->threadCount)
        FL2_copyCStreamOutput(fcs, output);

    size_t res = FL2_flushStream_internal(fcs, 0);
    CHECK_F(res);

    if (output != NULL && res != 0) {
        FL2_copyCStreamOutput(fcs, output);
        res = fcs->outThread < fcs->threadCount;
    }

    CHECK_F(FL2_loopCheck(fcs, output != NULL && prevOut == output->pos));

    return res;
}

FL2LIB_API size_t FL2LIB_CALL FL2_endStream(FL2_CStream* fcs, FL2_outBuffer *output)
{
    if (!fcs->endMarked && !fcs->lockParams)
        return FL2_ERROR(init_missing);

    size_t const prevOut = (output != NULL) ? output->pos : 0;
    
    if (output != NULL && fcs->outThread < fcs->threadCount)
        FL2_copyCStreamOutput(fcs, output);

    CHECK_F(FL2_flushStream_internal(fcs, 1));

    size_t res = FL2_waitStream(fcs);
    CHECK_F(res);

    if (!fcs->endMarked && !DICT_hasUnprocessed(&fcs->buf)) {
        FL2_writeEnd(fcs);
        res = 1;
    }

    if (output != NULL && res != 0) {
        FL2_copyCStreamOutput(fcs, output);
        res = fcs->outThread < fcs->threadCount || DICT_hasUnprocessed(&fcs->buf);
    }

    CHECK_F(FL2_loopCheck(fcs, output != NULL && prevOut == output->pos));

    return res;
}

FL2LIB_API size_t FL2LIB_CALL FL2_getLevelParameters(int compressionLevel, int high, FL2_compressionParameters * params)
{
    if (high) {
        if (compressionLevel < 0 || compressionLevel > FL2_MAX_HIGH_CLEVEL)
            return FL2_ERROR(parameter_outOfBound);
        *params = FL2_highCParameters[compressionLevel];
    }
    else {
        if (compressionLevel < 0 || compressionLevel > FL2_MAX_CLEVEL)
            return FL2_ERROR(parameter_outOfBound);
        *params = FL2_defaultCParameters[compressionLevel];
    }
    return FL2_error_no_error;
}

static size_t FL2_memoryUsage_internal(size_t const dictionarySize, unsigned const bufferLog,
    unsigned const searchDepth, unsigned const chainLog,
    FL2_strategy const strategy,
    unsigned const nbThreads)
{
    return RMF_memoryUsage(dictionarySize, bufferLog, searchDepth, nbThreads)
        + LZMA2_encMemoryUsage(chainLog, strategy, nbThreads);
}

FL2LIB_API size_t FL2LIB_CALL FL2_estimateCCtxSize(int compressionLevel, unsigned nbThreads)
{
    if (compressionLevel == 0)
        compressionLevel = FL2_CLEVEL_DEFAULT;

    CLAMPCHECK(compressionLevel, 1, FL2_MAX_CLEVEL);

    return FL2_estimateCCtxSize_byParams(FL2_defaultCParameters + compressionLevel, nbThreads);
}

FL2LIB_API size_t FL2LIB_CALL FL2_estimateCCtxSize_byParams(const FL2_compressionParameters * params, unsigned nbThreads)
{
    return FL2_memoryUsage_internal(params->dictionarySize,
        params->bufferLog,
        params->searchDepth,
        params->chainLog,
        params->strategy,
        nbThreads);
}

FL2LIB_API size_t FL2LIB_CALL FL2_estimateCCtxSize_usingCCtx(const FL2_CCtx * cctx)
{
    return FL2_memoryUsage_internal(cctx->params.rParams.dictionary_size,
        cctx->params.rParams.match_buffer_log,
        cctx->params.rParams.depth,
        cctx->params.cParams.second_dict_bits,
        cctx->params.cParams.strategy,
        cctx->jobCount) + DICT_memUsage(&cctx->buf);
}

FL2LIB_API size_t FL2LIB_CALL FL2_estimateCStreamSize(int compressionLevel, unsigned nbThreads, int dualBuffer)
{
    return FL2_estimateCCtxSize(compressionLevel, nbThreads)
        + (FL2_defaultCParameters[compressionLevel].dictionarySize << (dualBuffer != 0));
}

FL2LIB_API size_t FL2LIB_CALL FL2_estimateCStreamSize_byParams(const FL2_compressionParameters * params, unsigned nbThreads, int dualBuffer)
{
    return FL2_estimateCCtxSize_byParams(params, nbThreads)
        + (params->dictionarySize << (dualBuffer != 0));
}

FL2LIB_API size_t FL2LIB_CALL FL2_estimateCStreamSize_usingCStream(const FL2_CStream* fcs)
{
    return FL2_estimateCCtxSize_usingCCtx(fcs);
}
