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

/*-=====  Pre-defined compression levels  =====-*/

#define FL2_MAX_HIGH_CLEVEL 9

#ifdef FL2_XZ_BUILD

#define FL2_CLEVEL_DEFAULT  6
#define FL2_MAX_CLEVEL      9

static const FL2_compressionParameters FL2_defaultCParameters[FL2_MAX_CLEVEL + 1] = {
    { 0,0,0,0,0,0,0,0,0 },
    { 20, 1, 7, 0, 6, 32, 1, 8, FL2_fast }, /* 1 */
    { 21, 2, 7, 0, 14, 32, 1, 8, FL2_fast }, /* 2 */
    { 21, 2, 7, 0, 14, 40, 1, 8, FL2_opt }, /* 3 */
    { 22, 2, 7, 0, 26, 40, 1, 8, FL2_opt }, /* 4 */
    { 24, 2, 8, 0, 42, 48, 1, 8, FL2_opt }, /* 5 */
    { 24, 2, 9, 0, 42, 48, 1, 8, FL2_ultra }, /* 6 */
    { 25, 2, 10, 0, 50, 64, 1, 8, FL2_ultra }, /* 7 */
    { 26, 2, 11, 1, 60, 64, 1, 9, FL2_ultra }, /* 8 */
    { 27, 2, 12, 2, 126, 96, 1, 10, FL2_ultra }, /* 9 */
};

#elif defined(FL2_7ZIP_BUILD)

#define FL2_CLEVEL_DEFAULT  5
#define FL2_MAX_CLEVEL      9

static const FL2_compressionParameters FL2_defaultCParameters[FL2_MAX_CLEVEL + 1] = {
    { 0,0,0,0,0,0,0,0,0 },
    { 20, 1, 7, 0, 6, 32, 1, 8, FL2_fast }, /* 1 */
    { 20, 2, 7, 0, 12, 32, 1, 8, FL2_fast }, /* 2 */
    { 21, 2, 7, 0, 16, 32, 1, 8, FL2_fast }, /* 3 */
    { 20, 2, 7, 0, 16, 32, 1, 8, FL2_opt }, /* 4 */
    { 24, 2, 9, 0, 40, 48, 1, 8, FL2_ultra }, /* 5 */
    { 25, 2, 10, 0, 48, 64, 1, 8, FL2_ultra }, /* 6 */
    { 26, 2, 11, 1, 60, 96, 1, 9, FL2_ultra }, /* 7 */
    { 27, 2, 12, 2, 128, 128, 1, 10, FL2_ultra }, /* 8 */
    { 27, 3, 14, 3, 252, 160, 0, 10, FL2_ultra } /* 9 */
};

#else

#define FL2_CLEVEL_DEFAULT   9
#define FL2_MAX_CLEVEL      12

static const FL2_compressionParameters FL2_defaultCParameters[FL2_MAX_CLEVEL + 1] = {
    { 0,0,0,0,0,0,0,0,0 },
    { 20, 1, 7, 0, 6, 32, 1, 8, FL2_fast }, /* 1 */
    { 20, 2, 7, 0, 12, 32, 1, 8, FL2_fast }, /* 2 */
    { 21, 2, 7, 0, 14, 32, 1, 8, FL2_fast }, /* 3 */
    { 20, 2, 7, 0, 12, 32, 1, 8, FL2_opt }, /* 4 */
    { 21, 2, 7, 0, 14, 40, 1, 8, FL2_opt }, /* 5 */
    { 22, 2, 7, 0, 26, 40, 1, 8, FL2_opt }, /* 6 */
    { 23, 2, 8, 0, 42, 48, 1, 8, FL2_opt }, /* 7 */
    { 24, 2, 9, 0, 42, 48, 1, 8, FL2_ultra }, /* 8 */
    { 25, 2, 10, 0, 50, 64, 1, 8, FL2_ultra }, /* 9 */
    { 26, 2, 11, 1, 60, 64, 1, 9, FL2_ultra }, /* 10 */
    { 27, 2, 12, 2, 126, 96, 1, 10, FL2_ultra }, /* 11 */
    { 28, 2, 14, 3, 254, 160, 1, 10, FL2_ultra } /* 12 */
};

#endif

static const FL2_compressionParameters FL2_highCParameters[FL2_MAX_HIGH_CLEVEL + 1] = {
    { 0,0,0,0,0,0,0,0,0 },
    { 20, 3, 9, 1, 60, 128, 0, 8, FL2_ultra }, /* 1 */
    { 21, 3, 10, 1, 60, 128, 0, 8, FL2_ultra }, /* 2 */
    { 22, 3, 11, 2, 60, 128, 0, 8, FL2_ultra }, /* 3 */
    { 23, 3, 12, 2, 60, 128, 0, 8, FL2_ultra }, /* 4 */
    { 24, 3, 13, 3, 60, 128, 0, 8, FL2_ultra }, /* 5 */
    { 25, 3, 14, 3, 60, 160, 0, 8, FL2_ultra }, /* 6 */
    { 26, 3, 14, 4, 60, 160, 0, 8, FL2_ultra }, /* 7 */
    { 27, 3, 14, 4, 128, 160, 0, 8, FL2_ultra }, /* 8 */
    { 28, 3, 14, 5, 128, 160, 0, 9, FL2_ultra } /* 9 */
};

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
    RMF_parameters* const rParams = &cctx->params.rParams;
    cParams->lc = 3;
    cParams->lp = 0;
    cParams->pb = 2;
    cParams->fast_length = params->fastLength;
    cParams->match_cycles = 1U << params->searchLog;
    cParams->strategy = params->strategy;
    cParams->second_dict_bits = params->chainLog;
    cParams->random_filter = 0;
    rParams->dictionary_log = MIN(params->dictionaryLog, FL2_DICTLOG_MAX); /* allow for reduced dict in 32-bit version */
    rParams->match_buffer_log = params->bufferLog;
    rParams->overlap_fraction = params->overlapFraction;
    rParams->block_size_log = rParams->dictionary_log + 2;
    rParams->divide_and_conquer = params->divideAndConquer;
    rParams->depth = params->searchDepth;
}

FL2_CCtx* FL2_createCCtx_internal(unsigned nbThreads)
{
    FL2_CCtx* cctx;

    nbThreads = FL2_checkNbThreads(nbThreads);

    DEBUGLOG(3, "FL2_createCCtxMt : %u threads", nbThreads);

    cctx = malloc(sizeof(FL2_CCtx) + (nbThreads - 1) * sizeof(FL2_job));
    if (cctx == NULL)
        return NULL;

    cctx->jobCount = nbThreads;
    for (unsigned u = 0; u < nbThreads; ++u) {
        cctx->jobs[u].enc = NULL;
    }

    cctx->params.highCompression = 0;
    FL2_CCtx_setParameter(cctx, FL2_p_compressionLevel, FL2_CLEVEL_DEFAULT);
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
#endif

    cctx->compressThread = FL2POOL_create(1);
    if (cctx->compressThread == NULL) {
        FL2_freeCCtx(cctx);
        return NULL;
    }

    for (unsigned u = 0; u < nbThreads; ++u) {
        cctx->jobs[u].enc = FL2_lzma2Create();
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
    cctx->asyncRes = 0;
    cctx->threadCount = 0;
    cctx->timeout = 0;

    return cctx;
}

FL2LIB_API FL2_CCtx* FL2LIB_CALL FL2_createCCtx(void)
{
    return FL2_createCCtx_internal(0);
}

FL2LIB_API FL2_CCtx* FL2LIB_CALL FL2_createCCtxMt(unsigned nbThreads)
{
    return FL2_createCCtx_internal(nbThreads);
}

FL2LIB_API FL2_CCtx *FL2LIB_CALL FL2_createCCtxAsync(unsigned nbThreads)
{
    return FL2_createCCtx_internal(nbThreads);
}

FL2LIB_API void FL2LIB_CALL FL2_freeCCtx(FL2_CCtx* cctx)
{
    if (cctx == NULL) 
        return;

    DEBUGLOG(3, "FL2_freeCCtx : %u threads", cctx->jobCount);

    for (unsigned u = 0; u < cctx->jobCount; ++u) {
        FL2_lzma2Free(cctx->jobs[u].enc);
    }

#ifndef FL2_SINGLETHREAD
    FL2POOL_free(cctx->factory);
#endif
    FL2POOL_free(cctx->compressThread);

    RMF_freeMatchTable(cctx->matchTable);
    free(cctx);
}

FL2LIB_API unsigned FL2LIB_CALL FL2_CCtx_nbThreads(const FL2_CCtx* cctx)
{
    return cctx->jobCount;
}

/* FL2_buildRadixTable() : FL2POOL_function type */
static void FL2_buildRadixTable(void* const jobDescription, size_t n)
{
    const FL2_job* const job = (FL2_job*)jobDescription;
    FL2_CCtx* const cctx = job->cctx;

    RMF_buildTable(cctx->matchTable, n, 1, cctx->curBlock);
}

/* FL2_compressRadixChunk() : FL2POOL_function type */
static void FL2_compressRadixChunk(void* const jobDescription, size_t n)
{
    const FL2_job* const job = (FL2_job*)jobDescription;
    FL2_CCtx* const cctx = job->cctx;

    cctx->jobs[n].cSize = FL2_lzma2Encode(cctx->jobs[n].enc, cctx->matchTable, job->block, &cctx->params.cParams, -1, &cctx->encProgress, &cctx->canceled);
}

static int FL2_initEncoders(FL2_CCtx* const cctx)
{
    for(unsigned u = 0; u < cctx->jobCount; ++u) {
        if (FL2_lzma2HashAlloc(cctx->jobs[u].enc, &cctx->params.cParams) != 0)
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

static size_t FL2_compressCurBlock_blocking(FL2_CCtx* const cctx, int streamProp)
{
    size_t const encodeSize = (cctx->curBlock.end - cctx->curBlock.start);
    size_t init_done;
    int err = 0;
#ifndef FL2_SINGLETHREAD
    size_t mfThreads = cctx->curBlock.end / RMF_MIN_BYTES_PER_THREAD;
    size_t nbThreads = MIN(cctx->jobCount, encodeSize / ENC_MIN_BYTES_PER_THREAD);
    nbThreads += !nbThreads;
#else
    size_t mfThreads = 1;
    size_t nbThreads = 1;
#endif

    DEBUGLOG(5, "FL2_compressCurBlock : %u threads, %u start, %u bytes", (U32)nbThreads, (U32)cctx->curBlock.start, (U32)encodeSize);

    /* Free unsuitable match table before reallocating anything else */
    if (cctx->matchTable && !RMF_compatibleParameters(cctx->matchTable, &cctx->params.rParams, cctx->curBlock.end)) {
        RMF_freeMatchTable(cctx->matchTable);
        cctx->matchTable = NULL;
    }

    if(FL2_initEncoders(cctx) != 0) /* Create hash objects together, leaving the (large) match table last */
        return FL2_ERROR(memory_allocation);

    if (!cctx->matchTable) {
        cctx->matchTable = RMF_createMatchTable(&cctx->params.rParams, cctx->curBlock.end, cctx->jobCount);
        if (cctx->matchTable == NULL)
            return FL2_ERROR(memory_allocation);
    }
    else {
        DEBUGLOG(5, "Have compatible match table");
        RMF_applyParameters(cctx->matchTable, &cctx->params.rParams, cctx->curBlock.end);
    }

    {   size_t sliceStart = cctx->curBlock.start;
        size_t sliceSize = encodeSize / nbThreads;
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
    }

    /* initialize to length 2 */
    cctx->matchTable->progress = RMF_initTable(cctx->matchTable, cctx->curBlock.data, cctx->curBlock.start, cctx->curBlock.end);

#ifndef FL2_SINGLETHREAD
    mfThreads = MIN(RMF_threadCount(cctx->matchTable), mfThreads);
    for (size_t u = 1; u < mfThreads; ++u) {
		FL2POOL_add(cctx->factory, FL2_buildRadixTable, &cctx->jobs[u], u);
    }
#endif

    err = RMF_buildTable(cctx->matchTable, 0, mfThreads > 1, cctx->curBlock);

#ifndef FL2_SINGLETHREAD

    FL2POOL_waitAll(cctx->factory, 0);

    if (err)
        return FL2_ERROR(canceled);

#ifdef RMF_CHECK_INTEGRITY
    err = RMF_integrityCheck(cctx->matchTable, cctx->curBlock.data, cctx->curBlock.start, cctx->curBlock.end, cctx->params.rParams.depth);
    if (err)
        return FL2_ERROR(internal);
#endif

    for (size_t u = 1; u < nbThreads; ++u) {
		FL2POOL_add(cctx->factory, FL2_compressRadixChunk, &cctx->jobs[u], u);
    }

    cctx->jobs[0].cSize = FL2_lzma2Encode(cctx->jobs[0].enc, cctx->matchTable, cctx->jobs[0].block, &cctx->params.cParams, streamProp, &cctx->encProgress, &cctx->canceled);
    FL2POOL_waitAll(cctx->factory, 0);

#else /* FL2_SINGLETHREAD */

    if (err)
        return FL2_ERROR(canceled);

#ifdef RMF_CHECK_INTEGRITY
    err = RMF_integrityCheck(cctx->matchTable, cctx->curBlock.data, cctx->curBlock.start, cctx->curBlock.end, cctx->params.rParams.depth);
    if (err)
        return FL2_ERROR(internal);
#endif
    cctx->jobs[0].cSize = FL2_lzma2Encode(cctx->jobs[0].enc, cctx->matchTable, cctx->jobs[0].block, &cctx->params.cParams, streamProp, &cctx->encProgress, &cctx->canceled);

#endif

    for (size_t u = 0; u < nbThreads; ++u)
        if (FL2_isError(cctx->jobs[u].cSize))
            return cctx->jobs[u].cSize;

    cctx->blockTotal += encodeSize;

    cctx->threadCount = nbThreads;

    return FL2_error_no_error;
}

/* FL2_compressRadixChunk() : FL2POOL_function type */
static void FL2_compressCurBlock_async(void* const jobDescription, size_t n)
{
    FL2_CCtx* const cctx = (FL2_CCtx*)jobDescription;

    cctx->asyncRes = FL2_compressCurBlock_blocking(cctx, (int)n);
}

static size_t FL2_compressCurBlock(FL2_CCtx* const cctx, int streamProp)
{
    FL2_initProgress(cctx);

    if (cctx->curBlock.start == cctx->curBlock.end)
        return FL2_error_no_error;

    /* update largest dict size used */
    cctx->dictMax = MAX(cctx->dictMax, cctx->curBlock.end);

    cctx->outThread = 0;
    cctx->threadCount = 0;

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

    FL2POOL_add(cctx->compressThread, FL2_compressCurBlock_async, cctx, streamProp);
    return FL2_error_no_error;
}

FL2LIB_API void FL2LIB_CALL FL2_beginFrame(FL2_CCtx* const cctx)
{
    cctx->dictMax = 0;
    cctx->blockTotal = 0;
    cctx->streamTotal = 0;
    cctx->outThread = 0;
    cctx->threadCount = 0;
}

static size_t FL2_compressBlock(FL2_CCtx* const cctx,
    const void* const src, size_t const srcStart, size_t const srcEnd,
    void* const dst, size_t dstCapacity)
{
    BYTE* dstBuf = dst;
    size_t const dictionarySize = (size_t)1 << cctx->params.rParams.dictionary_log;
    size_t const blockOverlap = OVERLAP_FROM_DICT_LOG(cctx->params.rParams.dictionary_log, cctx->params.rParams.overlap_fraction);

    if (srcStart >= srcEnd)
        return 0;

    cctx->curBlock.data = src;
    cctx->curBlock.start = srcStart;

    size_t srcSize = srcEnd - srcStart;

    do {
        cctx->curBlock.end = cctx->curBlock.start + MIN(srcSize, dictionarySize - cctx->curBlock.start);

        CHECK_F(FL2_compressCurBlock(cctx, -1));
        FL2POOL_waitAll(cctx->compressThread, 0);

        if(dst != NULL) for (size_t u = 0; u < cctx->threadCount; ++u) {
            const BYTE* const outBuf = RMF_getTableAsOutputBuffer(cctx->matchTable, cctx->jobs[u].block.start);

            DEBUGLOG(5, "Write thread %u : %u bytes", (U32)u, (U32)cctx->jobs[u].cSize);

            if (dstCapacity < cctx->jobs[u].cSize) {
                return FL2_ERROR(dstSize_tooSmall);
            }
            memcpy(dstBuf, outBuf, cctx->jobs[u].cSize);
            dstBuf += cctx->jobs[u].cSize;
            dstCapacity -= cctx->jobs[u].cSize;
        }
        srcSize -= cctx->curBlock.end - cctx->curBlock.start;
        if (cctx->params.rParams.block_size_log && cctx->blockTotal + MIN(cctx->curBlock.end - blockOverlap, srcSize) > ((U64)1 << cctx->params.rParams.block_size_log)) {
            /* periodically reset the dictionary for mt decompression */
            cctx->curBlock.start = 0;
            cctx->blockTotal = 0;
        }
        else {
            cctx->curBlock.start = blockOverlap;
        }
        cctx->curBlock.data += cctx->curBlock.end - cctx->curBlock.start;
    } while (dst != NULL && srcSize != 0);
    return (dst == NULL) ? 0 : dstBuf - (const BYTE*)dst;
}

static BYTE FL2_getProp(FL2_CCtx* cctx, size_t dictionarySize)
{
#ifndef NO_XXHASH
    return FL2_getDictSizeProp(dictionarySize) | (BYTE)((cctx->params.doXXH != 0) << FL2_PROP_HASH_BIT);
#else
    (void)cctx;
    return FL2_getDictSizeProp(dictionarySize);
#endif
}

FL2LIB_API size_t FL2LIB_CALL FL2_compressCCtx(FL2_CCtx* cctx,
    void* dst, size_t dstCapacity,
    const void* src, size_t srcSize,
    int compressionLevel)
{
    BYTE* dstBuf = dst;
    BYTE* const end = dstBuf + dstCapacity;
    size_t cSize = 0;

    if (compressionLevel > 0)
        FL2_CCtx_setParameter(cctx, FL2_p_compressionLevel, compressionLevel);

    DEBUGLOG(4, "FL2_compressCCtx : level %u, %u src => %u avail", cctx->params.compressionLevel, (U32)srcSize, (U32)dstCapacity);

    if (dstCapacity < 2U - cctx->params.omitProp) /* empty LZMA2 stream is byte sequence {0, 0} */
        return FL2_ERROR(dstSize_tooSmall);

    FL2_beginFrame(cctx);

    dstBuf += !cctx->params.omitProp;
    cSize = FL2_compressBlock(cctx, src, 0, srcSize, dstBuf, end - dstBuf);
    if(!cctx->params.omitProp)
        dstBuf[-1] = FL2_getProp(cctx, cctx->dictMax);

    if (FL2_isError(cSize))
        return cSize;

    dstBuf += cSize;
    if(dstBuf >= end)
        return FL2_ERROR(dstSize_tooSmall);
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
    return dstBuf - (BYTE*)dst;
}

FL2LIB_API void FL2LIB_CALL FL2_setTimeout(FL2_CCtx * cctx, unsigned timeout)
{
    cctx->timeout = timeout;
}

FL2LIB_API size_t FL2LIB_CALL FL2_waitCCtx(FL2_CCtx * cctx)
{
    return FL2POOL_waitAll(cctx->compressThread, cctx->timeout);
}

unsigned long long FL2_getProgress(const FL2_CCtx * cctx)
{
    U64 const encodeSize = cctx->curBlock.end - cctx->curBlock.start;

    if (cctx->encProgress == 0)
        return cctx->streamTotal + (cctx->matchTable->progress * encodeSize / cctx->curBlock.end * cctx->rmfWeight) >> 4;

    return cctx->streamTotal + ((cctx->rmfWeight * encodeSize) >> 4) + ((cctx->encProgress * cctx->encWeight) >> 4);
}

FL2LIB_API size_t FL2LIB_CALL FL2_getNextCStreamBuffer(FL2_CStream* fcs, void **src)
{
    FL2_CCtx* const cctx = fcs->cctx;
    size_t cSize = 0;

    *src = NULL;

    if (FL2_isError(cctx->asyncRes))
        return cctx->asyncRes;

    if (FL2POOL_waitAll(fcs->cctx->compressThread, fcs->cctx->timeout) != 0)
        return FL2_ERROR(timedOut);

    if (cctx->outThread < cctx->threadCount) {
        *src = RMF_getTableAsOutputBuffer(cctx->matchTable, cctx->jobs[cctx->outThread].block.start);
        cSize = cctx->jobs[cctx->outThread].cSize;
        ++cctx->outThread;
    }
    return cSize;
}

FL2LIB_API size_t FL2LIB_CALL FL2_getCStreamOutput(FL2_CStream* fcs, void *dst, size_t dstCapacity)
{
    FL2_CCtx* const cctx = fcs->cctx;
    size_t cSize = 0;
    for (; cctx->outThread < cctx->threadCount; ++cctx->outThread) {
        const BYTE* const outBuf = RMF_getTableAsOutputBuffer(cctx->matchTable, cctx->jobs[cctx->outThread].block.start);
        BYTE* const dstBuf = (BYTE*)dst + cSize;

        size_t to_write = cctx->jobs[cctx->outThread].cSize;
        if(dstCapacity - cSize < to_write)
            return FL2_ERROR(dstSize_tooSmall);

        DEBUGLOG(5, "CStream : writing %u bytes", (U32)to_write);

        memcpy(dstBuf, outBuf, to_write);
        cSize += to_write;
    }
    return cSize;
}

FL2LIB_API size_t FL2LIB_CALL FL2_blockOverlap(const FL2_CCtx* cctx)
{
	return OVERLAP_FROM_DICT_LOG(cctx->params.rParams.dictionary_log, cctx->params.rParams.overlap_fraction);
}

FL2LIB_API size_t FL2LIB_CALL FL2_endFrame(void* dst, size_t dstCapacity)
{
    if (!dstCapacity)
        return FL2_ERROR(dstSize_tooSmall);
    *(BYTE*)dst = LZMA2_END_MARKER;
    return 1;
}

FL2LIB_API size_t FL2LIB_CALL FL2_compressMt(void* dst, size_t dstCapacity,
    const void* src, size_t srcSize,
    int compressionLevel,
    unsigned nbThreads)
{
    size_t cSize;
    FL2_CCtx* const cctx = FL2_createCCtxMt(nbThreads);
    if (cctx == NULL)
        return FL2_ERROR(memory_allocation);

    cSize = FL2_compressCCtx(cctx, dst, dstCapacity, src, srcSize, compressionLevel);

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
    return FL2_getDictSizeProp(cctx->dictMax ? cctx->dictMax : (size_t)1 << cctx->params.rParams.dictionary_log);
}

#define MAXCHECK(val,max) {            \
    if ((val)>(max)) {     \
        return FL2_ERROR(parameter_outOfBound);  \
}   }

#define CLAMPCHECK(val,min,max) {            \
    if (((val)<(min)) | ((val)>(max))) {     \
        return FL2_ERROR(parameter_outOfBound);  \
}   }

FL2LIB_API size_t FL2LIB_CALL FL2_CCtx_setParameter(FL2_CCtx* cctx, FL2_cParameter param, unsigned value)
{
    switch (param)
    {
    case FL2_p_compressionLevel:
        if (value > 0) { /* 0 : does not change current level */
            if (cctx->params.highCompression) {
                if ((int)value > FL2_MAX_HIGH_CLEVEL) value = FL2_MAX_HIGH_CLEVEL;
                FL2_fillParameters(cctx, &FL2_highCParameters[value]);
            }
            else {
                if ((int)value > FL2_MAX_CLEVEL) value = FL2_MAX_CLEVEL;
                FL2_fillParameters(cctx, &FL2_defaultCParameters[value]);
            }
            cctx->params.compressionLevel = value;
        }
        return cctx->params.compressionLevel;

    case FL2_p_highCompression:
        if ((int)value >= 0) { /* < 0 : does not change highCompression */
            cctx->params.highCompression = value != 0;
            FL2_CCtx_setParameter(cctx, FL2_p_compressionLevel, cctx->params.compressionLevel);
        }
        return cctx->params.highCompression;

    case FL2_p_dictionaryLog:
        if (value) {  /* 0 : does not change current dictionaryLog */
            CLAMPCHECK(value, FL2_DICTLOG_MIN, FL2_DICTLOG_MAX);
            cctx->params.rParams.dictionary_log = value;
        }
        return cctx->params.rParams.dictionary_log;

    case FL2_p_overlapFraction:
        if ((int)value >= 0) {  /* < 0 : does not change current overlapFraction */
            MAXCHECK(value, FL2_BLOCK_OVERLAP_MAX);
            cctx->params.rParams.overlap_fraction = value;
        }
        return cctx->params.rParams.overlap_fraction;

    case FL2_p_blockSizeLog:
        if ((int)value >= 0) {  /* < 0 : does not change current blockSizeLog */
            if(value != 0)
                CLAMPCHECK(value, FL2_BLOCK_LOG_MIN, FL2_BLOCK_LOG_MAX);
            cctx->params.rParams.block_size_log = value;
        }
        return cctx->params.rParams.block_size_log;

    case FL2_p_bufferLog:
        if (value) {  /* 0 : does not change current bufferLog */
            CLAMPCHECK(value, FL2_BUFFER_SIZE_LOG_MIN, FL2_BUFFER_SIZE_LOG_MAX);
            cctx->params.rParams.match_buffer_log = value;
        }
        return cctx->params.rParams.match_buffer_log;

    case FL2_p_chainLog:
        if (value) { /* 0 : does not change current chainLog */
            CLAMPCHECK(value, FL2_CHAINLOG_MIN, FL2_CHAINLOG_MAX);
            cctx->params.cParams.second_dict_bits = value;
        }
        return cctx->params.cParams.second_dict_bits;

    case FL2_p_searchLog:
        if ((int)value >= 0) { /* < 0 : does not change current searchLog */
            MAXCHECK(value, FL2_SEARCHLOG_MAX);
            cctx->params.cParams.match_cycles = 1U << value;
        }
        return value;

    case FL2_p_literalCtxBits:
        if ((int)value >= 0) { /* < 0 : does not change current lc */
            MAXCHECK(value, FL2_LC_MAX);
            if(value + cctx->params.cParams.lp > FL2_LCLP_MAX)
                return FL2_ERROR(parameter_outOfBound);
            cctx->params.cParams.lc = value;
        }
        return cctx->params.cParams.lc;

    case FL2_p_literalPosBits:
        if ((int)value >= 0) { /* < 0 : does not change current lp */
            MAXCHECK(value, FL2_LP_MAX);
            if (cctx->params.cParams.lc + value > FL2_LCLP_MAX)
                return FL2_ERROR(parameter_outOfBound);
            cctx->params.cParams.lp = value;
        }
        return cctx->params.cParams.lp;

    case FL2_p_posBits:
        if ((int)value >= 0) { /* < 0 : does not change current pb */
            MAXCHECK(value, FL2_PB_MAX);
            cctx->params.cParams.pb = value;
        }
        return cctx->params.cParams.pb;

    case FL2_p_searchDepth:
        if (value) { /* 0 : does not change current depth */
            CLAMPCHECK(value, FL2_SEARCH_DEPTH_MIN, FL2_SEARCH_DEPTH_MAX);
            cctx->params.rParams.depth = value;
        }
        return cctx->params.rParams.depth;

    case FL2_p_fastLength:
        if (value) { /* 0 : does not change current fast_length */
            CLAMPCHECK(value, FL2_FASTLENGTH_MIN, FL2_FASTLENGTH_MAX);
            cctx->params.cParams.fast_length = value;
        }
        return cctx->params.cParams.fast_length;

    case FL2_p_divideAndConquer:
        if ((int)value >= 0) { /* < 0 : does not change current divide_and_conquer */
            cctx->params.rParams.divide_and_conquer = value;
        }
        return cctx->params.rParams.divide_and_conquer;

    case FL2_p_strategy:
        if ((int)value >= 0) { /* < 0 : does not change current strategy */
            MAXCHECK(value, (unsigned)FL2_ultra);
            cctx->params.cParams.strategy = (FL2_strategy)value;
        }
        return (size_t)cctx->params.cParams.strategy;

#ifndef NO_XXHASH
    case FL2_p_doXXHash:
        if ((int)value >= 0) { /* < 0 : does not change doXXHash */
            cctx->params.doXXH = value != 0;
        }
        return cctx->params.doXXH;
#endif

    case FL2_p_omitProperties:
        if ((int)value >= 0) { /* < 0 : does not change omitProp */
            cctx->params.omitProp = value != 0;
        }
        return cctx->params.omitProp;
#ifdef RMF_REFERENCE
    case FL2_p_useReferenceMF:
        if ((int)value >= 0) { /* < 0 : does not change useRefMF */
            cctx->params.rParams.use_ref_mf = value != 0;
        }
        return cctx->params.rParams.use_ref_mf;
#endif
    default: return FL2_ERROR(parameter_unsupported);
    }
}

static FL2_CStream* FL2_createCStream_internal(unsigned nbThreads, int async)
{
    FL2_CCtx* const cctx = FL2_createCCtx_internal(nbThreads);
    FL2_CStream* const fcs = malloc(sizeof(FL2_CStream));

    DEBUGLOG(3, "FL2_createCStream");

    if (cctx == NULL || fcs == NULL) {
        free(cctx);
        free(fcs);
        return NULL;
    }
    fcs->cctx = cctx;
    DICT_construct(&fcs->buf, async);
    fcs->hashPos = 0;
    fcs->endMarked = 0;
    fcs->wroteProp = 0;
    return fcs;
}

FL2LIB_API FL2_CStream* FL2LIB_CALL FL2_createCStream(void)
{
    return FL2_createCStream_internal(1, 0);
}

FL2LIB_API FL2_CStream* FL2LIB_CALL FL2_createCStreamMt(unsigned nbThreads)
{
    return FL2_createCStream_internal(nbThreads, 0);
}

FL2LIB_API FL2_CStream *FL2LIB_CALL FL2_createCStreamAsync(unsigned nbThreads)
{
    return FL2_createCStream_internal(nbThreads, 1);
}

FL2LIB_API size_t FL2LIB_CALL FL2_freeCStream(FL2_CStream* fcs)
{
    if (fcs == NULL)
        return 0;

    DEBUGLOG(3, "FL2_freeCStream");

    DICT_free(&fcs->buf);
    FL2_freeCCtx(fcs->cctx);
    free(fcs);
    return 0;
}

FL2LIB_API size_t FL2LIB_CALL FL2_initCStream(FL2_CStream* fcs, int compressionLevel)
{
    DEBUGLOG(4, "FL2_initCStream level %d", compressionLevel);

    fcs->hashPos = 0;
    fcs->endMarked = 0;
    fcs->wroteProp = 0;
    fcs->loopCount = 0;

    FL2_CCtx_setParameter(fcs->cctx, FL2_p_compressionLevel, compressionLevel);

    if(DICT_init(&fcs->buf,
        (size_t)1 << fcs->cctx->params.rParams.dictionary_log,
        fcs->cctx->params.doXXH && !fcs->cctx->params.omitProp) != 0)
        return FL2_ERROR(memory_allocation);

    FL2_beginFrame(fcs->cctx);
    return 0;
}

static size_t FL2_compressStream_internal(FL2_CStream* const fcs, int const ending)
{
    FL2_CCtx* const cctx = fcs->cctx;
    DICT_buffer *const buf = &fcs->buf;

    /* no compression can occur while compressed output exists */
    if (cctx->outThread == cctx->threadCount && DICT_hasUnprocessed(buf)) {
        if (FL2POOL_waitAll(fcs->cctx->compressThread, fcs->cctx->timeout) != 0)
            return FL2_ERROR(timedOut);

        if (cctx->outThread < cctx->threadCount)
            return 1;
            
        int streamProp = -1;

        if (!fcs->wroteProp && !cctx->params.omitProp) {
            size_t dictionarySize = ending ? cctx->dictMax : (size_t)1 << cctx->params.rParams.dictionary_log;
            streamProp = FL2_getProp(cctx, dictionarySize);
            DEBUGLOG(4, "Writing property byte : 0x%X", streamProp);
            fcs->wroteProp = 1;
        }

        DICT_getBlock(buf, &cctx->curBlock);

        cctx->streamTotal += cctx->curBlock.end - cctx->curBlock.start;

        CHECK_F(FL2_compressCurBlock(cctx, streamProp));

        return 0;
    }
    return 1;
}

FL2LIB_API size_t FL2LIB_CALL FL2_compressStream(FL2_CStream* fcs, FL2_inBuffer* input)
{
    DICT_buffer * const buf = &fcs->buf;
    FL2_CCtx* const cctx = fcs->cctx;
    size_t blockOverlap = OVERLAP_FROM_DICT_LOG(cctx->params.rParams.dictionary_log, cctx->params.rParams.overlap_fraction);
    size_t prevIn = input->pos;

    if (FL2_isError(cctx->asyncRes))
        return cctx->asyncRes;

    while (input->pos < input->size) {
        /* read input and/or write output until a buffer is full */
        if (DICT_needShift(buf, blockOverlap)) {
            /* cannot shift single dict during compression */
            if(!DICT_async(buf) && FL2POOL_waitAll(cctx->compressThread, cctx->timeout) != 0)
                return FL2_ERROR(timedOut);
            DICT_shift(buf, blockOverlap);
        }
        
        DICT_put(buf, input);
        
        if (!DICT_availSpace(buf)) {
            size_t res = FL2_compressStream_internal(fcs, 0);
            if (FL2_isError(res))
                return res;
            if (res != 0)
                break;
        }
    }
    if (prevIn == input->pos) {
        ++fcs->loopCount;
        if (fcs->loopCount > 1)
            return FL2_ERROR(infinite_loop);
    }
    else {
        fcs->loopCount = 0;
    }
    return cctx->outThread < cctx->threadCount;
}

FL2LIB_API size_t FL2LIB_CALL FL2_remainingOutputSize(const FL2_CStream* fcs)
{
    const FL2_CCtx* const cctx = fcs->cctx;
    size_t cSize = 0;

    if (FL2_isError(cctx->asyncRes))
        return cctx->asyncRes;

    for (size_t u = cctx->outThread; u < cctx->threadCount; ++u) {
        size_t to_write = cctx->jobs[u].cSize;

        cSize += to_write;
    }
    return cSize;
}

static size_t FL2_writeEnd(FL2_CStream* fcs)
{
    FL2_CCtx* const cctx = fcs->cctx;
    BYTE *dst;
    if (cctx->threadCount > 0) {
        dst = RMF_getTableAsOutputBuffer(cctx->matchTable, cctx->jobs[cctx->threadCount - 1].block.start)
            + cctx->jobs[cctx->threadCount - 1].cSize;
    }
    else {
        dst = RMF_getTableAsOutputBuffer(cctx->matchTable, 0);
    }
    DEBUGLOG(4, "Writing end marker");
    *dst++ = LZMA2_END_MARKER;

#ifndef NO_XXHASH
    if (cctx->params.doXXH && !cctx->params.omitProp) {
        XXH32_canonical_t canonical;

        XXH32_canonicalFromHash(&canonical, DICT_getDigest(&fcs->buf));
        DEBUGLOG(4, "Writing XXH32", (U32)to_write);
        memcpy(dst, &canonical, XXHASH_SIZEOF);

        return 1 + XXHASH_SIZEOF;
    }
#endif
    return 1;
}

static size_t FL2_flushStream_internal(FL2_CStream* fcs, int ending)
{
    if (FL2_isError(fcs->cctx->asyncRes))
        return fcs->cctx->asyncRes;

    DEBUGLOG(4, "FL2_flushStream_internal : %u to compress, %u to write",
        (U32)(fcs->inBuf.end - fcs->inBuf.start),
        (U32)FL2_remainingOutputSize(fcs->cctx));

    size_t res = 0;

    do {
        res = FL2_compressStream_internal(fcs, ending);
        CHECK_F(res);
    } while (res == 0);

    if(FL2POOL_waitAll(fcs->cctx->compressThread, fcs->cctx->timeout) != 0)
        return FL2_ERROR(timedOut);

    return FL2_remainingOutputSize(fcs);
}

FL2LIB_API size_t FL2LIB_CALL FL2_flushStream(FL2_CStream* fcs)
{
    return FL2_flushStream_internal(fcs, 0);
}

FL2LIB_API size_t FL2LIB_CALL FL2_endStream(FL2_CStream* fcs)
{
    size_t cSize = FL2_flushStream_internal(fcs, 1);

    CHECK_F(cSize);

    if (cSize != 0 && !DICT_hasUnprocessed(&fcs->buf))
        return cSize + FL2_writeEnd(fcs);

    return cSize;
}

FL2LIB_API size_t FL2LIB_CALL FL2_waitStream(FL2_CStream * fcs)
{
    return FL2_waitCCtx(fcs->cctx);
}

FL2LIB_API void FL2LIB_CALL FL2_getDictionaryBuffer(FL2_CStream * fcs, FL2_outBuffer * dict)
{
    FL2_CCtx* const cctx = fcs->cctx;
    size_t blockOverlap = OVERLAP_FROM_DICT_LOG(cctx->params.rParams.dictionary_log, cctx->params.rParams.overlap_fraction);
    DICT_get(&fcs->buf, blockOverlap, dict);
}

FL2LIB_API size_t FL2LIB_CALL FL2_updateDictionary(FL2_CStream * fcs, size_t addedSize)
{
    DICT_update(&fcs->buf, addedSize);
    if (!DICT_availSpace(&fcs->buf))
        CHECK_F(FL2_compressStream_internal(fcs, 0));
    return FL2_error_no_error;
}

FL2LIB_API size_t FL2LIB_CALL FL2_CStream_setParameter(FL2_CStream* fcs, FL2_cParameter param, unsigned value)
{
    if (fcs->cctx->dictMax != 0)
        return FL2_ERROR(stage_wrong);
    return FL2_CCtx_setParameter(fcs->cctx, param, value);
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


size_t FL2_memoryUsage_internal(unsigned const dictionaryLog, unsigned const bufferLog, unsigned const searchDepth,
    unsigned chainLog, FL2_strategy strategy,
    unsigned nbThreads)
{
    size_t size = RMF_memoryUsage(dictionaryLog, bufferLog, searchDepth, nbThreads);
    return size + FL2_lzma2MemoryUsage(chainLog, strategy, nbThreads);
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
    return FL2_memoryUsage_internal(params->dictionaryLog,
        params->bufferLog,
        params->searchDepth,
        params->chainLog,
        params->strategy,
        nbThreads);
}

FL2LIB_API size_t FL2LIB_CALL FL2_estimateCCtxSize_usingCCtx(const FL2_CCtx * cctx)
{
    return FL2_memoryUsage_internal(cctx->params.rParams.dictionary_log,
        cctx->params.rParams.match_buffer_log,
        cctx->params.rParams.depth,
        cctx->params.cParams.second_dict_bits,
        cctx->params.cParams.strategy,
        cctx->jobCount);
}

FL2LIB_API size_t FL2LIB_CALL FL2_estimateCStreamSize(int compressionLevel, unsigned nbThreads)
{
    return FL2_estimateCCtxSize(compressionLevel, nbThreads)
        + ((size_t)1 << FL2_defaultCParameters[compressionLevel].dictionaryLog);
}

FL2LIB_API size_t FL2LIB_CALL FL2_estimateCStreamSize_byParams(const FL2_compressionParameters * params, unsigned nbThreads)
{
    return FL2_estimateCCtxSize_byParams(params, nbThreads)
        + ((size_t)1 << params->dictionaryLog);
}

FL2LIB_API size_t FL2LIB_CALL FL2_estimateCStreamSize_usingCStream(const FL2_CStream* fcs)
{
    return FL2_estimateCCtxSize_usingCCtx(fcs->cctx)
        + ((size_t)1 << fcs->cctx->params.rParams.dictionary_log);
}
