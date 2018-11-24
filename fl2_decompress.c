/*
* Copyright (c) 2018, Conor McCarthy
* All rights reserved.
* Parts based on zstd_decompress.c copyright Yann Collet
*
* This source code is licensed under both the BSD-style license (found in the
* LICENSE file in the root directory of this source tree) and the GPLv2 (found
* in the COPYING file in the root directory of this source tree).
* You may select, at your option, one of the above-listed licenses.
*/

#include <string.h>
#include "fast-lzma2.h"
#include "fl2_internal.h"
#include "mem.h"
#include "util.h"
#include "lzma2_dec.h"
#include "fl2_pool.h"
#ifndef NO_XXHASH
#  include "xxhash.h"
#endif

FL2LIB_API size_t FL2LIB_CALL FL2_findDecompressedSize(const void *src, size_t srcSize)
{
    return FLzma2Dec_UnpackSize(src, srcSize);
}

FL2LIB_API size_t FL2LIB_CALL FL2_decompress(void* dst, size_t dstCapacity,
    const void* src, size_t compressedSize)
{
    size_t dSize;
    FL2_DCtx* const dctx = FL2_createDCtx();
    if(dctx == NULL)
        return FL2_ERROR(memory_allocation);
    dSize = FL2_decompressDCtx(dctx,
        dst, dstCapacity,
        src, compressedSize);
    FL2_freeDCtx(dctx);
    return dSize;
}

FL2LIB_API FL2_DCtx* FL2LIB_CALL FL2_createDCtx(void)
{
    DEBUGLOG(3, "FL2_createDCtx");
    FL2_DCtx* const dctx = malloc(sizeof(FL2_DCtx));
    if (dctx) {
        LzmaDec_Construct(dctx);
    }
    return dctx;
}

FL2LIB_API size_t FL2LIB_CALL FL2_freeDCtx(FL2_DCtx* dctx)
{
    if (dctx != NULL) {
        DEBUGLOG(3, "FL2_freeDCtx");
        FLzmaDec_Free(dctx);
        free(dctx);
    }
    return 0;
}

typedef struct
{
    FL2_DCtx* dctx;
    const void *src;
    size_t packPos;
    size_t packSize;
    size_t unpackPos;
    size_t unpackSize;
    size_t res;
    ELzmaFinishMode finish;
} BlockInfo;

/* FL2_decompressCtxBlock() : FL2POOL_function type */
static void FL2_decompressCtxBlock(void* const jobDescription, size_t n)
{
    BlockInfo* const blocks = (BlockInfo*)jobDescription;
    size_t srcLen = blocks[n].packSize;
    blocks[n].res = FLzma2Dec_DecodeToDic(blocks[n].dctx, blocks[n].unpackSize, blocks[n].src, &srcLen, blocks[n].finish);
    if (!FL2_isError(blocks[n].res))
        blocks[n].res = blocks[n].dctx->dicPos;
}

static size_t FL2_decompressCtxBlocksMt(BlockInfo* const blocks, const BYTE *src, BYTE *dst, size_t dstCapacity, BYTE prop, FL2POOL_ctx * const factory, size_t numThreads)
{
    if (dstCapacity < blocks[0].unpackSize)
        return FL2_ERROR(dstSize_tooSmall);
    blocks[0].packPos = 0;
    blocks[0].unpackPos = 0;
    blocks[0].src = src;
    for (size_t thread = 1; thread < numThreads; ++thread) {
        blocks[thread].packPos = blocks[thread - 1].packPos + blocks[thread - 1].packSize;
        blocks[thread].unpackPos = blocks[thread - 1].unpackPos + blocks[thread - 1].unpackSize;
        blocks[thread].src = src + blocks[thread].packPos;
        CHECK_F(FLzma2Dec_Init(blocks[thread].dctx, prop, dst + blocks[thread].unpackPos, blocks[thread].unpackSize));
    }
    if (dstCapacity < blocks[numThreads - 1].unpackPos + blocks[numThreads - 1].unpackSize)
        return FL2_ERROR(dstSize_tooSmall);
    for (size_t thread = 1; thread < numThreads; ++thread) {
        FL2POOL_add(factory, FL2_decompressCtxBlock, blocks, thread);
    }
    CHECK_F(FLzma2Dec_Init(blocks[0].dctx, prop, dst + blocks[0].unpackPos, blocks[0].unpackSize));
    FL2_decompressCtxBlock(blocks, 0);
    FL2POOL_waitAll(factory);
    size_t dSize = 0;
    for (size_t thread = 0; thread < numThreads; ++thread) {
        if (FL2_isError(blocks[thread].res))
            return blocks[thread].res;
        dSize += blocks[thread].res;
    }
    return dSize;
}

static FL2_resetMtBlocks(BlockInfo *blocks, unsigned numThreads)
{
    for (size_t thread = 0; thread < numThreads; ++thread) {
        blocks[thread].finish = LZMA_FINISH_ANY;
        blocks[thread].packSize = 0;
        blocks[thread].unpackSize = 0;
    }
}

size_t FL2_process_MtBlocks(FL2_DCtx* dctx,
    BlockInfo *blocks, FL2POOL_ctx *factory, unsigned numThreads,
    void* dst, size_t dstCapacity,
    const void* src, size_t *srcLen,
    BYTE prop)
{
    size_t srcSize = *srcLen;
    *srcLen = 0;
    size_t pos = 0;
    size_t unpackSize = 0;
    unsigned thread = 0;
    FL2_resetMtBlocks(blocks, numThreads);
    while (pos < srcSize) {
        ChunkParseInfo inf;
        int type = FLzma2Dec_ParseInput(src, pos, srcSize - pos, &inf);
        if (type == CHUNK_ERROR || type == CHUNK_MORE_DATA)
            return FL2_ERROR(corruption_detected);
        if (pos == 0 && type == CHUNK_DICT_RESET)
            type = CHUNK_CONTINUE;
        if (type == CHUNK_DICT_RESET || type == CHUNK_FINAL) {
            if (type == CHUNK_FINAL) {
                blocks[thread].finish = LZMA_FINISH_END;
                ++blocks[thread].packSize;
            }
            ++thread;
        }
        if (type == CHUNK_FINAL || (type == CHUNK_DICT_RESET && thread == numThreads)) {
            size_t res = FL2_decompressCtxBlocksMt(blocks, (BYTE*)src, dst, dstCapacity, prop, factory, thread);
            if (FL2_isError(res))
                return res;
            assert(res == blocks[thread - 1].unpackPos + blocks[thread - 1].unpackSize);
            unpackSize += res;
            dctx->dicPos = unpackSize;
            *srcLen += blocks[thread - 1].packPos + blocks[thread - 1].packSize;
            if (type == CHUNK_FINAL)
                return LZMA_STATUS_FINISHED_WITH_MARK;
            src = (BYTE*)src + pos;
            srcSize -= pos;
            dst = (BYTE*)dst + res;
            dstCapacity -= res;
            pos = 0;
            thread = 0;
            FL2_resetMtBlocks(blocks, numThreads);
        }
        else {
            blocks[thread].packSize += inf.packSize;
            blocks[thread].unpackSize += inf.unpackSize;
            pos += inf.packSize;
        }
    }
    return FL2_ERROR(srcSize_wrong);
}

size_t FL2_decompressDCtxMt(FL2_DCtx* dctx,
    void* dst, size_t dstCapacity,
    const void* src, size_t *srcLen,
    BYTE prop)
{
    size_t res = FL2_error_no_error;
    unsigned numThreads = UTIL_countPhysicalCores();
    numThreads += !numThreads;
    BlockInfo *blocks = malloc((numThreads) * sizeof(BlockInfo));
    FL2POOL_ctx *factory = FL2POOL_create(numThreads - 1);
    if (!blocks || !factory) {
        res = FL2_ERROR(memory_allocation);
        goto out;
    }
    blocks[0].dctx = dctx;
    for (unsigned thread = 1; thread < numThreads; ++thread) {
        blocks[thread].dctx = NULL;
    }
    for (unsigned thread = 1; thread < numThreads; ++thread) {
        blocks[thread].dctx = FL2_createDCtx();
        if (!blocks[thread].dctx){
            res = FL2_ERROR(memory_allocation);
            goto out;
        }
    }
    res = FL2_process_MtBlocks(dctx, blocks, factory, numThreads, dst, dstCapacity, src, srcLen, prop);
out:
    for (unsigned thread = 1; thread < numThreads; ++thread) {
        FL2_freeDCtx(blocks[thread].dctx);
    }
    free(blocks);
    FL2POOL_free(factory);
    return res;
}

FL2LIB_API size_t FL2LIB_CALL FL2_decompressDCtx(FL2_DCtx* dctx,
    void* dst, size_t dstCapacity,
    const void* src, size_t srcSize)
{
    size_t res;
    BYTE prop = *(const BYTE*)src;
    BYTE const do_hash = prop >> FL2_PROP_HASH_BIT;
    size_t dicPos = 0;
    const BYTE *srcBuf = src;
    size_t srcPos;
    size_t const srcEnd = srcSize - 1;

    ++srcBuf;
    --srcSize;

    prop &= FL2_LZMA_PROP_MASK;

    DEBUGLOG(4, "FL2_decompressDCtx : dict prop 0x%X, do hash %u", prop, do_hash);

    srcPos = srcSize;

#if 1
    res = FL2_decompressDCtxMt(dctx, dst, dstCapacity, srcBuf, &srcPos, prop);
#else
    CHECK_F(FLzma2Dec_Init(dctx, prop, dst, dstCapacity));

    dicPos = dctx->dicPos;

    res = FLzma2Dec_DecodeToDic(dctx, dstCapacity, srcBuf, &srcPos, LZMA_FINISH_END);
#endif

    if (FL2_isError(res))
        return res;
    if (res == LZMA_STATUS_NEEDS_MORE_INPUT)
        return FL2_ERROR(srcSize_wrong);

    dicPos = dctx->dicPos - dicPos;

#ifndef NO_XXHASH
    if (do_hash) {
        XXH32_canonical_t canonical;
        U32 hash;

        DEBUGLOG(4, "Checking hash");

        if (srcEnd - srcPos < XXHASH_SIZEOF)
            return FL2_ERROR(srcSize_wrong);
        memcpy(&canonical, srcBuf + srcPos, XXHASH_SIZEOF);
        hash = XXH32_hashFromCanonical(&canonical);
        if (hash != XXH32(dst, dicPos, 0))
            return FL2_ERROR(checksum_wrong);
    }
#endif
    return dicPos;
}

typedef enum
{
    FL2DEC_STAGE_INIT,
    FL2DEC_STAGE_MT_PARSE,
    FL2DEC_STAGE_DECOMP,
    FL2DEC_STAGE_MT_WRITE,
    FL2DEC_STAGE_HASH,
    FL2DEC_STAGE_FINISHED
} DecoderStage;

typedef struct
{
    CLzma2Dec dec;
    InputBlock inBlock;
    BYTE *outBuf;
    size_t bufSize;
    size_t res;
} ThreadInfo;

typedef struct
{
    FL2POOL_ctx* factory;
    InBufNode *head;
    size_t numThreads;
    size_t maxThreads;
    size_t srcThread;
    size_t srcPos;
    size_t hashPos;
    int isFinal;
    BYTE prop;
    XXH32_canonical_t hash;
    ThreadInfo threads[1];
} Lzma2DecMt;

struct FL2_DStream_s
{
#ifndef FL2_SINGLETHREAD
    Lzma2DecMt *decmt;
#endif
    CLzma2Dec dec;
#ifndef NO_XXHASH
    XXH32_state_t *xxh;
#endif
    DecoderStage stage;
    BYTE do_hash;
};

static void FL2_FreeInputBuffersUnfinished(Lzma2DecMt *decmt)
{
}

static void FL2_FreeOutputBuffers(Lzma2DecMt *decmt)
{
    for (size_t thread = 0; thread < decmt->maxThreads; ++thread) {
        free(decmt->threads[thread].outBuf);
        decmt->threads[thread].outBuf = NULL;
    }
    decmt->numThreads = 0;
}

static void FL2_Lzma2DecMt_Free(Lzma2DecMt *decmt)
{
    if (decmt) {
        FL2_FreeOutputBuffers(decmt);
        FLzma2Dec_FreeInbufNodeChain(decmt->head, NULL);
        FL2POOL_free(decmt->factory);
        free(decmt);
    }
}

static FL2_Lzma2DecMt_Init(Lzma2DecMt *decmt)
{
    if (decmt) {
        decmt->isFinal = 0;
        decmt->hashPos = 0;
        FL2_FreeOutputBuffers(decmt);
        FLzma2Dec_FreeInbufNodeChain(decmt->head->next, NULL);
        decmt->head->length = 0;
        decmt->threads[0].inBlock.first = decmt->head;
        decmt->threads[0].inBlock.last = decmt->head;
        decmt->threads[0].inBlock.startPos = 0;
        decmt->threads[0].inBlock.endPos = 0;
    }
}

static Lzma2DecMt *FL2_Lzma2DecMt_Create(unsigned maxThreads)
{
    maxThreads += !maxThreads;
    Lzma2DecMt *decmt = malloc(sizeof(Lzma2DecMt) + (maxThreads - 1) * sizeof(ThreadInfo));
    if (!decmt)
        return NULL;
    decmt->head = FLzma2Dec_CreateInbufNode(NULL);
    decmt->factory = FL2POOL_create(maxThreads - 1);
    if (maxThreads > 1 && decmt->factory == NULL) {
        FL2_Lzma2DecMt_Free(decmt);
        return NULL;
    }
    decmt->numThreads = 0;
    decmt->maxThreads = maxThreads;
    for (size_t n = 0; n < maxThreads; ++n) {
        decmt->threads[n].outBuf = NULL;
        LzmaDec_Construct(&decmt->threads[n].dec);
    }
    FL2_Lzma2DecMt_Init(decmt);
    return decmt;
}

static int FL2_ParseMt(Lzma2DecMt* decmt, InputBlock* inBlock)
{
    int res = CHUNK_MORE_DATA;
    ChunkParseInfo inf;
    InBufNode* node = inBlock->last;
    int first = inBlock->unpackSize == 0;
    if (node == NULL)
        return res;
    while (inBlock->endPos < node->length) {
        res = FLzma2Dec_ParseInput(node->inBuf, inBlock->endPos, node->length - inBlock->endPos, &inf);
        if (first && res == CHUNK_DICT_RESET)
            res = CHUNK_CONTINUE;
        if (res != CHUNK_CONTINUE)
            break;
        inBlock->endPos += inf.packSize;
        inBlock->unpackSize += inf.unpackSize;
        first = 0;
    }
    inBlock->endPos += (res == CHUNK_FINAL);
    return res;
}

static size_t FL2_decompressBlockMt(FL2_DStream* fds, size_t thread)
{
    Lzma2DecMt *decmt = fds->decmt;
    ThreadInfo *ti = &decmt->threads[thread];
    CLzma2Dec *dec = &ti->dec;
    CHECK_F(FLzma2Dec_Init(dec, decmt->prop, ti->outBuf, ti->bufSize));

    InBufNode *node = ti->inBlock.first;
    size_t inPos = ti->inBlock.startPos;
    int last = (thread == fds->decmt->numThreads - 1);
    while (1) {
        size_t srcSize = node->length - inPos;
        size_t const res = FLzma2Dec_DecodeToDic(dec, ti->bufSize, node->inBuf + inPos, &srcSize, last && node == ti->inBlock.last ? LZMA_FINISH_END : LZMA_FINISH_ANY);

        if (FL2_isError(res))
            return res;
        if (res == LZMA_STATUS_FINISHED_WITH_MARK) {
            DEBUGLOG(4, "Found end mark");
        }
        if (node == ti->inBlock.last)
            break;
        inPos += srcSize;
        if (inPos + LZMA_REQUIRED_INPUT_MAX >= node->length) {
            inPos -= node->length - LZMA_REQUIRED_INPUT_MAX;
            node = node->next;
        }
    }
    return 0;
}

static size_t FL2_writeStreamBlocks(FL2_DStream* fds, FL2_outBuffer* output)
{
    Lzma2DecMt *decmt = fds->decmt;
    for (; decmt->srcThread < fds->decmt->numThreads; ++decmt->srcThread) {
        ThreadInfo *thread = decmt->threads + decmt->srcThread;
        size_t to_write = MIN(thread->bufSize - decmt->srcPos, output->size - output->pos);
        memcpy((BYTE*)output->dst + output->pos, thread->outBuf + decmt->srcPos, to_write);
#ifndef NO_XXHASH
        if (fds->do_hash)
            XXH32_update(fds->xxh, (BYTE*)output->dst + output->pos, to_write);
#endif
        decmt->srcPos += to_write;
        output->pos += to_write;
        if (decmt->srcPos < thread->bufSize)
            break;
        decmt->srcPos = 0;
    }
    if (decmt->srcThread < fds->decmt->numThreads)
        return 0;
    FL2_FreeOutputBuffers(fds->decmt);
    fds->decmt->numThreads = 0;
    return 1;
}

/* FL2_decompressBlock() : FL2POOL_function type */
static void FL2_decompressBlock(void* const jobDescription, size_t n)
{
    FL2_DStream* const fds = (FL2_DStream*)jobDescription;
    fds->decmt->threads[n].res = FL2_decompressBlockMt(fds, n);
}

static size_t FL2_decompressBlocksMt(FL2_DStream* fds)
{
    Lzma2DecMt * const decmt = fds->decmt;
    for (size_t thread = 1; thread < fds->decmt->numThreads; ++thread) {
        FL2POOL_add(fds->decmt->factory, FL2_decompressBlock, fds, thread);
    }
    fds->decmt->threads[0].res = FL2_decompressBlockMt(fds, 0);
    FL2POOL_waitAll(fds->decmt->factory);

    if (decmt->numThreads > 0) {
        InBufNode *keep = decmt->threads[decmt->numThreads - 1].inBlock.last;
        FLzma2Dec_FreeInbufNodeChain(decmt->head, keep);
        decmt->head = keep;
        decmt->threads[0].inBlock.first = keep;
        decmt->threads[0].inBlock.last = keep;
        decmt->threads[0].inBlock.endPos = decmt->threads[decmt->numThreads - 1].inBlock.endPos;
        decmt->threads[0].inBlock.startPos = decmt->threads[0].inBlock.endPos;
        decmt->threads[0].inBlock.unpackSize = 0;
    }

    if (FL2_isError(fds->decmt->threads[0].res))
        return fds->decmt->threads[0].res;

    fds->decmt->srcThread = 0;
    fds->decmt->srcPos = 0;

    return 0;
}

static size_t FL2_LoadInputMt(Lzma2DecMt *decmt, FL2_inBuffer* input)
{
    InputBlock *inBlock = &decmt->threads[decmt->numThreads].inBlock;
    int res = CHUNK_CONTINUE;
    while (input->pos < input->size || inBlock->endPos < inBlock->last->length) {
        if (inBlock->endPos < inBlock->last->length) {
            res = FL2_ParseMt(decmt, inBlock);
            if (res == CHUNK_ERROR)
                return FL2_ERROR(corruption_detected);
            if (res == CHUNK_DICT_RESET || res == CHUNK_FINAL) {
                ThreadInfo * const done = decmt->threads + decmt->numThreads;

                done->bufSize = done->inBlock.unpackSize;
                done->outBuf = malloc(done->bufSize);

                if (!done->outBuf)
                    return FL2_ERROR(memory_allocation);

                decmt->isFinal = (res == CHUNK_FINAL);

                ++decmt->numThreads;
                if (decmt->numThreads == decmt->maxThreads || res == CHUNK_FINAL)
                    return 1;

                inBlock = &decmt->threads[decmt->numThreads].inBlock;
                inBlock->first = done->inBlock.last;
                inBlock->last = inBlock->first;
                inBlock->endPos = done->inBlock.endPos;
                inBlock->startPos = inBlock->endPos;
                inBlock->unpackSize = 0;
            }
        }
        if (inBlock->last->length >= LZMA2_MT_INPUT_SIZE && inBlock->endPos + LZMA_REQUIRED_INPUT_MAX >= inBlock->last->length) {
            InBufNode* prev = inBlock->last;
            inBlock->last = FLzma2Dec_CreateInbufNode(inBlock->last);
            if (!inBlock->last)
                return FL2_ERROR(memory_allocation);
            inBlock->endPos -= LZMA2_MT_INPUT_SIZE - LZMA_REQUIRED_INPUT_MAX;
        }

        {
            size_t toread = MIN(input->size - input->pos, LZMA2_MT_INPUT_SIZE - inBlock->last->length);
            memcpy(inBlock->last->inBuf + inBlock->last->length, (BYTE*)input->src + input->pos, toread);
            inBlock->last->length += toread;
            input->pos += toread;
            if (res == CHUNK_MORE_DATA && toread == 0)
                break;
        }
    }
    return res == CHUNK_FINAL;
}

static size_t FL2_decompressStreamMt(FL2_DStream* fds, FL2_outBuffer* output, FL2_inBuffer* input)
{
    Lzma2DecMt *decmt = fds->decmt;
    if (fds->stage == FL2DEC_STAGE_DECOMP) {
        size_t res;
        res = FL2_LoadInputMt(decmt, input);
        CHECK_F(res);
        if (res > 0) {
            CHECK_F(FL2_decompressBlocksMt(fds));
            fds->stage = FL2DEC_STAGE_MT_WRITE;
        }
    }
    if (fds->stage == FL2DEC_STAGE_MT_WRITE) {
        if (FL2_writeStreamBlocks(fds, output))
            fds->stage = decmt->isFinal ? (fds->do_hash ? FL2DEC_STAGE_HASH : FL2DEC_STAGE_FINISHED)
                : FL2DEC_STAGE_DECOMP;
    }
    if (fds->stage == FL2DEC_STAGE_HASH) {
        if (decmt->hashPos == 0) {
            size_t pos = decmt->threads[0].inBlock.endPos;
            decmt->hashPos = MIN(XXHASH_SIZEOF, decmt->head->length - pos);
            memcpy(&decmt->hash, decmt->head->inBuf + pos, decmt->hashPos);
        }
        size_t to_read = MIN(XXHASH_SIZEOF - decmt->hashPos, input->size - input->pos);
        memcpy(&decmt->hash + decmt->hashPos, (BYTE*)input->src + input->pos, to_read);
        decmt->hashPos += to_read;
        if (decmt->hashPos == XXHASH_SIZEOF) {
#ifndef NO_XXHASH
            U32 hash;

            DEBUGLOG(4, "Checking hash");

            hash = XXH32_hashFromCanonical(&decmt->hash);
            if (hash != XXH32_digest(fds->xxh))
                return FL2_ERROR(checksum_wrong);
#endif
            fds->stage = FL2DEC_STAGE_FINISHED;
        }
    }
    return fds->stage != FL2DEC_STAGE_FINISHED;
}

FL2LIB_API FL2_DStream* FL2LIB_CALL FL2_createDStream(void)
{
    FL2_DStream* const fds = malloc(sizeof(FL2_DStream));
    DEBUGLOG(3, "FL2_createDStream");
    if (fds) {
        LzmaDec_Construct(&fds->dec);
        fds->decmt = FL2_Lzma2DecMt_Create(UTIL_countPhysicalCores());
        fds->stage = FL2DEC_STAGE_INIT;
#ifndef NO_XXHASH
        fds->xxh = NULL;
#endif
        fds->do_hash = 0;
    }
    return fds;
}

FL2LIB_API size_t FL2LIB_CALL FL2_freeDStream(FL2_DStream* fds)
{
    if (fds != NULL) {
        DEBUGLOG(3, "FL2_freeDStream");
        FLzmaDec_Free(&fds->dec);
#ifndef NO_XXHASH
        XXH32_freeState(fds->xxh);
#endif
        free(fds);
    }
    return 0;
}

/*===== Streaming decompression functions =====*/
FL2LIB_API size_t FL2LIB_CALL FL2_initDStream(FL2_DStream* fds)
{
    DEBUGLOG(4, "FL2_initDStream");
    fds->stage = FL2DEC_STAGE_INIT;
    FL2_Lzma2DecMt_Init(fds->decmt);
    return 0;
}

FL2LIB_API size_t FL2LIB_CALL FL2_decompressStream(FL2_DStream* fds, FL2_outBuffer* output, FL2_inBuffer* input)
{
    if (input->pos < input->size || fds->decmt) {
        if (fds->stage == FL2DEC_STAGE_INIT) {
            BYTE prop = ((const BYTE*)input->src)[input->pos];
            ++input->pos;
            fds->do_hash = prop >> FL2_PROP_HASH_BIT;
            prop &= FL2_LZMA_PROP_MASK;

            if (fds->decmt)
                fds->decmt->prop = prop;
            else
                CHECK_F(FLzma2Dec_Init(&fds->dec, prop, NULL, 0));

#ifndef NO_XXHASH
            if (fds->do_hash) {
                if (fds->xxh == NULL) {
                    DEBUGLOG(3, "Creating hash state");
                    fds->xxh = XXH32_createState();
                    if (fds->xxh == NULL)
                        return FL2_ERROR(memory_allocation);
                }
                XXH32_reset(fds->xxh, 0);
            }
#endif
            fds->stage = FL2DEC_STAGE_DECOMP;
        }
        if (fds->decmt)
            return FL2_decompressStreamMt(fds, output, input);
        if (fds->stage == FL2DEC_STAGE_DECOMP) {
            size_t destSize = output->size - output->pos;
            size_t srcSize = input->size - input->pos;
            size_t const res = FLzma2Dec_DecodeToBuf(&fds->dec, (BYTE*)output->dst + output->pos, &destSize, (const BYTE*)input->src + input->pos, &srcSize, LZMA_FINISH_ANY);

            DEBUGLOG(5, "Decoded %u bytes", (U32)destSize);

#ifndef NO_XXHASH
            if(fds->do_hash)
                XXH32_update(fds->xxh, (BYTE*)output->dst + output->pos, destSize);
#endif

            output->pos += destSize;
            input->pos += srcSize;

            if (FL2_isError(res))
                return res;
            if (res == LZMA_STATUS_FINISHED_WITH_MARK) {
                DEBUGLOG(4, "Found end mark");
                fds->stage = fds->do_hash ? FL2DEC_STAGE_HASH : FL2DEC_STAGE_FINISHED;
            }
        }
        if (fds->stage == FL2DEC_STAGE_HASH) {
#ifndef NO_XXHASH
            XXH32_canonical_t canonical;
            U32 hash;

            DEBUGLOG(4, "Checking hash");

            if (input->size - input->pos < XXHASH_SIZEOF)
                return 1;
            memcpy(&canonical, (BYTE*)input->src + input->pos, XXHASH_SIZEOF);
            hash = XXH32_hashFromCanonical(&canonical);
            if (hash != XXH32_digest(fds->xxh))
                return FL2_ERROR(checksum_wrong);
#endif
            fds->stage = FL2DEC_STAGE_FINISHED;
        }
    }
    return fds->stage != FL2DEC_STAGE_FINISHED;
}

