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
#include "xxhash.h"

FL2LIB_API size_t FL2_findDecompressedSize(const void *src, size_t srcSize)
{
    return Lzma2Dec_UnpackSize(src, srcSize);
}

FL2LIB_API size_t FL2_decompress(void* dst, size_t dstCapacity,
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

FL2LIB_API FL2_DCtx* FL2_createDCtx(void)
{
    DEBUGLOG(3, "FL2_createDCtx");
    FL2_DCtx* const dctx = malloc(sizeof(FL2_DCtx));
    if (dctx) {
        LzmaDec_Construct(dctx);
    }
    return dctx;
}

FL2LIB_API size_t FL2_freeDCtx(FL2_DCtx* dctx)
{
    if (dctx != NULL) {
        DEBUGLOG(3, "FL2_freeDCtx");
        LzmaDec_Free(dctx);
        free(dctx);
    }
    return 0;
}

FL2LIB_API size_t FL2_decompressDCtx(FL2_DCtx* dctx,
    void* dst, size_t dstCapacity,
    const void* src, size_t srcSize)
{
    size_t res;
    BYTE prop = *(const BYTE*)src;
    BYTE const do_hash = prop >> FL2_PROP_HASH_BIT;
    size_t dicPos;
    const BYTE *srcBuf = src;
    size_t const srcEnd = srcSize - 1;
    ++srcBuf;
    --srcSize;
    prop &= FL2_LZMA_PROP_MASK;

    DEBUGLOG(4, "FL2_decompressDCtx : dict prop 0x%X, do hash %u", prop, do_hash);

    CHECK_F(Lzma2Dec_Init(dctx, prop, dst, dstCapacity));

    dicPos = dctx->dicPos;

    res = Lzma2Dec_DecodeToDic(dctx, dstCapacity, srcBuf, &srcSize, LZMA_FINISH_END);
    if (FL2_isError(res))
        return res;
    if (res == LZMA_STATUS_NEEDS_MORE_INPUT)
        return FL2_ERROR(srcSize_wrong);

    dicPos = dctx->dicPos - dicPos;

    if (do_hash) {
        XXH32_canonical_t canonical;
        U32 hash;
        DEBUGLOG(4, "Checking hash");
        if (srcEnd - srcSize < XXHASH_SIZEOF)
            return FL2_ERROR(srcSize_wrong);
        memcpy(&canonical, srcBuf + srcSize, XXHASH_SIZEOF);
        hash = XXH32_hashFromCanonical(&canonical);
        if (hash != XXH32(dst, dicPos, 0))
            return FL2_ERROR(corruption_detected);
    }
    return dicPos;
}

typedef enum
{
    FL2DEC_STAGE_INIT,
    FL2DEC_STAGE_DECOMP,
    FL2DEC_STAGE_HASH,
    FL2DEC_STAGE_FINISHED
} DecoderStage;

struct FL2_DStream_s
{
    CLzma2Dec dec;
    XXH32_state_t *xxh;
    DecoderStage stage;
    BYTE do_hash;
};

FL2LIB_API FL2_DStream* FL2_createDStream(void)
{
    FL2_DStream* const fds = malloc(sizeof(FL2_DStream));
    DEBUGLOG(3, "FL2_createDStream");
    if (fds) {
        LzmaDec_Construct(&fds->dec);
        fds->stage = FL2DEC_STAGE_INIT;
        fds->xxh = NULL;
        fds->do_hash = 0;
    }
    return fds;
}

FL2LIB_API size_t FL2_freeDStream(FL2_DStream* fds)
{
    if (fds != NULL) {
        DEBUGLOG(3, "FL2_freeDStream");
        LzmaDec_Free(&fds->dec);
        XXH32_freeState(fds->xxh);
        free(fds);
    }
    return 0;
}

/*===== Streaming decompression functions =====*/
FL2LIB_API size_t FL2_initDStream(FL2_DStream* fds)
{
    DEBUGLOG(4, "FL2_initDStream");
    fds->stage = FL2DEC_STAGE_INIT;
    return 0;
}

FL2LIB_API size_t FL2_decompressStream(FL2_DStream* fds, FL2_outBuffer* output, FL2_inBuffer* input)
{
    if (input->pos < input->size) {
        if (fds->stage == FL2DEC_STAGE_INIT) {
            BYTE prop = ((const BYTE*)input->src)[input->pos];
            ++input->pos;
            fds->do_hash = prop >> FL2_PROP_HASH_BIT;
            prop &= FL2_LZMA_PROP_MASK;

            CHECK_F(Lzma2Dec_Init(&fds->dec, prop, NULL, 0));

            if (fds->do_hash) {
                if (fds->xxh == NULL) {
                    DEBUGLOG(3, "Creating hash state");
                    fds->xxh = XXH32_createState();
                    if (fds->xxh == NULL)
                        return FL2_ERROR(memory_allocation);
                }
                XXH32_reset(fds->xxh, 0);
            }
            fds->stage = FL2DEC_STAGE_DECOMP;
        }
        if (fds->stage == FL2DEC_STAGE_DECOMP) {
            size_t destSize = output->size - output->pos;
            size_t srcSize = input->size - input->pos;
            size_t const res = Lzma2Dec_DecodeToBuf(&fds->dec, (BYTE*)output->dst + output->pos, &destSize, (const BYTE*)input->src + input->pos, &srcSize, LZMA_FINISH_ANY);

            DEBUGLOG(5, "Decoded %u bytes", (U32)destSize);

            if(fds->do_hash)
                XXH32_update(fds->xxh, (BYTE*)output->dst + output->pos, destSize);

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
            XXH32_canonical_t canonical;
            U32 hash;
            DEBUGLOG(4, "Checking hash");
            if (input->size - input->pos < XXHASH_SIZEOF)
                return 1;
            memcpy(&canonical, (BYTE*)input->src + input->pos, XXHASH_SIZEOF);
            hash = XXH32_hashFromCanonical(&canonical);
            if (hash != XXH32_digest(fds->xxh))
                return FL2_ERROR(corruption_detected);
            fds->stage = FL2DEC_STAGE_FINISHED;
        }
    }
    return fds->stage != FL2DEC_STAGE_FINISHED;
}