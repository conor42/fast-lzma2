/*
 * Copyright (c) 2018, Conor McCarthy
 * All rights reserved.
 * Parts based on zstd_compress_internal.h copyright Yann Collet
 *
 * This source code is licensed under both the BSD-style license (found in the
 * LICENSE file in the root directory of this source tree) and the GPLv2 (found
 * in the COPYING file in the root directory of this source tree).
 * You may select, at your option, one of the above-listed licenses.
 */

#ifndef FL2_COMPRESS_H
#define FL2_COMPRESS_H

/*-*************************************
*  Dependencies
***************************************/
#include "mem.h"
#include "data_block.h"
#include "radix_internal.h"
#include "lzma2_enc.h"
#include "fast-lzma2.h"
#include "fl2_threading.h"
#include "fl2_pool.h"
#include "dict_buffer.h"
#ifndef NO_XXHASH
#  include "xxhash.h"
#endif

#if defined (__cplusplus)
extern "C" {
#endif

/*-*************************************
*  Context memory management
***************************************/

typedef struct {
    FL2_lzma2Parameters cParams;
    RMF_parameters rParams;
    unsigned compressionLevel;
    BYTE highCompression;
#ifndef NO_XXHASH
    BYTE doXXH;
#endif
    BYTE omitProp;
} FL2_CCtx_params;

typedef struct {
    FL2_CCtx* cctx;
    FL2_lzmaEncoderCtx* enc;
    FL2_dataBlock block;
    size_t cSize;
} FL2_job;

struct FL2_CCtx_s {
    DICT_buffer buf;
    FL2_CCtx_params params;
#ifndef FL2_SINGLETHREAD
    FL2POOL_ctx* factory;
#endif
    FL2POOL_ctx* compressThread;
    FL2_dataBlock curBlock;
    size_t asyncRes;
    size_t threadCount;
    size_t outThread;
    size_t dictMax;
    U64 blockTotal;
    U64 streamTotal;
    FL2_matchTable* matchTable;
    U32 timeout;
    U32 rmfWeight;
    U32 encWeight;
    FL2_atomic encProgress;
    int canceled;
    BYTE wroteProp;
    BYTE endMarked;
    BYTE loopCount;
    unsigned jobCount;
    FL2_job jobs[1];
};

#if defined (__cplusplus)
}
#endif


#endif /* FL2_COMPRESS_H */
