/* lzma2_enc.h -- LZMA2 Encoder
Based on LzmaEnc.h and Lzma2Enc.h : Igor Pavlov
Modified for FL2 by Conor McCarthy
Public domain
*/

#ifndef RADYX_LZMA2_ENCODER_H
#define RADYX_LZMA2_ENCODER_H

#include "mem.h"
#include "data_block.h"
#include "radix_mf.h"

#if defined (__cplusplus)
extern "C" {
#endif

#define kFastDistBits 12U

#define LZMA2_END_MARKER '\0'
#define LZMA_MIN_DICT_BITS 12
#define ENC_MIN_BYTES_PER_THREAD 0x20000


typedef struct FL2_lzmaEncoderCtx_s FL2_lzmaEncoderCtx;

typedef struct
{
    unsigned lc;
    unsigned lp;
    unsigned pb;
    unsigned fast_length;
    unsigned match_cycles;
    FL2_strategy strategy;
    unsigned second_dict_bits;
    unsigned random_filter;
} FL2_lzma2Parameters;


FL2_lzmaEncoderCtx* FL2_lzma2Create(void);

void FL2_lzma2Free(FL2_lzmaEncoderCtx* enc);

void FL2_lzma2InitProgress(FL2_lzmaEncoderCtx* enc);

size_t FL2_lzma2GetProgress(FL2_lzmaEncoderCtx* enc);

int FL2_lzma2HashAlloc(FL2_lzmaEncoderCtx* enc, const FL2_lzma2Parameters* options);

size_t FL2_lzma2Encode(FL2_lzmaEncoderCtx* enc,
    FL2_matchTable* tbl,
    const FL2_dataBlock block,
    const FL2_lzma2Parameters* options);

BYTE FL2_getDictSizeProp(size_t dictionary_size);

size_t FL2_lzma2MemoryUsage(unsigned chain_log, FL2_strategy strategy, unsigned thread_count);

#if defined (__cplusplus)
}
#endif

#endif /* RADYX_LZMA2_ENCODER_H */