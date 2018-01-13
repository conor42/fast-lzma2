/* Lzma2Dec.h -- LZMA2 Decoder
2017-04-03 : Igor Pavlov : Public domain */

#ifndef __LZMA2_DEC_H
#define __LZMA2_DEC_H

#include "mem.h"
#include "LzmaDec.h"

#if defined (__cplusplus)
extern "C" {
#endif

/* ---------- State Interface ---------- */

#define Lzma2Dec_Free(p) LzmaDec_Free((p));

SRes Lzma2Dec_AllocateProbs(CLzma2Dec *p, BYTE prop);
SRes Lzma2Dec_Allocate(CLzma2Dec *p, BYTE prop);
void Lzma2Dec_Init(CLzma2Dec *p);


/*
finishMode:
  It has meaning only if the decoding reaches output limit (*destLen or dicLimit).
  LZMA_FINISH_ANY - use smallest number of input bytes
  LZMA_FINISH_END - read EndOfStream marker after decoding

Returns:
  SZ_OK
    status:
      LZMA_STATUS_FINISHED_WITH_MARK
      LZMA_STATUS_NOT_FINISHED
      LZMA_STATUS_NEEDS_MORE_INPUT
  SZ_ERROR_DATA - Data error
*/

SRes Lzma2Dec_DecodeToDic(CLzma2Dec *p, size_t dicLimit,
    const BYTE *src, size_t *srcLen, ELzmaFinishMode finishMode, ELzmaStatus *status);

SRes Lzma2Dec_DecodeToBuf(CLzma2Dec *p, BYTE *dest, size_t *destLen,
    const BYTE *src, size_t *srcLen, ELzmaFinishMode finishMode, ELzmaStatus *status);


/* ---------- One Call Interface ---------- */

/*
finishMode:
  It has meaning only if the decoding reaches output limit (*destLen).
  LZMA_FINISH_ANY - use smallest number of input bytes
  LZMA_FINISH_END - read EndOfStream marker after decoding

Returns:
  SZ_OK
    status:
      LZMA_STATUS_FINISHED_WITH_MARK
      LZMA_STATUS_NOT_FINISHED
  SZ_ERROR_DATA - Data error
  SZ_ERROR_MEM  - Memory allocation error
  SZ_ERROR_UNSUPPORTED - Unsupported properties
  SZ_ERROR_INPUT_EOF - It needs more bytes in input buffer (src).
*/

SRes Lzma2Decode(BYTE *dest, size_t *destLen, const BYTE *src, size_t *srcLen,
    BYTE prop, ELzmaFinishMode finishMode, ELzmaStatus *status);

#if defined (__cplusplus)
}
#endif

#endif
