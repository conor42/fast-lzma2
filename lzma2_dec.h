/* lzma2_dec.h -- LZMA2 Decoder
2017-04-03 : Igor Pavlov : Public domain
Modified for FL2 by Conor McCarthy */

#ifndef __LZMA_DEC_H
#define __LZMA_DEC_H

#include "mem.h"

#if defined (__cplusplus)
extern "C" {
#endif

/* #define LZMA_DEC_PROB16 */
/* 32-bit probs can increase the speed on some CPUs,
   but memory usage for CLzma2Dec::probs will be doubled in that case */

#ifdef LZMA_DEC_PROB16
#define Probability U16
#else
#define Probability U32
#endif


/* ---------- LZMA Properties ---------- */

typedef struct _CLzmaProps
{
  unsigned lc, lp, pb;
  U32 dicSize;
} CLzmaProps;

/* LzmaProps_Decode - decodes properties
Returns:
  SZ_OK
  SZ_ERROR_UNSUPPORTED - Unsupported properties
*/


/* ---------- LZMA Decoder state ---------- */

/* LZMA_REQUIRED_INPUT_MAX = number of required input bytes for worst case.
   Num bits = log2((2^11 / 31) ^ 22) + 26 < 134 + 26 = 160; */

#define LZMA_REQUIRED_INPUT_MAX 20

#define kNumPosBitsMax 4U
#define kNumPosStatesMax (1U << kNumPosBitsMax)

#define kLenNumLowBits 3U
#define kLenNumLowSymbols (1U << kLenNumLowBits)
#define kLenNumMidBits 3U
#define kLenNumMidSymbols (1U << kLenNumMidBits)
#define kLenNumHighBits 8U
#define kLenNumHighSymbols (1U << kLenNumHighBits)

#define LenChoice 0U
#define LenChoice2 (LenChoice + 1U)
#define LenLow (LenChoice2 + 1U)
#define LenMid (LenLow + (kNumPosStatesMax << kLenNumLowBits))
#define LenHigh (LenMid + (kNumPosStatesMax << kLenNumMidBits))
#define kNumLenProbs (LenHigh + kLenNumHighSymbols)


#define kNumStates 12U
#define kNumLitStates 7U

#define kStartPosModelIndex 4U
#define kEndPosModelIndex 14U
#define kNumFullDistances (1U << (kEndPosModelIndex >> 1))

#define kNumPosSlotBits 6U
#define kNumLenToPosStates 4U

#define kNumAlignBits 4U
#define kAlignTableSize (1U << kNumAlignBits)

#define kMatchMinLen 2U
#define kMatchSpecLenStart (kMatchMinLen + kLenNumLowSymbols + kLenNumMidSymbols + kLenNumHighSymbols)

#define IsMatch 0U
#define IsRep (IsMatch + (kNumStates << kNumPosBitsMax))
#define IsRepG0 (IsRep + kNumStates)
#define IsRepG1 (IsRepG0 + kNumStates)
#define IsRepG2 (IsRepG1 + kNumStates)
#define IsRep0Long (IsRepG2 + kNumStates)
#define PosSlot (IsRep0Long + (kNumStates << kNumPosBitsMax))
#define SpecPos (PosSlot + (kNumLenToPosStates << kNumPosSlotBits))
#define Align (SpecPos + kNumFullDistances - kEndPosModelIndex)
#define LenCoder (Align + kAlignTableSize)
#define RepLenCoder (LenCoder + kNumLenProbs)
#define Literal (RepLenCoder + kNumLenProbs)

#define LZMA_BASE_SIZE 1846U
#define LZMA_LIT_SIZE 0x300

#if Literal != LZMA_BASE_SIZE
StopCompilingDueBUG
#endif

#define LZMA2_LCLP_MAX 4U

typedef struct CLzma2Dec_s
{
    CLzmaProps prop;
    BYTE *dic;
    const BYTE *buf;
    U32 range, code;
    size_t dicPos;
    size_t dicBufSize;
    U32 processedPos;
    U32 checkDicSize;
    unsigned state;
    U32 reps[4];
    unsigned remainLen;
    int needFlush;
    int needInitState;
    U32 numProbs;
    U32 packSize;
    U32 unpackSize;
    unsigned state2;
    BYTE control;
    BYTE needInitDic;
    BYTE needInitState2;
    BYTE needInitProp;
    BYTE extDic;
    Probability probs[Literal + ((U32)LZMA_LIT_SIZE << LZMA2_LCLP_MAX)];
} CLzma2Dec;

typedef struct
{
    U32 packSize;
    U32 unpackSize;
    CLzmaProps prop;
} ChunkInfo;

void LzmaDec_Construct(CLzma2Dec *p);

void FLzmaDec_Init(CLzma2Dec *p);

typedef enum
{
  LZMA_FINISH_ANY,   /* finish at any point */
  LZMA_FINISH_END    /* block must be finished at the end */
} ELzmaFinishMode;

/* ELzmaFinishMode has meaning only if the decoding reaches output limit !!!

   You must use LZMA_FINISH_END, when you know that current output buffer
   covers last bytes of block. In other cases you must use LZMA_FINISH_ANY.

   If LZMA decoder sees end marker before reaching output limit, it returns SZ_OK,
   and output value of destLen will be less than output buffer size limit.
   You can check status result also.

   You can use multiple checks to test data integrity after full decompression:
     1) Check Result and "status" variable.
     2) Check that output(destLen) = uncompressedSize, if you know real uncompressedSize.
     3) Check that output(srcLen) = compressedSize, if you know real compressedSize.
        You must use correct finish mode in that case. */

typedef enum
{
  LZMA_STATUS_NOT_SPECIFIED,               /* use main error code instead */
  LZMA_STATUS_FINISHED_WITH_MARK,          /* stream was finished with end mark. */
  LZMA_STATUS_NOT_FINISHED,                /* stream was not finished */
  LZMA_STATUS_NEEDS_MORE_INPUT,            /* you must provide more input bytes */
  LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK  /* there is probability that stream was finished without end mark */
} ELzmaStatus;

/* ELzmaStatus is used only as output value for function call */


void FLzmaDec_Free(CLzma2Dec *state);

size_t FLzmaDec_DecodeToDic(CLzma2Dec *p, size_t dicLimit,
    const BYTE *src, size_t *srcLen, ELzmaFinishMode finishMode);


size_t FLzmaDec_DecodeToBuf(CLzma2Dec *p, BYTE *dest, size_t *destLen,
    const BYTE *src, size_t *srcLen, ELzmaFinishMode finishMode);

#define LZMA2_CONTENTSIZE_ERROR   (size_t)-1

size_t FLzma2Dec_UnpackSize(const BYTE *src, size_t srcLen);

size_t FLzma2Dec_Init(CLzma2Dec *p, BYTE dictProp, BYTE *dic, size_t dicBufSize);

size_t FLzma2Dec_DecodeToDic(CLzma2Dec *p, size_t dicLimit,
    const BYTE *src, size_t *srcLen, ELzmaFinishMode finishMode);

size_t FLzma2Dec_DecodeToBuf(CLzma2Dec *p, BYTE *dest, size_t *destLen,
    const BYTE *src, size_t *srcLen, ELzmaFinishMode finishMode);


#if defined (__cplusplus)
}
#endif

#endif
