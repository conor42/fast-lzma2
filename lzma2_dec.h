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

typedef struct CLzmaProps_s
{
	BYTE lc;
	BYTE lp;
	BYTE pb;
	BYTE pad_;
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

#define kNumPosBitsMax 4
#define kNumPosStatesMax (1 << kNumPosBitsMax)

#define kLenNumLowBits 3
#define kLenNumLowSymbols (1 << kLenNumLowBits)
#define kLenNumHighBits 8
#define kLenNumHighSymbols (1 << kLenNumHighBits)

#define LenLow 0
#define LenHigh (LenLow + 2 * (kNumPosStatesMax << kLenNumLowBits))
#define kNumLenProbs (LenHigh + kLenNumHighSymbols)

#define LenChoice LenLow
#define LenChoice2 (LenLow + (1 << kLenNumLowBits))

#define kNumStates 12
#define kNumStates2 16
#define kNumLitStates 7

#define kStartPosModelIndex 4
#define kEndPosModelIndex 14
#define kNumFullDistances (1 << (kEndPosModelIndex >> 1))

#define kNumPosSlotBits 6
#define kNumLenToPosStates 4

#define kNumAlignBits 4
#define kAlignTableSize (1 << kNumAlignBits)

#define kMatchMinLen 2
#define kMatchSpecLenStart (kMatchMinLen + kLenNumLowSymbols * 2 + kLenNumHighSymbols)

/* External ASM code needs same CLzmaProb array layout. So don't change it. */

/* (probs_1664) is faster and better for code size at some platforms */
/*
#ifdef MY_CPU_X86_OR_AMD64
*/
#define kStartOffset 1664
#define GET_PROBS p->probs_1664
/*
#define GET_PROBS p->probs + kStartOffset
#else
#define kStartOffset 0
#define GET_PROBS p->probs
#endif
*/

#define SpecPos (-kStartOffset)
#define IsRep0Long (SpecPos + kNumFullDistances)
#define RepLenCoder (IsRep0Long + (kNumStates2 << kNumPosBitsMax))
#define LenCoder (RepLenCoder + kNumLenProbs)
#define IsMatch (LenCoder + kNumLenProbs)
#define Align (IsMatch + (kNumStates2 << kNumPosBitsMax))
#define IsRep (Align + kAlignTableSize)
#define IsRepG0 (IsRep + kNumStates)
#define IsRepG1 (IsRepG0 + kNumStates)
#define IsRepG2 (IsRepG1 + kNumStates)
#define PosSlot (IsRepG2 + kNumStates)
#define Literal (PosSlot + (kNumLenToPosStates << kNumPosSlotBits))
#define NUM_BASE_PROBS (Literal + kStartOffset)

#if Align != 0 && kStartOffset != 0
#error Stop_Compiling_Bad_LZMA_kAlign
#endif

#if NUM_BASE_PROBS != 1984
#error Stop_Compiling_Bad_LZMA_PROBS
#endif


#define LZMA_LIT_SIZE 0x300

#define LzmaProps_GetNumProbs(p) (NUM_BASE_PROBS + ((U32)LZMA_LIT_SIZE << ((p)->lc + (p)->lp)))


#define CALC_POS_STATE(processedPos, pbMask) (((processedPos) & (pbMask)) << 4)
#define COMBINED_PS_STATE (posState + state)
#define GET_LEN_STATE (posState)

#define LZMA2_LCLP_MAX 4U


typedef struct CLzma2Dec_s
{
    CLzmaProps prop;
    BYTE *dic;
	size_t dicPos;
	size_t dicBufSize;
	const BYTE *buf;
	Probability *probs_1664;
	U32 range;
	U32 code;
    U32 processedPos;
    U32 checkDicSize;
    U32 reps[4];
	unsigned state;
	unsigned state2;
	unsigned remainLen;
    U32 packSize;
    U32 unpackSize;
    BYTE control;
    BYTE needInitDic;
	BYTE needInitState;
	BYTE needInitState2;
    BYTE needInitProp;
	BYTE needFlush;
	BYTE extDic;
	BYTE pad_;
    Probability probs[NUM_BASE_PROBS + ((U32)LZMA_LIT_SIZE << LZMA2_LCLP_MAX)];
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
