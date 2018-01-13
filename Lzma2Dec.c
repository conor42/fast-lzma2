/* Lzma2Dec.c -- LZMA2 Decoder
2017-04-03 : Igor Pavlov : Public domain */

/* #define SHOW_DEBUG_INFO */

#ifdef SHOW_DEBUG_INFO
#include <stdio.h>
#endif

#include <string.h>

#include "Lzma2Dec.h"

/*
00000000  -  EOS
00000001 U U  -  Uncompressed Reset Dic
00000010 U U  -  Uncompressed No Reset
100uuuuu U U P P  -  LZMA no reset
101uuuuu U U P P  -  LZMA reset state
110uuuuu U U P P S  -  LZMA reset state + new prop
111uuuuu U U P P S  -  LZMA reset state + new prop + reset dic

  u, U - Unpack Size
  P - Pack Size
  S - Props
*/

#define LZMA2_CONTROL_LZMA (1 << 7)
#define LZMA2_CONTROL_COPY_NO_RESET 2
#define LZMA2_CONTROL_COPY_RESET_DIC 1
#define LZMA2_CONTROL_EOF 0

#define LZMA2_IS_UNCOMPRESSED_STATE(p) (((p)->control & LZMA2_CONTROL_LZMA) == 0)

#define LZMA2_GET_LZMA_MODE(p) (((p)->control >> 5) & 3)
#define LZMA2_IS_THERE_PROP(mode) ((mode) >= 2)

#define LZMA2_LCLP_MAX 4
#define LZMA2_DIC_SIZE_FROM_PROP(p) (((U32)2 | ((p) & 1)) << ((p) / 2 + 11))

#ifdef SHOW_DEBUG_INFO
#define PRF(x) x
#else
#define PRF(x)
#endif

typedef enum
{
  LZMA2_STATE_CONTROL,
  LZMA2_STATE_UNPACK0,
  LZMA2_STATE_UNPACK1,
  LZMA2_STATE_PACK0,
  LZMA2_STATE_PACK1,
  LZMA2_STATE_PROP,
  LZMA2_STATE_DATA,
  LZMA2_STATE_DATA_CONT,
  LZMA2_STATE_FINISHED,
  LZMA2_STATE_ERROR
} ELzma2State;

static SRes Lzma2Dec_GetOldProps(BYTE prop, BYTE *props)
{
  U32 dicSize;
  if (prop > 40)
    return SZ_ERROR_UNSUPPORTED;
  dicSize = (prop == 40) ? 0xFFFFFFFF : LZMA2_DIC_SIZE_FROM_PROP(prop);
  props[0] = (BYTE)LZMA2_LCLP_MAX;
  props[1] = (BYTE)(dicSize);
  props[2] = (BYTE)(dicSize >> 8);
  props[3] = (BYTE)(dicSize >> 16);
  props[4] = (BYTE)(dicSize >> 24);
  return SZ_OK;
}

SRes Lzma2Dec_AllocateProbs(CLzma2Dec *p, BYTE prop)
{
  BYTE props[LZMA_PROPS_SIZE];
  RINOK(Lzma2Dec_GetOldProps(prop, props));
  return LzmaDec_AllocateProbs(p, props, LZMA_PROPS_SIZE);
}

SRes Lzma2Dec_Allocate(CLzma2Dec *p, BYTE prop)
{
  BYTE props[LZMA_PROPS_SIZE];
  RINOK(Lzma2Dec_GetOldProps(prop, props));
  return LzmaDec_Allocate(p, props, LZMA_PROPS_SIZE);
}

void Lzma2Dec_Init(CLzma2Dec *p)
{
  p->state2 = LZMA2_STATE_CONTROL;
  p->needInitDic = 1;
  p->needInitState2 = 1;
  p->needInitProp = 1;
  LzmaDec_Init(p);
}

static ELzma2State Lzma2Dec_UpdateState(CLzma2Dec *p, BYTE b)
{
  switch (p->state2)
  {
    case LZMA2_STATE_CONTROL:
      p->control = b;
      PRF(printf("\n %4X ", (unsigned)p->dicPos));
      PRF(printf(" %2X", (unsigned)b));
      if (b == 0)
        return LZMA2_STATE_FINISHED;
      if (LZMA2_IS_UNCOMPRESSED_STATE(p))
      {
        if (b > 2)
          return LZMA2_STATE_ERROR;
        p->unpackSize = 0;
      }
      else
        p->unpackSize = (U32)(b & 0x1F) << 16;
      return LZMA2_STATE_UNPACK0;
    
    case LZMA2_STATE_UNPACK0:
      p->unpackSize |= (U32)b << 8;
      return LZMA2_STATE_UNPACK1;
    
    case LZMA2_STATE_UNPACK1:
      p->unpackSize |= (U32)b;
      p->unpackSize++;
      PRF(printf(" %8u", (unsigned)p->unpackSize));
      return (LZMA2_IS_UNCOMPRESSED_STATE(p)) ? LZMA2_STATE_DATA : LZMA2_STATE_PACK0;
    
    case LZMA2_STATE_PACK0:
      p->packSize = (U32)b << 8;
      return LZMA2_STATE_PACK1;

    case LZMA2_STATE_PACK1:
      p->packSize |= (U32)b;
      p->packSize++;
      PRF(printf(" %8u", (unsigned)p->packSize));
      return LZMA2_IS_THERE_PROP(LZMA2_GET_LZMA_MODE(p)) ? LZMA2_STATE_PROP:
        (p->needInitProp ? LZMA2_STATE_ERROR : LZMA2_STATE_DATA);

    case LZMA2_STATE_PROP:
    {
      unsigned lc, lp;
      if (b >= (9 * 5 * 5))
        return LZMA2_STATE_ERROR;
      lc = b % 9;
      b /= 9;
      p->prop.pb = b / 5;
      lp = b % 5;
      if (lc + lp > LZMA2_LCLP_MAX)
        return LZMA2_STATE_ERROR;
      p->prop.lc = lc;
      p->prop.lp = lp;
      p->needInitProp = 0;
      return LZMA2_STATE_DATA;
    }
  }
  return LZMA2_STATE_ERROR;
}

static void LzmaDec_UpdateWithUncompressed(CLzma2Dec *p, const BYTE *src, size_t size)
{
  memcpy(p->dic + p->dicPos, src, size);
  p->dicPos += size;
  if (p->checkDicSize == 0 && p->prop.dicSize - p->processedPos <= size)
    p->checkDicSize = p->prop.dicSize;
  p->processedPos += (U32)size;
}

void LzmaDec_InitDicAndState(CLzma2Dec *p, BYTE initDic, BYTE initState);


SRes Lzma2Dec_DecodeToDic(CLzma2Dec *p, size_t dicLimit,
    const BYTE *src, size_t *srcLen, ELzmaFinishMode finishMode, ELzmaStatus *status)
{
  size_t inSize = *srcLen;
  *srcLen = 0;
  *status = LZMA_STATUS_NOT_SPECIFIED;

  while (p->state2 != LZMA2_STATE_ERROR)
  {
    size_t dicPos;

    if (p->state2 == LZMA2_STATE_FINISHED)
    {
      *status = LZMA_STATUS_FINISHED_WITH_MARK;
      return SZ_OK;
    }
    
    dicPos = p->dicPos;
    
    if (dicPos == dicLimit && finishMode == LZMA_FINISH_ANY)
    {
      *status = LZMA_STATUS_NOT_FINISHED;
      return SZ_OK;
    }

    if (p->state2 != LZMA2_STATE_DATA && p->state2 != LZMA2_STATE_DATA_CONT)
    {
      if (*srcLen == inSize)
      {
        *status = LZMA_STATUS_NEEDS_MORE_INPUT;
        return SZ_OK;
      }
      (*srcLen)++;
      p->state2 = Lzma2Dec_UpdateState(p, *src++);
      if (dicPos == dicLimit && p->state2 != LZMA2_STATE_FINISHED)
        break;
      continue;
    }
    
    {
      size_t inCur = inSize - *srcLen;
      size_t outCur = dicLimit - dicPos;
      ELzmaFinishMode curFinishMode = LZMA_FINISH_ANY;
      
      if (outCur >= p->unpackSize)
      {
        outCur = (size_t)p->unpackSize;
        curFinishMode = LZMA_FINISH_END;
      }

      if (LZMA2_IS_UNCOMPRESSED_STATE(p))
      {
        if (inCur == 0)
        {
          *status = LZMA_STATUS_NEEDS_MORE_INPUT;
          return SZ_OK;
        }

        if (p->state2 == LZMA2_STATE_DATA)
        {
          BYTE initDic = (p->control == LZMA2_CONTROL_COPY_RESET_DIC);
          if (initDic)
            p->needInitProp = p->needInitState2 = 1;
          else if (p->needInitDic)
            break;
          p->needInitDic = 0;
          LzmaDec_InitDicAndState(p, initDic, 0);
        }

        if (inCur > outCur)
          inCur = outCur;
        if (inCur == 0)
          break;

        LzmaDec_UpdateWithUncompressed(p, src, inCur);

        src += inCur;
        *srcLen += inCur;
        p->unpackSize -= (U32)inCur;
        p->state2 = (p->unpackSize == 0) ? LZMA2_STATE_CONTROL : LZMA2_STATE_DATA_CONT;
      }
      else
      {
        SRes res;

        if (p->state2 == LZMA2_STATE_DATA)
        {
          unsigned mode = LZMA2_GET_LZMA_MODE(p);
          BYTE initDic = (mode == 3);
          BYTE initState = (mode != 0);
          if ((!initDic && p->needInitDic) || (!initState && p->needInitState2))
            break;

          LzmaDec_InitDicAndState(p, initDic, initState);
          p->needInitDic = 0;
          p->needInitState2 = 0;
          p->state2 = LZMA2_STATE_DATA_CONT;
        }
  
        if (inCur > p->packSize)
          inCur = (size_t)p->packSize;
          
        res = LzmaDec_DecodeToDic(p, dicPos + outCur, src, &inCur, curFinishMode, status);
        
        src += inCur;
        *srcLen += inCur;
        p->packSize -= (U32)inCur;
        outCur = p->dicPos - dicPos;
        p->unpackSize -= (U32)outCur;

        if (res != 0)
          break;
        
        if (*status == LZMA_STATUS_NEEDS_MORE_INPUT)
        {
          if (p->packSize == 0)
            break;
          return SZ_OK;
        }

        if (inCur == 0 && outCur == 0)
        {
          if (*status != LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK
              || p->unpackSize != 0
              || p->packSize != 0)
            break;
          p->state2 = LZMA2_STATE_CONTROL;
        }
        
        *status = LZMA_STATUS_NOT_SPECIFIED;
      }
    }
  }
  
  *status = LZMA_STATUS_NOT_SPECIFIED;
  p->state2 = LZMA2_STATE_ERROR;
  return SZ_ERROR_DATA;
}


SRes Lzma2Dec_DecodeToBuf(CLzma2Dec *p, BYTE *dest, size_t *destLen, const BYTE *src, size_t *srcLen, ELzmaFinishMode finishMode, ELzmaStatus *status)
{
  size_t outSize = *destLen, inSize = *srcLen;
  *srcLen = *destLen = 0;
  
  for (;;)
  {
    size_t inCur = inSize, outCur, dicPos;
    ELzmaFinishMode curFinishMode;
    SRes res;
    
    if (p->dicPos == p->dicBufSize)
      p->dicPos = 0;
    dicPos = p->dicPos;
    curFinishMode = LZMA_FINISH_ANY;
    outCur = p->dicBufSize - dicPos;
    
    if (outCur >= outSize)
    {
      outCur = outSize;
      curFinishMode = finishMode;
    }

    res = Lzma2Dec_DecodeToDic(p, dicPos + outCur, src, &inCur, curFinishMode, status);
    
    src += inCur;
    inSize -= inCur;
    *srcLen += inCur;
    outCur = p->dicPos - dicPos;
    memcpy(dest, p->dic + dicPos, outCur);
    dest += outCur;
    outSize -= outCur;
    *destLen += outCur;
    if (res != 0)
      return res;
    if (outCur == 0 || outSize == 0)
      return SZ_OK;
  }
}


SRes Lzma2Decode(BYTE *dest, size_t *destLen, const BYTE *src, size_t *srcLen,
    BYTE prop, ELzmaFinishMode finishMode, ELzmaStatus *status)
{
    CLzma2Dec *p = LzmaDec_Create();
    if (p == NULL)
        return SZ_ERROR_MEM;
    SRes res;
  size_t outSize = *destLen, inSize = *srcLen;
  *destLen = *srcLen = 0;
  *status = LZMA_STATUS_NOT_SPECIFIED;
  RINOK(Lzma2Dec_AllocateProbs(p, prop));
  p->dic = dest;
  p->dicBufSize = outSize;
  Lzma2Dec_Init(p);
  *srcLen = inSize;
  res = Lzma2Dec_DecodeToDic(p, outSize, src, srcLen, finishMode, status);
  *destLen = p->dicPos;
  if (res == SZ_OK && *status == LZMA_STATUS_NEEDS_MORE_INPUT)
    res = SZ_ERROR_INPUT_EOF;
  return res;
}
