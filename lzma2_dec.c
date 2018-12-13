/* lzma2_dec.c -- LZMA2 Decoder
Based upon LzmaDec.c 2018-02-28 : Igor Pavlov : Public domain
Modified for FL2 by Conor McCarthy */

#include <stdlib.h>
#include "lzma2_dec.h"
#include "fl2_internal.h"
#include "platform.h"

#include <string.h>
#include <stdlib.h>

#ifdef HAVE_SMALL
#  define LZMA_SIZE_OPT
#endif

#define kNumTopBits 24
#define kTopValue ((U32)1 << kNumTopBits)

#define kNumBitModelTotalBits 11
#define kBitModelTotal (1 << kNumBitModelTotalBits)
#define kNumMoveBits 5

#define RC_INIT_SIZE 5

#define NORMALIZE if (range < kTopValue) { range <<= 8; code = (code << 8) | (*buf++); }

#define IF_BIT_0(p) ttt = *(p); NORMALIZE; bound = (range >> kNumBitModelTotalBits) * ttt; if (code < bound)
#define UPDATE_0(p) range = bound; *(p) = (Probability)(ttt + ((kBitModelTotal - ttt) >> kNumMoveBits));
#define UPDATE_1(p) range -= bound; code -= bound; *(p) = (Probability)(ttt - (ttt >> kNumMoveBits));
#define GET_BIT2(p, i, A0, A1) IF_BIT_0(p) \
  { UPDATE_0(p); i = (i + i); A0; } else \
  { UPDATE_1(p); i = (i + i) + 1; A1; }

#if defined __x86_64__s || defined _M_X64

#define USE_CMOV

#define PREP_BIT(p) ttt = *(p); NORMALIZE; bound = (range >> kNumBitModelTotalBits) * ttt
#define UPDATE_PREP_0 U32 r0 = bound; unsigned p0 = (ttt + ((kBitModelTotal - ttt) >> kNumMoveBits))
#define UPDATE_PREP_1 U32 r1 = range - bound; unsigned p1 = (ttt - (ttt >> kNumMoveBits))
#define UPDATE_COND(p) range=(code < bound) ? r0 : r1; *p = (Probability)((code < bound) ? p0 : p1)
#define UPDATE_CODE code = code - ((code < bound) ? 0 : bound)

#define TREE_GET_BIT(probs, i) { Probability *pp = (probs)+(i); PREP_BIT(pp); \
  UPDATE_PREP_0; unsigned i0 = (i + i); \
  UPDATE_PREP_1; unsigned i1 = (i + i) + 1; \
  UPDATE_COND(pp); \
  i = (code < bound) ? i0 : i1; \
  UPDATE_CODE; \
}

#define REV_BIT_VAR(probs, i, m) { Probability *pp = (probs)+(i); PREP_BIT(pp); \
  UPDATE_PREP_0; U32 i0 = i + m; U32 m2 = m + m; \
  UPDATE_PREP_1; U32 i1 = i + m2; \
  UPDATE_COND(pp); \
  i = (code < bound) ? i0 : i1; \
  m = m2; \
  UPDATE_CODE; \
}
#define REV_BIT_CONST(probs, i, m) { Probability *pp = (probs)+(i); PREP_BIT(pp); \
  UPDATE_PREP_0; \
  UPDATE_PREP_1; \
  UPDATE_COND(pp); \
  i += m + (code < bound ? 0 : m); \
  UPDATE_CODE; \
}
#define REV_BIT_LAST(probs, i, m) { Probability *pp = (probs)+(i); PREP_BIT(pp); \
  UPDATE_PREP_0; \
  UPDATE_PREP_1; \
  UPDATE_COND(pp); \
  i -= code < bound ? m : 0; \
  UPDATE_CODE; \
}

#define MATCHED_LITER_DEC \
  matchByte += matchByte; \
  bit = offs; \
  offs &= matchByte; \
  probLit = prob + (offs + bit + symbol); \
  PREP_BIT(probLit); \
  { UPDATE_PREP_0; unsigned i0 = (symbol + symbol); \
  UPDATE_PREP_1; unsigned i1 = (symbol + symbol) + 1; \
  UPDATE_COND(probLit); \
  symbol = (code < bound) ? i0 : i1; \
  offs = (code < bound) ? offs ^ bit : offs; \
  UPDATE_CODE; }

#else
#define TREE_GET_BIT(probs, i) { GET_BIT2(probs + i, i, ;, ;); }

#define REV_BIT(p, i, A0, A1) IF_BIT_0(p + i) \
  { UPDATE_0(p + i); A0; } else \
  { UPDATE_1(p + i); A1; }
#define REV_BIT_VAR(  p, i, m) REV_BIT(p, i, i += m; m += m, m += m; i += m; )
#define REV_BIT_CONST(p, i, m) REV_BIT(p, i, i += m;       , i += m * 2; )
#define REV_BIT_LAST( p, i, m) REV_BIT(p, i, i -= m        , ; )

#define MATCHED_LITER_DEC \
  matchByte += matchByte; \
  bit = offs; \
  offs &= matchByte; \
  probLit = prob + (offs + bit + symbol); \
  GET_BIT2(probLit, symbol, offs ^= bit; , ;)

#endif

#define TREE_DECODE(probs, limit, i) \
  { i = 1; do { TREE_GET_BIT(probs, i); } while (i < limit); i -= limit; }

#ifdef LZMA_SIZE_OPT
#define TREE_6_DECODE(probs, i) TREE_DECODE(probs, (1 << 6), i)
#else
#define TREE_6_DECODE(probs, i) \
  { i = 1; \
  TREE_GET_BIT(probs, i); \
  TREE_GET_BIT(probs, i); \
  TREE_GET_BIT(probs, i); \
  TREE_GET_BIT(probs, i); \
  TREE_GET_BIT(probs, i); \
  TREE_GET_BIT(probs, i); \
  i -= 0x40; }
#endif

#define NORMAL_LITER_DEC TREE_GET_BIT(prob, symbol)

#define NORMALIZE_CHECK if (range < kTopValue) { return 0; }

#define IF_BIT_0_CHECK(p) ttt = *(p); NORMALIZE_CHECK; bound = (range >> kNumBitModelTotalBits) * ttt; if (code < bound)
#define UPDATE_0_CHECK range = bound;
#define UPDATE_1_CHECK range -= bound; code -= bound;
#define GET_BIT2_CHECK(p, i, A0, A1) IF_BIT_0_CHECK(p) \
  { UPDATE_0_CHECK; i = (i + i); A0; } else \
  { UPDATE_1_CHECK; i = (i + i) + 1; A1; }
#define GET_BIT_CHECK(p, i) GET_BIT2_CHECK(p, i, ; , ;)
#define TREE_DECODE_CHECK(probs, limit, i) \
  { i = 1; do { GET_BIT_CHECK(probs + i, i) } while (i < limit); i -= limit; }

#define REV_BIT_CHECK(p, i, m) IF_BIT_0_CHECK(p + i) \
  { UPDATE_0_CHECK; i += m; m += m; } else \
  { UPDATE_1_CHECK; m += m; i += m; }

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

#define LZMA2_IS_UNCOMPRESSED_STATE(control) ((control & LZMA2_CONTROL_LZMA) == 0)

#define LZMA2_GET_LZMA_MODE(control) ((control >> 5) & 3)
#define LZMA2_IS_THERE_PROP(mode) ((mode) >= 2)

#ifdef SHOW_DEBUG_INFO
#define PRF(x) x
#else
#define PRF(x)
#endif

typedef enum
{
    LZMA2_STATE_CONTROL,
    LZMA2_STATE_DATA,
    LZMA2_STATE_DATA_CONT,
    LZMA2_STATE_FINISHED,
    LZMA2_STATE_ERROR
} ELzma2State;

#define LZMA_DIC_MIN (1 << 12)

static BYTE LZMA_tryDummy(const LZMA2_DCtx *const p)
{
    const Probability *probs = GET_PROBS;
	unsigned state = p->state;
	U32 range = p->range;
	U32 code = p->code;

    const Probability *prob;
    U32 bound;
    unsigned ttt;
    unsigned posState = CALC_POS_STATE(p->processedPos, (1 << p->prop.pb) - 1);

    prob = probs + IsMatch + COMBINED_PS_STATE;
    IF_BIT_0_CHECK(prob)
    {
        UPDATE_0_CHECK

            prob = probs + Literal;
        if (p->checkDicSize != 0 || p->processedPos != 0)
            prob += ((U32)LZMA_LIT_SIZE *
            ((((p->processedPos) & ((1 << (p->prop.lp)) - 1)) << p->prop.lc) +
                (p->dic[(p->dicPos == 0 ? p->dicBufSize : p->dicPos) - 1] >> (8 - p->prop.lc))));

        if (state < kNumLitStates)
        {
            unsigned symbol = 1;
            do { GET_BIT_CHECK(prob + symbol, symbol) } while (symbol < 0x100);
        }
        else
        {
            unsigned matchByte = p->dic[p->dicPos - p->reps[0] +
                (p->dicPos < p->reps[0] ? p->dicBufSize : 0)];
            unsigned offs = 0x100;
            unsigned symbol = 1;
            do
            {
                unsigned bit;
                const Probability *probLit;
                matchByte += matchByte;
                bit = offs;
                offs &= matchByte;
                probLit = prob + (offs + bit + symbol);
                GET_BIT2_CHECK(probLit, symbol, offs ^= bit; , ; )
            } while (symbol < 0x100);
        }
    }
    else
    {
        unsigned len;
        UPDATE_1_CHECK;

        prob = probs + IsRep + state;
        IF_BIT_0_CHECK(prob)
        {
            UPDATE_0_CHECK;
            state = 0;
            prob = probs + LenCoder;
        }
        else
        {
            UPDATE_1_CHECK;
            prob = probs + IsRepG0 + state;
            IF_BIT_0_CHECK(prob)
            {
                UPDATE_0_CHECK;
                prob = probs + IsRep0Long + COMBINED_PS_STATE;
                IF_BIT_0_CHECK(prob)
                {
                    UPDATE_0_CHECK;
                    NORMALIZE_CHECK;
                    return 1;
                }
                else
                {
                    UPDATE_1_CHECK;
                }
            }
            else
            {
                UPDATE_1_CHECK;
                prob = probs + IsRepG1 + state;
                IF_BIT_0_CHECK(prob)
                {
                    UPDATE_0_CHECK;
                }
                else
                {
                    UPDATE_1_CHECK;
                    prob = probs + IsRepG2 + state;
                    IF_BIT_0_CHECK(prob)
                    {
                        UPDATE_0_CHECK;
                    }
                    else
                    {
                        UPDATE_1_CHECK;
                    }
                }
            }
            state = kNumStates;
            prob = probs + RepLenCoder;
        }
        {
            unsigned limit, offset;
            const Probability *probLen = prob + LenChoice;
            IF_BIT_0_CHECK(probLen)
            {
                UPDATE_0_CHECK;
                probLen = prob + LenLow + GET_LEN_STATE;
                offset = 0;
                limit = 1 << kLenNumLowBits;
            }
            else
            {
                UPDATE_1_CHECK;
                probLen = prob + LenChoice2;
                IF_BIT_0_CHECK(probLen)
                {
                    UPDATE_0_CHECK;
                    probLen = prob + LenLow + GET_LEN_STATE + (1 << kLenNumLowBits);
                    offset = kLenNumLowSymbols;
                    limit = 1 << kLenNumLowBits;
                }
                else
                {
                  UPDATE_1_CHECK;
                  probLen = prob + LenHigh;
                  offset = kLenNumLowSymbols * 2;
                  limit = 1 << kLenNumHighBits;
                }
            }
            TREE_DECODE_CHECK(probLen, limit, len);
            len += offset;
        }

        if (state < 4)
        {
            unsigned posSlot;
            prob = probs + PosSlot +
                ((len < kNumLenToPosStates - 1 ? len : kNumLenToPosStates - 1) <<
                    kNumPosSlotBits);
            TREE_DECODE_CHECK(prob, 1 << kNumPosSlotBits, posSlot);
            if (posSlot >= kStartPosModelIndex)
            {
                unsigned numDirectBits = ((posSlot >> 1) - 1);

                if (posSlot < kEndPosModelIndex)
                {
                    prob = probs + SpecPos + ((2 | (posSlot & 1)) << numDirectBits);
                }
                else
                {
                    numDirectBits -= kNumAlignBits;
                    do
                    {
                        NORMALIZE_CHECK;
                        range >>= 1;
                        code -= range & (((code - range) >> 31) - 1);
                        /* if (code >= range) code -= range; */
                    } while (--numDirectBits != 0);
                    prob = probs + Align;
                    numDirectBits = kNumAlignBits;
                }
                {
                    unsigned i = 1;
                    unsigned m = 1;
                    do
                    {
                        REV_BIT_CHECK(prob, i, m);
                    } while (--numDirectBits != 0);
                }
            }
        }
    }
    NORMALIZE_CHECK;
    return 1;
}

/* First LZMA-symbol is always decoded.
And it decodes new LZMA-symbols while (buf < bufLimit), but "buf" is without last normalization
Out:
  Result:
    0 - OK
    1 - Error
*/

#ifdef LZMA2_DEC_OPT

int LZMA_decodeReal_3(LZMA2_DCtx *p, size_t limit, const BYTE *bufLimit);

#else

static int LZMA_decodeReal_3(LZMA2_DCtx *p, size_t limit, const BYTE *bufLimit)
{
    Probability *const probs = GET_PROBS;

    unsigned state = p->state;
    U32 rep0 = p->reps[0], rep1 = p->reps[1], rep2 = p->reps[2], rep3 = p->reps[3];
    unsigned const pbMask = ((unsigned)1 << (p->prop.pb)) - 1;
    unsigned const lc = p->prop.lc;
    unsigned const lpMask = ((unsigned)0x100 << p->prop.lp) - ((unsigned)0x100 >> lc);

    BYTE *const dic = p->dic;
    size_t const dicBufSize = p->dicBufSize;
    size_t dicPos = p->dicPos;

    U32 processedPos = p->processedPos;
    U32 const checkDicSize = p->checkDicSize;
    unsigned len = 0;

    const BYTE *buf = p->buf;
    U32 range = p->range;
    U32 code = p->code;

    do
    {
        Probability *prob;
        U32 bound;
        unsigned ttt;
        unsigned posState = CALC_POS_STATE(processedPos, pbMask);

        prob = probs + IsMatch + COMBINED_PS_STATE;
        IF_BIT_0(prob)
        {
            unsigned symbol;
            UPDATE_0(prob);
            prob = probs + Literal;
            if (processedPos != 0 || checkDicSize != 0)
                prob += (U32)3 * ((((processedPos << 8) + dic[(dicPos == 0 ? dicBufSize : dicPos) - 1]) & lpMask) << lc);
            processedPos++;

            if (state < kNumLitStates)
            {
                state -= (state < 4) ? state : 3;
                symbol = 1;
#ifdef LZMA_SIZE_OPT
                do { NORMAL_LITER_DEC } while (symbol < 0x100);
#else
                NORMAL_LITER_DEC
                    NORMAL_LITER_DEC
                    NORMAL_LITER_DEC
                    NORMAL_LITER_DEC
                    NORMAL_LITER_DEC
                    NORMAL_LITER_DEC
                    NORMAL_LITER_DEC
                    NORMAL_LITER_DEC
#endif
            }
            else
            {
                unsigned matchByte = dic[dicPos - rep0 + (dicPos < rep0 ? dicBufSize : 0)];
                unsigned offs = 0x100;
                state -= (state < 10) ? 3 : 6;
                symbol = 1;
#ifdef LZMA_SIZE_OPT
                do
                {
                    unsigned bit;
                    Probability *probLit;
                    MATCHED_LITER_DEC
                } while (symbol < 0x100);
#else
                {
                    unsigned bit;
                    Probability *probLit;
                    MATCHED_LITER_DEC
                    MATCHED_LITER_DEC
                    MATCHED_LITER_DEC
                    MATCHED_LITER_DEC
                    MATCHED_LITER_DEC
                    MATCHED_LITER_DEC
                    MATCHED_LITER_DEC
                    MATCHED_LITER_DEC
                }
#endif
            }

            dic[dicPos++] = (BYTE)symbol;
            continue;
        }

        UPDATE_1(prob);
        prob = probs + IsRep + state;
        IF_BIT_0(prob)
        {
            UPDATE_0(prob);
            state += kNumStates;
            prob = probs + LenCoder;
        }
        else
        {
            UPDATE_1(prob);
            /*
            // that case was checked before with kBadRepCode
            if (checkDicSize == 0 && processedPos == 0)
            return 1;
            */
            prob = probs + IsRepG0 + state;
            IF_BIT_0(prob)
            {
                UPDATE_0(prob);
                prob = probs + IsRep0Long + COMBINED_PS_STATE;
                IF_BIT_0(prob)
                {
                      UPDATE_0(prob);
                      dic[dicPos] = dic[dicPos - rep0 + (dicPos < rep0 ? dicBufSize : 0)];
                      dicPos++;
                      processedPos++;
                      state = state < kNumLitStates ? 9 : 11;
                      continue;
                }
                UPDATE_1(prob);
            }
            else
            {
                U32 distance;
                UPDATE_1(prob);
                prob = probs + IsRepG1 + state;
                IF_BIT_0(prob)
                {
                    UPDATE_0(prob);
                    distance = rep1;
                }
                else
                {
                    UPDATE_1(prob);
                    prob = probs + IsRepG2 + state;
#ifdef USE_CMOV
                    PREP_BIT(prob);
                    UPDATE_PREP_0;
                    UPDATE_PREP_1;
                    UPDATE_COND(prob);
                    distance = code < bound ? rep2 : rep3;
                    rep3 = code < bound ? rep3 : rep2;
                    UPDATE_CODE;
#else
                    IF_BIT_0(prob)
                    {
                        UPDATE_0(prob);
                        distance = rep2;
                    }
                    else
                    {
                        UPDATE_1(prob);
                        distance = rep3;
                        rep3 = rep2;
                    }
#endif
                    rep2 = rep1;
                }
                rep1 = rep0;
                rep0 = distance;
            }
            state = state < kNumLitStates ? 8 : 11;
            prob = probs + RepLenCoder;
        }

#ifdef LZMA_SIZE_OPT
        unsigned lim, offset;
        Probability *probLen = prob + LenChoice;
        IF_BIT_0(probLen)
        {
              UPDATE_0(probLen);
              probLen = prob + LenLow + GET_LEN_STATE;
              offset = 0;
              lim = (1 << kLenNumLowBits);
        }
        else
        {
            UPDATE_1(probLen);
            probLen = prob + LenChoice2;
            IF_BIT_0(probLen)
            {
                UPDATE_0(probLen);
                probLen = prob + LenLow + GET_LEN_STATE + (1 << kLenNumLowBits);
                offset = kLenNumLowSymbols;
                lim = (1 << kLenNumLowBits);
            }
            else
            {
                UPDATE_1(probLen);
                probLen = prob + LenHigh;
                offset = kLenNumLowSymbols * 2;
                lim = (1 << kLenNumHighBits);
            }
        }
        TREE_DECODE(probLen, lim, len);
        len += offset;
#else
        Probability *probLen = prob + LenChoice;
        IF_BIT_0(probLen)
        {
              UPDATE_0(probLen);
              probLen = prob + LenLow + GET_LEN_STATE;
              len = 1;
              TREE_GET_BIT(probLen, len);
              TREE_GET_BIT(probLen, len);
              TREE_GET_BIT(probLen, len);
              len -= 8;
        }
        else
        {
            UPDATE_1(probLen);
            probLen = prob + LenChoice2;
            IF_BIT_0(probLen)
            {
                UPDATE_0(probLen);
                probLen = prob + LenLow + GET_LEN_STATE + (1 << kLenNumLowBits);
                len = 1;
                TREE_GET_BIT(probLen, len);
                TREE_GET_BIT(probLen, len);
                TREE_GET_BIT(probLen, len);
            }
            else
            {
                UPDATE_1(probLen);
                probLen = prob + LenHigh;
                TREE_DECODE(probLen, (1 << kLenNumHighBits), len);
                len += kLenNumLowSymbols * 2;
            }
        }
#endif

        if (state >= kNumStates)
        {
            U32 distance;
            prob = probs + PosSlot +
                ((len < kNumLenToPosStates ? len : kNumLenToPosStates - 1) << kNumPosSlotBits);
            TREE_6_DECODE(prob, distance);
            if (distance >= kStartPosModelIndex)
            {
                unsigned posSlot = (unsigned)distance;
                unsigned numDirectBits = (unsigned)(((distance >> 1) - 1));
                distance = (2 | (distance & 1));
                if (posSlot < kEndPosModelIndex)
                {
                    distance <<= numDirectBits;
                    prob = probs + SpecPos;
                    {
                        U32 m = 1;
                        distance++;
                        do
                        {
                            REV_BIT_VAR(prob, distance, m);
                        } while (--numDirectBits);
                        distance -= m;
                    }
                }
                else
                {
                    numDirectBits -= kNumAlignBits;
                    do
                    {
                        NORMALIZE
                        range >>= 1;

                        U32 t;
                        code -= range;
                        t = (0 - ((U32)code >> 31));
                        distance = (distance << 1) + (t + 1);
                        code += range & t;
                    } while (--numDirectBits != 0);
                    prob = probs + Align;
                    distance <<= kNumAlignBits;
                    {
                        U32 i = 1;
                        REV_BIT_CONST(prob, i, 1);
                        REV_BIT_CONST(prob, i, 2);
                        REV_BIT_CONST(prob, i, 4);
                        REV_BIT_LAST(prob, i, 8);
                        distance |= i;
                    }
                }
            }

            rep3 = rep2;
            rep2 = rep1;
            rep1 = rep0;
            rep0 = distance + 1;
            if (distance >= (checkDicSize == 0 ? processedPos : checkDicSize))
            {
                p->dicPos = dicPos;
                return 1;
            }
            state = (state < kNumStates + kNumLitStates) ? kNumLitStates : kNumLitStates + 3;
        }

        len += kMatchMinLen;

        size_t rem = limit - dicPos;
        if (rem == 0)
        {
            p->dicPos = dicPos;
            return 1;
        }

        unsigned curLen = ((rem < len) ? (unsigned)rem : len);
        size_t pos = dicPos - rep0 + (dicPos < rep0 ? dicBufSize : 0);

        processedPos += curLen;

        len -= curLen;
        if (curLen <= dicBufSize - pos)
        {
            BYTE *dest = dic + dicPos;
            ptrdiff_t src = (ptrdiff_t)pos - (ptrdiff_t)dicPos;
            const BYTE *end = dest + curLen;
            dicPos += curLen;
            do
                *(dest) = (BYTE)*(dest + src);
            while (++dest != end);
        }
        else
        {
            do
            {
                dic[dicPos++] = dic[pos];
                if (++pos == dicBufSize)
                    pos = 0;
            } while (--curLen != 0);
        }
    } while (dicPos < limit && buf < bufLimit);

    NORMALIZE;

    p->buf = buf;
    p->range = range;
    p->code = code;
    p->remainLen = len;
    p->dicPos = dicPos;
    p->processedPos = processedPos;
    p->reps[0] = rep0;
    p->reps[1] = rep1;
    p->reps[2] = rep2;
    p->reps[3] = rep3;
    p->state = state;

    return 0;
}

#endif

static void LZMA_writeRem(LZMA2_DCtx *const p, size_t const limit)
{
    if (p->remainLen != 0 && p->remainLen < kMatchSpecLenStart)
    {
        BYTE *const dic = p->dic;
        size_t dicPos = p->dicPos;
        size_t const dicBufSize = p->dicBufSize;
        unsigned len = p->remainLen;
        size_t const rep0 = p->reps[0];
        size_t const rem = limit - dicPos;
        if (rem < len)
            len = (unsigned)(rem);

        if (p->checkDicSize == 0 && p->prop.dicSize - p->processedPos <= len)
            p->checkDicSize = p->prop.dicSize;

        p->processedPos += len;
        p->remainLen -= len;
        while (len != 0)
        {
            len--;
            dic[dicPos] = dic[dicPos - rep0 + (dicPos < rep0 ? dicBufSize : 0)];
            dicPos++;
        }
        p->dicPos = dicPos;
    }
}

#define kRange0 0xFFFFFFFF
#define kBound0 ((kRange0 >> kNumBitModelTotalBits) << (kNumBitModelTotalBits - 1))
#define kBadRepCode (kBound0 + (((kRange0 - kBound0) >> kNumBitModelTotalBits) << (kNumBitModelTotalBits - 1)))
#if kBadRepCode != (0xC0000000 - 0x400)
#error Stop_Compiling_Bad_LZMA_Check
#endif

static size_t LZMA_decodeReal2(LZMA2_DCtx *const p, size_t const limit, const BYTE *const bufLimit)
{
    if (p->buf == bufLimit && !LZMA_tryDummy(p))
        return FL2_ERROR(corruption_detected);
    do
    {
        size_t limit2 = limit;
        if (p->checkDicSize == 0)
        {
            U32 const rem = p->prop.dicSize - p->processedPos;
            if (limit - p->dicPos > rem)
                limit2 = p->dicPos + rem;
            if (p->processedPos == 0)
                if (p->code >= kBadRepCode)
                    return FL2_ERROR(corruption_detected);
        }

        do {
            if (LZMA_decodeReal_3(p, limit2, bufLimit) != 0)
                return FL2_ERROR(corruption_detected);
        } while (p->dicPos < limit2 && p->buf == bufLimit && LZMA_tryDummy(p));

        if (p->checkDicSize == 0 && p->processedPos >= p->prop.dicSize)
            p->checkDicSize = p->prop.dicSize;

        LZMA_writeRem(p, limit);
    } while (p->dicPos < limit && p->buf < bufLimit && p->remainLen < kMatchSpecLenStart);

    if (p->remainLen > kMatchSpecLenStart)
        p->remainLen = kMatchSpecLenStart;

    return FL2_error_no_error;
}


static void LZMA_initDicAndState(LZMA2_DCtx *const p, BYTE const initDic, BYTE const initState)
{
    p->needFlush = 1;
    p->remainLen = 0;

    if (initDic)
    {
        p->processedPos = 0;
        p->checkDicSize = 0;
        p->needInitState = 1;
    }
    if (initState)
        p->needInitState = 1;
}

static void LZMA_init(LZMA2_DCtx *const p)
{
    p->dicPos = 0;
    LZMA_initDicAndState(p, 1, 1);
}

static void LZMA_initStateReal(LZMA2_DCtx *const p)
{
    size_t const numProbs = LzmaProps_GetNumProbs(&p->prop);
    Probability *probs = p->probs;
    for (size_t i = 0; i < numProbs; i++)
        probs[i] = kBitModelTotal >> 1;
    p->reps[0] = p->reps[1] = p->reps[2] = p->reps[3] = 1;
    p->state = 0;
    p->needInitState = 0;
}

static size_t LZMA_decodeToDic(LZMA2_DCtx *const p, size_t const dicLimit, const BYTE *src, size_t *const srcLen,
    ELzmaFinishMode finishMode)
{
    size_t inSize = *srcLen;
    (*srcLen) = 0;
    LZMA_writeRem(p, dicLimit);

    if (p->needFlush)
    {
        if (inSize < RC_INIT_SIZE)
        {
            return LZMA_STATUS_NEEDS_MORE_INPUT;
        }
        if (src[0] != 0)
            return FL2_ERROR(corruption_detected);
        p->code =
            ((U32)src[1] << 24)
            | ((U32)src[2] << 16)
            | ((U32)src[3] << 8)
            | ((U32)src[4]);
        src += RC_INIT_SIZE;
        (*srcLen) += RC_INIT_SIZE;
        inSize -= RC_INIT_SIZE;
        p->range = 0xFFFFFFFF;
        p->needFlush = 0;
    }

    while (1) {
        if (p->dicPos >= dicLimit)
        {
            if (p->remainLen == 0 && p->code == 0) {
                return LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK;
            }
                return LZMA_STATUS_NOT_FINISHED;
        }

        if (p->needInitState)
            LZMA_initStateReal(p);

        const BYTE *bufLimit;
        if (finishMode == LZMA_FINISH_END) {
            bufLimit = src + inSize;
        }
        else {
            if (inSize <= LZMA_REQUIRED_INPUT_MAX) {
                return LZMA_STATUS_NEEDS_MORE_INPUT;
            }
            bufLimit = src + inSize - LZMA_REQUIRED_INPUT_MAX;
        }
        p->buf = src;

        CHECK_F(LZMA_decodeReal2(p, dicLimit, bufLimit));

        size_t const processed = (size_t)(p->buf - src);
        (*srcLen) += processed;
        src += processed;
        inSize -= processed;
    }
}

void LZMA_constructDCtx(LZMA2_DCtx *p)
{
    p->dic = NULL;
    p->extDic = 1;
    p->state2 = LZMA2_STATE_FINISHED;
	p->probs_1664 = p->probs + 1664;
}

static void LZMA_freeDict(LZMA2_DCtx *const p)
{
    if (!p->extDic) {
        free(p->dic);
    }
    p->dic = NULL;
}

void LZMA_destructDCtx(LZMA2_DCtx *const p)
{
    LZMA_freeDict(p);
}

size_t LZMA2_getDictSizeFromProp(BYTE const dictProp)
{
    if (dictProp > 40)
        return FL2_ERROR(corruption_detected);

    size_t const dictSize = (dictProp == 40)
        ? (size_t)-1
        : (((size_t)2 | (dictProp & 1)) << (dictProp / 2 + 11));
    return dictSize;
}

static size_t LZMA2_dictBufSize(size_t const dictSize)
{
    size_t mask = ((size_t)1 << 12) - 1;
    if (dictSize >= ((size_t)1 << 30)) mask = ((size_t)1 << 22) - 1;
    else if (dictSize >= ((size_t)1 << 22)) mask = ((size_t)1 << 20) - 1;

    size_t dicBufSize = ((size_t)dictSize + mask) & ~mask;
    if (dicBufSize < dictSize)
        dicBufSize = dictSize;

    return dicBufSize;
}

size_t LZMA2_decMemoryUsage(size_t const dictSize)
{
    return sizeof(LZMA2_DCtx) + LZMA2_dictBufSize(dictSize);
}

size_t LZMA2_initDecoder(LZMA2_DCtx *const p, BYTE const dictProp, BYTE *const dic, size_t dicBufSize)
{
    size_t const dictSize = LZMA2_getDictSizeFromProp(dictProp);
    if (FL2_isError(dictSize))
        return dictSize;

    if (dic == NULL) {
        dicBufSize = LZMA2_dictBufSize(dictSize);

        if (!p->dic || dicBufSize != p->dicBufSize) {
            LZMA_freeDict(p);
            p->dic = (BYTE *)malloc(dicBufSize);
            if (!p->dic)
                return FL2_ERROR(memory_allocation);
            p->extDic = 0;
        }
    }
    else {
        LZMA_freeDict(p);
        p->dic = dic;
        p->extDic = 1;
    }
    p->dicBufSize = dicBufSize;
    p->prop.lc = 3;
    p->prop.lp = 0;
    p->prop.lc = 2;
    p->prop.dicSize = (U32)dictSize;

    p->state2 = LZMA2_STATE_CONTROL;
    p->needInitDic = 1;
    p->needInitState2 = 1;
    p->needInitProp = 1;
    LZMA_init(p);
    return FL2_error_no_error;
}

static void LZMA_updateWithUncompressed(LZMA2_DCtx *const p, const BYTE *const src, size_t const size)
{
    memcpy(p->dic + p->dicPos, src, size);
    p->dicPos += size;
    if (p->checkDicSize == 0 && p->prop.dicSize - p->processedPos <= size)
        p->checkDicSize = p->prop.dicSize;
    p->processedPos += (U32)size;
}

static unsigned LZMA2_nextChunkInfo(BYTE *const control,
    U32 *const unpackSize, U32 *const packSize,
    LZMA2_props *const prop,
    const BYTE *const src, ptrdiff_t *const srcLen)
{
    ptrdiff_t const len = *srcLen;
    *srcLen = 0;
    if (len <= 0)
        return LZMA2_STATE_CONTROL;
    *control = *src;
    if (*control == 0) {
        *srcLen = 1;
        return LZMA2_STATE_FINISHED;
    }
    if (len < 3)
        return LZMA2_STATE_CONTROL;
    if (LZMA2_IS_UNCOMPRESSED_STATE(*control)) {
        if (*control > 2)
            return LZMA2_STATE_ERROR;
        *srcLen = 3;
        *unpackSize = (((U32)src[1] << 8) | src[2]) + 1;
    }
    else {
        S32 const hasProp = LZMA2_IS_THERE_PROP(LZMA2_GET_LZMA_MODE(*control));
        if (len < 5 + hasProp)
            return LZMA2_STATE_CONTROL;
        *srcLen = 5 + hasProp;
        *unpackSize = ((U32)(*control & 0x1F) << 16) + ((U32)src[1] << 8) + src[2] + 1;
        *packSize = ((U32)src[3] << 8) + src[4] + 1;
        if (hasProp) {
            BYTE b = src[5];
            if (b >= (9 * 5 * 5))
                return LZMA2_STATE_ERROR;
            BYTE const lc = b % 9;
            b /= 9;
            prop->pb = b / 5;
            BYTE const lp = b % 5;
            if (lc + lp > LZMA2_LCLP_MAX)
                return LZMA2_STATE_ERROR;
            prop->lc = (BYTE)lc;
            prop->lp = (BYTE)lp;
        }
    }
    return LZMA2_STATE_DATA;
}

size_t LZMA2_decodeToDic(LZMA2_DCtx *const p, size_t const dicLimit,
    const BYTE *src, size_t *const srcLen, ELzmaFinishMode const finishMode)
{
    size_t const inSize = *srcLen;
    size_t res = FL2_error_no_error;
    *srcLen = 0;

    while (p->state2 != LZMA2_STATE_ERROR)
    {
        if(p->state2 == LZMA2_STATE_CONTROL) {
            ptrdiff_t len = inSize - *srcLen;
            p->state2 = LZMA2_nextChunkInfo(&p->control, &p->unpackSize, &p->packSize, &p->prop, src, &len);
            *srcLen += len;
            src += len;
        }

        if (p->state2 == LZMA2_STATE_FINISHED)
            return LZMA_STATUS_FINISHED_WITH_MARK;

        size_t const dicPos = p->dicPos;

        if (dicPos == dicLimit && finishMode == LZMA_FINISH_ANY)
            return LZMA_STATUS_NOT_FINISHED;

        if (p->state2 != LZMA2_STATE_DATA && p->state2 != LZMA2_STATE_DATA_CONT)
        {
            if (p->state2 == LZMA2_STATE_CONTROL)
            {
                return LZMA_STATUS_NEEDS_MORE_INPUT;
            }
            break;
        }

        size_t inCur = inSize - *srcLen;
        size_t outCur = dicLimit - dicPos;
        ELzmaFinishMode curFinishMode = LZMA_FINISH_ANY;

        if (outCur >= p->unpackSize)
        {
            outCur = (size_t)p->unpackSize;
        }
        if (inCur >= p->packSize)
            curFinishMode = LZMA_FINISH_END;

        if (LZMA2_IS_UNCOMPRESSED_STATE(p->control))
        {
            if (inCur == 0)
            {
                return LZMA_STATUS_NEEDS_MORE_INPUT;
            }

            if (p->state2 == LZMA2_STATE_DATA)
            {
                BYTE const initDic = (p->control == LZMA2_CONTROL_COPY_RESET_DIC);
                if (initDic)
                    p->needInitProp = p->needInitState2 = 1;
                else if (p->needInitDic)
                    break;
                p->needInitDic = 0;
                LZMA_initDicAndState(p, initDic, 0);
            }

            if (inCur > outCur)
                inCur = outCur;
            if (inCur == 0)
                break;

            LZMA_updateWithUncompressed(p, src, inCur);

            src += inCur;
            *srcLen += inCur;
            p->unpackSize -= (U32)inCur;
            p->state2 = (p->unpackSize == 0) ? LZMA2_STATE_CONTROL : LZMA2_STATE_DATA_CONT;
        }
        else
        {
            if (p->state2 == LZMA2_STATE_DATA)
            {
                unsigned const mode = LZMA2_GET_LZMA_MODE(p->control);
                BYTE const initDic = (mode == 3);
                BYTE const initState = (mode != 0);
                if ((!initDic && p->needInitDic) || (!initState && p->needInitState2))
                    break;

                LZMA_initDicAndState(p, initDic, initState);
                p->needInitDic = 0;
                p->needInitState2 = 0;
                p->state2 = LZMA2_STATE_DATA_CONT;
            }

            if (inCur > p->packSize)
                inCur = (size_t)p->packSize;

            res = LZMA_decodeToDic(p, dicPos + outCur, src, &inCur, curFinishMode);

            src += inCur;
            *srcLen += inCur;
            p->packSize -= (U32)inCur;
            outCur = p->dicPos - dicPos;
            p->unpackSize -= (U32)outCur;

            if (ERR_isError(res))
                break;

            if (res == LZMA_STATUS_NEEDS_MORE_INPUT)
            {
                if (p->packSize == 0)
                    break;
                return res;
            }

            if (p->packSize == 0 && p->unpackSize == 0)
            {
                if (res != LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK)
                    break;
                p->state2 = LZMA2_STATE_CONTROL;
            }
            else if (inCur == 0 && outCur == 0)
            {
                break;
            }

        }
    }

    p->state2 = LZMA2_STATE_ERROR;
    if (ERR_isError(res))
        return res;
    return FL2_ERROR(corruption_detected);
}


size_t LZMA2_decodeToBuf(LZMA2_DCtx *const p, BYTE *dest, size_t *const destLen, const BYTE *src, size_t *const srcLen, ELzmaFinishMode const finishMode)
{
    size_t outSize = *destLen, inSize = *srcLen;
    *srcLen = *destLen = 0;

    for (;;)
    {
        if (p->dicPos == p->dicBufSize)
            p->dicPos = 0;

        size_t const dicPos = p->dicPos;
        ELzmaFinishMode curFinishMode = LZMA_FINISH_ANY;
        size_t outCur = p->dicBufSize - dicPos;

        if (outCur >= outSize)
        {
            outCur = outSize;
            curFinishMode = finishMode;
        }

        size_t inCur = inSize;
        size_t const res = LZMA2_decodeToDic(p, dicPos + outCur, src, &inCur, curFinishMode);

        src += inCur;
        inSize -= inCur;
        *srcLen += inCur;
        outCur = p->dicPos - dicPos;
        memcpy(dest, p->dic + dicPos, outCur);
        dest += outCur;
        outSize -= outCur;
        *destLen += outCur;
        if (ERR_isError(res) || res == LZMA_STATUS_FINISHED_WITH_MARK)
            return res;
        if (outCur == 0 || outSize == 0)
            return FL2_error_no_error;
    }
}

size_t LZMA2_getUnpackSize(const BYTE *const src, size_t const srcLen)
{
    size_t unpackTotal = 0;
    size_t pos = 1;
    while (pos < srcLen) {
        LZMA2_chunk inf;
        int const type = LZMA2_parseInput(src, pos, srcLen - pos, &inf);
        if (type == CHUNK_FINAL)
            return unpackTotal;
        pos += inf.packSize;
        if (type == CHUNK_ERROR || type == CHUNK_MORE_DATA)
            break;
        unpackTotal += inf.unpackSize;
    }
    return LZMA2_CONTENTSIZE_ERROR;
}

LZMA2_mtInbuf * LZMA2_createInbufNode(LZMA2_mtInbuf *const prev)
{
    LZMA2_mtInbuf *const node = malloc(sizeof(LZMA2_mtInbuf) + LZMA2_MT_INPUT_SIZE - 1);
    if(!node)
        return NULL;
    node->next = NULL;
    node->length = 0;
    if (prev) {
        memcpy(node->inBuf, prev->inBuf + prev->length - LZMA_REQUIRED_INPUT_MAX, LZMA_REQUIRED_INPUT_MAX);
        prev->next = node;
        node->length = LZMA_REQUIRED_INPUT_MAX;
    }
    return node;
}

void LZMA2_freeInbufNodeChain(LZMA2_mtInbuf *node, LZMA2_mtInbuf *const keep)
{
    while (node) {
        LZMA2_mtInbuf *const next = node->next;
        if(node != keep)
            free(node);
        else 
            node->next = NULL;
        node = next;
    }
}

LZMA2_parseRes LZMA2_parseInput(const BYTE* const inBuf, size_t const pos, ptrdiff_t const len, LZMA2_chunk *const inf)
{
    inf->packSize = 0;
    inf->unpackSize = 0;

    if (len <= 0)
        return CHUNK_ERROR;

    BYTE const control = inBuf[pos];
    if (control == 0) {
        inf->packSize = 1;
        return CHUNK_FINAL;
    }
    if (len < 3)
        return CHUNK_MORE_DATA;
    if (LZMA2_IS_UNCOMPRESSED_STATE(control)) {
        if (control > 2)
            return CHUNK_ERROR;
        inf->unpackSize = (((U32)inBuf[pos + 1] << 8) | inBuf[pos + 2]) + 1;
        inf->packSize = 3 + inf->unpackSize;
    }
    else {
        S32 const hasProp = LZMA2_IS_THERE_PROP(LZMA2_GET_LZMA_MODE(control));
        if (len < 5 + hasProp)
            return CHUNK_MORE_DATA;
        inf->unpackSize = ((U32)(control & 0x1F) << 16) + ((U32)inBuf[pos + 1] << 8) + inBuf[pos + 2] + 1;
        inf->packSize = 5 + hasProp + ((U32)inBuf[pos + 3] << 8) + inBuf[pos + 4] + 1;
        if (LZMA2_GET_LZMA_MODE(control) == 3)
            return CHUNK_DICT_RESET;
    }
    return CHUNK_CONTINUE;
}
