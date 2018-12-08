#include "dict_buffer.h"
#include "fl2_internal.h"

#define ALIGNMENT_MASK (~(size_t)15)

int DICT_construct(DICT_buffer * const buf, int const async)
{
    buf->data[0] = NULL;
    buf->data[1] = NULL;
    buf->bufSize = 0;

    buf->async = (async != 0);

#ifndef NO_XXHASH
    buf->xxh = NULL;
#endif

    return 0;
}

int DICT_init(DICT_buffer * const buf, size_t const dictSize, int const doHash)
{
    if (buf->data[0] == NULL || dictSize > buf->bufSize) {
        DICT_destruct(buf);
        buf->data[0] = malloc(dictSize);

        buf->data[1] = NULL;
        if (buf->async)
            buf->data[1] = malloc(dictSize);

        if (buf->data[0] == NULL || (buf->async && buf->data[1] == NULL)) {
            DICT_destruct(buf);
            return 1;
        }
    }
    buf->index = 0;
    buf->start = 0;
    buf->end = 0;
    buf->bufSize = dictSize;

#ifndef NO_XXHASH
    if (doHash) {
        if (buf->xxh == NULL) {
            buf->xxh = XXH32_createState();
            if (buf->xxh == NULL) {
                DICT_destruct(buf);
                return 1;
            }
        }
        XXH32_reset(buf->xxh, 0);
    }
    else {
        XXH32_freeState(buf->xxh);
        buf->xxh = NULL;
    }
#endif

    return 0;
}

void DICT_destruct(DICT_buffer * const buf)
{
    free(buf->data[0]);
    free(buf->data[1]);
    buf->data[0] = NULL;
    buf->data[1] = NULL;
    buf->bufSize = 0;
#ifndef NO_XXHASH
    XXH32_freeState(buf->xxh);
    buf->xxh = NULL;
#endif
}

size_t DICT_size(const DICT_buffer * const buf)
{
    return buf->bufSize;
}

size_t DICT_get(DICT_buffer * const buf, size_t const overlap, FL2_outBuffer * const dict)
{
    DICT_shift(buf, overlap);

    dict->dst = buf->data[buf->index] + buf->end;
    dict->pos = 0;
    dict->size = buf->bufSize - buf->end;

    return dict->size - dict->pos;
}

int DICT_update(DICT_buffer * const buf, size_t const addedSize)
{
    buf->end += addedSize;
    assert(buf->end <= buf->bufSize);
    return !DICT_availSpace(buf);
}

void DICT_put(DICT_buffer * const buf, FL2_inBuffer * const input)
{
    size_t const toRead = MIN(buf->bufSize - buf->end, input->size - input->pos);

    DEBUGLOG(5, "CStream : reading %u bytes", (U32)toRead);

    memcpy(buf->data[buf->index] + buf->end, (BYTE*)input->src + input->pos, toRead);

    input->pos += toRead;
    buf->end += toRead;
}

size_t DICT_availSpace(const DICT_buffer * const buf)
{
    return buf->bufSize - buf->end;
}

int DICT_hasUnprocessed(const DICT_buffer * const buf)
{
    return buf->start < buf->end;
}

void DICT_getBlock(DICT_buffer * const buf, FL2_dataBlock * const block)
{
    block->data = buf->data[buf->index];
    block->start = buf->start;
    block->end = buf->end;

#ifndef NO_XXHASH
    if (buf->xxh != NULL)
        XXH32_update(buf->xxh, buf->data[buf->index] + buf->start, buf->end - buf->start);
#endif

    buf->start = buf->end;
}

int DICT_needShift(DICT_buffer * const buf, size_t const overlap)
{
    return buf->start == buf->end && (overlap == 0 || buf->end > overlap + ALIGNMENT_MASK);
}

int DICT_async(const DICT_buffer * const buf)
{
    return (int)buf->async;
}

void DICT_shift(DICT_buffer * const buf, size_t overlap)
{
    if (buf->start < buf->end)
        return;

    if (overlap == 0) {
        buf->start = 0;
        buf->end = 0;
        buf->index ^= buf->async;
    }
    else if (buf->end > overlap + ALIGNMENT_MASK) {
        size_t const from = (buf->end - overlap) & ALIGNMENT_MASK;
        const BYTE *const src = buf->data[buf->index];
        BYTE *const dst = buf->data[buf->index ^ buf->async];

        overlap = buf->end - from;

        if (overlap <= from || dst != src) {
            DEBUGLOG(5, "Copy overlap data : %u bytes from %u", (U32)overlap, (U32)from);
            memcpy(dst, src + from, overlap);
        }
        else if (from != 0) {
            DEBUGLOG(5, "Move overlap data : %u bytes from %u", (U32)overlap, (U32)from);
            memmove(dst, src + from, overlap);
        }
        buf->start = overlap;
        buf->end = overlap;
        buf->index ^= buf->async;
    }
    else {
        buf->start = buf->end;
    }
}

#ifndef NO_XXHASH
XXH32_hash_t DICT_getDigest(const DICT_buffer * const buf)
{
    return XXH32_digest(buf->xxh);
}
#endif

size_t DICT_memUsage(const DICT_buffer * const buf)
{
    return (1 + buf->async) * buf->bufSize;
}
