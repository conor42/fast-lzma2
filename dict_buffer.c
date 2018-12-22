#include <stdlib.h>
#include "dict_buffer.h"
#include "fl2_internal.h"

#define ALIGNMENT_SIZE 16U
#define ALIGNMENT_MASK (~(size_t)(ALIGNMENT_SIZE-1))

int DICT_construct(DICT_buffer * const buf, int const async)
{
    buf->data[0] = NULL;
    buf->data[1] = NULL;
    buf->size = 0;

    buf->async = (async != 0);

#ifndef NO_XXHASH
    buf->xxh = NULL;
#endif

    return 0;
}

int DICT_init(DICT_buffer * const buf, size_t const dict_size, int const do_hash)
{
    if (buf->data[0] == NULL || dict_size > buf->size) {
        DICT_destruct(buf);
        buf->data[0] = malloc(dict_size);

        buf->data[1] = NULL;
        if (buf->async)
            buf->data[1] = malloc(dict_size);

        if (buf->data[0] == NULL || (buf->async && buf->data[1] == NULL)) {
            DICT_destruct(buf);
            return 1;
        }
    }
    buf->index = 0;
    buf->start = 0;
    buf->end = 0;
    buf->size = dict_size;

#ifndef NO_XXHASH
    if (do_hash) {
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
    buf->size = 0;
#ifndef NO_XXHASH
    XXH32_freeState(buf->xxh);
    buf->xxh = NULL;
#endif
}

size_t DICT_size(const DICT_buffer * const buf)
{
    return buf->size;
}

size_t DICT_get(DICT_buffer * const buf, size_t const overlap, FL2_outBuffer * const dict)
{
    DICT_shift(buf, overlap);

    dict->dst = buf->data[buf->index] + buf->end;
    dict->pos = 0;
    dict->size = buf->size - buf->end;

    return dict->size - dict->pos;
}

int DICT_update(DICT_buffer * const buf, size_t const added_size)
{
    buf->end += added_size;
    assert(buf->end <= buf->size);
    return !DICT_availSpace(buf);
}

void DICT_put(DICT_buffer * const buf, FL2_inBuffer * const input)
{
    size_t const to_read = MIN(buf->size - buf->end, input->size - input->pos);

    DEBUGLOG(5, "CStream : reading %u bytes", (U32)to_read);

    memcpy(buf->data[buf->index] + buf->end, (BYTE*)input->src + input->pos, to_read);

    input->pos += to_read;
    buf->end += to_read;
}

size_t DICT_availSpace(const DICT_buffer * const buf)
{
    return buf->size - buf->end;
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
    return buf->start == buf->end && (overlap == 0 || buf->end >= overlap + ALIGNMENT_SIZE);
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
    else if (buf->end >= overlap + ALIGNMENT_SIZE) {
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
    return (1 + buf->async) * buf->size;
}
