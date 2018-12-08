#include "fast-lzma2.h"
#include "mem.h"
#include "data_block.h"
#ifndef NO_XXHASH
#  include "xxhash.h"
#endif

#ifndef FL2_DICT_BUFFER_H_
#define FL2_DICT_BUFFER_H_

typedef struct {
    BYTE* data[2];
    size_t index;
    size_t async;
    size_t start;   /* start = 0 (first block) or overlap */
    size_t end;     /* never < overlap */
    size_t bufSize; /* allocation size */
#ifndef NO_XXHASH
    XXH32_state_t *xxh;
#endif
} DICT_buffer;

int DICT_construct(DICT_buffer *const buf, int const async);

int DICT_init(DICT_buffer *const buf, size_t const dictSize, int const doHash);

void DICT_destruct(DICT_buffer *const buf);

size_t DICT_size(const DICT_buffer *const buf);

size_t DICT_get(DICT_buffer *const buf, size_t const overlap, FL2_outBuffer* const dict);

int DICT_update(DICT_buffer *const buf, size_t const addedSize);

void DICT_put(DICT_buffer *const buf, FL2_inBuffer* const input);

size_t DICT_availSpace(const DICT_buffer *const buf);

int DICT_hasUnprocessed(const DICT_buffer *const buf);

void DICT_getBlock(DICT_buffer *const buf, FL2_dataBlock *const block);

int DICT_needShift(DICT_buffer *const buf, size_t const overlap);

int DICT_async(const DICT_buffer *const buf);

void DICT_shift(DICT_buffer *const buf, size_t overlap);

#ifndef NO_XXHASH
XXH32_hash_t DICT_getDigest(const DICT_buffer *const buf);
#endif

size_t DICT_memUsage(const DICT_buffer *const buf);

#endif /* FL2_DICT_BUFFER_H_ */