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

int DICT_construct(DICT_buffer *buf, int async);

int DICT_init(DICT_buffer *buf, size_t dictSize, int doHash);

void DICT_free(DICT_buffer *buf);

size_t DICT_get(DICT_buffer *buf, size_t overlap, FL2_outBuffer* dict);

int DICT_update(DICT_buffer *buf, size_t addedSize);

void DICT_put(DICT_buffer *buf, FL2_inBuffer* input);

size_t DICT_availSpace(const DICT_buffer *buf);

int DICT_hasUnprocessed(const DICT_buffer *buf);

void DICT_getBlock(DICT_buffer *buf, FL2_dataBlock *block);

int DICT_needShift(DICT_buffer *buf, size_t overlap);

int DICT_async(const DICT_buffer *buf);

void DICT_shift(DICT_buffer *buf, size_t overlap);

#ifndef NO_XXHASH
XXH32_hash_t DICT_getDigest(const DICT_buffer *buf);
#endif

size_t DICT_memUsage(const DICT_buffer *buf);

#endif /* FL2_DICT_BUFFER_H_ */