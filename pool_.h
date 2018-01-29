/*
 * Copyright (c) 2016-present, Yann Collet, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under both the BSD-style license (found in the
 * LICENSE file in the root directory of this source tree) and the GPLv2 (found
 * in the COPYING file in the root directory of this source tree).
 * You may select, at your option, one of the above-listed licenses.
 */

#ifndef POOL_H
#define POOL_H

#if defined (__cplusplus)
extern "C" {
#endif


#include <stddef.h>   /* size_t */

typedef struct POOL_ctx_s POOL_ctx;

/*! POOL_create() :
 *  Create a thread pool with at most `numThreads` threads.
 * `numThreads` must be at least 1.
 *  The maximum number of queued jobs before blocking is `queueSize`.
 * @return : POOL_ctx pointer on success, else NULL.
*/
POOL_ctx *POOL_create(size_t numThreads, size_t queueSize);

POOL_ctx *POOL_create_advanced(size_t numThreads, size_t queueSize);

/*! POOL_free() :
    Free a thread pool returned by POOL_create().
*/
void POOL_free(POOL_ctx *ctx);

/*! POOL_sizeof() :
    return memory usage of pool returned by POOL_create().
*/
size_t POOL_sizeof(POOL_ctx *ctx);

/*! POOL_function :
    The function type that can be added to a thread pool.
*/
typedef void (*POOL_function)(void *, size_t);

/*! POOL_add() :
    Add the job `function(opaque)` to the thread pool.
    Note : The function may be executed asynchronously, so `opaque` must live until the function has been completed.
*/
int POOL_set(void *ctx, size_t i, POOL_function function, void *opaque, size_t n);

void POOL_waitAll(void *ctx);

#if defined (__cplusplus)
}
#endif

#endif
