/*
 * Copyright (c) 2016-present, Yann Collet, Facebook, Inc.
 * All rights reserved.
 * Modified for FL2 by Conor McCarthy
 *
 * This source code is licensed under both the BSD-style license (found in the
 * LICENSE file in the root directory of this source tree) and the GPLv2 (found
 * in the COPYING file in the root directory of this source tree).
 * You may select, at your option, one of the above-listed licenses.
 */


/* ======   Dependencies   ======= */
#include <stddef.h>  /* size_t */
#include <stdlib.h>  /* malloc, calloc */
#include "fl2_pool.h"
#include "fl2_internal.h"


#ifndef FL2_SINGLETHREAD

#include "fl2_threading.h"   /* pthread adaptation */

/* A job is a function and an opaque argument */
typedef struct FL2POOL_job_s {
    FL2POOL_function function;
    void *opaque;
	int n;
} FL2POOL_job;

struct FL2POOL_ctx_s {
    /* Keep track of the threads */
    size_t numThreads;

    /* The queue is a single job */
    FL2POOL_job queue;

    /* The number of threads working on jobs */
    size_t numThreadsBusy;
    /* Indicates if the queue is empty */
    size_t queueIndex;
    size_t queueEnd;

    /* The mutex protects the queue */
    ZSTD_pthread_mutex_t queueMutex;
    /* Condition variable for pushers to wait on when the queue is full */
    ZSTD_pthread_cond_t queuePushCond;
    /* Condition variables for poppers to wait on when the queue is empty */
    ZSTD_pthread_cond_t queuePopCond;
    /* Indicates if the queue is shutting down */
    int shutdown;
    ZSTD_pthread_t threads[1];
};

/* FL2POOL_thread() :
   Work thread for the thread pool.
   Waits for jobs and executes them.
   @returns : NULL on failure else non-null.
*/
static void* FL2POOL_thread(void* opaque) {
    FL2POOL_ctx* const ctx = (FL2POOL_ctx*)opaque;
    if (!ctx) { return NULL; }
    ZSTD_pthread_mutex_lock(&ctx->queueMutex);
    for (;;) {

        /* While the mutex is locked, wait for a non-empty queue or until shutdown */
        while (ctx->queueIndex >= ctx->queueEnd && !ctx->shutdown) {
            ZSTD_pthread_cond_wait(&ctx->queuePopCond, &ctx->queueMutex);
        }
        /* empty => shutting down: so stop */
        if (ctx->shutdown) {
            ZSTD_pthread_mutex_unlock(&ctx->queueMutex);
            return opaque;
        }
        /* Pop a job off the queue */
        size_t thread = ctx->queueIndex;
        ++ctx->queueIndex;
        ++ctx->numThreadsBusy;
        /* Unlock the mutex, signal a pusher, and run the job */
        ZSTD_pthread_cond_signal(&ctx->queuePushCond);
        ZSTD_pthread_mutex_unlock(&ctx->queueMutex);

        ctx->queue.function(ctx->queue.opaque, thread, ctx->queue.n);

        ZSTD_pthread_mutex_lock(&ctx->queueMutex);
        ZSTD_pthread_cond_signal(&ctx->queuePushCond);
        --ctx->numThreadsBusy;
    }  /* for (;;) */
    /* Unreachable */
}

FL2POOL_ctx* FL2POOL_create(size_t numThreads) {
    FL2POOL_ctx* ctx;
    /* Check the parameters */
    if (!numThreads) { return NULL; }
    /* Allocate the context and zero initialize */
    ctx = (FL2POOL_ctx*)calloc(1, sizeof(FL2POOL_ctx) + (numThreads - 1) * sizeof(ZSTD_pthread_t));
    if (!ctx) { return NULL; }
    /* Initialize the job queue.
     * It needs one extra space since one space is wasted to differentiate empty
     * and full queues.
     */
    ctx->numThreadsBusy = 0;
    ctx->queueIndex = 0;
    ctx->queueEnd = 0;
    (void)ZSTD_pthread_mutex_init(&ctx->queueMutex, NULL);
    (void)ZSTD_pthread_cond_init(&ctx->queuePushCond, NULL);
    (void)ZSTD_pthread_cond_init(&ctx->queuePopCond, NULL);
    ctx->shutdown = 0;
    ctx->numThreads = 0;
    /* Initialize the threads */
    {   size_t i;
        for (i = 0; i < numThreads; ++i) {
            if (ZSTD_pthread_create(&ctx->threads[i], NULL, &FL2POOL_thread, ctx)) {
                ctx->numThreads = i;
                FL2POOL_free(ctx);
                return NULL;
        }   }
        ctx->numThreads = numThreads;
    }
    return ctx;
}

/*! FL2POOL_join() :
    Shutdown the queue, wake any sleeping threads, and join all of the threads.
*/
static void FL2POOL_join(FL2POOL_ctx* ctx) {
    /* Shut down the queue */
    ZSTD_pthread_mutex_lock(&ctx->queueMutex);
    ctx->shutdown = 1;
    /* Wake up sleeping threads */
    ZSTD_pthread_cond_broadcast(&ctx->queuePushCond);
    ZSTD_pthread_cond_broadcast(&ctx->queuePopCond);
    ZSTD_pthread_mutex_unlock(&ctx->queueMutex);
    /* Join all of the threads */
    {   size_t i;
        for (i = 0; i < ctx->numThreads; ++i) {
            ZSTD_pthread_join(ctx->threads[i], NULL);
    }   }
}

void FL2POOL_free(FL2POOL_ctx *ctx) {
    if (!ctx) { return; }
    FL2POOL_join(ctx);
    ZSTD_pthread_mutex_destroy(&ctx->queueMutex);
    ZSTD_pthread_cond_destroy(&ctx->queuePushCond);
    ZSTD_pthread_cond_destroy(&ctx->queuePopCond);
    free(ctx);
}

size_t FL2POOL_sizeof(FL2POOL_ctx *ctx) {
    if (ctx==NULL) return 0;  /* supports sizeof NULL */
    return sizeof(*ctx)
        + ctx->numThreads * sizeof(ZSTD_pthread_t);
}

void FL2POOL_add(void* ctxVoid, FL2POOL_function function, void *opaque, size_t first, size_t end, int n) {
    FL2POOL_ctx* const ctx = (FL2POOL_ctx*)ctxVoid;
    if (!ctx)
		return; 

    ZSTD_pthread_mutex_lock(&ctx->queueMutex);
    ctx->queue.function = function;
    ctx->queue.opaque = opaque;
    ctx->queue.n = n;
    ctx->queueIndex = first;
    ctx->queueEnd = end;
    ZSTD_pthread_cond_broadcast(&ctx->queuePopCond);
    /* Wait until the requested number of threads have a job */
    while (ctx->queueIndex < ctx->queueEnd && !ctx->shutdown) {
        ZSTD_pthread_cond_wait(&ctx->queuePushCond, &ctx->queueMutex);
    }
    ZSTD_pthread_mutex_unlock(&ctx->queueMutex);
}

int FL2POOL_waitAll(void *ctxVoid, unsigned timeout)
{
    FL2POOL_ctx* const ctx = (FL2POOL_ctx*)ctxVoid;
    if (!ctx || !ctx->numThreadsBusy || ctx->shutdown) { return 0; }

    ZSTD_pthread_mutex_lock(&ctx->queueMutex);
    if (timeout != 0) {
        if (ctx->numThreadsBusy && !ctx->shutdown)
            ZSTD_pthread_cond_timedwait(&ctx->queuePushCond, &ctx->queueMutex, timeout);
    }
    else {
        while (ctx->numThreadsBusy && !ctx->shutdown)
            ZSTD_pthread_cond_wait(&ctx->queuePushCond, &ctx->queueMutex);
    }
    ZSTD_pthread_mutex_unlock(&ctx->queueMutex);
    return ctx->numThreadsBusy && !ctx->shutdown;
}

size_t FL2POOL_threadsBusy(void * ctx)
{
    return ((FL2POOL_ctx*)ctx)->numThreadsBusy;
}

#endif  /* FL2_SINGLETHREAD */
