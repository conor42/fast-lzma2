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
#include "pool.h"
#include "fl2_internal.h"

/* ======   Compiler specifics   ====== */
#if defined(_MSC_VER)
#  pragma warning(disable : 4204)        /* disable: C4204: non-constant aggregate initializer */
#endif


#ifndef FL2_SINGLETHREAD

#include "threading.h"   /* pthread adaptation */

/* A job is a function and an opaque argument */
typedef struct POOL_job_s {
    POOL_function function;
    void *opaque;
	size_t n;
} POOL_job;

struct POOL_ctx_s {
    /* Keep track of the threads */
    ZSTD_pthread_t *threads;
    size_t numThreads;

    /* The queue is a single job */
    POOL_job queue;

    /* The number of threads working on jobs */
    size_t numThreadsBusy;
    /* Indicates if the queue is empty */
    int queueEmpty;

    /* The mutex protects the queue */
    ZSTD_pthread_mutex_t queueMutex;
    /* Condition variable for pushers to wait on when the queue is full */
    ZSTD_pthread_cond_t queuePushCond;
    /* Condition variables for poppers to wait on when the queue is empty */
    ZSTD_pthread_cond_t queuePopCond;
    /* Indicates if the queue is shutting down */
    int shutdown;
};

/* POOL_thread() :
   Work thread for the thread pool.
   Waits for jobs and executes them.
   @returns : NULL on failure else non-null.
*/
static void* POOL_thread(void* opaque) {
    POOL_ctx* const ctx = (POOL_ctx*)opaque;
    if (!ctx) { return NULL; }
    for (;;) {
        /* Lock the mutex and wait for a non-empty queue or until shutdown */
        ZSTD_pthread_mutex_lock(&ctx->queueMutex);

        while (ctx->queueEmpty && !ctx->shutdown) {
            ZSTD_pthread_cond_wait(&ctx->queuePopCond, &ctx->queueMutex);
        }
        /* empty => shutting down: so stop */
        if (ctx->queueEmpty) {
            ZSTD_pthread_mutex_unlock(&ctx->queueMutex);
            return opaque;
        }
        /* Pop a job off the queue */
        {   POOL_job const job = ctx->queue;
            ctx->queueEmpty = 1;
            /* Unlock the mutex, signal a pusher, and run the job */
            ZSTD_pthread_mutex_unlock(&ctx->queueMutex);
            ZSTD_pthread_cond_signal(&ctx->queuePushCond);

            job.function(job.opaque, job.n);

			ZSTD_pthread_mutex_lock(&ctx->queueMutex);
			ctx->numThreadsBusy--;
			ZSTD_pthread_mutex_unlock(&ctx->queueMutex);
			ZSTD_pthread_cond_signal(&ctx->queuePushCond);
		}
    }  /* for (;;) */
    /* Unreachable */
}

POOL_ctx* POOL_create(size_t numThreads) {
    POOL_ctx* ctx;
    /* Check the parameters */
    if (!numThreads) { return NULL; }
    /* Allocate the context and zero initialize */
    ctx = (POOL_ctx*)calloc(1, sizeof(POOL_ctx));
    if (!ctx) { return NULL; }
    /* Initialize the job queue.
     * It needs one extra space since one space is wasted to differentiate empty
     * and full queues.
     */
    ctx->numThreadsBusy = 0;
    ctx->queueEmpty = 1;
    (void)ZSTD_pthread_mutex_init(&ctx->queueMutex, NULL);
    (void)ZSTD_pthread_cond_init(&ctx->queuePushCond, NULL);
    (void)ZSTD_pthread_cond_init(&ctx->queuePopCond, NULL);
    ctx->shutdown = 0;
    /* Allocate space for the thread handles */
    ctx->threads = (ZSTD_pthread_t*)malloc(numThreads * sizeof(ZSTD_pthread_t));
    ctx->numThreads = 0;
    /* Check for errors */
    if (!ctx->threads) { POOL_free(ctx); return NULL; }
    /* Initialize the threads */
    {   size_t i;
        for (i = 0; i < numThreads; ++i) {
            if (ZSTD_pthread_create(&ctx->threads[i], NULL, &POOL_thread, ctx)) {
                ctx->numThreads = i;
                POOL_free(ctx);
                return NULL;
        }   }
        ctx->numThreads = numThreads;
    }
    return ctx;
}

/*! POOL_join() :
    Shutdown the queue, wake any sleeping threads, and join all of the threads.
*/
static void POOL_join(POOL_ctx* ctx) {
    /* Shut down the queue */
    ZSTD_pthread_mutex_lock(&ctx->queueMutex);
    ctx->shutdown = 1;
    ZSTD_pthread_mutex_unlock(&ctx->queueMutex);
    /* Wake up sleeping threads */
    ZSTD_pthread_cond_broadcast(&ctx->queuePushCond);
    ZSTD_pthread_cond_broadcast(&ctx->queuePopCond);
    /* Join all of the threads */
    {   size_t i;
        for (i = 0; i < ctx->numThreads; ++i) {
            ZSTD_pthread_join(ctx->threads[i], NULL);
    }   }
}

void POOL_free(POOL_ctx *ctx) {
    if (!ctx) { return; }
    POOL_join(ctx);
    ZSTD_pthread_mutex_destroy(&ctx->queueMutex);
    ZSTD_pthread_cond_destroy(&ctx->queuePushCond);
    ZSTD_pthread_cond_destroy(&ctx->queuePopCond);
    free(ctx->threads);
    free(ctx);
}

size_t POOL_sizeof(POOL_ctx *ctx) {
    if (ctx==NULL) return 0;  /* supports sizeof NULL */
    return sizeof(*ctx)
        + ctx->numThreads * sizeof(ZSTD_pthread_t);
}

void POOL_add(void* ctxVoid, POOL_function function, void *opaque, size_t n) {
    POOL_ctx* const ctx = (POOL_ctx*)ctxVoid;
    if (!ctx)
		return; 

    ZSTD_pthread_mutex_lock(&ctx->queueMutex);
    {   POOL_job const job = {function, opaque, n};

        /* Wait until there is space in the queue for the new job */
        while (!ctx->queueEmpty && !ctx->shutdown) {
          ZSTD_pthread_cond_wait(&ctx->queuePushCond, &ctx->queueMutex);
        }
        /* The queue is still going => there is space */
        if (!ctx->shutdown) {
			ctx->numThreadsBusy++;
			ctx->queueEmpty = 0;
            ctx->queue = job;
        }
    }
    ZSTD_pthread_mutex_unlock(&ctx->queueMutex);
    ZSTD_pthread_cond_signal(&ctx->queuePopCond);
}

void POOL_waitAll(void *ctxVoid)
{
    POOL_ctx* const ctx = (POOL_ctx*)ctxVoid;
    if (!ctx) { return; }

    ZSTD_pthread_mutex_lock(&ctx->queueMutex);
    while (ctx->numThreadsBusy && !ctx->shutdown) {
        ZSTD_pthread_cond_wait(&ctx->queuePushCond, &ctx->queueMutex);
    }
    ZSTD_pthread_mutex_unlock(&ctx->queueMutex);
}

#endif  /* FL2_SINGLETHREAD */
