/*
 * Copyright (c) 2016-present, Yann Collet, Facebook, Inc.
 * All rights reserved.
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
#include "atomic.h"

/* ======   Compiler specifics   ====== */
#if defined(_MSC_VER)
#  pragma warning(disable : 4204)        /* disable: C4204: non-constant aggregate initializer */
#endif


#ifndef FL2_SINGLETHREAD

#include "threading.h"   /* pthread adaptation */

/* A job is a function, an opaque argument, and a numeric argument */
typedef struct POOL_job_s {
	/* The mutex protects the job */
	ZSTD_pthread_mutex_t jobMutex;
	ZSTD_pthread_cond_t jobCond;
	POOL_function function;
    void *opaque;
	size_t n;
} POOL_job;

struct POOL_ctx_s {
    /* Keep track of the threads */
    ZSTD_pthread_t *threads;
    size_t numThreads;

    /* Jobs buffer */
    POOL_job *jobs;
	FL2_atomic jobAssign;

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
	/* Take a job number for the duration of the thread */
	ptrdiff_t job = FL2_atomic_increment(ctx->jobAssign);
	ZSTD_pthread_mutex_lock(&ctx->jobs[job].jobMutex);
	for (;;) {
        while (ctx->jobs[job].function == NULL && !ctx->shutdown) {
            ZSTD_pthread_cond_wait(&ctx->jobs[job].jobCond, &ctx->jobs[job].jobMutex);
        }
        /* empty => shutting down: so stop */
        if (ctx->jobs[job].function == NULL) {
            ZSTD_pthread_mutex_unlock(&ctx->jobs[job].jobMutex);
            return opaque;
        }

		ctx->jobs[job].function(ctx->jobs[job].opaque, ctx->jobs[job].n);
		ctx->jobs[job].function = NULL;

		/* signal any threads waiting for completion */
		ZSTD_pthread_cond_signal(&ctx->jobs[job].jobCond);
	}  /* for (;;) */
    /* Unreachable */
}

POOL_ctx* POOL_create(size_t numThreads, size_t queueSize) {
    return POOL_create_advanced(numThreads, queueSize);
}

POOL_ctx* POOL_create_advanced(size_t numThreads, size_t queueSize) {
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
    ctx->jobs = (POOL_job*)malloc(numThreads * sizeof(POOL_job));
    ctx->shutdown = 0;
    /* Allocate space for the thread handles */
    ctx->threads = (ZSTD_pthread_t*)malloc(numThreads * sizeof(ZSTD_pthread_t));
    ctx->numThreads = 0;
	ctx->jobAssign = ATOMIC_INITIAL_VALUE;
    /* Check for errors */
    if (!ctx->threads || !ctx->jobs) { POOL_free(ctx); return NULL; }
    /* Initialize the threads */
    {   size_t i;
        for (i = 0; i < numThreads; ++i) {
			ctx->jobs[i].function = NULL;
			(void)ZSTD_pthread_mutex_init(&ctx->jobs[i].jobMutex, NULL);
			(void)ZSTD_pthread_cond_init(&ctx->jobs[i].jobCond, NULL);
			if (ZSTD_pthread_create(&ctx->threads[i], NULL, &POOL_thread, ctx)) {
				ZSTD_pthread_mutex_destroy(&ctx->jobs[i].jobMutex);
				ZSTD_pthread_cond_destroy(&ctx->jobs[i].jobCond);
				ctx->numThreads = i;
				POOL_free(ctx);
				return NULL;
			}
		}
        ctx->numThreads = numThreads;
    }
    return ctx;
}

/*! POOL_join() :
    Shutdown the queue, wake any sleeping threads, and join all of the threads.
*/
static void POOL_join(POOL_ctx* ctx) {
    /* Shut down the queue */
    ctx->shutdown = 1;
    /* Wake up sleeping threads */
    /* Join all of the threads */
    {   size_t i;
        for (i = 0; i < ctx->numThreads; ++i) {
			ZSTD_pthread_mutex_lock(&ctx->jobs[i].jobMutex);
			ZSTD_pthread_cond_signal(&ctx->jobs[i].jobCond);
			ZSTD_pthread_mutex_unlock(&ctx->jobs[i].jobMutex);
			ZSTD_pthread_join(ctx->threads[i], NULL);
			ZSTD_pthread_mutex_destroy(&ctx->jobs[i].jobMutex);
			ZSTD_pthread_cond_destroy(&ctx->jobs[i].jobCond);
	}   }
}

void POOL_free(POOL_ctx *ctx) {
    if (!ctx) { return; }
    POOL_join(ctx);
    free(ctx->jobs);
    free(ctx->threads);
    free(ctx);
}

size_t POOL_sizeof(POOL_ctx *ctx) {
    if (ctx==NULL) return 0;  /* supports sizeof NULL */
    return sizeof(*ctx)
        + ctx->numThreads * sizeof(POOL_job)
        + ctx->numThreads * sizeof(ZSTD_pthread_t);
}

int POOL_set(POOL_ctx *ctx, size_t i, POOL_function function, void *opaque, size_t n)
{
    if (!ctx || i >= ctx->numThreads || ctx->jobs[i].function != NULL) { return 1; }

    ZSTD_pthread_mutex_lock(&ctx->jobs[i].jobMutex);

	/* Still going => set the params */
	if (!ctx->shutdown) {
		ctx->jobs[i].function = function;
		ctx->jobs[i].opaque = opaque;
		ctx->jobs[i].n = n;
	}
    ZSTD_pthread_mutex_unlock(&ctx->jobs[i].jobMutex);
    ZSTD_pthread_cond_signal(&ctx->jobs[i].jobCond);
	return 0;
}

void POOL_waitAll(void *ctxVoid)
{
    POOL_ctx* const ctx = (POOL_ctx*)ctxVoid;
    if (!ctx) { return; }

	for (size_t i = 0; i < ctx->numThreads; ++i) {
		while (ctx->jobs[i].function != NULL && !ctx->shutdown) {
			ZSTD_pthread_mutex_lock(&ctx->jobs[i].jobMutex);
			ZSTD_pthread_mutex_unlock(&ctx->jobs[i].jobMutex);
		}
	}
}

#endif  /* FL2_SINGLETHREAD */
