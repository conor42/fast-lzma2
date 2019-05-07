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



/*-*************************************
*  Dependencies
***************************************/
#include <stdlib.h>
#include "fast-lzma2.h"
#include "fl2_errors.h"
#include "fl2_internal.h"
#include "lzma2_enc.h"


/*-****************************************
*  Version
******************************************/
FL2LIB_API unsigned FL2LIB_CALL FL2_versionNumber(void) { return FL2_VERSION_NUMBER; }

FL2LIB_API const char* FL2LIB_CALL FL2_versionString(void) { return FL2_VERSION_STRING; }


/*-****************************************
*  Custom allocator handlers
******************************************/
void* (*FL2_g_alloc)(size_t size);
void  (*FL2_g_free)(void* address);
void* (*FL2_g_large_alloc)(size_t size);
void  (*FL2_g_large_free)(void* address);
unsigned char FL2_g_alloc_called;

void *FL2_malloc(size_t size)
{
    FL2_g_alloc_called = 1;
    char *address = (FL2_g_alloc != NULL) ? FL2_g_alloc(size) : malloc(size);
    DEBUGLOG(3, "FL2_malloc: %lu bytes at 0x%lX", (long)size, (long)(address - (char*)0));
    return address;
}

void *FL2_calloc(size_t count, size_t size)
{
    size *= count;
    void *block = FL2_malloc(size);
    if (block != NULL)
        memset(block, 0, size);
    return block;
}

void FL2_free(void *address)
{
    DEBUGLOG(3, "FL2_free: 0x%lX", (long)((char*)address - (char*)0));
    if (FL2_g_free != NULL)
        FL2_g_free(address);
    else
        free(address);
}

void *FL2_large_malloc(size_t size)
{
    FL2_g_alloc_called = 1;
    char *address = (FL2_g_large_alloc != NULL) ? FL2_g_large_alloc(size) : FL2_malloc(size);
    DEBUGLOG(3, "FL2_large_malloc: %lu bytes at 0x%lX", (long)size, (long)(address - (char*)0));
    return address;
}

void FL2_large_free(void *address)
{
    DEBUGLOG(3, "FL2_large_free: 0x%lX", (long)((char*)address - (char*)0));
    if (FL2_g_large_free != NULL)
        FL2_g_large_free(address);
    else
        FL2_free(address);
}

FL2LIB_API size_t FL2LIB_CALL FL2_setAllocator(void* (*allocFunction)(size_t size),
    void(*freeFunction)(void* address))
{
    if (FL2_g_alloc_called)
        return FL2_ERROR(stage_wrong);
    FL2_g_alloc = allocFunction;
    FL2_g_free = freeFunction;
    return FL2_error_no_error;
}

FL2LIB_API size_t FL2LIB_CALL FL2_setLargeAllocator(void* (*allocFunction)(size_t size),
    void(*freeFunction)(void* address))
{
    if (FL2_g_alloc_called)
        return FL2_ERROR(stage_wrong);
    FL2_g_large_alloc = allocFunction;
    FL2_g_large_free = freeFunction;
    return FL2_error_no_error;
}


/*-****************************************
*  Compression helpers
******************************************/
FL2LIB_API size_t FL2LIB_CALL FL2_compressBound(size_t srcSize)
{
	return LZMA2_compressBound(srcSize);
}

/*-****************************************
*  FL2 Error Management
******************************************/
HINT_INLINE
unsigned IsError(size_t code)
{
    return (code > FL2_ERROR(maxCode));
}

/*! FL2_isError() :
 *  tells if a return value is an error code */
FL2LIB_API unsigned FL2LIB_CALL FL2_isError(size_t code)
{
    return IsError(code);
}

/*! FL2_isTimedOut() :
 *  tells if a return value is the timeout code */
FL2LIB_API unsigned FL2LIB_CALL FL2_isTimedOut(size_t code)
{
    return (code == FL2_ERROR(timedOut));
}

/*! FL2_getErrorName() :
 *  provides error code string from function result (useful for debugging) */
FL2LIB_API const char* FL2LIB_CALL FL2_getErrorName(size_t code)
{
    return FL2_getErrorString(FL2_getErrorCode(code));
}

/*! FL2_getError() :
 *  convert a `size_t` function result into a proper FL2_errorCode enum */
FL2LIB_API FL2_ErrorCode FL2LIB_CALL FL2_getErrorCode(size_t code)
{
    if (!IsError(code)) 
        return (FL2_ErrorCode)0;

    return (FL2_ErrorCode)(0 - code);
}

/*! FL2_getErrorString() :
 *  provides error code string from enum */
FL2LIB_API const char* FL2LIB_CALL FL2_getErrorString(FL2_ErrorCode code)
{
    static const char* const notErrorCode = "Unspecified error code";
    switch (code)
    {
    case PREFIX(no_error): return "No error detected";
    case PREFIX(GENERIC):  return "Error (generic)";
    case PREFIX(internal): return "Internal error (bug)";
    case PREFIX(corruption_detected): return "Corrupted block detected";
    case PREFIX(checksum_wrong): return "Restored data doesn't match checksum";
    case PREFIX(parameter_unsupported): return "Unsupported parameter";
    case PREFIX(parameter_outOfBound): return "Parameter is out of bound";
    case PREFIX(lclpMax_exceeded): return "Parameters lc+lp > 4";
    case PREFIX(stage_wrong): return "Not possible at this stage of encoding";
    case PREFIX(init_missing): return "Context should be init first";
    case PREFIX(memory_allocation): return "Allocation error : not enough memory";
    case PREFIX(dstSize_tooSmall): return "Destination buffer is too small";
    case PREFIX(srcSize_wrong): return "Src size is incorrect";
    case PREFIX(canceled): return "Processing was canceled by a call to FL2_cancelCStream() or FL2_cancelDStream()";
    case PREFIX(buffer): return "Streaming progress halted due to buffer(s) full/empty";
    case PREFIX(timedOut): return "Wait timed out. Timeouts should be handled before errors using FL2_isTimedOut()";
        /* following error codes are not stable and may be removed or changed in a future version */
    case PREFIX(maxCode):
    default: return notErrorCode;
    }
}

/*! g_debuglog_enable :
 *  turn on/off debug traces (global switch) */
#if defined(FL2_DEBUG) && (FL2_DEBUG >= 2)
int g_debuglog_enable = 1;
#endif

