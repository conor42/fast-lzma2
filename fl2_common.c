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
#include <stdlib.h>      /* malloc, calloc, free */
#include <string.h>      /* memset */
#include "fast-lzma2.h"
#include "error_private.h"
#include "fl2_internal.h"


/*-****************************************
*  Version
******************************************/
unsigned FL2_versionNumber(void) { return FL2_VERSION_NUMBER; }

const char* FL2_versionString(void) { return FL2_VERSION_STRING; }


FL2LIB_API size_t FL2_compressBound(size_t srcSize)
{
    return FL2_COMPRESSBOUND(srcSize);
}

/*-****************************************
*  ZSTD Error Management
******************************************/
/*! FL2_isError() :
 *  tells if a return value is an error code */
unsigned FL2_isError(size_t code) { return ERR_isError(code); }

/*! FL2_getErrorName() :
 *  provides error code string from function result (useful for debugging) */
const char* FL2_getErrorName(size_t code) { return ERR_getErrorName(code); }

/*! FL2_getError() :
 *  convert a `size_t` function result into a proper FL2_errorCode enum */
FL2_ErrorCode FL2_getErrorCode(size_t code) { return ERR_getErrorCode(code); }

/*! FL2_getErrorString() :
 *  provides error code string from enum */
const char* FL2_getErrorString(FL2_ErrorCode code) { return ERR_getErrorString(code); }

/*! g_debuglog_enable :
 *  turn on/off debug traces (global switch) */
#if defined(FL2_DEBUG) && (FL2_DEBUG >= 2)
int g_debuglog_enable = 1;
#endif

