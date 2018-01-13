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

#ifndef FL2_ERRORS_H_398273423
#define FL2_ERRORS_H_398273423

#if defined (__cplusplus)
extern "C" {
#endif

/*===== dependency =====*/
#include <stddef.h>   /* size_t */


/* =====   FL2ERRORLIB_API : control library symbols visibility   ===== */
#ifndef FL2ERRORLIB_VISIBILITY
#  if defined(__GNUC__) && (__GNUC__ >= 4)
#    define FL2ERRORLIB_VISIBILITY __attribute__ ((visibility ("default")))
#  else
#    define FL2ERRORLIB_VISIBILITY
#  endif
#endif
#if defined(FL2_DLL_EXPORT) && (FL2_DLL_EXPORT==1)
#  define FL2ERRORLIB_API __declspec(dllexport) FL2ERRORLIB_VISIBILITY
#elif defined(FL2_DLL_IMPORT) && (FL2_DLL_IMPORT==1)
#  define FL2ERRORLIB_API __declspec(dllimport) FL2ERRORLIB_VISIBILITY /* It isn't required but allows to generate better code, saving a function pointer load from the IAT and an indirect jump.*/
#else
#  define FL2ERRORLIB_API FL2ERRORLIB_VISIBILITY
#endif

/*-****************************************
 *  error codes list
 *  note : this API is still considered unstable
 *         and shall not be used with a dynamic library.
 *         only static linking is allowed
 ******************************************/
typedef enum {
  FL2_error_no_error = 0,
  FL2_error_GENERIC  = 1,
  FL2_error_internal = 2,
  FL2_error_prefix_unknown                = 10,
  FL2_error_version_unsupported           = 12,
  FL2_error_corruption_detected = 20,
  FL2_error_checksum_wrong      = 22,
  FL2_error_parameter_unsupported   = 40,
  FL2_error_parameter_outOfBound    = 42,
  FL2_error_tableLog_tooLarge       = 44,
  FL2_error_maxSymbolValue_tooLarge = 46,
  FL2_error_maxSymbolValue_tooSmall = 48,
  FL2_error_stage_wrong       = 60,
  FL2_error_init_missing      = 62,
  FL2_error_memory_allocation = 64,
  FL2_error_dstSize_tooSmall = 70,
  FL2_error_srcSize_wrong    = 72,
  /* following error codes are not stable and may be removed or changed in a future version */
  FL2_error_frameIndex_tooLarge = 100,
  FL2_error_seekableIO          = 102,
  FL2_error_maxCode = 120  /* never EVER use this value directly, it can change in future versions! Use FL2_isError() instead */
} FL2_ErrorCode;

/*! FL2_getErrorCode() :
    convert a `size_t` function result into a `FL2_ErrorCode` enum type,
    which can be used to compare with enum list published above */
FL2ERRORLIB_API FL2_ErrorCode FL2_getErrorCode(size_t functionResult);
FL2ERRORLIB_API const char* FL2_getErrorString(FL2_ErrorCode code);   /**< Same as FL2_getErrorName, but using a `FL2_ErrorCode` enum argument */


#if defined (__cplusplus)
}
#endif

#endif /* FL2_ERRORS_H_398273423 */
