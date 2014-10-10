/*******************************************************************************
** Copyright(C) 2003-2014 Intel Corporation. All Rights Reserved.
**                                                                             
** The source code, information and material ("Material") contained herein is 
** owned by Intel Corporation or its suppliers or licensors, and title 
** to such Material remains with Intel Corporation or its suppliers or 
** licensors. The Material contains proprietary information of Intel or 
** its suppliers and licensors. The Material is protected by worldwide 
** copyright laws and treaty provisions. No part of the Material may be used, 
** copied, reproduced, modified, published, uploaded, posted, transmitted, 
** distributed or disclosed in any way without Intel's prior express written 
** permission. No license under any patent, copyright or other intellectual 
** property rights in the Material is granted to or conferred upon you, 
** either expressly, by implication, inducement, estoppel or otherwise. 
** Any license under such intellectual property rights must be express and 
** approved by Intel in writing. Unless otherwise agreed by Intel in writing, 
** you may not remove or alter this notice or any other notice embedded in 
** Materials by Intel or Intel's suppliers or licensors in any way.
*/

#ifndef VM_BASE_H
#define VM_BASE_H

#ifdef __cplusplus
extern "C" {
#endif

/*
// Base definitions
*/
#ifdef _WIN32
    #include <windows.h>
    #include <winbase.h>
    #if _MSC_VER <= 1400 && !defined(_WIN32_WINNT)
        #define _WIN32_WINNT 0x0500
    #endif
#else
    #include <stdlib.h>
    #include <errno.h>
    #include <sys/types.h>
#endif

typedef enum e_vm_Status
{
    VM_OK                           = 0,    // no error
    VM_OPERATION_FAILED             =-999,
    VM_NOT_INITIALIZED              =-998,
    VM_TIMEOUT                      =-987,
    VM_NOT_ENOUGH_DATA              =-996,  // not enough input data

    VM_NULL_PTR                     =-995,
    VM_SO_CANT_LOAD                 =-994,
    VM_SO_INVALID_HANDLE            =-993,
    VM_SO_CANT_GET_ADDR             =-992
} vm_status;

/*
// Memory operations
*/
void* vm_malloc(size_t iSize, size_t iAlign);
void  vm_free(void* pBuffer);

/*
// Strings operations
*/
#if defined _WIN32
#if !defined(__MIC__)
#include <tchar.h>
#endif

#define vm_main _tmain

#define VM_STRING(x) __T(x)

#if !defined(__MIC__)
typedef TCHAR vm_char;
#else
typedef char vm_char;
#endif

#define vm_string_printf    _tprintf
#define vm_string_fprintf   vm_file_fprintf
#define vm_string_snprintf  _sntprintf
#define vm_string_vprintf   _vtprintf
#define vm_string_vfprintf  vm_file_vfprintf
#define vm_string_vsprintf  _vstprintf

#define vm_string_strcat    _tcscat
#define vm_string_strncat   _tcsncat
#define vm_string_strcpy    _tcscpy
#define vm_string_strcspn   _tcscspn
#define vm_string_strspn    _tcsspn

#if !defined(__MIC__)
#define vm_string_strlen    _tcslen
#else
#define vm_string_strlen    strlen
#endif
#define vm_string_strcmp    _tcscmp
#define vm_string_strncmp   _tcsnccmp
#define vm_string_stricmp   _tcsicmp
#define vm_string_strnicmp  _tcsncicmp
#define vm_string_strncpy   _tcsncpy
#define vm_string_strrchr   _tcsrchr
#ifdef _UNICODE
#define vm_string_tolower   towlower
#else
#define vm_string_tolower   tolower
#endif

#define vm_string_isdigit   _istdigit
#define vm_string_atoi      _ttoi
#define vm_string_atol      _ttol
#define vm_string_atoll     _ttoi64
#define vm_string_atof      _tstof
#define vm_string_strstr    _tcsstr
#define vm_string_sscanf    _stscanf
#define vm_string_strchr    _tcschr
#define vm_string_strtok    _tcstok
#define vm_string_makepath  _tmakepath

#define vm_findptr intptr_t
#define vm_finddata_t struct _tfinddata_t
#define vm_string_splitpath _tsplitpath
#define vm_string_findclose _findclose
#else
#include <dirent.h>

#ifdef _UNICODE
#include <wchar.h>

#define vm_main wmain

#define VM_STRING(x) Lx

#else
#include <string.h>
#include <ctype.h>

#define vm_main main

#define VM_STRING(x) x

typedef char vm_char;

#define vm_string_printf    printf
#define vm_string_fprintf   vm_file_fprintf
#define vm_string_snprintf  snprintf
#define vm_string_vprintf   vprintf
#define vm_string_vfprintf  vm_file_vfprintf
#define vm_string_vsprintf  vsprintf

#define vm_string_strcat    strcat
#define vm_string_strncat   strncat
#define vm_string_strcpy    strcpy
#define vm_string_strncpy   strncpy
#define vm_string_strcspn   strcspn
#define vm_string_strspn    strspn

#define vm_string_strlen    strlen
#define vm_string_strcmp    strcmp
#define vm_string_strncmp   strncmp
#define vm_string_stricmp   strcasecmp
#define vm_string_strnicmp  strncasecmp
#define vm_string_strrchr   strrchr
#define vm_string_tolower   tolower

#define vm_string_isdigit   isdigit
#define vm_string_atoi      atoi
#define vm_string_atol      atol
#define vm_string_atoll     atoll
#define vm_string_atof      atof

#define vm_string_strstr    strstr
#define vm_string_sscanf    sscanf
#define vm_string_strchr    strchr

#define vm_finddata_t struct _finddata_t
#define vm_string_splitpath _splitpath
#endif
#endif

/*
// Files operations
*/
#if defined _WIN32
#include <stdio.h>
#if !defined(__MIC__)
#include <io.h>
#endif

#define vm_file_fopen  _tfopen
#define vm_file_fclose fclose
#define vm_file_feof   feof
#define vm_file_remove _tremove
#define vm_file_fflush fflush

/* binary file IO */
#define vm_file_fread  fread
#define vm_file_fwrite fwrite

/* character (string) file IO */
#define vm_file_fgets    _fgetts
#define vm_file_fputs    _fputts
#define vm_file_fscanf   _ftscanf
#define vm_file_fprintf  _ftprintf
#define vm_file_vfprintf _vftprintf

/* temporary file support */
#define vm_file_tmpfile  tmpfile
#define vm_file_tmpnam   tmpnam
#define vm_file_tmpnam_r tmpnam
#define vm_file_tempnam  tempnam

#define vm_file_fseek _fseeki64
#define vm_file_ftell _ftelli64

#define vm_file_access  _taccess
#else
#include <stdio.h>
#include <unistd.h>

#ifndef __USE_LARGEFILE64
#define __USE_LARGEFILE64
#endif

#ifndef __USE_LARGEFILE
#define __USE_LARGEFILE
#endif

#if defined __APPLE__ || defined (INTEL64) || defined(__USE_LARGEFILE64)
/* native fopen is 64-bits */
#define vm_file_fopen fopen
#else
#define vm_file_fopen fopen64
#endif

#define vm_file_fclose  fclose
#define vm_file_feof    feof
#define vm_file_remove  remove
#define vm_file_fflush  fflush

/* binary file IO */
#define vm_file_fread  fread
#define vm_file_fwrite fwrite

/* character (string) file IO */
#define vm_file_fgets    fgets
#define vm_file_fputs    fputs
#define vm_file_fscanf   fscanf
#define vm_file_fprintf  fprintf
#define vm_file_vfprintf vfprintf

/* temporary file support */
#define vm_file_tmpfile  tmpfile
#define vm_file_tmpnam   tmpnam
#define vm_file_tmpnam_r tmpnam_r
#define vm_file_tempnam  tempnam

unsigned long long vm_file_fseek(FILE *fd, long long position, int mode);
unsigned long long vm_file_ftell(FILE *fd);

#define vm_file_access  access
#endif

/*
// Sysinfo operations
*/
unsigned int vm_sys_info_get_cpu_speed(void);
unsigned int vm_sys_info_get_avail_cpu_num(void);

/*
// Time operations
*/
typedef long long vm_tick;

/* yield the execution of current thread for msec milliseconds */
void vm_time_sleep(unsigned int msec);

/* obtain the clock tick of an uninterrupted master clock */
vm_tick vm_time_get_tick(void);

/* obtain the clock resolution */
vm_tick vm_time_get_frequency(void);


#ifdef __cplusplus
}
#endif

#endif
