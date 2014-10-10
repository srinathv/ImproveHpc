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

#include "vm_base.h"

/*
// Memory operations
*/
#ifdef UNIX
    #include <malloc.h>
    #include <stdlib.h>
#endif

#ifdef __APPLE__
    #include <sys/sysctl.h>
#endif

void* vm_malloc(size_t iSize, size_t iAlign)
{
#if defined _WIN32
    return _aligned_malloc(iSize, iAlign);
#elif defined __APPLE__
    if(iAlign <= 1)
        return malloc(iSize);
    else
    {
        void *pBuffer  = malloc(iSize + (iAlign - 1) + sizeof(void*));
        char *pABuffer = ((char*)pBuffer) + sizeof(void*);

        pABuffer += (iAlign - (((size_t)pABuffer) & (iAlign - 1)));

        ((void**)pABuffer)[-1] = pBuffer;
        return pABuffer;
    }
#else
    return memalign(iAlign, iSize);
#endif
}

void vm_free(void* pBuffer)
{
#if defined _WIN32
    _aligned_free(pBuffer);
#elif defined __APPLE__
    free(((void**)pBuffer)[-1]);
#else
    free(pBuffer);
#endif
}

/*
// Files operations
*/
#if defined unix || defined UNIX || defined __APPLE__
unsigned long long vm_file_fseek(FILE *fd, long long position, int mode)
{
#if defined ANDROID
    return fseek(fd, (size_t)position, mode);
#else
#if defined __APPLE__ || defined INTEL64
    return fseeko(fd, (off_t)position, mode);
#else
    return fseeko64(fd, (__off64_t)position, mode);
#endif
#endif
}

unsigned long long vm_file_ftell(FILE *fd)
{
#if defined ANDROID
    return (unsigned long long) ftell(fd);
#else
#if defined __APPLE__ || defined INTEL64
    return (unsigned long long)ftello(fd);
#else
    return (unsigned long long)ftello64(fd);
#endif
#endif
}
#endif

/*
// Sysinfo operations
*/
#ifdef _WIN32
unsigned int vm_sys_info_get_cpu_speed(void)
{
    return 0;
}

unsigned int vm_sys_info_get_avail_cpu_num(void)
{
    unsigned int rtval = 0, i;
    SYSTEM_INFO siSysInfo;

    ZeroMemory(&siSysInfo, sizeof(SYSTEM_INFO));
    GetSystemInfo(&siSysInfo);
    for(i = 0; i < 32; ++i )
    {
        if (siSysInfo.dwActiveProcessorMask & 1)
            ++rtval;
        siSysInfo.dwActiveProcessorMask >>= 1;
    }
    return rtval;
}
#else
#ifdef __APPLE__
#include <sys/types.h>
#include <sys/sysctl.h>

/*
 * retrieve information about integer parameter
 * defined as CTL_... (ctl_class) . ENTRY (ctl_entry)
 * return value in res.
 *  status : 1  - OK
 *           0 - operation failed
 */
static unsigned int osx_sysctl_entry_32u( int ctl_class, int ctl_entry, unsigned int *res )
{
    int dcb[2];
    size_t i;
    dcb[0] = ctl_class;  dcb[1] = ctl_entry;
    i = sizeof(res[0]);
    return (sysctl(dcb, 2, res, &i, NULL, 0) != -1) ? 1 : 0;
}
#endif

unsigned int vm_sys_info_get_cpu_speed(void)
{
#ifdef __APPLE__
    unsigned int freq;
    return (osx_sysctl_entry_32u(CTL_HW, HW_CPU_FREQ, &freq)) ? (unsigned int)(freq/1000000) : 1000;
#elif defined __FreeBSD__
    unsigned long long freq = 0;
    return (osx_sysctl_entry_32u(CTL_MACHDEP, 8, &freq)) ? (unsigned int)(freq/1000000) : 1000;
#else
    double ret = 0;
    FILE *pFile = NULL;
    vm_char buf[PATH_MAX];

    pFile = vm_file_fopen(VM_STRING("/proc/cpuinfo"), "r" );
    if (!pFile)
        return 1000;

    while ((vm_file_fgets(buf, PATH_MAX, pFile)))
    {
        if (!vm_string_strncmp(buf, VM_STRING("cpu MHz"), 7))
        {
            ret = vm_string_atol((vm_char *)(buf + 10));
            break;
        }
    }
    fclose(pFile);
    return ((unsigned int)ret);
#endif
}

unsigned int vm_sys_info_get_avail_cpu_num(void)
{
#ifdef __APPLE__
    unsigned int cpu_num;
    return (unsigned int)((osx_sysctl_entry_32u(CTL_HW, HW_AVAILCPU, &cpu_num)) ? cpu_num : 1);
#elif defined __FreeBSD__
    unsigned int cpu_num;
    return (unsigned int)((osx_sysctl_entry_32u(CTL_HW, HW_NCPU, &cpu_num)) ? cpu_num : 1);
#else
    return (unsigned int)sysconf(_SC_NPROCESSORS_ONLN);
#endif
}
#endif

/*
// Time operations
*/
#ifdef _WIN32
void vm_time_sleep(unsigned int msec)
{
    if (msec)
        Sleep(msec);
    else
        SwitchToThread();
}

vm_tick vm_time_get_tick(void)
{
    LARGE_INTEGER t1;

    QueryPerformanceCounter(&t1);
    return t1.QuadPart;
}

vm_tick vm_time_get_frequency(void)
{
    LARGE_INTEGER t1;

    QueryPerformanceFrequency(&t1);
    return t1.QuadPart;
}
#else
#include <sched.h>
#include <stdlib.h>

#define VM_TIME_MHZ 1000000

void vm_time_sleep(unsigned int msec)
{
    if (msec)
        usleep(1000 * msec);
    else
        sched_yield();
}

vm_tick vm_time_get_tick(void)
{
    unsigned int a,d;
    asm volatile("rdtsc" : "=a" (a), "=d" (d));
    return (vm_tick)a | (vm_tick)d << 32;
}

vm_tick vm_time_get_frequency(void)
{
    return vm_sys_info_get_cpu_speed()*VM_TIME_MHZ;
}
#endif


