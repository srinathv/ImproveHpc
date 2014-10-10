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

#if !defined(__MIC__)

#ifndef __VM_THREAD_H__
#define __VM_THREAD_H__

#include "vm_base.h"

#ifdef __cplusplus
extern "C" {
#endif

#if defined UNIX
#ifndef __USE_UNIX98
#define __USE_UNIX98
#endif
#include <pthread.h>
#endif

/*
// Mutexes operations
*/
#if defined _WIN32
typedef struct vm_mutex
{
    CRITICAL_SECTION sCritSection;
    int iInited;
} vm_mutex;
#else
typedef struct vm_mutex
{
    pthread_mutex_t handle;
    int is_valid;
} vm_mutex;
#endif

/* Invalidate a mutex */
void vm_mutex_set_invalid(vm_mutex *mutex);

/* Verify if a mutex is valid */
int  vm_mutex_is_valid(vm_mutex *mutex);

/* Init a mutex, return VM_OK if success */
vm_status vm_mutex_init(vm_mutex *mutex);

/* Lock the mutex with blocking. */
vm_status vm_mutex_lock(vm_mutex *mutex);

/* Unlock the mutex. */
vm_status vm_mutex_unlock(vm_mutex *mutex);

/* Lock the mutex without blocking, return VM_OK if success */
vm_status vm_mutex_try_lock(vm_mutex *mutex);

/* Destroy a mutex */
void vm_mutex_destroy(vm_mutex *mutex);

/*
// Events operations
*/
#if defined _WIN32
typedef struct vm_event
{
    HANDLE handle;
} vm_event;
#else
typedef struct vm_event
{
    pthread_cond_t cond;
    pthread_mutex_t mutex;
    int manual;
    int state;
} vm_event;
#endif

/* Invalidate an event */
void vm_event_set_invalid(vm_event *event);

/* Verify if an event is valid */
int vm_event_is_valid(vm_event *event);

/* Init an event. Event is created unset. return 1 if success */
vm_status vm_event_init(vm_event *event, int manual, int state);

/* Set the event to either HIGH (>0) or LOW (0) state */
vm_status vm_event_signal(vm_event *event);
vm_status vm_event_reset(vm_event *event);

/* Pulse the event 0 -> 1 -> 0 */
vm_status vm_event_pulse(vm_event *event);

/* Wait for event to be high with blocking */
vm_status vm_event_wait(vm_event *event);

/* Wait for event to be high without blocking, return 1 if signaled */
vm_status vm_event_timed_wait(vm_event *event, unsigned int msec);

/* Destroy the event */
void vm_event_destroy(vm_event *event);

/*
// Semaphores operations
*/
#if defined _WIN32
typedef struct vm_semaphore
{
    HANDLE handle;
} vm_semaphore;
#else
#include <semaphore.h>

typedef struct vm_semaphore
{
    pthread_cond_t cond;
    pthread_mutex_t mutex;
    int count;
} vm_semaphore;
#endif

/*
// Threads operations
*/
#if defined _WIN32
#define VM_THREAD_CALLCONVENTION __stdcall
#define vm_thread_handle HANDLE

typedef enum
{
    VM_THREAD_PRIORITY_LOWEST   = THREAD_PRIORITY_LOWEST,
    VM_THREAD_PRIORITY_LOW      = THREAD_PRIORITY_BELOW_NORMAL,
    VM_THREAD_PRIORITY_NORMAL   = THREAD_PRIORITY_NORMAL,
    VM_THREAD_PRIORITY_HIGH     = THREAD_PRIORITY_ABOVE_NORMAL,
    VM_THREAD_PRIORITY_HIGHEST  = THREAD_PRIORITY_HIGHEST
} vm_thread_priority;

typedef struct vm_thread
{
    HANDLE handle;
    vm_mutex access_mut;
    int i_wait_count;

    unsigned int (__stdcall * protected_func)(void *);
    void *protected_arg;
    DWORD preset_affinity_mask;
//    int selected_cpu;
} vm_thread;
#else
#define VM_THREAD_CALLCONVENTION
#define vm_thread_handle pthread_t

typedef enum
{
    VM_THREAD_PRIORITY_HIGHEST,
    VM_THREAD_PRIORITY_HIGH,
    VM_THREAD_PRIORITY_NORMAL,
    VM_THREAD_PRIORITY_LOW,
    VM_THREAD_PRIORITY_LOWEST
} vm_thread_priority;

typedef struct vm_thread
{
    pthread_t handle;
    int is_valid;
    unsigned int (*p_thread_func)(void *);
    void *p_arg;
    vm_event exit_event;
    vm_mutex access_mut;
    int i_wait_count;
//    int selected_cpu;
} vm_thread;
#endif

/* IMPORTANT:
 *    The thread return value is always saved for the calling thread to
 *    collect. To avoid memory leak, always vm_thread_wait() for the
 *    child thread to terminate in the calling thread.
 */
typedef unsigned int (VM_THREAD_CALLCONVENTION * vm_thread_callback)(void *);

/* Set the thread handler to an invalid value */
void vm_thread_set_invalid(vm_thread *thread);

/* Verify if the thread handler is valid */
int vm_thread_is_valid(vm_thread *thread);

/* Create a thread. The thread is set to run at the call. Return 1 if successful */
int vm_thread_create(vm_thread *thread, vm_thread_callback func, void *arg);

/* Wait until a thread exists */
void vm_thread_wait(vm_thread *thread);

/* Set thread priority. Return 1 if successful */
int  vm_thread_set_priority(vm_thread *thread, vm_thread_priority priority);

/* Close thread after all */
void vm_thread_close(vm_thread *thread);

/* Get current thread priority */
vm_thread_priority vm_get_current_thread_priority(void);

/* Set current thread priority */
void vm_set_current_thread_priority(vm_thread_priority priority);

unsigned int vm_sys_info_get_avail_cpu_num(void);

#ifdef __cplusplus
}
#endif

#endif

#endif  // !defined(USE_MIC)
