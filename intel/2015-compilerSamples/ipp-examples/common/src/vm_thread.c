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

#if !defined(WINDOWS)
    #include <pthread.h>
#endif

#include "vm_thread.h"

/*
// Threads operations
*/
#if defined _WIN32
    #if !defined _WIN32_WCE
    #include <process.h>
#endif

#ifdef VM_THREAD_CATCHCRASH
static unsigned int __stdcall vm_thread_exception_reaction(void* p)
{
    p;
    return 0;
}
#endif

/* set the thread handler an invalid value */
void vm_thread_set_invalid(vm_thread *thread)
{
    /* check error(s) */
    if (NULL == thread)
        return;

    thread->handle = NULL;
    thread->i_wait_count = 0;
    vm_mutex_set_invalid(&(thread->access_mut));

#ifdef VM_THREAD_CATCHCRASH
    thread->protected_exception_reaction = vm_thread_exception_reaction;
#endif
}

/* verify if the thread handler is valid */
int vm_thread_is_valid(vm_thread *thread)
{
    int iRes = 1;

    /* check error(s) */
    if (NULL == thread)
        return 0;

    vm_mutex_lock(&thread->access_mut);
    if (NULL != thread->handle)
    {
        if (WAIT_OBJECT_0 == WaitForSingleObject(thread->handle, 0))
        {
            iRes = 0;
            if (0 == thread->i_wait_count)
            {
                CloseHandle(thread->handle);
                thread->handle = NULL;
            }
        }
    }

    if (NULL == thread->handle)
        iRes = 0;

    vm_mutex_unlock(&(thread->access_mut));
    return iRes;
}

#ifdef VM_THREAD_CATCHCRASH
static unsigned int __stdcall vm_thread_protectedfunc(void* params)
{
    vm_thread* thread = (vm_thread*)params;
    if (NULL == thread)
        return 0;

    __try
    {
        thread->protected_func(thread->protected_arg);
    }
    __except(EXCEPTION_EXECUTE_HANDLER)
    {
        thread->protected_exception_reaction(thread->protected_arg);
    };
    return 0;
}
#endif

/* create a thread. return NULL if failed to create the thread */
int vm_thread_create(vm_thread *thread, vm_thread_callback func, void *arg)
{
    int i_res = 1;

    /* check error(s) */
    if ((NULL == thread) || (NULL == func))
        return 0;

#ifdef VM_THREAD_CATCHCRASH
    thread->protected_func = func;
    thread->protected_arg  = arg;
#endif

    if(!vm_mutex_is_valid(&thread->access_mut))
    {
        if(VM_OK != vm_mutex_init(&thread->access_mut))
            i_res = 0;
    }

    if (i_res)
    {
        if (NULL != thread->handle)
            vm_thread_wait(thread);

        vm_mutex_lock(&thread->access_mut);

        thread->handle = (HANDLE)
#if defined _WIN32_WCE
        CreateThread(0, 0, (LPTHREAD_START_ROUTINE) func, arg, 0, 0);
#else
#if defined VM_THREAD_CATCHCRASH
        _beginthreadex(0, 0, &vm_thread_protectedfunc, thread, 0, 0);
#else
        _beginthreadex(0, 0, (unsigned int (__stdcall *)(void*))func, arg, 0, 0);
#endif
#endif
        i_res = (int) ((thread->handle) ? (1) : (0));
        vm_mutex_unlock(&thread->access_mut);
    }

    thread->preset_affinity_mask = 0;

    return i_res;
}

/* set thread priority. return 1 if success */
int vm_thread_set_priority(vm_thread *thread, vm_thread_priority priority)
{
    int i_res = 0;

    /* check error(s) */
    if (NULL == thread)
        return 0;

    if (NULL != thread->handle)
    {
        vm_mutex_lock(&thread->access_mut);
        i_res = SetThreadPriority(thread->handle,priority);
        vm_mutex_unlock(&thread->access_mut);
    }
    return i_res;
}

/* wait until a thread exists */
void vm_thread_wait(vm_thread *thread)
{
    /* check error(s) */
    if (NULL == thread)
        return;

    vm_mutex_lock(&thread->access_mut);
    if (thread->handle)
    {
        thread->i_wait_count++;
        vm_mutex_unlock(&thread->access_mut);

        WaitForSingleObject(thread->handle, INFINITE);

        vm_mutex_lock(&thread->access_mut);
        thread->i_wait_count--;
        if (0 == thread->i_wait_count)
        {
            CloseHandle(thread->handle);
            thread->handle = NULL;
        }
    }
    vm_mutex_unlock(&thread->access_mut);
}

/* close thread after all */
void vm_thread_close(vm_thread *thread)
{
    /* check error(s) */
    if (NULL == thread)
        return;

    vm_thread_wait(thread);
    vm_mutex_destroy(&thread->access_mut);
}

vm_thread_priority vm_get_current_thread_priority()
{
    return (vm_thread_priority)GetThreadPriority(GetCurrentThread());
}

void vm_set_current_thread_priority(vm_thread_priority priority)
{
    SetThreadPriority(GetCurrentThread(), (int)priority);
}

#else
#include <unistd.h>
#include <sys/time.h>
#include <sched.h>


static void *vm_thread_proc(void *pv_params)
{
    vm_thread *p_thread = (vm_thread *) pv_params;

    /* check error(s) */
    if (NULL == pv_params)
        return ((void *) -1);

    p_thread->p_thread_func(p_thread->p_arg);
    vm_event_signal(&p_thread->exit_event);

    return ((void *) 1);
}

/* set the thread handler an invalid value */
void vm_thread_set_invalid(vm_thread *thread)
{
    /* check error(s) */
    if (NULL == thread)
        return;

    thread->is_valid = 0;
    thread->i_wait_count = 0;
    vm_event_set_invalid(&thread->exit_event);
    vm_mutex_set_invalid(&thread->access_mut);
}

/* verify if the thread handler is valid */
int vm_thread_is_valid(vm_thread *thread)
{
    /* check error(s) */
    if (NULL == thread)
        return 0;

    if (thread->is_valid)
    {
        vm_mutex_lock(&thread->access_mut);
        if (VM_OK == vm_event_timed_wait(&thread->exit_event, 0))
        {
            vm_mutex_unlock(&thread->access_mut);
            vm_thread_wait(thread);
        }
        else
            vm_mutex_unlock(&thread->access_mut);
    }
    return thread->is_valid;
}

/* create a thread. return 1 if success */
int vm_thread_create(vm_thread *thread, unsigned int (*vm_thread_func)(void *), void *arg)
{
    int i_res = 1;
    pthread_attr_t attr;

    /* check error(s) */
    if ((NULL == thread) ||
        (NULL == vm_thread_func))
        return 0;

    if (0 != i_res)
    {
        if (VM_OK != vm_event_init(&thread->exit_event, 1, 0))
            i_res = 0;
    }

    if ((0 != i_res) &&
        (VM_OK != vm_mutex_init(&thread->access_mut)))
        i_res = 0;

    if (0 != i_res)
    {
        vm_mutex_lock(&thread->access_mut);
        thread->p_thread_func = vm_thread_func;
        thread->p_arg = arg;
        pthread_attr_init(&attr);
        pthread_attr_setschedpolicy(&attr, geteuid() ? SCHED_OTHER : SCHED_RR);

        thread->is_valid =! pthread_create(&thread->handle,
                                           &attr,
                                           vm_thread_proc,
                                           (void*)thread);
        i_res = (thread->is_valid) ? 1 : 0;
        vm_mutex_unlock(&thread->access_mut);
        pthread_attr_destroy(&attr);
    }
    vm_thread_set_priority(thread, VM_THREAD_PRIORITY_LOWEST);
    return i_res;
}

/* set thread priority, return 1 if successful */
int vm_thread_set_priority(vm_thread *thread, vm_thread_priority priority)
{
    int i_res = 1;
    int policy, pmin, pmax, pmean;
    struct sched_param param;

    /* check error(s) */
    if (NULL == thread)
        return 0;

    if (thread->is_valid)
    {
        vm_mutex_lock(&thread->access_mut);
        pthread_getschedparam(thread->handle,&policy,&param);

        pmin = sched_get_priority_min(policy);
        pmax = sched_get_priority_max(policy);
        pmean = (pmin + pmax) / 2;

        switch (priority)
        {
        case VM_THREAD_PRIORITY_HIGHEST:
            param.sched_priority = pmax;
            break;

        case VM_THREAD_PRIORITY_LOWEST:
            param.sched_priority = pmin;
            break;

        case VM_THREAD_PRIORITY_NORMAL:
            param.sched_priority = pmean;
            break;

        case VM_THREAD_PRIORITY_HIGH:
            param.sched_priority = (pmax + pmean) / 2;
            break;

        case VM_THREAD_PRIORITY_LOW:
            param.sched_priority = (pmin + pmean) / 2;
            break;

        default:
            i_res = 0;
            break;
        }

        if (i_res)
            i_res = !pthread_setschedparam(thread->handle, policy, &param);
        vm_mutex_unlock(&thread->access_mut);
    }
    return i_res;
}

/* wait until a thread exists */
void vm_thread_wait(vm_thread *thread)
{
    /* check error(s) */
    if (NULL == thread)
        return;

    if (thread->is_valid)
    {
        vm_mutex_lock(&thread->access_mut);
        thread->i_wait_count++;
        vm_mutex_unlock(&thread->access_mut);

        vm_event_wait(&thread->exit_event);

        vm_mutex_lock(&thread->access_mut);
        thread->i_wait_count--;
        if (0 == thread->i_wait_count)
        {
            pthread_join(thread->handle, NULL);
            thread->is_valid = 0;
        }
        vm_mutex_unlock(&thread->access_mut);
    }
}

/* close thread after all */
void vm_thread_close(vm_thread *thread)
{
    /* check error(s) */
    if (NULL == thread)
        return;

    vm_thread_wait(thread);
    vm_event_destroy(&thread->exit_event);
    vm_mutex_destroy(&thread->access_mut);
}

vm_thread_priority vm_get_current_thread_priority()
{
    return VM_THREAD_PRIORITY_NORMAL;
}

void vm_set_current_thread_priority(vm_thread_priority priority)
{
    priority = priority;
}
#endif

/*
// Mutexes operations
*/
#if defined _WIN32
/* Invalidate a mutex */
void vm_mutex_set_invalid(vm_mutex *mutex)
{
    /* check error(s) */
    if (NULL == mutex)
        return;

    memset(&mutex->sCritSection, 0, sizeof(CRITICAL_SECTION));
    mutex->iInited = 0;
}

/* Verify if a mutex is valid */
int vm_mutex_is_valid(vm_mutex *mutex)
{
    /* check error(s) */
    if (NULL == mutex)
        return 0;

    return mutex->iInited;
}

/* Init a mutex, return 1 if successful */
vm_status vm_mutex_init(vm_mutex *mutex)
{
    /* check error(s) */
    if (NULL == mutex)
        return VM_NULL_PTR;

    vm_mutex_destroy(mutex);
    InitializeCriticalSection(&mutex->sCritSection);
    mutex->iInited = 1;
    return VM_OK;
}

/* Lock the mutex with blocking. */
vm_status vm_mutex_lock(vm_mutex *mutex)
{
    vm_status umcRes = VM_OK;

    /* check error(s) */
    if (NULL == mutex)
        return VM_NULL_PTR;

    if (mutex->iInited)
    {
        __try
        {
            EnterCriticalSection(&mutex->sCritSection);
        }
        __except ((GetExceptionCode() == STATUS_INVALID_HANDLE) ?  EXCEPTION_EXECUTE_HANDLER : EXCEPTION_CONTINUE_SEARCH )
        {
            umcRes = VM_OPERATION_FAILED;
        }
    }
    else
        umcRes = VM_NOT_INITIALIZED;

    return umcRes;
}

/* Unlock the mutex. */
vm_status vm_mutex_unlock(vm_mutex *mutex)
{
    /* check error(s) */
    if (NULL == mutex)
        return VM_NULL_PTR;

    if (mutex->iInited)
    {
        LeaveCriticalSection(&mutex->sCritSection);
        return VM_OK;
    }
    else
        return VM_NOT_INITIALIZED;
}

/* Lock the mutex without blocking, return 1 if success */
vm_status vm_mutex_try_lock(vm_mutex *mutex)
{
    /* check error(s) */
    if (NULL == mutex)
        return VM_NULL_PTR;

    if (mutex->iInited)
    {
        if (TryEnterCriticalSection(&mutex->sCritSection))
            return VM_OK;
        else
            return VM_OPERATION_FAILED;
    }
    else
        return VM_NOT_INITIALIZED;
}

/* Destroy the mutex */
void vm_mutex_destroy(vm_mutex *mutex)
{
    /* check error(s) */
    if (NULL == mutex)
        return;

    if (mutex->iInited)
    {
        mutex->iInited = 0;
        DeleteCriticalSection(&mutex->sCritSection);
    }
}
#else
/* Invalidate a mutex */
void vm_mutex_set_invalid(vm_mutex *mutex)
{
    /* check error(s) */
    if (NULL == mutex)
        return;

    mutex->is_valid = 0;
}

/* Verify if a mutex is valid */
int vm_mutex_is_valid(vm_mutex *mutex)
{
    /* check error(s) */
    if (NULL == mutex)
        return 0;

    return mutex->is_valid;
}

/* Init a mutex, return 1 if success */
vm_status vm_mutex_init(vm_mutex *mutex)
{
    vm_status umc_res;
    pthread_mutexattr_t mutex_attr;

    /* check error(s) */
    if (NULL == mutex)
        return VM_NULL_PTR;

    vm_mutex_destroy(mutex);
    pthread_mutexattr_init(&mutex_attr);
    pthread_mutexattr_settype(&mutex_attr, PTHREAD_MUTEX_RECURSIVE);
    mutex->is_valid = !pthread_mutex_init(&mutex->handle, &mutex_attr);
    umc_res = mutex->is_valid ? VM_OK : VM_OPERATION_FAILED;
    pthread_mutexattr_destroy(&mutex_attr);
    return umc_res;
}

/* Lock the mutex with blocking. */
vm_status vm_mutex_lock(vm_mutex *mutex)
{
    vm_status umc_res = VM_NOT_INITIALIZED;

    /* check error(s) */
    if (NULL == mutex)
        return VM_NULL_PTR;

    if (mutex->is_valid)
    {
        if (0 == pthread_mutex_lock(&mutex->handle))
            umc_res = VM_OK;
        else
            umc_res = VM_OPERATION_FAILED;
    }
    return umc_res;
}

/* Unlock the mutex. */
vm_status vm_mutex_unlock(vm_mutex *mutex)
{
    vm_status umc_res = VM_NOT_INITIALIZED;

    /* check error(s) */
    if (NULL == mutex)
        return VM_NULL_PTR;

    if (mutex->is_valid)
    {
        if (0 == pthread_mutex_unlock(&mutex->handle))
            umc_res = VM_OK;
        else
            umc_res = VM_OPERATION_FAILED;
    }
    return umc_res;
}

/* Lock the mutex without blocking, return 1 if success */
vm_status vm_mutex_try_lock(vm_mutex *mutex)
{
    vm_status umc_res = VM_NOT_INITIALIZED;

    /* check error(s) */
    if (NULL == mutex)
        return VM_NULL_PTR;

    if (mutex->is_valid)
    {
        int i_res = pthread_mutex_trylock(&mutex->handle);
        switch (i_res)
        {
        case 0:
            umc_res = VM_OK;
            break;

        case EBUSY:
            umc_res = VM_TIMEOUT;
            break;

        default:
            umc_res = VM_OPERATION_FAILED;
            break;
        }
    }
    return umc_res;
}

/* Destroy the mutex */
void vm_mutex_destroy(vm_mutex *mutex)
{
    /* check error(s) */
    if (NULL == mutex)
        return;

    if (mutex->is_valid)
    {
        pthread_mutex_destroy(&mutex->handle);
        mutex->is_valid = 0;
    }
}
#endif

/*
// Events operations
*/
#if defined _WIN32
/* Invalidate an event */
void vm_event_set_invalid(vm_event *event)
{
    /* check error(s) */
    if (NULL == event)
        return;

    event->handle = NULL;
}

/* Verify if the event handle is valid */
int vm_event_is_valid(vm_event *event)
{
    /* check error(s) */
    if (NULL == event)
        return 0;

    return (int)(NULL != event->handle);
}

/* Init an event. Event is created unset. return 1 if success */
vm_status vm_event_init(vm_event *event, int manual, int state)
{
    /* check error(s) */
    if (NULL == event)
        return VM_NULL_PTR;

    vm_event_destroy(event);
    event->handle = CreateEvent(NULL, manual, state, NULL);

    if (NULL == event->handle)
        return VM_OPERATION_FAILED;
    else
        return VM_OK;
}

/* Set the event to either HIGH (1) or LOW (0) state */
vm_status vm_event_signal(vm_event *event)
{
    /* check error(s) */
    if (NULL == event)
        return VM_NULL_PTR;

    if (NULL == event->handle)
        return VM_NOT_INITIALIZED;
    else if (SetEvent(event->handle))
        return VM_OK;
    else
        return VM_OPERATION_FAILED;
}

/* Set the event to either HIGH (1) or LOW (0) state */
vm_status vm_event_reset(vm_event *event)
{
    /* check error(s) */
    if (NULL == event)
        return VM_NULL_PTR;

    if (NULL == event->handle)
        return VM_NOT_INITIALIZED;
    else if (ResetEvent(event->handle))
        return VM_OK;
    else
        return VM_OPERATION_FAILED;
}

/* Pulse the event 0 -> 1 -> 0 */
vm_status vm_event_pulse(vm_event *event)
{
    /* check error(s) */
    if (NULL == event)
        return VM_NULL_PTR;

    if (NULL == event->handle)
        return VM_NOT_INITIALIZED;
    else if (PulseEvent(event->handle))
        return VM_OK;
    else
        return VM_OPERATION_FAILED;
}

/* Wait for event to be high with blocking */
vm_status vm_event_wait(vm_event *event)
{
    /* check error(s) */
    if (NULL == event)
        return VM_NULL_PTR;

    if (NULL == event->handle)
        return VM_NOT_INITIALIZED;
    else if (WAIT_OBJECT_0 == WaitForSingleObject(event->handle, INFINITE))
        return VM_OK;
    else
        return VM_OPERATION_FAILED;
}

/* Wait for event to be high without blocking, return 1 if signaled */
vm_status vm_event_timed_wait(vm_event *event, unsigned int msec)
{
    vm_status umcRes = VM_NOT_INITIALIZED;

    /* check error(s) */
    if (NULL == event)
        return VM_NULL_PTR;

    if (NULL != event->handle)
    {
        unsigned int dwRes = WaitForSingleObject(event->handle, msec);

        if (WAIT_OBJECT_0 == dwRes)
            umcRes = VM_OK;
        else if (WAIT_TIMEOUT == dwRes)
            umcRes = VM_TIMEOUT;
        else
            umcRes = VM_OPERATION_FAILED;
    }
    return umcRes;
}

/* Destroy the event */
void vm_event_destroy(vm_event *event)
{
    /* check error(s) */
    if (NULL == event)
        return;

    if (event->handle)
    {
        CloseHandle(event->handle);
        event->handle = NULL;
    }
}
#else
/* Invalidate an event */
void vm_event_set_invalid(vm_event *event)
{
    /* check error(s) */
    if (NULL == event)
        return;

    event->state= -1;
}

/* Verify if an event is valid */
int vm_event_is_valid(vm_event *event)
{
    /* check error(s) */
    if (NULL == event)
        return 0;

    return event->state >= 0;
}

/* Init an event. Event is created unset. return 1 if success */
vm_status vm_event_init(vm_event *event, int manual, int state)
{
    /* check error(s) */
    if(NULL == event)
        return VM_NULL_PTR;

    vm_event_destroy(event);

    event->manual = manual;
    event->state = state ? 1 : 0;
    pthread_cond_init(&event->cond, 0);
    pthread_mutex_init(&event->mutex, 0);

    return VM_OK;
}

/* Set the event to either HIGH (1) or LOW (0) state */
vm_status vm_event_signal(vm_event *event)
{
    vm_status umc_status = VM_NOT_INITIALIZED;

    /* check error(s) */
    if (NULL == event)
        return VM_NULL_PTR;

    if (0 <= event->state)
    {
        pthread_mutex_lock(&event->mutex);
        if (0 == event->state)
        {
            event->state = 1;
            if (event->manual)
                pthread_cond_broadcast(&event->cond);
            else
                pthread_cond_signal(&event->cond);
        }
        umc_status = VM_OK;
        pthread_mutex_unlock(&event->mutex);
    }
    return umc_status;
}

vm_status vm_event_reset(vm_event *event)
{
    vm_status umc_status = VM_NOT_INITIALIZED;

    /* check error(s) */
    if (NULL == event)
        return VM_NULL_PTR;

    if (0 <= event->state)
    {
        pthread_mutex_lock(&event->mutex);

        if (1 == event->state)
            event->state = 0;

        pthread_mutex_unlock(&event->mutex);
        umc_status = VM_OK;
    }
    return umc_status;
}

/* Pulse the event 0 -> 1 -> 0 */
vm_status vm_event_pulse(vm_event *event)
{
    vm_status umc_status = VM_NOT_INITIALIZED;

    /* check error(s) */
    if (NULL == event)
        return VM_NULL_PTR;

    if (0 <= event->state)
    {
        pthread_mutex_lock(&event->mutex);

        if (event->manual)
            pthread_cond_broadcast(&event->cond);
        else
            pthread_cond_signal(&event->cond);

        event->state = 0;
        pthread_mutex_unlock(&event->mutex);
        umc_status = VM_OK;
    }
    return umc_status;
}

/* Wait for event to be high with blocking */
vm_status vm_event_wait(vm_event *event)
{
    vm_status umc_status = VM_NOT_INITIALIZED;

    /* check error(s) */
    if (NULL == event)
        return VM_NULL_PTR;

    if (0 <= event->state)
    {
        pthread_mutex_lock(&event->mutex);

        if (!event->state)
            pthread_cond_wait(&event->cond,&event->mutex);

        if (!event->manual)
            event->state = 0;

        pthread_mutex_unlock(&event->mutex);
        umc_status = VM_OK;
    }
    return umc_status;
}

/* Wait for event to be high without blocking, return 1 if successful */
vm_status vm_event_timed_wait(vm_event *event, unsigned int msec)
{
    vm_status umc_status = VM_NOT_INITIALIZED;

    /* check error(s) */
    if (NULL == event)
        return VM_NULL_PTR;

    if (0 <= event->state)
    {
        pthread_mutex_lock(&event->mutex);

        if (0 == event->state)
        {
            struct timeval tval;
            struct timespec tspec;
            int i_res;

            gettimeofday(&tval, NULL);
            msec = 1000 * msec + tval.tv_usec;
            tspec.tv_sec = tval.tv_sec + msec / 1000000;
            tspec.tv_nsec = (msec % 1000000) * 1000;
            i_res = pthread_cond_timedwait(&event->cond,
                                           &event->mutex,
                                           &tspec);
            if (0 == i_res)
                umc_status = VM_OK;
            else if (ETIMEDOUT == i_res)
                umc_status = VM_TIMEOUT;
            else
                umc_status = VM_OPERATION_FAILED;
        }
        else
            umc_status = VM_OK;

        if (!event->manual)
            event->state = 0;

        pthread_mutex_unlock(&event->mutex);
    }
    return umc_status;
}

/* Destory the event */
void vm_event_destroy(vm_event *event)
{
    /* check error(s) */
    if (NULL == event)
        return;

    if (event->state >= 0)
    {
        pthread_cond_destroy(&event->cond);
        pthread_mutex_destroy(&event->mutex);
        event->state= -1;
    }
}
#endif

#endif      // !defined(__MIC__)
