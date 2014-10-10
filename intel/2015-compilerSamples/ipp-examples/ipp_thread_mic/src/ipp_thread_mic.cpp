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

#if defined(__MIC__)
#pragma offload_attribute(push, target(mic))
#include <ipp.h>
#pragma offload_attribute(pop)
#endif

#include <memory>

#include "base.h"
#include "base_image.h"
#include "base_ipp.h"

#define MASK_SIZE 7
#define MIN_BLOCK_HEIGHT (MASK_SIZE + 1)

#define VM_THREADS 32

#define ALLOC   alloc_if(1)
#define FREE    free_if(1)
#define RETAIN  free_if(0)
#define REUSE   alloc_if(0)

enum ParallelMethod {
    PAR_SEQ,
    PAR_VM
};

#if defined(__MIC__)
#include <pthread.h>        // Required for the functions below

// VM MIC-specific definitions
typedef struct vm_event
{
    pthread_cond_t cond;
    pthread_mutex_t mutex;
    int manual;
    int state;
} vm_event;

#define VM_THREAD_CALLCONVENTION
#define vm_thread_handle pthread_t

typedef struct vm_mutex
{
    pthread_mutex_t handle;
    int is_valid;
} vm_mutex;

typedef struct vm_thread
{
    pthread_t handle;
    int is_valid;
    unsigned int (*p_thread_func)(void *);
    void *p_arg;
    vm_event exit_event;
    vm_mutex access_mut;
    int i_wait_count;
} vm_thread;

// VM functions (Unix clone)
/* Set the event to either HIGH (1) or LOW (0) state */
__declspec(target(mic)) vm_status vm_event_signal(vm_event *event)
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

/* Lock the mutex with blocking. */
__declspec(target(mic)) vm_status vm_mutex_lock(vm_mutex *mutex)
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
__declspec(target(mic)) vm_status vm_mutex_unlock(vm_mutex *mutex)
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

/* Wait for event to be high with blocking */
__declspec(target(mic)) vm_status vm_event_wait(vm_event *event)
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

/* wait until a thread exists */
__declspec(target(mic)) void vm_thread_wait(vm_thread *thread)
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

/* Destroy the event */
__declspec(target(mic)) void vm_event_destroy(vm_event *event)
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

/* Destroy the mutex */
__declspec(target(mic)) void vm_mutex_destroy(vm_mutex *mutex)
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

/* close thread after all */
__declspec(target(mic)) void vm_thread_close(vm_thread *thread)
{
    /* check error(s) */
    if (NULL == thread)
        return;

    vm_thread_wait(thread);
    vm_event_destroy(&thread->exit_event);
    vm_mutex_destroy(&thread->access_mut);
}

/* Invalidate an event */
__declspec(target(mic)) void vm_event_set_invalid(vm_event *event)
{
    /* check error(s) */
    if (NULL == event)
        return;

    event->state= -1;
}

/* Invalidate a mutex */
__declspec(target(mic)) void vm_mutex_set_invalid(vm_mutex *mutex)
{
    /* check error(s) */
    if (NULL == mutex)
        return;

    mutex->is_valid = 0;
}

/* set the thread handler an invalid value */
__declspec(target(mic)) void vm_thread_set_invalid(vm_thread *thread)
{
    /* check error(s) */
    if (NULL == thread)
        return;

    thread->is_valid = 0;
    thread->i_wait_count = 0;
    vm_event_set_invalid(&thread->exit_event);
    vm_mutex_set_invalid(&thread->access_mut);
}

/* Init an event. Event is created unset. return 1 if success */
__declspec(target(mic)) vm_status vm_event_init(vm_event *event, int manual, int state)
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

/* Init a mutex, return 1 if success */
__declspec(target(mic)) vm_status vm_mutex_init(vm_mutex *mutex)
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

__declspec(target(mic)) static void *vm_thread_proc(void *pv_params)
{
    vm_thread *p_thread = (vm_thread *) pv_params;

    /* check error(s) */
    if (NULL == pv_params)
        return ((void *) -1);

    p_thread->p_thread_func(p_thread->p_arg);
    vm_event_signal(&p_thread->exit_event);

    return ((void *) 1);
}

typedef enum
{
    VM_THREAD_PRIORITY_HIGHEST,
    VM_THREAD_PRIORITY_HIGH,
    VM_THREAD_PRIORITY_NORMAL,
    VM_THREAD_PRIORITY_LOW,
    VM_THREAD_PRIORITY_LOWEST
} vm_thread_priority;

/* set thread priority, return 1 if successful */
__declspec(target(mic)) int vm_thread_set_priority(vm_thread *thread, vm_thread_priority priority)
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

/* create a thread. return 1 if success */
__declspec(target(mic)) int vm_thread_create(vm_thread *thread, unsigned int (*vm_thread_func)(void *), void *arg)
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


#pragma offload_attribute(push, target(mic))
class Harmonize
{
public:
    Harmonize()
    {
        m_iThreads = 0;

        m_filterBoxMask.width  = MASK_SIZE;
        m_filterBoxMask.height = MASK_SIZE;
        m_filterBoxAnchor.x = 3;
        m_filterBoxAnchor.y = 3;

        m_iMulVal1[0] = m_iMulVal1[1] = m_iMulVal1[2] = m_iMulVal1[3] = 255;
        m_iMulVal2[0] = m_iMulVal2[1] = m_iMulVal2[2] = m_iMulVal2[3] = 205;
        m_iThresLTVal[0] = m_iThresLTVal[1] = m_iThresLTVal[2] = m_iThresLTVal[3] = 6;
        m_iThresGTVal[0] = m_iThresGTVal[1] = m_iThresGTVal[2] = m_iThresGTVal[3] = 250;
    }

    virtual ~Harmonize()
    {
        Close();
    }

    virtual void Close()
    {
        if(m_pTmpImage)
            ippFree(m_pTmpImage);
    }

    virtual int Init(unsigned int width, unsigned int height, unsigned int step, unsigned int samples)
    {
        if(samples != 1 && samples != 3 && samples != 4 || step == 0)
            return STS_ERR_INVALID_PARAMS;

        m_iWidth = width;
        m_iHeight = height;
        m_iSamples = samples;
        m_iStep = step;
        // Allocate temporary image
        switch(m_iSamples) {
        case 1:
            m_pTmpImage = ippiMalloc_8u_C1(m_iWidth, m_iHeight, &m_iTmpStep);
            break;
        case 3:
            m_pTmpImage = ippiMalloc_8u_C3(m_iWidth, m_iHeight, &m_iTmpStep);
            break;
        case 4:
            m_pTmpImage = ippiMalloc_8u_C4(m_iWidth, m_iHeight, &m_iTmpStep);
            break;
        }
        if(m_pTmpImage == NULL)
            return STS_ERR_ALLOC;
        return STS_OK;
    }

    virtual int SetImages(Ipp8u* pSrcImage, Ipp8u* pDstImage)
    {
        m_pSrcImage = pSrcImage;
        m_pDstImage = pDstImage;
        return STS_OK;
    }

    //
    // HarmonizeBlock applies complex filter to image slices.
    // Arguments:
    //      pSrcPtr - address of source image slice
    //      pDstPtr - address of destination image slice
    //      pTmpPtr - address of temporary image slice
    //      dstRoiSize - slice size (W x H)
    //      border  - type of box filter border
    //      pExtBuffer (optional) - buffer to be used in operations
    int HarmonizeBlock(Ipp8u* pSrcPtr, Ipp8u* pDstPtr, Ipp8u* pTmpPtr, IppiSize dstRoiSize, IppiBorderType border, unsigned char *pExtBuffer = 0)
    {
        int      status;
        IppStatus   ippSts;

        unsigned char iBorderValue = 0;

        unsigned char *pBuffer = 0;
        int            iBufferSize = 0;

        if(!pSrcPtr|| !pDstPtr)
            return STS_ERR_NULL_PTR;

        // Zero size mean full size
        if(!dstRoiSize.width)
            dstRoiSize.width = m_iWidth;
        if(!dstRoiSize.height)
            dstRoiSize.height = m_iHeight;
        if(!pExtBuffer)
        {
            ippSts = ippiFilterBoxBorderGetBufferSize(dstRoiSize, m_filterBoxMask, ipp8u, m_iSamples, &iBufferSize);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiFilterBoxBorderGetBufferSize()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            pBuffer = ippsMalloc_8u(iBufferSize);
            if(!pBuffer)
            {
                PRINT_MESSAGE("Cannot allocate memory for FilterBox buffer");
                return STS_ERR_ALLOC;
            }
        }
        else
            pBuffer = pExtBuffer;

        if(m_iSamples == 1)
        {
            // Apply box filter
            ippSts = ippiFilterBoxBorder_8u_C1R(pSrcPtr, m_iStep, pTmpPtr, m_iTmpStep, dstRoiSize, m_filterBoxMask, border, &iBorderValue, pBuffer);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiFilterBoxBorder_8u_C1R()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Multiply by a constant in-place
            ippSts = ippiMulC_8u_C1RSfs(pTmpPtr, m_iTmpStep, m_iMulVal1[0], pTmpPtr, m_iTmpStep, dstRoiSize, 12);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiMulC_8u_C1RSfs()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Subtract
            ippSts = ippiSub_8u_C1RSfs(pTmpPtr, m_iTmpStep, pSrcPtr, m_iStep, pDstPtr, m_iStep, dstRoiSize, 0);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiSub_8u_C1RSfs()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Multiply by a constant in-place
            ippSts = ippiMulC_8u_C1RSfs(pDstPtr, m_iStep, m_iMulVal2[0], pDstPtr, m_iStep, dstRoiSize, 8);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiMulC_8u_C1RSfs()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Apply threshold
            ippSts = ippiThreshold_LTValGTVal_8u_C1R(pDstPtr, m_iStep, pDstPtr, m_iStep, dstRoiSize, m_iThresLTVal[0], m_iThresLTVal[0], m_iThresGTVal[0], m_iThresGTVal[0]);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiThreshold_LTValGTVal_8u_C1R()", ippGetStatusString(ippSts), return STS_ERR_FAILED);
        }
        else if(m_iSamples == 3)
        {
            // Apply box filter
            ippSts = ippiFilterBoxBorder_8u_C3R(pSrcPtr, m_iStep, pTmpPtr, m_iTmpStep, dstRoiSize, m_filterBoxMask, border, &iBorderValue, pBuffer);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiFilterBoxBorder_8u_C3R()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Multiply by a constant in-place
            ippSts = ippiMulC_8u_C3RSfs(pTmpPtr, m_iTmpStep, m_iMulVal1, pTmpPtr, m_iTmpStep, dstRoiSize, 12);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiMulC_8u_C3RSfs()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Subtract
            ippSts = ippiSub_8u_C3RSfs(pTmpPtr, m_iTmpStep, pSrcPtr, m_iStep, pDstPtr, m_iStep, dstRoiSize, 0);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiSub_8u_C3RSfs()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Multiply by a constant in-place
            ippSts = ippiMulC_8u_C3RSfs(pDstPtr, m_iStep, m_iMulVal2, pDstPtr, m_iStep, dstRoiSize, 8);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiMulC_8u_C3RSfs()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Apply threshold
            ippSts = ippiThreshold_LTValGTVal_8u_C3R(pDstPtr, m_iStep, pDstPtr, m_iStep, dstRoiSize, m_iThresLTVal, m_iThresLTVal, m_iThresGTVal, m_iThresGTVal);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiThreshold_LTValGTVal_8u_C3R()", ippGetStatusString(ippSts), return STS_ERR_FAILED);
        }
        else if(m_iSamples == 4)
        {
            // Apply box filter
            ippSts = ippiFilterBoxBorder_8u_C4R(pSrcPtr, m_iStep, pTmpPtr, m_iTmpStep, dstRoiSize, m_filterBoxMask, border, &iBorderValue, pBuffer);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiFilterBoxBorder_8u_C4R()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Multiply by a constant in-place
            ippSts = ippiMulC_8u_C4RSfs(pTmpPtr, m_iTmpStep, m_iMulVal1, pTmpPtr, m_iTmpStep, dstRoiSize, 12);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiMulC_8u_C4RSfs()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Subtract
            ippSts = ippiSub_8u_C4RSfs(pTmpPtr, m_iTmpStep, pSrcPtr, m_iStep, pDstPtr, m_iStep, dstRoiSize, 0);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiSub_8u_C4RSfs()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Multiply by a constant in-place
            ippSts = ippiMulC_8u_C4RSfs(pDstPtr, m_iStep, m_iMulVal2, pDstPtr, m_iStep, dstRoiSize, 8);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiMulC_8u_C4RSfs()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Apply threshold
            ippSts = ippiThreshold_LTValGTVal_8u_AC4R(pDstPtr, m_iStep, pDstPtr, m_iStep, dstRoiSize, m_iThresLTVal, m_iThresLTVal, m_iThresGTVal, m_iThresGTVal);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiThreshold_LTValGTVal_8u_AC4R()", ippGetStatusString(ippSts), return STS_ERR_FAILED);
        }

        if(!pExtBuffer)
            ippsFree(pBuffer);

        return STS_OK;
    }

    virtual int HarmonizeImage()
    {
        if(!m_pSrcImage || !m_pDstImage)
            return STS_ERR_NULL_PTR;

        IppiSize roi = {(int)m_iWidth, (int)m_iHeight};

        return HarmonizeBlock(m_pSrcImage, m_pDstImage, m_pTmpImage, roi, ippBorderRepl);
    }

public:
    unsigned int m_iThreads;

    unsigned int m_iWidth, m_iHeight, m_iSamples, m_iStep;

    IppiSize  m_filterBoxMask;
    IppiPoint m_filterBoxAnchor;

    unsigned char m_iMulVal1[4];
    unsigned char m_iMulVal2[4];
    unsigned char m_iThresLTVal[4];
    unsigned char m_iThresGTVal[4];

    Ipp8u*  m_pTmpImage;
    int     m_iTmpStep;
    Ipp8u*  m_pSrcImage;
    Ipp8u*  m_pDstImage;
};

class HarmonizeVM : public Harmonize
{
public:
    HarmonizeVM()
    {
        m_pThreads = 0;
        m_pParams  = 0;
    }

    virtual void Close()
    {
        unsigned int i;

        Harmonize::Close();

        if(m_pThreads)
        {
            for(i = 0; i < m_iThreads; i++)
            {
                m_pParams[i].bQuit = true;
                vm_event_signal(&m_pParams[i].eStart);
            }
            for(i = 0; i < m_iThreads; i++)
            {
                vm_thread_wait(&m_pThreads[i]);
                vm_thread_close(&m_pThreads[i]);

                if(m_pParams[i].pBuffer)
                    ippsFree(m_pParams[i].pBuffer);
            }

            delete[] m_pThreads;
            delete[] m_pParams;
            m_pThreads = 0;
            m_pParams  = 0;
        }
    }

    virtual int SetImages(Ipp8u* pSrcImage, Ipp8u* pDstImage)
    {
        IppStatus ippSts;
        int    status;

        Harmonize::SetImages(pSrcImage, pDstImage);

        m_pThreads = new vm_thread[m_iThreads];
        m_pParams  = new TaskParams[m_iThreads];
        if(!m_pThreads || !m_pParams)
            return STS_ERR_ALLOC;

        unsigned int iBlockSize = m_iHeight / m_iThreads;
        unsigned int iRemainder = m_iHeight - iBlockSize* m_iThreads;

        IppiSize roiSize;
        int      iBufferSize = 0;

        for(unsigned int i = 0; i < m_iThreads; i++)
        {
            m_pParams[i].roi.width = m_iWidth;
            if(i == (m_iThreads-1))
                m_pParams[i].roi.height = iRemainder + iBlockSize;
            else
                m_pParams[i].roi.height = iBlockSize;

            m_pParams[i].border = ippBorderConst;
            if(i != 0) // non-top
                m_pParams[i].border = (IppiBorderType)(m_pParams[i].border | ippBorderInMemTop);
            if(i != (m_iThreads - 1)) // non-bottom
                m_pParams[i].border = (IppiBorderType)(m_pParams[i].border | ippBorderInMemBottom);

            roiSize.width = m_pParams[i].roi.width;
            roiSize.height = m_pParams[i].roi.height;

            ippSts = ippiFilterBoxBorderGetBufferSize(roiSize, m_filterBoxMask, ipp8u, m_iSamples, &iBufferSize);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiFilterBoxBorderGetBufferSize()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            m_pParams[i].pBuffer = ippsMalloc_8u(iBufferSize);
            if(!m_pParams[i].pBuffer)
                return STS_ERR_ALLOC;

            m_pParams[i].pSrcData   = pSrcImage + m_iStep * i * iBlockSize;
            m_pParams[i].pDstData   = pDstImage + m_iStep * i * iBlockSize;
            m_pParams[i].pTmpData   = m_pTmpImage + m_iStep * i * iBlockSize;
            m_pParams[i].pHarmonize = this;
            m_pParams[i].bQuit      = false;

            vm_thread_set_invalid(&m_pThreads[i]);
            if(vm_thread_create(&m_pThreads[i], HarmonizeVMTask, &m_pParams[i]) != 1)
                return STS_ERR_INIT;
        }

        return STS_OK;
    }

    virtual int HarmonizeImage()
    {
        unsigned int i;

        for(i = 0; i < m_iThreads; i++)
            vm_event_signal(&m_pParams[i].eStart);

        for(i = 0; i < m_iThreads; i++)
                vm_event_wait(&m_pParams[i].eEnd);

        return STS_OK;
    }

private:
    struct TaskParams
    {
        TaskParams()
        {
            vm_event_set_invalid(&eStart);
            vm_event_set_invalid(&eEnd);
            vm_event_init(&eStart, 0, 0);
            vm_event_init(&eEnd, 0, 0);

            pBuffer = 0;
            border  = ippBorderRepl;

            bQuit   = false;
        }
        ~TaskParams()
        {
            vm_event_destroy(&eStart);
            vm_event_destroy(&eEnd);
        }

        unsigned char *pBuffer;
        IppiBorderType border;

        Harmonize *pHarmonize;
        Ipp8u     *pSrcData;
        Ipp8u     *pDstData;
        Ipp8u     *pTmpData;
        IppiSize   roi;

        vm_event      eStart;
        vm_event      eEnd;
        volatile bool bQuit;
    };

    static unsigned int VM_THREAD_CALLCONVENTION HarmonizeVMTask(void *pParams)
    {
        int status;
        TaskParams *pPar = (TaskParams*)pParams;

        while(!pPar->bQuit)
        {
            vm_event_wait(&pPar->eStart);
            if(pPar->bQuit)
                break;

            status = pPar->pHarmonize->HarmonizeBlock(pPar->pSrcData, pPar->pDstData, pPar->pTmpData, pPar->roi, pPar->border, pPar->pBuffer);
            if(status != STS_OK)
                return 1;

            vm_event_signal(&pPar->eEnd);
        }

        return 0;
    }

private:
    vm_thread  *m_pThreads;
    TaskParams *m_pParams;
};
#pragma offload_attribute(pop)

// MIC data
__declspec(target(mic)) Harmonize* pHarmonize = 0;

// MIC harmonize initialization function
template<class T>
__declspec(target(mic)) void Create(int threads, unsigned int width, unsigned int height, unsigned int step, unsigned int samples)
{
    pHarmonize = new T;
    pHarmonize->m_iThreads = threads;
    pHarmonize->Init(width, height, step, samples);
}

__declspec(target(mic)) void ClearHarmonizeObject(Ipp8u* pOutput)
{
    if(pHarmonize)
        delete pHarmonize;
}

__declspec(target(mic)) int SetImages(Ipp8u* pSrcImage, Ipp8u* pDstImage)
{
    return pHarmonize->SetImages(pSrcImage, pDstImage);
}

__declspec(target(mic)) int HarmonizeImage()
{
    return pHarmonize->HarmonizeImage();
}

__declspec(target(mic)) void Create(ParallelMethod par_method, int threads, unsigned int width, unsigned int height, unsigned int step, unsigned int samples)
{
    switch(par_method) {
    case PAR_SEQ:
        Create<Harmonize>(threads, width, height, step, samples);
        break;
    case PAR_VM:
        Create<HarmonizeVM>(threads > 0? threads : VM_THREADS, width, height, step, samples);
        break;
    }
}

__declspec(target(mic)) void printVersion()
{
    const IppLibraryVersion *pVersion;
    printf("\nIntel(R) IPP:\n");
    //    PRINT_LIB_VERSION(  , pVersion)
    pVersion = ippGetLibVersion();
    printf("  %s %s %s \n", pVersion->Name, pVersion->Version, pVersion->BuildDate);

    PRINT_LIB_VERSION(s,  pVersion)
    PRINT_LIB_VERSION(i,  pVersion)
    fflush(0);
}
#endif

__declspec(target(mic)) void printVersion();
__declspec(target(mic)) void Create(ParallelMethod, int, unsigned int, unsigned int, unsigned int, unsigned int);
__declspec(target(mic)) int SetImages(Ipp8u*, Ipp8u*);
__declspec(target(mic)) int HarmonizeImage();
__declspec(target(mic)) void ClearHarmonizeObject(Ipp8u*);

#if !defined(__MIC__)
#include "base_renderer.h"
#endif  // !defined(__MIC__)

void printHelp(const OptDef pOptions[], char* argv[])
{
    printf("\nUsage: %s [-i] InputFile [[-o] OutputFile] [Options]\n", GetProgName(argv));
    printf("Options:\n");
    OptUsage(pOptions);
}


int main(int argc, char *argv[])
{
    /*
    // Variables initialization
    */
    int       status         = STS_OK;
    DString      sInputFile     = CheckTestDirs("lena.bmp");
    char*        sOutputFile    = 0;
    unsigned int iThreads       = 0;
    bool         bNoWindow      = false;
    bool         bPrintHelp     = false;

    Image      srcData;
    Image      dstData;
    char      *sMethod = "native";
    int        iMethod;

    // General timing
    vm_tick      tickStart   = 0;
    vm_tick      tickAcc     = 0;
    vm_tick      tickFreq    = vm_time_get_frequency()/1000;
    double       fTime       = 0;
    double       fTimeLimit  = 0;
    unsigned int iLoops      = 0;
    unsigned int iLoopsLimit = 0;

    /*
    // Cmd parsing
    */
    OptDef cmdOpts[] = {
        { 'i', 1, KT_DSTRING,   KF_OPTIONAL,  &sInputFile,      "input file name" },
        { 'o', 1, KT_STRING,    KF_OPTIONAL,  &sOutputFile,     "output file name" },
        { 't', 1, KT_INTEGER,   0,            &iThreads,        "number of threads (0 - auto, 0 by default)" },
        { 's', 1, KT_BOOL,      0,            &bNoWindow,       "suppress window output" },
        { 'w', 1, KT_DOUBLE,    0,            &fTimeLimit,      "minimum test time in milliseconds" },
        { 'l', 1, KT_INTEGER,   0,            &iLoopsLimit,     "number of loops (overrides test time)" },
        { 'h', 1, KT_BOOL,      0,            &bPrintHelp,      "print help and exit" },
        {0}
    };

    if(OptParse(argc, argv, cmdOpts))
    {
        printHelp(cmdOpts, argv);
        PRINT_MESSAGE("invalid input parameters");
        return 1;
    }

#pragma offload target(mic)
    printVersion();                 // Print IPP version from the MIC environment

    if(bPrintHelp)
    {
        printHelp(cmdOpts, argv);
        return 0;
    }

    if(!sInputFile || !strlen(sInputFile))
    {
        printHelp(cmdOpts, argv);
        PRINT_MESSAGE("Cannot open input file");
        return 1;
    }

    for(;;)
    {
        // Read from file
        printf("\nInput file: %s\n", (char*)sInputFile);
        status = srcData.Read(sInputFile);
        CHECK_STATUS_PRINT_BR(status, "Image::Read()", GetBaseStatusString(status));
        printf("Input info: %dx%d %s\n", srcData.m_iWidth, srcData.m_iHeight, colorName[srcData.m_color]);

        // Prepare destination buffer
        dstData = srcData;
        status  = dstData.Alloc();
        CHECK_STATUS_PRINT_BR(status, "Image::Alloc()", GetBaseStatusString(status));

        printf("\nOutput file: %s\n", (sOutputFile)?sOutputFile:"-");
        printf("Output info: %dx%d %s\n", dstData.m_iWidth, dstData.m_iHeight, colorName[dstData.m_color]);

        unsigned int mic_width, mic_height, mic_step, mic_samples;
        mic_width = srcData.m_iWidth;
        mic_height = srcData.m_iHeight;
        mic_step = srcData.m_iStep;
        mic_samples = srcData.m_iSamples;

        if(iThreads == 1)
        {
            printf("\nSequential harmonization\n");
#pragma offload target(mic) in(iThreads, mic_width, mic_height, mic_step, mic_samples)
            Create(PAR_SEQ, iThreads, mic_width, mic_height, mic_step, mic_samples);
        }
        else
        {
            if(iThreads == 0)   // No threads specified
                iThreads = VM_THREADS;
            if((mic_height / iThreads) < MIN_BLOCK_HEIGHT) {    // Max number of threads is limited by box filter matrix size
                iThreads = mic_height / MIN_BLOCK_HEIGHT;
                printf("\nWARNING: Maximum number of threads is %d\n\n", iThreads);
            }
            printf("\nNative threaded harmonization\n");
#pragma offload target(mic) in(iThreads, mic_width, mic_height, mic_step, mic_samples)
            Create(PAR_VM, iThreads, mic_width, mic_height, mic_step, mic_samples);
        }

        // pre-init
        Ipp8u *pmicSrc, *pmicDst;
        pmicSrc = srcData.m_pPointer;
        pmicDst = dstData.m_pPointer;
#pragma offload target(mic) in(pmicSrc: length(srcData.m_iBufferSize) RETAIN) out(pmicDst: length(dstData.m_iBufferSize) RETAIN)
        SetImages(pmicSrc, pmicDst);

        if(iThreads != 1)
            printf("Threads: %d\n", iThreads);

        for(iLoops = 1, tickAcc = 0;; iLoops++)
        {
            tickStart = vm_time_get_tick();


#pragma offload target(mic) out(status)
            status = HarmonizeImage();

            tickAcc += (vm_time_get_tick() - tickStart);

            CHECK_STATUS_PRINT_BR(status, "HarmonizeImage()", GetBaseStatusString(status));

            fTime = (double)tickAcc/tickFreq;
            if(iLoopsLimit)
            {
                if(iLoops >= iLoopsLimit)
                    break;
            }
            else
            {
                if(fTime >= fTimeLimit)
                    break;
            }
        }
        if(status < 0) break;

        /*
        // Results output
        */
        printf("\nLoops:      %d\n", iLoops);
        printf("Time total: %0.3fms\n",  fTime);
        printf("Loop avg:   %0.3fms\n",  fTime/iLoops);

// Get output image from coprocessor
#pragma offload target(mic) out(pmicDst: length(dstData.m_iBufferSize) FREE)
        ClearHarmonizeObject(pmicDst);

        if(sOutputFile && strlen(sOutputFile))
        {
            status = dstData.Write(sOutputFile);
            CHECK_STATUS_PRINT_BR(status, "Image::Write()", GetBaseStatusString(status));
        }

        // Rendering
#if !defined(__MIC__)
        if(!bNoWindow)
        {
            WndDesc *pDesc = WindowNew("IPP Threaded example");
            if(pDesc)
            {
                printf("\nPress Space to cycle through stages:\n");
                printf("1 - result image\n");
                printf("2 - original image\n");
                printf("\nClose window to exit.\n");

                int  iIndex  = 0;
                bool bRedraw = true;
                while(!WindowIsClosed(pDesc))
                {
                    vm_time_sleep(10);
                    if(WindowCheckKey(pDesc) == KK_SPACE)
                    {
                        iIndex = (iIndex+1)%2;
                        bRedraw = true;
                    }
                    if(WindowIsInvalidated(pDesc))
                        bRedraw = true;

                    if(bRedraw)
                    {
                        if(iIndex == 0)
                            WindowDrawImage(pDesc, &dstData);
                        else if(iIndex == 1)
                            WindowDrawImage(pDesc, &srcData);
                        bRedraw = false;
                    }
                }
                WindowRelease(pDesc);
            }
        }
#endif // !defined(__MIC__)
        break;
    }

    if(status < 0)
        return status;
    return 0;
}

