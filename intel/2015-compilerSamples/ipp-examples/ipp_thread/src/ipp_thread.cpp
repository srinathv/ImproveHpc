/*******************************************************************************
** Copyright(C) 2012-2014 Intel Corporation. All Rights Reserved.
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

#include <math.h>
#include <memory>

#include "vm_thread.h"

#include "base.h"
#include "base_image.h"
#include "base_ipp.h"
#include "base_renderer.h"

#include "ippcore.h"
#include "ipps.h"
#include "ippi.h"

#ifdef USE_TBB
#define __TBB_NO_IMPLICIT_LINKAGE 1
#define TBB_PREVIEW_MEMORY_POOL 1
#include "tbb/task_scheduler_init.h"
#include "tbb/parallel_for.h"
#include "tbb/blocked_range2d.h"
#include "tbb/memory_pool.h"

using namespace tbb;
#endif

#define CMD_METHOD_TBB 0
#define CMD_METHOD_VM  1


static void printVersion()
{
    const IppLibraryVersion *pVersion;
    printf("\nIntel(R) IPP:\n");
    PRINT_LIB_VERSION(  , pVersion)
    PRINT_LIB_VERSION(s,  pVersion)
    PRINT_LIB_VERSION(i,  pVersion)
}

static void printHelp(const OptDef pOptions[], char* argv[])
{
    printf("\nUsage: %s [-i] InputFile [[-o] OutputFile] [Options]\n", GetProgName(argv));
    printf("Options:\n");
    OptUsage(pOptions);
}

class Harmonize
{
public:
    Harmonize()
    {
        m_iThreads = 0;

        m_filterBoxMask.width  = 7;
        m_filterBoxMask.height = 7;
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
    }

    virtual Status Init(Image *pSrcImage, Image *pDstImage)
    {
        if(!pSrcImage || !pSrcImage->m_pPointer || !pDstImage)
            return STS_ERR_NULL_PTR;

        if(pSrcImage->m_iSamples != 1 && pSrcImage->m_iSamples != 3 && pSrcImage->m_iSamples != 4)
            return STS_ERR_INVALID_PARAMS;

        Close();

        m_tmpImage = *pSrcImage;
        m_tmpImage.Alloc();

        m_templ = *pSrcImage;

        return STS_OK;
    }

    Status HarmonizeBlock(Image *pSrcImage, Image *pDstImage, Rect roi, IppiBorderType border, unsigned char *pExtBuffer = 0)
    {
        Status      status;
        IppStatus   ippSts;
        IppiSize    dstRoiSize   = {static_cast<int>(roi.width), static_cast<int>(roi.height)};

        unsigned char iBorderValue = 0;

        unsigned char *pSrcPtr = 0;
        unsigned char *pDstPtr = 0;
        unsigned char *pTmpPtr = 0;
        unsigned char *pBuffer = 0;
        int            iBufferSize = 0;

        if(!pSrcImage || !pDstImage)
            return STS_ERR_NULL_PTR;

        // Zero size mean full size
        if(!dstRoiSize.width)
            dstRoiSize.width = pDstImage->m_iWidth;
        if(!dstRoiSize.height)
            dstRoiSize.height = pDstImage->m_iHeight;

        if(m_templ != *pSrcImage)
        {
            status = Init(pSrcImage, pDstImage);
            CHECK_STATUS_PRINT_RS(status, "Harmonize::Init()", GetBaseStatusString(status));
        }

        // adjust input and output buffers to current ROI
        pSrcPtr = pSrcImage->m_pPointer + roi.y*pSrcImage->m_iStep + roi.x*pSrcImage->m_iSamples;
        pDstPtr = pDstImage->m_pPointer + roi.y*pDstImage->m_iStep + roi.x*pDstImage->m_iSamples;
        pTmpPtr = m_tmpImage.m_pPointer + roi.y*m_tmpImage.m_iStep + roi.x*m_tmpImage.m_iSamples;

        if(!pExtBuffer)
        {
            ippSts = ippiFilterBoxBorderGetBufferSize(dstRoiSize, m_filterBoxMask, ipp8u, pSrcImage->m_iSamples, &iBufferSize);
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

        if(pSrcImage->m_iSamples == 1)
        {
            // Apply box filter
            ippSts = ippiFilterBoxBorder_8u_C1R(pSrcPtr, pSrcImage->m_iStep, pTmpPtr, m_tmpImage.m_iStep, dstRoiSize, m_filterBoxMask, border, &iBorderValue, pBuffer);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiFilterBoxBorder_8u_C1R()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Multiply by a constant in-place
            ippSts = ippiMulC_8u_C1RSfs(pTmpPtr, m_tmpImage.m_iStep, m_iMulVal1[0], pTmpPtr, m_tmpImage.m_iStep, dstRoiSize, 12);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiMulC_8u_C1RSfs()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Subtract
            ippSts = ippiSub_8u_C1RSfs(pTmpPtr, m_tmpImage.m_iStep, pSrcPtr, pSrcImage->m_iStep, pDstPtr, pDstImage->m_iStep, dstRoiSize, 0);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiSub_8u_C1RSfs()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Multiply by a constant in-place
            ippSts = ippiMulC_8u_C1RSfs(pDstPtr, pDstImage->m_iStep, m_iMulVal2[0], pDstPtr, pDstImage->m_iStep, dstRoiSize, 8);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiMulC_8u_C1RSfs()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Apply threshold
            ippSts = ippiThreshold_LTValGTVal_8u_C1R(pDstPtr, pDstImage->m_iStep, pDstPtr, pDstImage->m_iStep, dstRoiSize, m_iThresLTVal[0], m_iThresLTVal[0], m_iThresGTVal[0], m_iThresGTVal[0]);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiThreshold_LTValGTVal_8u_C1R()", ippGetStatusString(ippSts), return STS_ERR_FAILED);
        }
        else if(pSrcImage->m_iSamples == 3)
        {
            // Apply box filter
            ippSts = ippiFilterBoxBorder_8u_C3R(pSrcPtr, pSrcImage->m_iStep, pTmpPtr, m_tmpImage.m_iStep, dstRoiSize, m_filterBoxMask, border, &iBorderValue, pBuffer);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiFilterBoxBorder_8u_C3R()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Multiply by a constant in-place
            ippSts = ippiMulC_8u_C3RSfs(pTmpPtr, m_tmpImage.m_iStep, m_iMulVal1, pTmpPtr, m_tmpImage.m_iStep, dstRoiSize, 12);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiMulC_8u_C3RSfs()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Subtract
            ippSts = ippiSub_8u_C3RSfs(pTmpPtr, m_tmpImage.m_iStep, pSrcPtr, pSrcImage->m_iStep, pDstPtr, pDstImage->m_iStep, dstRoiSize, 0);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiSub_8u_C3RSfs()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Multiply by a constant in-place
            ippSts = ippiMulC_8u_C3RSfs(pDstPtr, pDstImage->m_iStep, m_iMulVal2, pDstPtr, pDstImage->m_iStep, dstRoiSize, 8);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiMulC_8u_C3RSfs()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Apply threshold
            ippSts = ippiThreshold_LTValGTVal_8u_C3R(pDstPtr, pDstImage->m_iStep, pDstPtr, pDstImage->m_iStep, dstRoiSize, m_iThresLTVal, m_iThresLTVal, m_iThresGTVal, m_iThresGTVal);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiThreshold_LTValGTVal_8u_C3R()", ippGetStatusString(ippSts), return STS_ERR_FAILED);
        }
        else if(pSrcImage->m_iSamples == 4)
        {
            // Apply box filter
            ippSts = ippiFilterBoxBorder_8u_C4R(pSrcPtr, pSrcImage->m_iStep, pTmpPtr, m_tmpImage.m_iStep, dstRoiSize, m_filterBoxMask, border, &iBorderValue, pBuffer);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiFilterBoxBorder_8u_C4R()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Multiply by a constant in-place
            ippSts = ippiMulC_8u_C4RSfs(pTmpPtr, m_tmpImage.m_iStep, m_iMulVal1, pTmpPtr, m_tmpImage.m_iStep, dstRoiSize, 12);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiMulC_8u_C4RSfs()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Subtract
            ippSts = ippiSub_8u_C4RSfs(pTmpPtr, m_tmpImage.m_iStep, pSrcPtr, pSrcImage->m_iStep, pDstPtr, pDstImage->m_iStep, dstRoiSize, 0);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiSub_8u_C4RSfs()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Multiply by a constant in-place
            ippSts = ippiMulC_8u_C4RSfs(pDstPtr, pDstImage->m_iStep, m_iMulVal2, pDstPtr, pDstImage->m_iStep, dstRoiSize, 8);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiMulC_8u_C4RSfs()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            // Apply threshold
            ippSts = ippiThreshold_LTValGTVal_8u_AC4R(pDstPtr, pDstImage->m_iStep, pDstPtr, pDstImage->m_iStep, dstRoiSize, m_iThresLTVal, m_iThresLTVal, m_iThresGTVal, m_iThresGTVal);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiThreshold_LTValGTVal_8u_AC4R()", ippGetStatusString(ippSts), return STS_ERR_FAILED);
        }

        if(!pExtBuffer)
            ippsFree(pBuffer);

        return STS_OK;
    }

    virtual Status HarmonizeImage(Image *pSrcImage, Image *pDstImage)
    {
        if(!pSrcImage || !pDstImage)
            return STS_ERR_NULL_PTR;

        Rect roi = {0, 0, pDstImage->m_iWidth, pDstImage->m_iHeight};

        return HarmonizeBlock(pSrcImage, pDstImage, roi, ippBorderRepl);
    }

public:
    unsigned int m_iThreads;

    IppiSize  m_filterBoxMask;
    IppiPoint m_filterBoxAnchor;

    unsigned char m_iMulVal1[4];
    unsigned char m_iMulVal2[4];
    unsigned char m_iThresLTVal[4];
    unsigned char m_iThresGTVal[4];

protected:
    Image m_templ;
    Image m_tmpImage;
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

    virtual Status Init(Image *pSrcImage, Image *pDstImage)
    {
        IppStatus ippSts;
        Status    status;

        status = Harmonize::Init(pSrcImage, pDstImage);
        CHECK_STATUS_PRINT_RS(status, "Harmonize::Init()", GetBaseStatusString(status));

        if(m_iThreads == 0) // automatic threads number
            m_iThreads = vm_sys_info_get_avail_cpu_num();

        m_pThreads = new vm_thread[m_iThreads];
        m_pParams  = new TaskParams[m_iThreads];
        if(!m_pThreads || !m_pParams)
            return STS_ERR_ALLOC;

        unsigned int iBlockSize = (pSrcImage->m_iHeight + m_iThreads - 1)/m_iThreads;
        unsigned int iRemainder = pSrcImage->m_iHeight - iBlockSize*(m_iThreads - 1);

        IppiSize roiSize;
        int      iBufferSize = 0;

        for(unsigned int i = 0; i < m_iThreads; i++)
        {
            m_pParams[i].roi.x = 0;
            m_pParams[i].roi.y = i*iBlockSize;
            m_pParams[i].roi.width = pSrcImage->m_iWidth;
            m_pParams[i].roi.height = (i == (m_iThreads - 1))?iRemainder:iBlockSize;

            if(i != 0) // non-top
                m_pParams[i].border = (IppiBorderType)(m_pParams[i].border | ippBorderInMemTop);
            if(i != (m_iThreads - 1)) // non-bottom
                m_pParams[i].border = (IppiBorderType)(m_pParams[i].border | ippBorderInMemBottom);

            roiSize.width = m_pParams[i].roi.width;
            roiSize.height = m_pParams[i].roi.height;

            ippSts = ippiFilterBoxBorderGetBufferSize(roiSize, m_filterBoxMask, ipp8u, pSrcImage->m_iSamples, &iBufferSize);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiFilterBoxBorderGetBufferSize()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            m_pParams[i].pBuffer = ippsMalloc_8u(iBufferSize);
            if(!m_pParams[i].pBuffer)
                return STS_ERR_ALLOC;

            m_pParams[i].pSrcData   = pSrcImage;
            m_pParams[i].pDstData   = pDstImage;
            m_pParams[i].pHarmonize = this;
            m_pParams[i].bQuit      = false;

            vm_thread_set_invalid(&m_pThreads[i]);
            if(vm_thread_create(&m_pThreads[i], HarmonizeVMTask, &m_pParams[i]) != 1)
                return STS_ERR_INIT;
        }

        return STS_OK;
    }

    Status HarmonizeImage(Image *pSrcImage, Image *pDstImage)
    {
        unsigned int i;

        if(!pSrcImage || !pDstImage)
            return STS_ERR_NULL_PTR;

        if(m_templ != *pSrcImage)
        {
            Status status = Init(pSrcImage, pDstImage);
            CHECK_STATUS_PRINT_RS(status, "Harmonize::Init()", GetBaseStatusString(status));
        }

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
        Image     *pSrcData;
        Image     *pDstData;
        Rect       roi;

        vm_event      eStart;
        vm_event      eEnd;
        volatile bool bQuit;
    };

    static unsigned int VM_THREAD_CALLCONVENTION HarmonizeVMTask(void *pParams)
    {
        Status status;
        TaskParams *pPar = (TaskParams*)pParams;

        while(!pPar->bQuit)
        {
            vm_event_wait(&pPar->eStart);
            if(pPar->bQuit)
                break;

            status = pPar->pHarmonize->HarmonizeBlock(pPar->pSrcData, pPar->pDstData, pPar->roi, pPar->border, pPar->pBuffer);
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

#ifdef USE_TBB
class HarmonizeTBB : public Harmonize
{
public:
    HarmonizeTBB(): m_scheduler(task_scheduler_init::deferred)
    {
        m_iGrainX  = 0;
        m_iGrainY  = 0;
    }

    virtual Status Init(Image *pSrcImage, Image *pDstImage)
    {
        Status status;

        status = Harmonize::Init(pSrcImage, pDstImage);
        CHECK_STATUS_PRINT_RS(status, "Harmonize::Init()", GetBaseStatusString(status));

        if(m_iThreads == 0) // automatic threads number
        {
            m_iThreads = task_scheduler_init::default_num_threads();
            m_scheduler.initialize(m_iThreads);
        }
        else // specific threads number
            m_scheduler.initialize(m_iThreads);

        if(!m_iGrainX)
            m_iGrainX = (pDstImage->m_iWidth + m_iThreads - 1)/m_iThreads;

        if(!m_iGrainY)
            m_iGrainY = (pDstImage->m_iHeight + m_iThreads - 1)/m_iThreads;

        m_task.m_pSrcData   = pSrcImage;
        m_task.m_pDstData   = pDstImage;
        m_task.m_pHarmonize = this;

        return STS_OK;
    }

    Status HarmonizeImage(Image *pSrcImage, Image *pDstImage)
    {
        if(!pSrcImage || !pDstImage)
            return STS_ERR_NULL_PTR;

        if(m_templ != *pSrcImage)
        {
            Status status = Init(pSrcImage, pDstImage);
            CHECK_STATUS_PRINT_RS(status, "Harmonize::Init()", GetBaseStatusString(status));
        }

        blocked_range2d<unsigned int, unsigned int> tbbRange(0, pDstImage->m_iHeight, m_iGrainY, 0, pDstImage->m_iWidth, m_iGrainX);

        try
        {
            parallel_for(tbbRange, m_task, m_part_auto);
        }
        catch(Status status)
        {
            return status;
        }

        return STS_OK;
    }

private:
    class HarmonizeTBBTask
    {
    public:
        HarmonizeTBBTask() {};

        void operator()(blocked_range2d<unsigned int, unsigned int> &r) const
        {
            IppStatus ippSts;
            Status    status;
            Rect      roi         = {r.cols().begin(), r.rows().begin(), r.cols().end() - r.cols().begin(), r.rows().end() - r.rows().begin()};
            IppiSize  dstRoiSize  = {roi.width, roi.height};
            IppiBorderType border = ippBorderRepl;

            unsigned char *pBuffer = 0;
            int iBufferSize;

            ippSts = ippiFilterBoxBorderGetBufferSize(dstRoiSize, m_pHarmonize->m_filterBoxMask, ipp8u, m_pSrcData->m_iSamples, &iBufferSize);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiFilterBoxBorderGetBufferSize()", ippGetStatusString(ippSts), throw(STS_ERR_FAILED));

            pBuffer = (unsigned char*)m_pHarmonize->m_memPool.malloc(iBufferSize);
            if(!pBuffer)
            {
                PRINT_MESSAGE("Cannot allocate memory for FilterBox buffer");
                throw(STS_ERR_ALLOC);
            }

            if(roi.x != 0) // non-left
                border = (IppiBorderType)(border | ippBorderInMemLeft);
            if(roi.y != 0) // non-top
                border = (IppiBorderType)(border | ippBorderInMemTop);
            if(r.cols().end() != m_pSrcData->m_iWidth) // non-right
                border = (IppiBorderType)(border | ippBorderInMemRight);
            if(r.rows().end() != m_pSrcData->m_iHeight) // non-bottom
                border = (IppiBorderType)(border | ippBorderInMemBottom);

            status = m_pHarmonize->HarmonizeBlock(m_pSrcData, m_pDstData, roi, border, pBuffer);
            if(status != STS_OK)
                throw(status);

            m_pHarmonize->m_memPool.free(pBuffer);
        }

        HarmonizeTBB *m_pHarmonize;
        Image        *m_pSrcData;
        Image        *m_pDstData;
    };

public:
    unsigned int m_iGrainX;
    unsigned int m_iGrainY;

private:
    HarmonizeTBBTask    m_task;
    task_scheduler_init m_scheduler;
    auto_partitioner    m_part_auto;
    memory_pool< scalable_allocator<unsigned char> > m_memPool;
};
#endif

int main(int argc, char *argv[])
{
    /*
    // Variables initialization
    */
    Status       status         = STS_OK;
    DString      sInputFile     = CheckTestDirs("lena.bmp");
    char*        sOutputFile    = 0;
    char*        sIppCpu        = 0;
    unsigned int iThreads       = 0;
    bool         bNoWindow      = false;
    bool         bPrintHelp     = false;

    Image      srcData;
    Image      dstData;
#ifdef USE_TBB
    char      *sMethod = "tbb";
#else
    char      *sMethod = "native";
#endif
    int        iMethod;
    Harmonize *pHarmonize = 0;

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
#ifdef USE_TBB
        { 'm', 1, KT_STRING,    0,            &sMethod,         "threading method: tbb (default), native"},
#endif
        { 't', 1, KT_POSITIVE,  0,            &iThreads,        "number of threads (0 - auto, 0 by default)" },
        { 's', 1, KT_BOOL,      0,            &bNoWindow,       "suppress window output" },
        { 'w', 1, KT_DOUBLE,    0,            &fTimeLimit,      "minimum test time in milliseconds" },
        { 'l', 1, KT_POSITIVE,  0,            &iLoopsLimit,     "number of loops (overrides test time)" },
        { 'T', 1, KT_STRING,    0,            &sIppCpu,         "target IPP optimization (SSE, SSE2, SSE3, SSSE3, SSE41, SSE42, AES, AVX, AVX2)" },
        { 'h', 1, KT_BOOL,      0,            &bPrintHelp,      "print help and exit" },
        {0}
    };

    if(OptParse(argc, argv, cmdOpts))
    {
        printHelp(cmdOpts, argv);
        PRINT_MESSAGE("invalid input parameters");
        return 1;
    }

    InitPreferredCpu(sIppCpu);

    printVersion();

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

    if(!vm_string_stricmp(sMethod, "tbb"))
        iMethod = CMD_METHOD_TBB;
    else if(!vm_string_stricmp(sMethod, "native"))
        iMethod = CMD_METHOD_VM;
    else
    {
        printHelp(cmdOpts, argv);
        PRINT_MESSAGE("Invalid threading method");
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

        if(iThreads == 1)
        {
            printf("\nSequential harmonization\n");
            pHarmonize = new Harmonize;
            if(!pHarmonize)
            {
                PRINT_MESSAGE("Failed to allocate sequential harmonization class");
                return 1;
            }
        }
        else
        {
            if(iMethod == CMD_METHOD_VM)
            {
                printf("\nNative threaded harmonization\n");
                pHarmonize = new HarmonizeVM;
            }
#ifdef USE_TBB
            else
            {
                printf("\nTBB harmonization\n");
                pHarmonize = new HarmonizeTBB;
                if(!pHarmonize)
                {
                    PRINT_MESSAGE("Failed to allocate TBB harmonization class");
                    return 1;
                }
            }
#endif
            pHarmonize->m_iThreads = iThreads;
        }

        // pre-init
        status = pHarmonize->Init(&srcData, &dstData);
        CHECK_STATUS_PRINT_BR(status, "Harmonize::Init()", GetBaseStatusString(status));

        if(iThreads != 1)
        {
            iThreads = pHarmonize->m_iThreads;
            printf("Threads: %d\n", iThreads);
        }

        for(iLoops = 1, tickAcc = 0;; iLoops++)
        {
            tickStart = vm_time_get_tick();
            status = pHarmonize->HarmonizeImage(&srcData, &dstData);
            tickAcc += (vm_time_get_tick() - tickStart);

            CHECK_STATUS_PRINT_BR(status, "Harmonize::HarmonizeImage()", GetBaseStatusString(status));

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

        if(sOutputFile && strlen(sOutputFile))
        {
            status = dstData.Write(sOutputFile);
            CHECK_STATUS_PRINT_BR(status, "Image::Write()", GetBaseStatusString(status));
        }

        // Rendering
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

        break;
    }

    if(pHarmonize) delete pHarmonize;
    if(status < 0)
        return status;
    return 0;
}
