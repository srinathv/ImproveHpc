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

#define CMD_INTER_NN  0
#define CMD_INTER_LIN 1
#define CMD_INTER_CUB 2
#define CMD_INTER_LAN 3


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
    printf("\nUsage: %s [-i] InputFile [[-o] OutputFile] -r DstWidth DstHeight [Options]\n", GetProgName(argv));
    printf("Options:\n");
    OptUsage(pOptions);
}

class Resize
{
public:
    Resize()
    {
        m_iThreads = 0;

        m_interpolation = ippLinear;
        m_pSpec         = 0;
        m_pInitBuffer   = 0;

        m_fBVal   = 1;
        m_fCVal   = 0;
        m_iLobes  = 3;
    }

    virtual ~Resize()
    {
        Close();
    }

    void Close()
    {
        if(m_pSpec)
        {
            ippsFree(m_pSpec);
            m_pSpec = 0;
        }

        if(m_pInitBuffer)
        {
            ippsFree(m_pInitBuffer);
            m_pInitBuffer = 0;
        }
    }

    virtual Status Init(Image *pSrcImage, Image *pDstImage)
    {
        IppStatus       ippSts;
        IppiBorderSize  borderSize;
        int             iSpecSize = 0;
        int             iInitSize = 0;

        if(!pSrcImage || !pSrcImage->m_pPointer || !pDstImage)
            return STS_ERR_NULL_PTR;

        if(pSrcImage->m_iSamples != 1 && pSrcImage->m_iSamples != 3 && pSrcImage->m_iSamples != 4)
            return STS_ERR_INVALID_PARAMS;

        Close();

        IppiSize srcSize = {static_cast<int>(pSrcImage->m_iWidth), static_cast<int>(pSrcImage->m_iHeight)};
        IppiSize dstSize = {static_cast<int>(pDstImage->m_iWidth), static_cast<int>(pDstImage->m_iHeight)};

        // Get sizes for internal and initialization buffers
        ippSts = ippiResizeGetSize_8u(srcSize, dstSize, m_interpolation, 0, &iSpecSize, &iInitSize);
        CHECK_STATUS_PRINT_AC(ippSts, "ippiResizeGetSize_8u()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

        // allocate internal buffer
        m_pSpec = (IppiResizeSpec_32f*)ippsMalloc_8u(iSpecSize);
        if(!m_pSpec)
        {
            PRINT_MESSAGE("Cannot allocate memory for resize spec");
            return STS_ERR_ALLOC;
        }

        // allocate initialization buffer
        if(iInitSize)
        {
            m_pInitBuffer = ippsMalloc_8u(iInitSize);
            if(!m_pInitBuffer)
            {
                PRINT_MESSAGE("Cannot allocate memory for resize init buffer");
                return STS_ERR_ALLOC;
            }
        }

        // init ipp resizer
        if(m_interpolation == ippNearest)
        {
            ippSts = ippiResizeNearestInit_8u(srcSize, dstSize, m_pSpec);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiResizeNearestInit_8u()", ippGetStatusString(ippSts), return STS_ERR_FAILED);
        }
        else if(m_interpolation == ippLinear)
        {
            ippSts = ippiResizeLinearInit_8u(srcSize, dstSize, m_pSpec);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiResizeLinearInit_8u()", ippGetStatusString(ippSts), return STS_ERR_FAILED);
        }
        else if(m_interpolation == ippCubic)
        {
            ippSts = ippiResizeCubicInit_8u(srcSize, dstSize, m_fBVal, m_fCVal, m_pSpec, m_pInitBuffer);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiResizeCubicInit_8u()", ippGetStatusString(ippSts), return STS_ERR_FAILED);
        }
        else if(m_interpolation == ippLanczos)
        {
            ippSts = ippiResizeLanczosInit_8u(srcSize, dstSize, m_iLobes, m_pSpec, m_pInitBuffer);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiResizeLanczosInit_8u()", ippGetStatusString(ippSts), return STS_ERR_FAILED);
        }

        ippSts = ippiResizeGetBorderSize_8u(m_pSpec, &borderSize);
        CHECK_STATUS_PRINT_AC(ippSts, "ippiResizeGetBorderSize_8u()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

        m_templ = *pSrcImage;

        return STS_OK;
    }

    Status ResizeBlock(Image *pSrcImage, Image *pDstImage, Rect roi, IppiBorderType border, unsigned char *pExtBuffer = 0)
    {
        Status      status;
        IppStatus   ippSts;
        IppiPoint   dstRoiOffset = {roi.x, roi.y};
        IppiSize    dstRoiSize   = {static_cast<int>(roi.width), static_cast<int>(roi.height)};
        IppiPoint   srcRoiOffset;
        IppiSize    srcRoiSize;

        unsigned char *pSrcPtr = 0;
        unsigned char *pDstPtr = 0;
        unsigned char *pBuffer = 0;
        int            iBufferSize = 0;

        if(!pSrcImage || !pDstImage)
            return STS_ERR_NULL_PTR;

        if(!m_pSpec)
            return STS_ERR_NOT_INITIALIZED;

        // Zero size mean full size
        if(!dstRoiSize.width)
            dstRoiSize.width = pDstImage->m_iWidth;
        if(!dstRoiSize.height)
            dstRoiSize.height = pDstImage->m_iHeight;

        if(m_templ != *pSrcImage)
        {
            status = Init(pSrcImage, pDstImage);
            CHECK_STATUS_PRINT_RS(status, "Resize::Init()", GetBaseStatusString(status));
        }

        // get src ROI from dst ROI
        ippSts = ippiResizeGetSrcRoi_8u(m_pSpec, dstRoiOffset, dstRoiSize, &srcRoiOffset, &srcRoiSize);
        CHECK_STATUS_PRINT_AC(ippSts, "ippiResizeGetSrcRoi_8u()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

        // adjust input and output buffers to current ROI
        pSrcPtr = pSrcImage->m_pPointer + srcRoiOffset.y*pSrcImage->m_iStep + srcRoiOffset.x*pSrcImage->m_iSamples;
        pDstPtr = pDstImage->m_pPointer + dstRoiOffset.y*pDstImage->m_iStep + dstRoiOffset.x*pDstImage->m_iSamples;

        if(!pExtBuffer)
        {
            ippSts = ippiResizeGetBufferSize_8u(m_pSpec, dstRoiSize, pSrcImage->m_iSamples, &iBufferSize);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiResizeGetBufferSize_8u()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

            pBuffer = ippsMalloc_8u(iBufferSize);
            if(!pBuffer)
            {
                PRINT_MESSAGE("Cannot allocate memory for resize buffer");
                return STS_ERR_ALLOC;
            }
        }
        else
            pBuffer = pExtBuffer;

        // perform resize
        if(m_interpolation == ippNearest)
        {
            if(pSrcImage->m_iSamples == 1)
                ippSts = ippiResizeNearest_8u_C1R(pSrcPtr, pSrcImage->m_iStep, pDstPtr, pDstImage->m_iStep, dstRoiOffset, dstRoiSize, m_pSpec, pBuffer);
            else if(pSrcImage->m_iSamples == 3)
                ippSts = ippiResizeNearest_8u_C3R(pSrcPtr, pSrcImage->m_iStep, pDstPtr, pDstImage->m_iStep, dstRoiOffset, dstRoiSize, m_pSpec, pBuffer);
            else if(pSrcImage->m_iSamples == 4)
                ippSts = ippiResizeNearest_8u_C4R(pSrcPtr, pSrcImage->m_iStep, pDstPtr, pDstImage->m_iStep, dstRoiOffset, dstRoiSize, m_pSpec, pBuffer);
        }
        else if(m_interpolation == ippLinear)
        {
            if(pSrcImage->m_iSamples == 1)
                ippSts = ippiResizeLinear_8u_C1R(pSrcPtr, pSrcImage->m_iStep, pDstPtr, pDstImage->m_iStep, dstRoiOffset, dstRoiSize, border, 0, m_pSpec, pBuffer);
            else if(pSrcImage->m_iSamples == 3)
                ippSts = ippiResizeLinear_8u_C3R(pSrcPtr, pSrcImage->m_iStep, pDstPtr, pDstImage->m_iStep, dstRoiOffset, dstRoiSize, border, 0, m_pSpec, pBuffer);
            else if(pSrcImage->m_iSamples == 4)
                ippSts = ippiResizeLinear_8u_C4R(pSrcPtr, pSrcImage->m_iStep, pDstPtr, pDstImage->m_iStep, dstRoiOffset, dstRoiSize, border, 0, m_pSpec, pBuffer);
        }
        else if(m_interpolation == ippCubic)
        {
            if(pSrcImage->m_iSamples == 1)
                ippSts = ippiResizeCubic_8u_C1R(pSrcPtr, pSrcImage->m_iStep, pDstPtr, pDstImage->m_iStep, dstRoiOffset, dstRoiSize, border, 0, m_pSpec, pBuffer);
            else if(pSrcImage->m_iSamples == 3)
                ippSts = ippiResizeCubic_8u_C3R(pSrcPtr, pSrcImage->m_iStep, pDstPtr, pDstImage->m_iStep, dstRoiOffset, dstRoiSize, border, 0, m_pSpec, pBuffer);
            else if(pSrcImage->m_iSamples == 4)
                ippSts = ippiResizeCubic_8u_C4R(pSrcPtr, pSrcImage->m_iStep, pDstPtr, pDstImage->m_iStep, dstRoiOffset, dstRoiSize, border, 0, m_pSpec, pBuffer);
        }
        else if(m_interpolation == ippLanczos)
        {
            if(pSrcImage->m_iSamples == 1)
                ippSts = ippiResizeLanczos_8u_C1R(pSrcPtr, pSrcImage->m_iStep, pDstPtr, pDstImage->m_iStep, dstRoiOffset, dstRoiSize, border, 0, m_pSpec, pBuffer);
            else if(pSrcImage->m_iSamples == 3)
                ippSts = ippiResizeLanczos_8u_C3R(pSrcPtr, pSrcImage->m_iStep, pDstPtr, pDstImage->m_iStep, dstRoiOffset, dstRoiSize, border, 0, m_pSpec, pBuffer);
            else if(pSrcImage->m_iSamples == 4)
                ippSts = ippiResizeLanczos_8u_C4R(pSrcPtr, pSrcImage->m_iStep, pDstPtr, pDstImage->m_iStep, dstRoiOffset, dstRoiSize, border, 0, m_pSpec, pBuffer);
        }
        CHECK_STATUS_PRINT_AC(ippSts, "ippiResizeX_8u_CXR()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

        if(!pExtBuffer)
            ippsFree(pBuffer);

        return STS_OK;
    }

    virtual Status ResizeImage(Image *pSrcImage, Image *pDstImage)
    {
        if(!pSrcImage || !pDstImage)
            return STS_ERR_NULL_PTR;

        Rect roi = {0, 0, pDstImage->m_iWidth, pDstImage->m_iHeight};

        return ResizeBlock(pSrcImage, pDstImage, roi, ippBorderRepl);
    }

public:
    unsigned int m_iThreads;

    IppiInterpolationType m_interpolation;
    float        m_fBVal;
    float        m_fCVal;
    unsigned int m_iLobes;

protected:
    Image m_templ;

    IppiResizeSpec_32f *m_pSpec;
    unsigned char      *m_pInitBuffer;
};

#ifdef USE_TBB
class ResizeTBB : public Resize
{
public:
    ResizeTBB(): m_scheduler(task_scheduler_init::deferred)
    {
        m_iGrainX  = 0;
        m_iGrainY  = 0;
    }

    Status Init(Image *pSrcImage, Image *pDstImage)
    {
        Status status;

        status = Resize::Init(pSrcImage, pDstImage);
        CHECK_STATUS_PRINT_RS(status, "Resize::Init()", GetBaseStatusString(status));

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

        m_task.m_pSrcData = pSrcImage;
        m_task.m_pDstData = pDstImage;
        m_task.m_pResize  = this;

        return STS_OK;
    }

    Status ResizeImage(Image *pSrcImage, Image *pDstImage)
    {
        if(!pSrcImage || !pDstImage)
            return STS_ERR_NULL_PTR;

        if(m_templ != *pSrcImage)
        {
            Status status = Init(pSrcImage, pDstImage);
            CHECK_STATUS_PRINT_RS(status, "Resize::Init()", GetBaseStatusString(status));
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
    class ResizeTBBTask
    {
    public:
        ResizeTBBTask() {};

        void operator()(blocked_range2d<unsigned int, unsigned int> &r) const
        {
            IppStatus ippSts;
            Status    status;
            Rect      roi         = {r.cols().begin(), r.rows().begin(), r.cols().end() - r.cols().begin(), r.rows().end() - r.rows().begin()};
            IppiSize  dstRoiSize  = {roi.width, roi.height};
            IppiBorderType border = ippBorderRepl;

            unsigned char *pBuffer = 0;
            int iBufferSize;

            ippSts = ippiResizeGetBufferSize_8u(m_pResize->m_pSpec, dstRoiSize, m_pSrcData->m_iSamples, &iBufferSize);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiResizeGetBufferSize_8u()", ippGetStatusString(ippSts), throw(STS_ERR_FAILED));

            pBuffer = (unsigned char*)m_pResize->m_memPool.malloc(iBufferSize);
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

            status = m_pResize->ResizeBlock(m_pSrcData, m_pDstData, roi, border, pBuffer);
            if(status != STS_OK)
                throw(status);

            m_pResize->m_memPool.free(pBuffer);
        }

        ResizeTBB *m_pResize;
        Image     *m_pSrcData;
        Image     *m_pDstData;
    };

public:
    unsigned int m_iGrainX;
    unsigned int m_iGrainY;

private:
    ResizeTBBTask       m_task;
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
    unsigned int iDstSize[2]    = {0, 0};
#if defined(USE_TBB)
    unsigned int iThreads       = 0;
#else
    unsigned int iThreads       = 1;
#endif
    unsigned int iInter         = 1;
    bool         bNoAspect      = false;
    bool         bNoWindow      = false;
    bool         bPrintHelp     = false;

    // Cubic specific values
    float  fBVal   = 1;
    float  fCVal   = 0;

    // Lanczos specific values
    unsigned int  iLobes  = 3;

    Image srcData;
    Image dstData;
    IppiInterpolationType interpolation;
    Resize *pResize = 0;

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
        { 'r', 2, KT_POSITIVE,  0,            &iDstSize[0],     "destination resolution (width height)" },
        { 'k', 1, KT_BOOL,      0,            &bNoAspect,       "do not keep aspect ratio" },
#if defined(USE_TBB)
        { 't', 1, KT_INTEGER,   0,            &iThreads,        "number of threads (0 - auto, 0 by default)" },
#endif
        { 'p', 1, KT_INTEGER,   0,            &iInter,          "interpolation:|0 - Nearest|1 - Linear (default)|2 - Cubic|3 - Lanczos" },
        { 's', 1, KT_BOOL,      0,            &bNoWindow,       "suppress window output" },
        { 'w', 1, KT_DOUBLE,    0,            &fTimeLimit,      "minimum test time in milliseconds" },
        { 'l', 1, KT_POSITIVE,  0,            &iLoopsLimit,     "number of loops (overrides test time)" },
        { 'T', 1, KT_STRING,    0,            &sIppCpu,         "target IPP optimization (SSE, SSE2, SSE3, SSSE3, SSE41, SSE42, AES, AVX, AVX2)" },
        { 'h', 1, KT_BOOL,      0,            &bPrintHelp,      "print help and exit" },
        {0}
    };

    if (OptParse(argc, argv, cmdOpts))
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

    if(iInter == CMD_INTER_NN)
        interpolation = ippNearest;
    else if(iInter == CMD_INTER_LIN)
        interpolation = ippLinear;
    else if(iInter == CMD_INTER_CUB)
        interpolation = ippCubic;
    else if(iInter == CMD_INTER_LAN)
    {
        interpolation = ippLanczos;
        if(iLobes != 2 && iLobes != 3)
        {
            printHelp(cmdOpts, argv);
            PRINT_MESSAGE("Invalid number of lobes");
            return 1;
        }
    }
    else
    {
        printHelp(cmdOpts, argv);
        PRINT_MESSAGE("Invalid interpolation type");
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
        if(!iDstSize[0])
            iDstSize[0] = srcData.m_iWidth/2;
        if(!iDstSize[1])
            iDstSize[1] = srcData.m_iHeight/2;

        if(!iDstSize[0] || !iDstSize[1])
        {
            PRINT_MESSAGE("Invalid output resolution");
            return 1;
        }

        dstData           = srcData;
        dstData.m_iWidth  = iDstSize[0];
        dstData.m_iHeight = iDstSize[1];

        if(!bNoAspect)
        {
            double fXRatio  = (double)dstData.m_iWidth/srcData.m_iWidth;
            dstData.m_iHeight = (unsigned int)ROUND0(srcData.m_iHeight*fXRatio);
            if(!dstData.m_iHeight)
                dstData.m_iHeight = 1;
        }

        status = dstData.Alloc();
        CHECK_STATUS_PRINT_BR(status, "Image::Alloc()", GetBaseStatusString(status));

        printf("\nOutput file: %s\n", (sOutputFile)?sOutputFile:"-");
        printf("Output info: %dx%d %s\n", dstData.m_iWidth, dstData.m_iHeight, colorName[dstData.m_color]);

        if(iThreads == 1)
        {
            printf("\nSequential resize\n");
            pResize = new Resize;
            if(!pResize)
            {
                PRINT_MESSAGE("Failed to allocate sequential resize class");
                return 1;
            }
        }
#ifdef USE_TBB
        else
        {
            printf("\nTBB resize\n");
            pResize = new ResizeTBB;
            if(!pResize)
            {
                PRINT_MESSAGE("Failed to allocate TBB resize class");
                return 1;
            }
            pResize->m_iThreads = iThreads;
        }
#endif

        // pre-init
        pResize->m_interpolation = interpolation;
        pResize->m_fBVal         = fBVal;
        pResize->m_fCVal         = fCVal;
        pResize->m_iLobes        = iLobes;

        status = pResize->Init(&srcData, &dstData);
        if(STS_OK != status)
            delete pResize;
        CHECK_STATUS_PRINT_RS(status, "Resize::Init()", GetBaseStatusString(status));

        if(iThreads != 1)
        {
            iThreads = pResize->m_iThreads;
            printf("Threads: %d\n", iThreads);
        }

        printf("\nInterpolation: ");
        if(iInter == CMD_INTER_NN)
            printf("nearest\n");
        else if(iInter == CMD_INTER_LIN)
            printf("linear\n");
        else if(iInter == CMD_INTER_CUB)
            printf("cubic (B = %.2f; C = %.2f)\n", fBVal, fCVal);
        else if(iInter == CMD_INTER_LAN)
            printf("Lanczos (L = %d)\n", iLobes);

        for(iLoops = 1, tickAcc = 0;; iLoops++)
        {
            tickStart = vm_time_get_tick();
            status = pResize->ResizeImage(&srcData, &dstData);
            tickAcc += (vm_time_get_tick() - tickStart);

            CHECK_STATUS_PRINT_BR(status, "Resize::ResizeImage()", GetBaseStatusString(status));

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
            WndDesc *pDesc = WindowNew("IPP Resize example", WF_FIT_TO_IMAGE);
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

    if(pResize) delete pResize;
    if(status < 0)
        return status;
    return 0;
}
