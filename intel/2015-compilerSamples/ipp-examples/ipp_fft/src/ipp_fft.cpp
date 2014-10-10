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

#if (defined _DEBUG && defined _WIN32)
#define _CRTDBG_MAP_ALLOC
#include <crtdbg.h>
#endif

#include <math.h>
#include <memory>

#include "base.h"
#include "base_image.h"
#include "base_ipp.h"
#include "base_renderer.h"

#include "ippcore.h"
#include "ipps.h"
#include "ippi.h"

#if defined(USE_TBB)
#include "tbb/parallel_for.h"
#endif

#define CMD_METHOD_FFT 0
#define CMD_METHOD_DFT 1
#define CMD_METHOD_DCT 2


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

class Transform
{
public:
    Transform()
    {
        m_ticks = 0;

        m_iBlockWidth  = 0;
        m_iBlockHeight = 0;
        m_iBlocksX = 1;
        m_iBlocksY = 1;

        m_iFlags = IPP_FFT_DIV_FWD_BY_N;
        m_hint   = ippAlgHintNone;
    }

    virtual ~Transform()
    {
    
    }

    virtual Status Init(Image *pSrcImage, Image *pDstImage) = 0;
    virtual Status Forward(Image *pSrcImage, Image *pDstImage) = 0;
    virtual Status Inverse(Image *pSrcImage, Image *pDstImage) = 0;

#if defined(USE_TBB)
    virtual void operator() (const tbb::blocked_range<int>& range) const = 0;
#endif

protected:
    Status Convert8u32f(Image *pSrcData)
    {
        IppStatus ippSts;
        IppiSize roi = {static_cast<int>(pSrcData->m_iWidth), static_cast<int>(pSrcData->m_iHeight)};

        if(pSrcData->m_iSamples == 1)
        {
            ippSts = ippiConvert_8u32f_C1R(pSrcData->m_pPointer, pSrcData->m_iStep, (Ipp32f*)m_tmpData32f.m_pPointer, m_tmpData32f.m_iStep, roi);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiConvert_8u32f_C1R()", ippGetStatusString(ippSts), return STS_ERR_FAILED);
        }
        else if(pSrcData->m_iSamples == 3)
        {
            ippSts = ippiConvert_8u32f_C3R(pSrcData->m_pPointer, pSrcData->m_iStep, (Ipp32f*)m_tmpData32f.m_pPointer, m_tmpData32f.m_iStep, roi);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiConvert_8u32f_C3R()", ippGetStatusString(ippSts), return STS_ERR_FAILED);
        }
        else if(pSrcData->m_iSamples == 4)
        {
            ippSts = ippiConvert_8u32f_C4R(pSrcData->m_pPointer, pSrcData->m_iStep, (Ipp32f*)m_tmpData32f.m_pPointer, m_tmpData32f.m_iStep, roi);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiConvert_8u32f_C4R()", ippGetStatusString(ippSts), return STS_ERR_FAILED);
        }
        else
        {
            PRINT_MESSAGE("Invalid image format");
            return STS_ERR_FAILED;
        }

        return STS_OK;
    }

    Status Convert32f8u(Image *pDstData)
    {
        IppStatus ippSts;
        IppiSize roi = {static_cast<int>(pDstData->m_iWidth), static_cast<int>(pDstData->m_iHeight)};

        if(pDstData->m_iSamples == 1)
        {
            ippSts = ippiConvert_32f8u_C1R((Ipp32f*)m_tmpData32f.m_pPointer, m_tmpData32f.m_iStep, pDstData->m_pPointer, pDstData->m_iStep, roi, ippRndNear);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiConvert_32f8u_C1R()", ippGetStatusString(ippSts), return STS_ERR_FAILED);
        }
        else if(pDstData->m_iSamples == 3)
        {
            ippSts = ippiConvert_32f8u_C3R((Ipp32f*)m_tmpData32f.m_pPointer, m_tmpData32f.m_iStep, pDstData->m_pPointer, pDstData->m_iStep, roi, ippRndNear);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiConvert_32f8u_C3R()", ippGetStatusString(ippSts), return STS_ERR_FAILED);
        }
        else if(pDstData->m_iSamples == 4)
        {
            ippSts = ippiConvert_32f8u_C4R((Ipp32f*)m_tmpData32f.m_pPointer, m_tmpData32f.m_iStep, pDstData->m_pPointer, pDstData->m_iStep, roi, ippRndNear);
            CHECK_STATUS_PRINT_AC(ippSts, "ippiConvert_32f8u_C4R()", ippGetStatusString(ippSts), return STS_ERR_FAILED);
        }
        else
        {
            PRINT_MESSAGE("Invalid image format");
            return STS_ERR_FAILED;
        }

        return STS_OK;
    }

public:
    vm_tick          m_ticks;
    int              m_iFlags;
    IppHintAlgorithm m_hint;

    unsigned int m_iBlockWidth;
    unsigned int m_iBlockHeight;
    unsigned int m_iBlocksX;
    unsigned int m_iBlocksY;

protected:
    Image   m_templ;
    Image   m_tmpData32f;
#if defined(USE_TBB)
    Image*  m_tbbPsrcImage;
    Image*  m_tbbPdstImage;
#endif
};

class FFT : public Transform
{
public:
    FFT()
    {
        m_iOrderX = 0;
        m_iOrderY = 0;
        m_pTrSpec = 0;
        m_pTrInit = 0;
        m_pTrBuffer = 0;
    }

    ~FFT()
    {
        Close();
    }

    void Close()
    {
        if(m_pTrBuffer)
        {
            ippsFree(m_pTrBuffer);
            m_pTrBuffer = 0;
        }

        if(m_pTrInit)
        {
            ippsFree(m_pTrInit);
            m_pTrInit = 0;
        }

        if(m_pTrSpec)
        {
            ippsFree(m_pTrSpec);
            m_pTrSpec = 0;
        }
    }

    Status Init(Image *pSrcImage, Image *pDstImage)
    {
        Status      status;
        IppStatus   ippSts;
        int         iTrBufferSize;
        int         iTrSpecSize;
        int         iTrInitSize;

        if(!pSrcImage || !pSrcImage->m_pPointer || !pDstImage)
            return STS_ERR_NULL_PTR;

        if(pSrcImage->m_iSamples != 1 && pSrcImage->m_iSamples != 3 && pSrcImage->m_iSamples != 4)
            return STS_ERR_INVALID_PARAMS;

        Close();

        // prepare internal coefficients buffer
        m_tmpData32f = *pSrcImage;
        m_tmpData32f.m_iSampleSize = 4;

        m_iOrderX = m_iOrderY = 0;

        // process image in blocks
        if(m_iBlockWidth && m_iBlockHeight)
        {
            // check for block size to be power of 2
            while(m_iBlockWidth > 1)
            {
                m_iBlockWidth >>= 1;
                m_iOrderX++;
            }
            while(m_iBlockHeight > 1)
            {
                m_iBlockHeight >>= 1;
                m_iOrderY++;
            }

            m_iBlockWidth  <<= m_iOrderX;
            m_iBlockHeight <<= m_iOrderY;

            // align buffer on block border
            m_iBlocksX = (m_tmpData32f.m_iWidth + m_iBlockWidth - 1)/m_iBlockWidth;
            m_iBlocksY = (m_tmpData32f.m_iHeight + m_iBlockHeight - 1)/m_iBlockHeight;
            m_tmpData32f.m_iWidth  = m_iBlocksX*m_iBlockWidth;
            m_tmpData32f.m_iHeight = m_iBlocksY*m_iBlockHeight;
        }
        else
        {
            // fft requires power of 2 image
            m_tmpData32f.m_iWidth = m_tmpData32f.m_iHeight = 1;

            while(m_tmpData32f.m_iWidth < pSrcImage->m_iWidth)
            {
                m_tmpData32f.m_iWidth <<= 1;
                m_iOrderX++;
            }
            while(m_tmpData32f.m_iHeight < pSrcImage->m_iHeight)
            {
                m_tmpData32f.m_iHeight <<= 1;
                m_iOrderY++;
            }
        }

        status = m_tmpData32f.Alloc();
        CHECK_STATUS_PRINT_RS(status, "Image::Alloc()", GetBaseStatusString(status));
        memset(m_tmpData32f.m_pPointer, 0, m_tmpData32f.m_iBufferSize);

        *pDstImage = m_tmpData32f;
        status = pDstImage->Alloc();
        CHECK_STATUS_PRINT_RS(status, "Image::Alloc()", GetBaseStatusString(status));

        // Retrieve required FFT spec/buffer sizes
        ippSts = ippiFFTGetSize_R_32f(m_iOrderX, m_iOrderY, m_iFlags, m_hint, &iTrSpecSize, &iTrInitSize, &iTrBufferSize);
        CHECK_STATUS_PRINT_AC(ippSts, "ippiFFTGetSize_R_32f()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

        // Allocate required buffers
        m_pTrSpec = (IppiFFTSpec_R_32f*)ippsMalloc_8u(iTrSpecSize);
        if(!m_pTrSpec)
        {
            PRINT_MESSAGE("Cannot allocate memory for FFT spec buffer");
            return STS_ERR_ALLOC;
        }

        if(iTrInitSize)
        {
            m_pTrInit = ippsMalloc_8u(iTrInitSize);
            if(!m_pTrInit)
            {
                PRINT_MESSAGE("Cannot allocate memory for FFT init buffer");
                return STS_ERR_ALLOC;
            }
        }

        // Allocate secondary buffer for FFT
        m_pTrBuffer = ippsMalloc_8u(iTrBufferSize);
        if(!m_pTrBuffer)
        {
            PRINT_MESSAGE("Cannot allocate memory for FFT buffer");
            return STS_ERR_ALLOC;
        }

        ippSts = ippiFFTInit_R_32f(m_iOrderX, m_iOrderY, m_iFlags, m_hint, m_pTrSpec, m_pTrInit);
        CHECK_STATUS_PRINT_AC(ippSts, "ippiFFTInit_R_32f()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

        m_templ = *pSrcImage;

        return STS_OK;
    }

    Status Forward(Image *pSrcImage, Image *pDstImage)
    {
        Status    status;
        IppStatus ippSts = ippStsNoErr;
        vm_tick   tickStart;

        float *pPtrSrc;
        float *pPtrDst;
        unsigned int i, j;

        if(!pSrcImage || !pDstImage)
            return STS_ERR_NULL_PTR;

        if(m_templ != *pSrcImage)
        {
            status = Init(pSrcImage, pDstImage);
            CHECK_STATUS_PRINT_RS(status, "Transform::Init()", GetBaseStatusString(status));
        }

        status = Convert8u32f(pSrcImage);
        CHECK_STATUS_PRINT_RS(status, "Transform::Convert8u32f()", GetBaseStatusString(status));

        tickStart = vm_time_get_tick();

#if !defined(USE_TBB)
        for(i = 0; i < m_iBlocksY; i++)
        {
            pPtrSrc = (Ipp32f*)(m_tmpData32f.m_pPointer + m_tmpData32f.m_iStep*m_iBlockHeight*i);
            pPtrDst = (Ipp32f*)(pDstImage->m_pPointer + pDstImage->m_iStep*m_iBlockHeight*i);

            for(j = 0; j < m_iBlocksX; j++)
            {
                if(pSrcImage->m_iSamples == 1)
                    ippSts = ippiFFTFwd_RToPack_32f_C1R(pPtrSrc, m_tmpData32f.m_iStep, pPtrDst, pDstImage->m_iStep, m_pTrSpec, m_pTrBuffer);
                else if(pSrcImage->m_iSamples == 3)
                    ippSts = ippiFFTFwd_RToPack_32f_C3R(pPtrSrc, m_tmpData32f.m_iStep, pPtrDst, pDstImage->m_iStep, m_pTrSpec, m_pTrBuffer);
                else if(pSrcImage->m_iSamples == 4)
                    ippSts = ippiFFTFwd_RToPack_32f_C4R(pPtrSrc, m_tmpData32f.m_iStep, pPtrDst, pDstImage->m_iStep, m_pTrSpec, m_pTrBuffer);

                pPtrSrc += m_tmpData32f.m_iSamples*m_iBlockWidth;
                pPtrDst += pDstImage->m_iSamples*m_iBlockWidth;
            }
        }
#else
        m_tbbPsrcImage = pSrcImage;
        m_tbbPdstImage = pDstImage;
        tbb::parallel_for(tbb::blocked_range<int>(1, m_iBlocksY), *this);
#endif
        m_ticks = (vm_time_get_tick() - tickStart);
        CHECK_STATUS_PRINT_AC(ippSts, "ippiFFTFwd_RToPack_32f_CXR()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

        return STS_OK;
    }

    Status Inverse(Image *pSrcImage, Image *pDstImage)
    {
        Status    status;
        IppStatus ippSts = ippStsNoErr;
        vm_tick   tickStart;

        float *pPtrSrc;
        float *pPtrDst;
        unsigned int i, j;

        if(!pSrcImage || !pDstImage)
            return STS_ERR_NULL_PTR;

        // Forward method must be used first
        if(m_templ != *pDstImage)
        {
            PRINT_MESSAGE("Images structure mismatch");
            return STS_ERR_FAILED;
        }

        memset(m_tmpData32f.m_pPointer, 0, m_tmpData32f.m_iBufferSize);

        tickStart = vm_time_get_tick();

        for(i = 0; i < m_iBlocksY; i++)
        {
            pPtrSrc = (Ipp32f*)(pSrcImage->m_pPointer + pSrcImage->m_iStep*m_iBlockHeight*i);
            pPtrDst = (Ipp32f*)(m_tmpData32f.m_pPointer + m_tmpData32f.m_iStep*m_iBlockHeight*i);

            for(j = 0; j < m_iBlocksX; j++)
            {
                if(pSrcImage->m_iSamples == 1)
                    ippSts = ippiFFTInv_PackToR_32f_C1R(pPtrSrc, pSrcImage->m_iStep, pPtrDst, m_tmpData32f.m_iStep, m_pTrSpec, m_pTrBuffer);
                else if(pSrcImage->m_iSamples == 3)
                    ippSts = ippiFFTInv_PackToR_32f_C3R(pPtrSrc, pSrcImage->m_iStep, pPtrDst, m_tmpData32f.m_iStep, m_pTrSpec, m_pTrBuffer);
                else if(pSrcImage->m_iSamples == 4)
                    ippSts = ippiFFTInv_PackToR_32f_C4R(pPtrSrc, pSrcImage->m_iStep, pPtrDst, m_tmpData32f.m_iStep, m_pTrSpec, m_pTrBuffer);

                pPtrSrc += pSrcImage->m_iSamples*m_iBlockWidth;
                pPtrDst += m_tmpData32f.m_iSamples*m_iBlockWidth;
            }
        }

        m_ticks = (vm_time_get_tick() - tickStart);
        CHECK_STATUS_PRINT_AC(ippSts, "ippiFFTInv_PackToR_32f_CXR()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

        status = Convert32f8u(pDstImage);
        CHECK_STATUS_PRINT_RS(status, "Transform::Convert32f8u()", GetBaseStatusString(status));

        return STS_OK;
    }

#if defined(USE_TBB)
    void operator()(const tbb::blocked_range<int>& range) const
    {
        Ipp32f *pPtrSrc, *pPtrDst;
        IppStatus ippSts;

        pPtrSrc = (Ipp32f*)(m_tmpData32f.m_pPointer + m_tmpData32f.m_iStep * range.begin());
        pPtrDst = (Ipp32f*)(m_tbbPdstImage->m_pPointer + m_tbbPdstImage->m_iStep*range.begin());

        for(int j = 0; j < m_iBlocksX; j++)
        {
            if(m_tbbPsrcImage->m_iSamples == 1)
                ippSts = ippiFFTFwd_RToPack_32f_C1R(pPtrSrc, m_tmpData32f.m_iStep, pPtrDst, m_tbbPdstImage->m_iStep, m_pTrSpec, m_pTrBuffer);
            else if(m_tbbPsrcImage->m_iSamples == 3)
                ippSts = ippiFFTFwd_RToPack_32f_C3R(pPtrSrc, m_tmpData32f.m_iStep, pPtrDst, m_tbbPdstImage->m_iStep, m_pTrSpec, m_pTrBuffer);
            else if(m_tbbPsrcImage->m_iSamples == 4)
                ippSts = ippiFFTFwd_RToPack_32f_C4R(pPtrSrc, m_tmpData32f.m_iStep, pPtrDst, m_tbbPdstImage->m_iStep, m_pTrSpec, m_pTrBuffer);

            pPtrSrc += m_tmpData32f.m_iSamples*m_iBlockWidth;
            pPtrDst += m_tbbPdstImage->m_iSamples*m_iBlockWidth;
        }
    }
#endif

private:
    unsigned int       m_iOrderX;
    unsigned int       m_iOrderY;
    IppiFFTSpec_R_32f *m_pTrSpec;
    unsigned char     *m_pTrInit;
#if !defined(USE_TBB)
    unsigned char     *m_pTrBuffer;
#else
    unsigned char     **m_
#endif
};

class DFT : public Transform
{
public:
    DFT()
    {
        m_pTrSpec = 0;
        m_pTrInit = 0;
        m_pTrBuffer = 0;
    }

    ~DFT()
    {
        Close();
    }

    void Close()
    {
        if(m_pTrBuffer)
        {
            ippsFree(m_pTrBuffer);
            m_pTrBuffer = 0;
        }

        if(m_pTrInit)
        {
            ippsFree(m_pTrInit);
            m_pTrInit = 0;
        }

        if(m_pTrSpec)
        {
            ippsFree(m_pTrSpec);
            m_pTrSpec = 0;
        }
    }

    Status Init(Image *pSrcImage, Image *pDstImage)
    {
        Status      status;
        IppStatus   ippSts;
        int         iTrBufferSize;
        int         iTrSpecSize;
        int         iTrInitSize;

        if(!pSrcImage || !pSrcImage->m_pPointer || !pDstImage)
            return STS_ERR_NULL_PTR;

        if(pSrcImage->m_iSamples != 1 && pSrcImage->m_iSamples != 3 && pSrcImage->m_iSamples != 4)
            return STS_ERR_INVALID_PARAMS;

        Close();

        m_roi.width  = pSrcImage->m_iWidth;
        m_roi.height = pSrcImage->m_iHeight;

        // prepare internal coefficients buffer
        m_tmpData32f = *pSrcImage;
        m_tmpData32f.m_iSampleSize = 4;

        // process image in blocks
        if(m_iBlockWidth && m_iBlockHeight)
        {
            unsigned int iOrderX = 0;
            unsigned int iOrderY = 0;

            // check for block size to be power of 2
            while(m_iBlockWidth > 1)
            {
                m_iBlockWidth >>= 1;
                iOrderX++;
            }
            while(m_iBlockHeight > 1)
            {
                m_iBlockHeight >>= 1;
                iOrderY++;
            }

            m_iBlockWidth  <<= iOrderX;
            m_iBlockHeight <<= iOrderY;

            // align buffer on block border
            m_iBlocksX = (m_tmpData32f.m_iWidth + m_iBlockWidth)/m_iBlockWidth;
            m_iBlocksY = (m_tmpData32f.m_iHeight + m_iBlockHeight)/m_iBlockHeight;
            m_tmpData32f.m_iWidth  = m_iBlocksX*m_iBlockWidth;
            m_tmpData32f.m_iHeight = m_iBlocksY*m_iBlockHeight;

            m_roi.width  = m_iBlockWidth;
            m_roi.height = m_iBlockHeight;
        }

        status = m_tmpData32f.Alloc();
        CHECK_STATUS_PRINT_RS(status, "Image::Alloc()", GetBaseStatusString(status));
        memset(m_tmpData32f.m_pPointer, 0, m_tmpData32f.m_iBufferSize);

        *pDstImage = m_tmpData32f;
        status = pDstImage->Alloc();
        CHECK_STATUS_PRINT_RS(status, "Image::Alloc()", GetBaseStatusString(status));

        // Retrive required DFT spec/buffer sizes
        ippSts = ippiDFTGetSize_R_32f(m_roi, m_iFlags, m_hint, &iTrSpecSize, &iTrInitSize, &iTrBufferSize);
        CHECK_STATUS_PRINT_AC(ippSts, "ippiDFTGetSize_R_32f()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

        // Allocate required buffers
        m_pTrSpec = (IppiDFTSpec_R_32f*)ippsMalloc_8u(iTrSpecSize);
        if(!m_pTrSpec)
        {
            PRINT_MESSAGE("Cannot allocate memory for DFT spec buffer");
            return STS_ERR_ALLOC;
        }

        if(iTrInitSize)
        {
            m_pTrInit = ippsMalloc_8u(iTrInitSize);
            if(!m_pTrInit)
            {
                PRINT_MESSAGE("Cannot allocate memory for DFT init buffer");
                return STS_ERR_ALLOC;
            }
        }

        // Allocate secondary buffer for DFT
        m_pTrBuffer = ippsMalloc_8u(iTrBufferSize);
        if(!m_pTrBuffer)
        {
            PRINT_MESSAGE("Cannot allocate memory for DFT buffer");
            return STS_ERR_ALLOC;
        }

        ippSts = ippiDFTInit_R_32f(m_roi, m_iFlags, m_hint, m_pTrSpec, m_pTrInit);
        CHECK_STATUS_PRINT_AC(ippSts, "ippiDFTInit_R_32f()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

        m_templ = *pSrcImage;

        return STS_OK;
    }

    Status Forward(Image *pSrcImage, Image *pDstImage)
    {
        Status    status;
        IppStatus ippSts = ippStsNoErr;
        vm_tick   tickStart;

        float *pPtrSrc;
        float *pPtrDst;
        unsigned int i, j;

        if(!pSrcImage || !pDstImage)
            return STS_ERR_NULL_PTR;

        if(m_templ != *pSrcImage)
        {
            status = Init(pSrcImage, pDstImage);
            CHECK_STATUS_PRINT_RS(status, "Transform::Init()", GetBaseStatusString(status));
        }

        status = Convert8u32f(pSrcImage);
        CHECK_STATUS_PRINT_RS(status, "Transform::Convert8u32f()", GetBaseStatusString(status));

        tickStart = vm_time_get_tick();

#if !defined(USE_TBB)
        for(i = 0; i < m_iBlocksY; i++)
        {
            pPtrSrc = (Ipp32f*)(m_tmpData32f.m_pPointer + m_tmpData32f.m_iStep*m_iBlockHeight*i);
            pPtrDst = (Ipp32f*)(pDstImage->m_pPointer + pDstImage->m_iStep*m_iBlockHeight*i);

            for(j = 0; j < m_iBlocksX; j++)
            {
                if(pSrcImage->m_iSamples == 1)
                    ippSts = ippiDFTFwd_RToPack_32f_C1R(pPtrSrc, m_tmpData32f.m_iStep, pPtrDst, pDstImage->m_iStep, m_pTrSpec, m_pTrBuffer);
                else if(pSrcImage->m_iSamples == 3)
                    ippSts = ippiDFTFwd_RToPack_32f_C3R(pPtrSrc, m_tmpData32f.m_iStep, pPtrDst, pDstImage->m_iStep, m_pTrSpec, m_pTrBuffer);
                else if(pSrcImage->m_iSamples == 4)
                    ippSts = ippiDFTFwd_RToPack_32f_C4R(pPtrSrc, m_tmpData32f.m_iStep, pPtrDst, pDstImage->m_iStep, m_pTrSpec, m_pTrBuffer);

                pPtrSrc += m_tmpData32f.m_iSamples*m_iBlockWidth;
                pPtrDst += pDstImage->m_iSamples*m_iBlockWidth;
            }
        }
#else
        m_tbbPsrcImage = pSrcImage;
        m_tbbPdstImage = pDstImage;
        tbb::parallel_for(tbb::blocked_range<int>(1, m_iBlocksY), *this);
#endif

        m_ticks = (vm_time_get_tick() - tickStart);
        CHECK_STATUS_PRINT_AC(ippSts, "ippiDFTFwd_RToPack_32f_CXR()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

        return STS_OK;
    }

    Status Inverse(Image *pSrcImage, Image *pDstImage)
    {
        Status    status;
        IppStatus ippSts = ippStsNoErr;
        vm_tick   tickStart;

        float *pPtrSrc;
        float *pPtrDst;
        unsigned int i, j;

        if(!pSrcImage || !pDstImage)
            return STS_ERR_NULL_PTR;

        // Forward method must be used first
        if(m_templ != *pDstImage)
        {
            PRINT_MESSAGE("Images structure mismatch");
            return STS_ERR_FAILED;
        }

        memset(m_tmpData32f.m_pPointer, 0, m_tmpData32f.m_iBufferSize);

        tickStart = vm_time_get_tick();

        for(i = 0; i < m_iBlocksY; i++)
        {
            pPtrSrc = (Ipp32f*)(pSrcImage->m_pPointer + pSrcImage->m_iStep*m_iBlockHeight*i);
            pPtrDst = (Ipp32f*)(m_tmpData32f.m_pPointer + m_tmpData32f.m_iStep*m_iBlockHeight*i);

            for(j = 0; j < m_iBlocksX; j++)
            {
                if(pSrcImage->m_iSamples == 1)
                    ippSts = ippiDFTInv_PackToR_32f_C1R(pPtrSrc, pSrcImage->m_iStep, pPtrDst, m_tmpData32f.m_iStep, m_pTrSpec, m_pTrBuffer);
                else if(pSrcImage->m_iSamples == 3)
                    ippSts = ippiDFTInv_PackToR_32f_C3R(pPtrSrc, pSrcImage->m_iStep, pPtrDst, m_tmpData32f.m_iStep, m_pTrSpec, m_pTrBuffer);
                else if(pSrcImage->m_iSamples == 4)
                    ippSts = ippiDFTInv_PackToR_32f_C4R(pPtrSrc, pSrcImage->m_iStep, pPtrDst, m_tmpData32f.m_iStep, m_pTrSpec, m_pTrBuffer);

                pPtrSrc += pSrcImage->m_iSamples*m_iBlockWidth;
                pPtrDst += m_tmpData32f.m_iSamples*m_iBlockWidth;
            }
        }

        m_ticks = (vm_time_get_tick() - tickStart);
        CHECK_STATUS_PRINT_AC(ippSts, "ippiDFTInv_PackToR_32f_CXR()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

        status = Convert32f8u(pDstImage);
        CHECK_STATUS_PRINT_RS(status, "Transform::Convert32f8u()", GetBaseStatusString(status));

        return STS_OK;
    }

#if defined(USE_TBB)
    void operator()(const tbb::blocked_range<int>& range) const;
#endif

private:
    IppiSize           m_roi;
    IppiDFTSpec_R_32f *m_pTrSpec;
    unsigned char     *m_pTrInit;
    unsigned char     *m_pTrBuffer;
};

class DCT : public Transform
{
public:
    DCT()
    {
        m_pTrFwdSpec = 0;
        m_pTrInvSpec = 0;
        m_pTrFwdInit = 0;
        m_pTrInvInit = 0;
        m_pTrFwdBuffer = 0;
        m_pTrInvBuffer = 0;
    }

    ~DCT()
    {
        Close();
    }

    void Close()
    {
        if(m_pTrFwdBuffer)
        {
            ippsFree(m_pTrFwdBuffer);
            m_pTrFwdBuffer = 0;
        }

        if(m_pTrInvBuffer)
        {
            ippsFree(m_pTrInvBuffer);
            m_pTrInvBuffer = 0;
        }

        if(m_pTrFwdInit)
        {
            ippsFree(m_pTrFwdInit);
            m_pTrFwdInit = 0;
        }

        if(m_pTrInvInit)
        {
            ippsFree(m_pTrInvInit);
            m_pTrInvInit = 0;
        }

        if(m_pTrFwdSpec)
        {
            ippsFree(m_pTrFwdSpec);
            m_pTrFwdSpec = 0;
        }

        if(m_pTrInvSpec)
        {
            ippsFree(m_pTrInvSpec);
            m_pTrInvSpec = 0;
        }
    }

    Status Init(Image *pSrcImage, Image *pDstImage)
    {
        Status      status;
        IppStatus   ippSts;
        int         iFwdSizeSpec, iFwdSizeInit, iFwdSizeBuf;
        int         iInvSizeSpec, iInvSizeInit, iInvSizeBuf;

        if(!pSrcImage || !pSrcImage->m_pPointer || !pDstImage)
            return STS_ERR_NULL_PTR;

        if(pSrcImage->m_iSamples != 1 && pSrcImage->m_iSamples != 3 && pSrcImage->m_iSamples != 4)
            return STS_ERR_INVALID_PARAMS;

        Close();

        m_roi.width  = pSrcImage->m_iWidth;
        m_roi.height = pSrcImage->m_iHeight;

        // prepare internal coefficients buffer
        m_tmpData32f = *pSrcImage;
        m_tmpData32f.m_iSampleSize = 4;

        // process image in blocks
        if(m_iBlockWidth && m_iBlockHeight)
        {
            unsigned int iOrderX = 0;
            unsigned int iOrderY = 0;

            // check for block size to be power of 2
            while(m_iBlockWidth > 1)
            {
                m_iBlockWidth >>= 1;
                iOrderX++;
            }
            while(m_iBlockHeight > 1)
            {
                m_iBlockHeight >>= 1;
                iOrderY++;
            }

            m_iBlockWidth  <<= iOrderX;
            m_iBlockHeight <<= iOrderY;

            // align buffer on block border
            m_iBlocksX = (m_tmpData32f.m_iWidth + m_iBlockWidth)/m_iBlockWidth;
            m_iBlocksY = (m_tmpData32f.m_iHeight + m_iBlockHeight)/m_iBlockHeight;
            m_tmpData32f.m_iWidth  = m_iBlocksX*m_iBlockWidth;
            m_tmpData32f.m_iHeight = m_iBlocksY*m_iBlockHeight;

            m_roi.width  = m_iBlockWidth;
            m_roi.height = m_iBlockHeight;
        }

        status = m_tmpData32f.Alloc();
        CHECK_STATUS_PRINT_RS(status, "Image::Alloc()", GetBaseStatusString(status));
        memset(m_tmpData32f.m_pPointer, 0, m_tmpData32f.m_iBufferSize);

        *pDstImage = m_tmpData32f;
        status = pDstImage->Alloc();
        CHECK_STATUS_PRINT_RS(status, "Image::Alloc()", GetBaseStatusString(status));

        // Allocate DCT specs
        ippSts = ippiDCTFwdGetSize_32f(m_roi, &iFwdSizeSpec, &iFwdSizeInit, &iFwdSizeBuf);
        CHECK_STATUS_PRINT_AC(ippSts, "ippiDCTFwdGetSize_32f()", ippGetStatusString(ippSts), return STS_ERR_FAILED);
        m_pTrFwdSpec = (IppiDCTFwdSpec_32f*)ippsMalloc_8u(iFwdSizeSpec);
        m_pTrFwdInit = ippsMalloc_8u(iFwdSizeInit);
        m_pTrFwdBuffer = ippsMalloc_8u(iFwdSizeBuf);
        if ((NULL == m_pTrFwdSpec && iFwdSizeSpec > 0) ||
            (NULL == m_pTrFwdInit && iFwdSizeInit > 0) || 
            (NULL == m_pTrFwdBuffer && iFwdSizeBuf > 0)) {
            PRINT_MESSAGE("Cannot allocate memory for Fwd DCT");
            return STS_ERR_ALLOC;
        }

        ippSts = ippiDCTFwdInit_32f(m_pTrFwdSpec, m_roi, m_pTrFwdInit);
        CHECK_STATUS_PRINT_AC(ippSts, "ippiDCTFwdInit_32f()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

        ippSts = ippiDCTInvGetSize_32f(m_roi, &iInvSizeSpec, &iInvSizeInit, &iInvSizeBuf);
        CHECK_STATUS_PRINT_AC(ippSts, "ippiDCTInvGetSize_32f()", ippGetStatusString(ippSts), return STS_ERR_FAILED);
        m_pTrInvSpec = (IppiDCTInvSpec_32f*)ippsMalloc_8u(iInvSizeSpec);
        m_pTrInvInit = ippsMalloc_8u(iInvSizeInit);
        m_pTrInvBuffer = ippsMalloc_8u(iInvSizeBuf);
        if ((NULL == m_pTrInvSpec && iInvSizeSpec  > 0) || 
            (NULL == m_pTrInvInit && iInvSizeInit  > 0) || 
            (NULL == m_pTrInvBuffer && iInvSizeBuf > 0)) {
            PRINT_MESSAGE("Cannot allocate memory for Inv DCT");
            return STS_ERR_ALLOC;
        }

        ippSts = ippiDCTInvInit_32f(m_pTrInvSpec, m_roi, m_pTrInvInit);
        CHECK_STATUS_PRINT_AC(ippSts, "ippiDCTInvInit_32f()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

        m_templ = *pSrcImage;

        return STS_OK;
    }

    Status Forward(Image *pSrcImage, Image *pDstImage)
    {
        Status    status;
        IppStatus ippSts = ippStsNoErr;
        vm_tick   tickStart;

        float *pPtrSrc;
        float *pPtrDst;
        unsigned int i, j;

        if(!pSrcImage || !pDstImage)
            return STS_ERR_NULL_PTR;

        if(m_templ != *pSrcImage)
        {
            status = Init(pSrcImage, pDstImage);
            CHECK_STATUS_PRINT_RS(status, "Transform::Init()", GetBaseStatusString(status));
        }

        status = Convert8u32f(pSrcImage);
        CHECK_STATUS_PRINT_RS(status, "Transform::Convert8u32f()", GetBaseStatusString(status));

        tickStart = vm_time_get_tick();

        for(i = 0; i < m_iBlocksY; i++)
        {
            pPtrSrc = (Ipp32f*)(m_tmpData32f.m_pPointer + m_tmpData32f.m_iStep*m_iBlockHeight*i);
            pPtrDst = (Ipp32f*)(pDstImage->m_pPointer + pDstImage->m_iStep*m_iBlockHeight*i);

            for(j = 0; j < m_iBlocksX; j++)
            {
                if(pSrcImage->m_iSamples == 1)
                    ippSts = ippiDCTFwd_32f_C1R(pPtrSrc, m_tmpData32f.m_iStep, pPtrDst, pDstImage->m_iStep, m_pTrFwdSpec, m_pTrFwdBuffer);
                else if(pSrcImage->m_iSamples == 3)
                    ippSts = ippiDCTFwd_32f_C3R(pPtrSrc, m_tmpData32f.m_iStep, pPtrDst, pDstImage->m_iStep, m_pTrFwdSpec, m_pTrFwdBuffer);
                else if(pSrcImage->m_iSamples == 4)
                    ippSts = ippiDCTFwd_32f_C4R(pPtrSrc, m_tmpData32f.m_iStep, pPtrDst, pDstImage->m_iStep, m_pTrFwdSpec, m_pTrFwdBuffer);

                pPtrSrc += m_tmpData32f.m_iSamples*m_iBlockWidth;
                pPtrDst += pDstImage->m_iSamples*m_iBlockWidth;
            }
        }

        m_ticks = (vm_time_get_tick() - tickStart);
        CHECK_STATUS_PRINT_AC(ippSts, "ippiDCTFwd_32f_CXR()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

        return STS_OK;
    }

    Status Inverse(Image *pSrcImage, Image *pDstImage)
    {
        Status    status;
        IppStatus ippSts = ippStsNoErr;
        vm_tick   tickStart;

        float *pPtrSrc;
        float *pPtrDst;
        unsigned int i, j;

        if(!pSrcImage || !pDstImage)
            return STS_ERR_NULL_PTR;

        // Forward method must be used first
        if(m_templ != *pDstImage)
        {
            PRINT_MESSAGE("Images structure mismatch");
            return STS_ERR_FAILED;
        }

        memset(m_tmpData32f.m_pPointer, 0, m_tmpData32f.m_iBufferSize);

        tickStart = vm_time_get_tick();

        for(i = 0; i < m_iBlocksY; i++)
        {
            pPtrSrc = (Ipp32f*)(pSrcImage->m_pPointer + pSrcImage->m_iStep*m_iBlockHeight*i);
            pPtrDst = (Ipp32f*)(m_tmpData32f.m_pPointer + m_tmpData32f.m_iStep*m_iBlockHeight*i);

            for(j = 0; j < m_iBlocksX; j++)
            {
                if(pSrcImage->m_iSamples == 1)
                    ippSts = ippiDCTInv_32f_C1R(pPtrSrc, pSrcImage->m_iStep, pPtrDst, m_tmpData32f.m_iStep, m_pTrInvSpec, m_pTrFwdBuffer);
                else if(pSrcImage->m_iSamples == 3)
                    ippSts = ippiDCTInv_32f_C3R(pPtrSrc, pSrcImage->m_iStep, pPtrDst, m_tmpData32f.m_iStep, m_pTrInvSpec, m_pTrFwdBuffer);
                else if(pSrcImage->m_iSamples == 4)
                    ippSts = ippiDCTInv_32f_C4R(pPtrSrc, pSrcImage->m_iStep, pPtrDst, m_tmpData32f.m_iStep, m_pTrInvSpec, m_pTrFwdBuffer);

                pPtrSrc += pSrcImage->m_iSamples*m_iBlockWidth;
                pPtrDst += m_tmpData32f.m_iSamples*m_iBlockWidth;
            }
        }

        m_ticks = (vm_time_get_tick() - tickStart);
        CHECK_STATUS_PRINT_AC(ippSts, "ippiDCTInv_32f_CXR()", ippGetStatusString(ippSts), return STS_ERR_FAILED);

        status = Convert32f8u(pDstImage);
        CHECK_STATUS_PRINT_RS(status, "Transform::Convert32f8u()", GetBaseStatusString(status));

        return STS_OK;
    }

#if defined(USE_TBB)
    void operator()(const tbb::blocked_range<int>& range) const;
#endif

private:
    IppiSize            m_roi;
    IppiDCTFwdSpec_32f *m_pTrFwdSpec;
    IppiDCTInvSpec_32f *m_pTrInvSpec;
    unsigned char      *m_pTrFwdInit;
    unsigned char      *m_pTrInvInit;
    unsigned char      *m_pTrFwdBuffer;
    unsigned char      *m_pTrInvBuffer;
};

int main(int argc, char *argv[])
{
    // Memory leaks detection
#if (defined _DEBUG && defined _WIN32)
    _CrtSetDbgFlag(_CRTDBG_LEAK_CHECK_DF | _CrtSetDbgFlag(_CRTDBG_REPORT_FLAG));
#endif

    /*
    // Variables initialization
    */
    Status       status         = STS_OK;
    DString      sInputFile     = CheckTestDirs("lena.bmp");
    char*        sOutputFile    = 0;
    char*        sMatrixFile    = 0;
    char*        sMapFile       = 0;
    char*        sIppCpu        = 0;
    unsigned int iBlockSize[2]  = {0, 0};
    bool         bNoWindow      = false;
    bool         bPrintHelp     = false;

    Image srcData;
    Image dstData;
    Image coefsData;
    Image coefsNorm;
    char            *sMethod = "fft";
    unsigned int     iMethod;
    unsigned int     iHint = 0;
    IppHintAlgorithm hint;
    Transform       *pTrans = 0;

    // General timing
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
        { 'd', 1, KT_STRING,    0,            &sMatrixFile,     "save coefficients matrix" },
        { 'x', 1, KT_STRING,    0,            &sMapFile,        "create coefficients map image" },
        { 'm', 1, KT_STRING,    0,            &sMethod,         "image transform method: FFT (default), DFT, DCT"},
        { 'b', 2, KT_INTEGER,   0,            &iBlockSize[0],   "apply transform by blocks (width height, 0 0 by default)|block size will be set to first lower power of 2 value"},
        { 'n', 1, KT_INTEGER,   0,            &iHint,           "algorithm mode: 0 - default, 1 - fast, 2 - accurate"},
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

    if(iHint == 0)
        hint = ippAlgHintNone;
    else if(iHint == 1)
        hint = ippAlgHintFast;
    else if(iHint == 2)
        hint = ippAlgHintAccurate;
    else
    {
        printHelp(cmdOpts, argv);
        PRINT_MESSAGE("Invalid algorithm mode");
        return 1;
    }

    if(!vm_string_stricmp(sMethod, "FFT"))
        iMethod = CMD_METHOD_FFT;
    else if(!vm_string_stricmp(sMethod, "DFT"))
        iMethod = CMD_METHOD_DFT;
    else if(!vm_string_stricmp(sMethod, "DCT"))
        iMethod = CMD_METHOD_DCT;
    else
    {
        printHelp(cmdOpts, argv);
        PRINT_MESSAGE("Invalid transform method");
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

        if(iMethod == CMD_METHOD_FFT)
        {
            printf("\nFFT forward\n");
            pTrans = new FFT();
        }
        else if(iMethod == CMD_METHOD_DFT)
        {
            printf("\nDFT forward\n");
            pTrans = new DFT();
        }
        else if(iMethod == CMD_METHOD_DCT)
        {
            printf("\nDCT forward\n");
            pTrans = new DCT();
        }
        if(!pTrans)
        {
            PRINT_MESSAGE("Failed to allocate transform class");
            status = STS_ERR_ALLOC;
            break;
        }

        pTrans->m_hint = hint;
        if (iBlockSize[0] > srcData.m_iWidth || iBlockSize[1] > srcData.m_iHeight)
        {
            printf("Invalid blocksize specified\n");
            break;
        }
        pTrans->m_iBlockWidth  = iBlockSize[0];
        pTrans->m_iBlockHeight = iBlockSize[1];

        // Pre-init to get internal information, this is not mandatory
        status = pTrans->Init(&srcData, &coefsData);
        CHECK_STATUS_PRINT_BR(status, "Transform::Init()", GetBaseStatusString(status));

        printf("\nBlock size:       %dx%d\n", pTrans->m_iBlockWidth, pTrans->m_iBlockHeight);
        printf("Number of blocks: %dx%d\n", pTrans->m_iBlocksX, pTrans->m_iBlocksY);
        printf("Internal buffer:  %dx%d\n", coefsData.m_iWidth, coefsData.m_iHeight);

        /*
        // Forward transform
        */
        for(iLoops = 1, tickAcc = 0;; iLoops++)
        {
            status = pTrans->Forward(&srcData, &coefsData);
            CHECK_STATUS_PRINT_BR(status, "Transform::Forward()", GetBaseStatusString(status));
            tickAcc += pTrans->m_ticks;

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

        if(iMethod == CMD_METHOD_FFT)
            printf("\nFFT inverse\n");
        else if(iMethod == CMD_METHOD_DFT)
            printf("\nDFT inverse\n");
        else if(iMethod == CMD_METHOD_DCT)
            printf("\nDCT inverse\n");

        /*
        // Inverse transform
        */
        for(iLoops = 1, tickAcc = 0;; iLoops++)
        {
            // don't use Inverse without Forward, reading of RAW coefficient data was not implemented
            status = pTrans->Inverse(&coefsData, &dstData);
            CHECK_STATUS_PRINT_BR(status, "Transform::Inverse()", GetBaseStatusString(status));
            tickAcc += pTrans->m_ticks;

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

        // Write to file
        if(sMatrixFile && strlen(sMatrixFile))
        {
            status = coefsData.Write(sMatrixFile, IT_RAW);
            CHECK_STATUS_PRINT_BR(status, "Image::Write()", GetBaseStatusString(status));
        }

        if(!bNoWindow || (sMapFile && strlen(sMapFile)))
        {
            coefsNorm = coefsData;
            coefsNorm.m_iSampleSize = 1;
            coefsNorm.m_iWidth  = srcData.m_iWidth;
            coefsNorm.m_iHeight = srcData.m_iHeight;
            coefsNorm.Alloc();

            NormalizeMinMax_32f8u(&coefsData, &coefsNorm);

            if(sMapFile && strlen(sMapFile))
            {
                status = coefsNorm.Write(sMapFile, IT_BMP);
                CHECK_STATUS_PRINT_BR(status, "Image::Write()", GetBaseStatusString(status));
            }
        }

        if(sOutputFile && strlen(sOutputFile))
        {
            status = dstData.Write(sOutputFile);
            CHECK_STATUS_PRINT_BR(status, "Image::Write()", GetBaseStatusString(status));
        }

        // Rendering
        if(!bNoWindow)
        {
            WndDesc *pDesc = WindowNew("IPP FFT example");
            if(pDesc)
            {
                printf("\nPress Space to cycle through stages:\n");
                printf("1 - result image\n");
                printf("2 - original image\n");
                printf("3 - coefficients map\n");
                printf("\nClose window to exit.\n");

                int  iIndex  = 0;
                bool bRedraw = true;
                while(!WindowIsClosed(pDesc))
                {
                    vm_time_sleep(10);
                    if(WindowCheckKey(pDesc) == KK_SPACE)
                    {
                        iIndex = (iIndex+1)%3;
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
                        else if(iIndex == 2)
                            WindowDrawImage(pDesc, &coefsNorm);
                        bRedraw = false;
                    }
                }
                WindowRelease(pDesc);
            }
        }

        break;
    }

    /*
    // Delete resources
    */
    if(pTrans)
        delete pTrans;

    if(status < 0)
        return status;
    return 0;
}
