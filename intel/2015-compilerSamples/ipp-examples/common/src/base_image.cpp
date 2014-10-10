/*******************************************************************************
** Copyright(C) 2005-2014 Intel Corporation. All Rights Reserved.
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

#include "base_image.h"

#ifdef USE_IPP
    #include "ippi.h"
    #include "ippcc.h"
#endif

void NormalizeMinMax_32f8u(Image *pSrcData, Image *pDstData)
{
    float min = 255;
    float max = 0;
    unsigned int i, j;

    float *pSrcRow;
    unsigned char *pDstRow;

    // find min/max
    for(i = 0; i < pSrcData->m_iHeight; i++)
    {
        pSrcRow = (float*)(pSrcData->m_pPointer + pSrcData->m_iStep*i);
        for(j = 0; j < pSrcData->m_iWidth*pSrcData->m_iSamples; j++)
        {
            if(min > pSrcRow[j])
                min = pSrcRow[j];
            if(max < pSrcRow[j])
                max = pSrcRow[j];
        }
    }

    // compress dynamic range
    for(i = 0; i < pDstData->m_iHeight; i++)
    {
        pSrcRow = (float*)(pSrcData->m_pPointer + pSrcData->m_iStep*i);
        pDstRow = (unsigned char*)(pDstData->m_pPointer + pDstData->m_iStep*i);

        for(j = 0; j < pDstData->m_iWidth*pSrcData->m_iSamples; j++)
        {
            pDstRow[j] = (unsigned char)(((pSrcRow[j] - min)*255)/(max - min));
        }
    }
}

static bool IsInitialized(Image *pImage)
{
    if(!pImage->m_pPointer || !pImage->m_iWidth || !pImage->m_iHeight || !pImage->m_iSamples || !pImage->m_iSampleSize || !pImage->m_iStep || pImage->m_color == IC_UNKNOWN)
        return false;
    return true;
}

static ImageType GetImageType(FILE *pFile)
{
    if(!pFile)
        return IT_UNKNOWN;

    unsigned char buf[4];

    vm_file_fseek(pFile, 0, SEEK_SET);
    vm_file_fread(buf, 1, 4, pFile);
    vm_file_fseek(pFile, 0, SEEK_SET);

    // check BMP
    if((buf[0] == 'B') && (buf[1] == 'M'))
        return IT_BMP;

    return IT_UNKNOWN;
}

Status cppiGrayToRGB_8u_P1C3R(unsigned char *pSrc, unsigned int iSrcStep, unsigned char *pDst, unsigned int iDstStep, unsigned int iWidth, unsigned int iHeight)
{
#ifdef USE_IPP
    IppStatus ippStatus;
    IppiSize roiSize = {iWidth, iHeight};
    ippStatus = ippiDup_8u_C1C3R((const Ipp8u*)pSrc, iSrcStep, (Ipp8u*)pDst, iDstStep, roiSize);
    return (ippStsNoErr == ippStatus)? STS_OK : STS_ERR_FAILED;
#else
    unsigned char *pSrcRow, *pDstRow;
    unsigned int i, j, k;

    for(i = 0; i < iHeight; i++)
    {
        pSrcRow = &pSrc[iSrcStep*i];
        pDstRow = &pDst[iDstStep*i];

        for(j = 0, k = 0; j < iWidth; j++, k += 3)
        {
            pDstRow[k] = pDstRow[k + 1] = pDstRow[k + 2] = pSrcRow[j];
        }
    }
    return STS_OK;
#endif
}

Status cppiRGBToGray_8u_C3P1R(unsigned char *pSrc, unsigned int iSrcStep, unsigned char *pDst, unsigned int iDstStep, unsigned int iWidth, unsigned int iHeight, bool bBGR)
{
#ifdef USE_IPP
        IppStatus ippStatus;
    IppiSize roiSize = {iWidth, iHeight};
    ippStatus = ippiRGBToGray_8u_C3C1R((const Ipp8u*)pSrc, iSrcStep, (Ipp8u*)pDst, iDstStep, roiSize);
    return (ippStsNoErr == ippStatus)? STS_OK : STS_ERR_FAILED;
#else
    unsigned char *pSrcRow, *pDstRow;
    unsigned int i, j, k;
    int ind1 = (bBGR)?2:0;
    int ind3 = (bBGR)?0:2;

    for(i = 0; i < iHeight; i++)
    {
        pSrcRow = &pSrc[iSrcStep*i];
        pDstRow = &pDst[iDstStep*i];

        for(j = 0, k = 0; j < iWidth; j++, k += 3)
        {
            pDstRow[j] = (unsigned char)((float)pSrcRow[k + ind1]*0.299 + (float)pSrcRow[k + 1]*0.587 + (float)pSrcRow[k + ind3]*0.114);
        }
    }

    return STS_OK;
#endif
}

Status cppiSwapChannes_C3(unsigned char *pSrc, unsigned int iSrcStep, unsigned char *pDst, unsigned int iDstStep, unsigned int iWidth, unsigned int iHeight)
{
#ifdef USE_IPP
    IppStatus ippStatus;
    const int dstOrder[3] = { 2, 1, 0 };
    IppiSize roiSize = { iWidth, iHeight };
    ippStatus = ippiSwapChannels_8u_C3R((const Ipp8u*)pSrc, iSrcStep, (Ipp8u*)pSrc, iSrcStep, roiSize, dstOrder);
    return (ippStsNoErr == ippStatus)? STS_OK : STS_ERR_FAILED;
#else
    unsigned char *pSrcRow;
    unsigned char *pDstRow;
    unsigned char iTemp;
    unsigned int i, j, k;

    for(i = 0; i < iHeight; i++)
    {
        pSrcRow = &pSrc[iSrcStep*i];
        pDstRow = &pDst[iDstStep*i];

        for(j = 0, k = 0; j < iWidth; j++, k += 3)
        {
            iTemp = pSrcRow[k];
            pDstRow[k] = pSrcRow[k + 2];
            pDstRow[k + 1] = pSrcRow[k + 1];
            pDstRow[k + 2] = iTemp;
        }
    }

    return STS_OK;
#endif
}

Image::Image()
{
    Reset();
}

Image::Image(unsigned int iWidth, unsigned int iHeight, unsigned int iSamples, unsigned int iSampleSize)
{
    Reset();

    m_iWidth      = iWidth;
    m_iHeight     = iHeight;
    m_iSamples    = iSamples;
    m_iSampleSize = iSampleSize;
}

Image::Image(unsigned int iWidth, unsigned int iHeight, ImageColor color, unsigned int iSampleSize)
{
    Reset();

    m_iWidth      = iWidth;
    m_iHeight     = iHeight;
    m_color       = color;
    m_iSampleSize = iSampleSize;
    m_iSamples    = colorChannels[m_color];
}

Image::~Image()
{
    Release();
}

void Image::Reset()
{
    m_pPointer         = 0;
    m_iImageSize       = 0;
    m_pBuffer          = 0;
    m_iBufferSize      = 0;
    m_iBufferAlignment = 64;
    m_iStep            = 0;
    m_iStepAlignment   = 4;
    m_iSampleSize      = 1;
    m_iSamples         = 0;
    m_iWidth           = 0;
    m_iHeight          = 0;
    m_color            = IC_UNKNOWN;

    m_border.iLeft = m_border.iRight = m_border.iTop = m_border.iBottom = 0;

    m_origType = IT_UNKNOWN;

    m_bAllocated = false;
}

Status Image::Alloc(unsigned int iWidth, unsigned int iHeight, unsigned int iSamples, unsigned int iSampleSize, unsigned int iBufferSize, unsigned int iStep)
{
    Border borderActual = {0,0,0,0};

    Release();

    if(!iWidth || !iHeight || !iSamples || !iSampleSize)
        return STS_ERR_INVALID_PARAMS;

    m_iWidth      = iWidth;
    m_iHeight     = iHeight;
    m_iSamples    = iSamples;
    m_iSampleSize = iSampleSize;

    borderActual.iLeft  = m_border.iLeft*m_iSamples*m_iSampleSize;
    borderActual.iRight = m_border.iRight*m_iSamples*m_iSampleSize;

    m_iStep = m_iWidth*m_iSamples*m_iSampleSize + borderActual.iLeft + borderActual.iRight;
    if(iStep >= m_iStep) // use custom step value
    {
        m_iStep = iStep;
        m_iStepAlignment = 1;
    }

    m_iStep = alignValue<unsigned int>(m_iStep, m_iStepAlignment);

    borderActual.iTop    = m_border.iTop*m_iStep;
    borderActual.iBottom = m_border.iBottom*m_iStep;

    m_iBufferSize = m_iStep*m_iHeight + borderActual.iTop + borderActual.iBottom;
    if(iBufferSize >= m_iBufferSize) // use custom buffer size value
        m_iBufferSize = iBufferSize;

    m_pBuffer = (unsigned char*)vm_malloc(m_iBufferSize, m_iBufferAlignment);
    if(!m_pBuffer)
        return STS_ERR_ALLOC;

    m_pPointer   = m_pBuffer + borderActual.iTop + borderActual.iLeft;
    m_iImageSize = m_iStep*m_iHeight;

    m_bAllocated  = true;

    return STS_OK;
}

Status Image::Alloc(unsigned int iWidth, unsigned int iHeight, ImageColor color, unsigned int iSampleSize, unsigned int iBufferSize, unsigned int iStep)
{
    m_color    = color;
    m_iSamples = colorChannels[m_color];

    return Alloc(iWidth, iHeight, m_iSamples, iSampleSize, iBufferSize, iStep);
}

Status Image::Alloc(unsigned int iBufferSize, unsigned int iStep)
{
    return Alloc(m_iWidth, m_iHeight, m_iSamples, m_iSampleSize, iBufferSize, iStep);
}

Status Image::Release()
{
    if(m_bAllocated && m_pBuffer)
        vm_free(m_pBuffer);

    m_pBuffer     = NULL;
    m_pPointer    = NULL;
    m_iImageSize  = 0;
    m_iBufferSize = 0;
    m_bAllocated  = false;

    return STS_OK;
}

Status Image::Read(const char *pFileName, ImageColor dstColor)
{
    FILE *pFile = 0;
    Status status = STS_OK;

    // allocate source buffer and read from file
    pFile = vm_file_fopen(pFileName, "rb");
    if(!pFile)
        return STS_ERR_FILE_OPEN;

    status = Read(pFile, dstColor);
    CHECK_STATUS_PRINT(status, "Image::Read()", GetBaseStatusString(status));

    vm_file_fclose(pFile);

    return status;
}

Status Image::Read(FILE *pFile, ImageColor dstColor)
{
    Status status = STS_OK;

    if(!pFile)
        return STS_ERR_NULL_PTR;

    m_origType = GetImageType(pFile);

    // read image to buffer
    if(m_origType == IT_BMP)
        status = BmpReadData(pFile, this);
    else
        status = STS_ERR_UNSUPPORTED;

    if(status != STS_OK)
        return status;

    if(dstColor != IC_UNKNOWN)
        return Convert(dstColor);

    return STS_OK;
}

Status Image::Write(const char *pFileName, ImageType dstType, bool bAppend)
{
    FILE *pFile = 0;
    Status status = STS_OK;

    if(!IsInitialized(this))
        return STS_ERR_NOT_INITIALIZED;

    if(bAppend)
        pFile = vm_file_fopen(pFileName, "ab");
    else
        pFile = vm_file_fopen(pFileName, "wb");
    if(!pFile)
        return STS_ERR_FILE_OPEN;

    status = Write(pFile, dstType);
    CHECK_STATUS_PRINT(status, "Image::Write()", GetBaseStatusString(status));

    vm_file_fclose(pFile);

    return status;
}

Status Image::Write(FILE *pFile, ImageType dstType)
{
    Status status = STS_OK;

    if(!pFile)
        return STS_ERR_NULL_PTR;

    if(!IsInitialized(this))
        return STS_ERR_NOT_INITIALIZED;

    if(dstType == IT_RAW)
    {
        unsigned int iLine = m_iWidth*m_iSamples*m_iSampleSize;

        for(unsigned int i = 0; i < m_iHeight; i++)
        {
            if(iLine != vm_file_fwrite((m_pPointer + m_iStep*i), 1, iLine, pFile))
            {
                status = STS_ERR_FAILED;
                break;
            }
        }
    }
    else if(dstType == IT_BMP)
        status = BmpWriteData(this, pFile);
    else
        status = STS_ERR_UNSUPPORTED;

    return status;
}

Status Image::Convert(ImageColor dstColor, Image *pDstImage)
{
    Status  status;
    unsigned char *pTmpBuffer  = 0;
    unsigned char *pSrcPointer = 0;
    unsigned int   iSrcStep    = 0;
    ImageColor     srcColor    = m_color;

    unsigned char *pDstPointer = 0;
    unsigned int   iDstStep    = 0;

    if(dstColor == IC_UNKNOWN)
        return STS_ERR_INVALID_PARAMS;

    if(!IsInitialized(this))
        return STS_ERR_NOT_INITIALIZED;

    if(srcColor == dstColor)
    {
        if(pDstImage)
        {
            if(pDstImage->m_bAllocated)
                return CopyTo(pDstImage);
            else
                *pDstImage = *this;
        }
        return STS_OK;
    }

    // inplace coversion
    if(!pDstImage)
    {
        pTmpBuffer  = m_pBuffer;
        pSrcPointer = m_pPointer;
        iSrcStep    = m_iStep;
        m_color     = dstColor;

        // swap
        if((srcColor == IC_RGB && dstColor == IC_BGR) || (srcColor == IC_BGR && dstColor == IC_RGB))
        {
            pDstPointer = m_pPointer;
            iDstStep    = m_iStep;
        }
        else
        {
            m_iSamples = colorChannels[m_color];
            m_pBuffer  = NULL;

            status = Alloc();
            if(status < 0)
                return status;

            pDstPointer = m_pPointer;
            iDstStep    = m_iStep;
        }
    }
    else
    {
        pSrcPointer = m_pPointer;
        iSrcStep    = m_iStep;

        if(!pDstImage->m_bAllocated)
        {
            *pDstImage = *this;
            pDstImage->m_color    = dstColor;
            pDstImage->m_iSamples = colorChannels[dstColor];

            pDstImage->Alloc();
        }
        else if(pDstImage->m_color != dstColor)
            return STS_ERR_INVALID_PARAMS;

        pDstPointer = pDstImage->m_pPointer;
        iDstStep    = pDstImage->m_iStep;
    }

    switch(srcColor)
    {
    case IC_GRAY:
        switch(dstColor)
        {
        case IC_RGB:
        case IC_BGR:
            status = cppiGrayToRGB_8u_P1C3R(pSrcPointer, iSrcStep, pDstPointer, iDstStep, m_iWidth, m_iHeight);
            break;
        default:
            status = STS_ERR_UNSUPPORTED;
            break;
        }
        break;
    case IC_RGB:
        switch(dstColor)
        {
        case IC_GRAY:
            status = cppiRGBToGray_8u_C3P1R(pSrcPointer, iSrcStep, pDstPointer, iDstStep, m_iWidth, m_iHeight, false);
            break;

        case IC_BGR:
            status = cppiSwapChannes_C3(pSrcPointer, iSrcStep, pDstPointer, iDstStep, m_iWidth, m_iHeight);
            break;

        default:
            status = STS_ERR_UNSUPPORTED;
            break;
        }
        break;
    case IC_BGR:
        switch(dstColor)
        {
        case IC_GRAY:
            status = cppiRGBToGray_8u_C3P1R(pSrcPointer, iSrcStep, pDstPointer, iDstStep, m_iWidth, m_iHeight, true);
            break;

        case IC_RGB:
            status = cppiSwapChannes_C3(pSrcPointer, iSrcStep, pDstPointer, iDstStep, m_iWidth, m_iHeight);
            break;

        default:
            status = STS_ERR_UNSUPPORTED;
            break;
        }
        break;
    default:
        status = STS_ERR_UNSUPPORTED;
        break;
    }

    // inplace coversion
    if(!pDstImage && pTmpBuffer)
        vm_free(pTmpBuffer);

    return status;
}

Status Image::CopyTo(Image *pDstImage)
{
    unsigned char *pSrcRow;
    unsigned char *pDstRow;
    unsigned int iMinHeight;
    unsigned int iMinStep;

    if(!pDstImage || !pDstImage->m_pPointer)
        return STS_ERR_NULL_PTR;

    if(m_iSamples != pDstImage->m_iSamples || m_iSampleSize != pDstImage->m_iSampleSize)
        return STS_ERR_INVALID_PARAMS;
    if(m_iSampleSize != pDstImage->m_iSampleSize)
        return STS_ERR_INVALID_PARAMS;

    iMinStep   = MIN(pDstImage->m_iStep, m_iStep);
    iMinHeight = MIN(pDstImage->m_iHeight, m_iHeight);

    for(unsigned int i = 0; i < iMinHeight; i++)
    {
        pSrcRow = m_pPointer + i*m_iStep;
        pDstRow = pDstImage->m_pPointer + i*pDstImage->m_iStep;

        memcpy(pDstRow, pSrcRow, iMinStep);
    }

    return STS_OK;
}

Status Image::FillBorder(BorderType borderType, int iConstValue)
{
    unsigned char *pSrcRow;
    unsigned char *pDstRow;
    unsigned int iPixSize = m_iSampleSize*m_iSamples;
    unsigned int i, j;

    if(!IsInitialized(this))
        return STS_ERR_NOT_INITIALIZED;

    if(borderType == BT_REPLICATE)
    {
        if(m_border.iTop)
        {
            pSrcRow = m_pPointer;
            for(i = 1; i <= m_border.iTop; i++)
            {
                pDstRow = m_pPointer - m_iStep*i;
                memcpy(pDstRow, pSrcRow, m_iWidth*iPixSize);
            }
        }
        if(m_border.iBottom)
        {
            pSrcRow = m_pPointer + m_iStep*(m_iHeight-1);
            for(i = m_iHeight; i < m_iHeight + m_border.iBottom; i++)
            {
                pDstRow = m_pPointer + m_iStep*i;
                memcpy(pDstRow, pSrcRow, m_iWidth*iPixSize);
            }
        }
        if(m_border.iLeft)
        {
            for(i = 0; i < m_iHeight; i++)
            {
                pSrcRow = m_pPointer + m_iStep*i;
                pDstRow = m_pPointer + m_iStep*i - m_border.iLeft*iPixSize;
                for(j = 0; j < m_border.iLeft; j++)
                    memcpy(&pDstRow[j*iPixSize], pSrcRow, iPixSize);
            }
            if(m_border.iTop)
            {
                pSrcRow = m_pPointer - m_border.iLeft*iPixSize;
                for(i = 1; i <= m_border.iTop; i++)
                {
                    pDstRow = m_pPointer - m_iStep*i - m_border.iLeft*iPixSize;
                    memcpy(pDstRow, pSrcRow, m_border.iLeft*iPixSize);
                }
            }
            if(m_border.iBottom)
            {
                pSrcRow = m_pPointer + m_iStep*(m_iHeight-1) - m_border.iLeft*iPixSize;
                for(i = m_iHeight; i < m_iHeight + m_border.iBottom; i++)
                {
                    pDstRow = m_pPointer + m_iStep*i - m_border.iLeft*iPixSize;
                    memcpy(pDstRow, pSrcRow, m_border.iLeft*iPixSize);
                }
            }
        }
        if(m_border.iRight)
        {
            for(i = 0; i < m_iHeight; i++)
            {
                pSrcRow = m_pPointer + m_iStep*i + (m_iWidth-1)*iPixSize;
                pDstRow = m_pPointer + m_iStep*i + (m_iWidth-1)*iPixSize;
                for(j = 1; j <= m_border.iRight; j++)
                    memcpy(&pDstRow[j*iPixSize], pSrcRow, iPixSize);
            }
            if(m_border.iTop)
            {
                pSrcRow = m_pPointer + m_iWidth*iPixSize;
                for(i = 1; i <= m_border.iTop; i++)
                {
                    pDstRow = m_pPointer - m_iStep*i + m_iWidth*iPixSize;
                    memcpy(pDstRow, pSrcRow, m_border.iRight*iPixSize);
                }
            }
            if(m_border.iBottom)
            {
                pSrcRow = m_pPointer + m_iStep*(m_iHeight-1) + m_iWidth*iPixSize;
                for(i = m_iHeight; i < m_iHeight + m_border.iBottom; i++)
                {
                    pDstRow = m_pPointer + m_iStep*i + m_iWidth*iPixSize;
                    memcpy(pDstRow, pSrcRow, m_border.iRight*iPixSize);
                }
            }
        }
    }
    else
        return STS_ERR_UNSUPPORTED;

    return STS_OK;
}

bool Image::Compare(Image *pImage)
{
    if(!pImage)
        return false;

    if(m_iSampleSize != pImage->m_iSampleSize ||
        m_iWidth != pImage->m_iWidth ||
        m_iHeight != pImage->m_iHeight ||
        m_iSamples != pImage->m_iSamples ||
        m_color != pImage->m_color)
        return false;

    return true;
}

bool Image::operator==(const Image& image)
{
    return Compare((Image*)&image);
}

bool Image::operator!=(const Image& image)
{
    return !Compare((Image*)&image);
}

Image::Image(const Image& ref)
{
    m_bAllocated = false;

    *this = ref;
}

Image& Image::operator=(const Image &image)
{
    Release();

    m_pPointer         = image.m_pPointer;
    m_iImageSize       = image.m_iImageSize;
    m_pBuffer          = image.m_pBuffer;
    m_iBufferSize      = image.m_iBufferSize;
    m_iBufferAlignment = image.m_iBufferAlignment;
    m_iStep            = image.m_iStep;
    m_iStepAlignment   = image.m_iStepAlignment;
    m_iSampleSize      = image.m_iSampleSize;
    m_iSamples         = image.m_iSamples;
    m_iWidth           = image.m_iWidth;
    m_iHeight          = image.m_iHeight;
    m_color            = image.m_color;
    m_border           = image.m_border;

    m_bAllocated = false;

    return *this;
}

#include <math.h>
#define M_PI 3.14159265358979323846

Status Image::Generate(ImageGen imageGen)
{
    if(!IsInitialized(this))
        return STS_ERR_NOT_INITIALIZED;

    if(imageGen == IG_HSIN)
    {
        // We need to initialize a single horizontal line and to copy this line to others
        double          horizontalStep = (4 * M_PI) / m_iWidth;
        unsigned char  *pLocSrc = m_pPointer;
        unsigned int    i;
        int             chans;

        switch(m_color)
        {
        case IC_RGBA:
        case IC_BGRA:
            chans = 4;
            break;
        case IC_RGB:
        case IC_BGR:
            chans = 3;
            break;
        default:
            chans = 0;
        }

        if(chans != 3 && chans != 4)
            return 0;

        for(i = 0; i < m_iWidth; i++)
        {
            double sampleValue = sin(horizontalStep * i);
            unsigned char pixelValue = (unsigned char)(sampleValue * 126. + 127);
            *pLocSrc = pixelValue;

            switch(m_color)
            {
            case IC_RGBA:
            case IC_BGRA:
                *(pLocSrc+3) = pixelValue;
            case IC_RGB:
            case IC_BGR:
                *(pLocSrc+2) = pixelValue;
                *(pLocSrc+1) = pixelValue;
                break;
            }
            pLocSrc += chans;
        }

        /* Duplicate generated source image line */
        for(i = 0; i < m_iHeight-1; i++)
        {
            memcpy(m_pPointer + m_iStep * (i+1), m_pPointer, m_iWidth * chans); /* Do not copy rest of line (beyond width * chans) */
        }
    }
    else
        return STS_ERR_INVALID_PARAMS;

    return STS_OK;
}

Status Image::DrawPixel(unsigned int iX, unsigned int iY, unsigned char color[4])
{
    if(!m_pPointer)
        return STS_ERR_NOT_INITIALIZED;

    if(m_iSamples > 4)
        return STS_ERR_UNSUPPORTED;

    if(iX >= m_iWidth || iY >= m_iHeight)
        return STS_OK;

    unsigned char *pPtr = m_pPointer + m_iStep*iY + m_iSamples*m_iSampleSize*iX;
    for(unsigned int k = 0; k < m_iSamples; k++)
        pPtr[k] = (unsigned char)color[k];

    return STS_OK;
}

Status Image::DrawRect(Rect rect, unsigned char color[4], unsigned int iThickness)
{
    int i, j;

    if(!m_pPointer)
        return STS_ERR_NOT_INITIALIZED;

    if(m_iSamples > 4)
        return STS_ERR_UNSUPPORTED;

    for(i = rect.y; (i < rect.y + (int)rect.height) && (i < (int)m_iHeight); i++)
    {
        if(i >= 0)
        {
            for(j = rect.x; (j < rect.x + (int)rect.width) && (j < (int)m_iWidth); j++)
            {
                if(j >= 0)
                {
                    if((i < rect.y + (int)iThickness) || (j < rect.x + (int)iThickness) || (i >= rect.y + (int)rect.height - (int)iThickness) || 
                                (j >= rect.x + (int)rect.width - (int)iThickness))
                        DrawPixel(j, i, color);
                }
            }
        }
    }

    return STS_OK;
}

// Bresenham line algorithm
Status Image::DrawLine(Vect vect, unsigned char color[4])
{
    if(!m_pPointer)
        return STS_ERR_NOT_INITIALIZED;

    if(m_iSamples > 4)
        return STS_ERR_UNSUPPORTED;

    int i;
    int x, y;
    int xl, yl;

    int dx = vect.x2 - vect.x1;
    int dy = vect.y2 - vect.y1;

    int dxa = abs(dx);
    int dya = abs(dy);

    int px  = 2*dya - dxa;
    int py  = 2*dxa - dya;

    if(dya <= dxa)
    {
        if(dx >= 0)
        {
            x  = vect.x1;
            y  = vect.y1;
            xl = vect.x2;
        }
        else
        {
            x  = vect.x2;
            y  = vect.y2;
            xl = vect.x1;
        }
        DrawPixel(x, y, color);

        for(i = 0; x < xl; i++)
        {
            x = x + 1;
            if(px < 0)
                px = px + 2*dya;
            else
            {
                if((dx < 0 && dy < 0) || (dx > 0 && dy > 0))
                    y = y + 1;
                else
                    y = y - 1;

                px = px + 2*(dya - dxa);
            }
            DrawPixel(x, y, color);
        }
    }
    else
    {
        if(dy >= 0)
        {
            x  = vect.x1;
            y  = vect.y1;
            yl = vect.y2;
        }
        else
        {
            x  = vect.x2;
            y  = vect.y2;
            yl = vect.y1;
        }
        DrawPixel(x, y, color);

        for(i = 0; y < yl; i++)
        {
            y = y + 1;
            if(py <= 0)
                py = py + 2*dxa;
            else
            {
                if((dx < 0 && dy < 0) || (dx > 0 && dy > 0))
                    x = x + 1;
                else
                    x = x - 1;

                py = py + 2*(dxa - dya);
            }
            DrawPixel(x, y, color);
        }
    }

    return STS_OK;
}
