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

struct BMPHeader
{
    // file header
    unsigned short  bfType;
    unsigned int    bfSize;
    unsigned short  bfReserved1;
    unsigned short  bfReserved2;
    unsigned int    bfOffBits;

    // image header
    unsigned int    biSize;
    unsigned int    biWidth;
    int             biHeight;
    unsigned short  biPlanes;
    unsigned short  biBitCount;
    unsigned int    biCompression;
    unsigned int    biSizeImage;
    unsigned int    biXPelsPerMeter;
    unsigned int    biYPelsPerMeter;
    unsigned int    biClrUsed;
    unsigned int    biClrImportant;
};

struct RGBquad
{
    unsigned char    rgbBlue;
    unsigned char    rgbGreen;
    unsigned char    rgbRed;
    unsigned char    rgbReserved;
};

Status BmpReadData(FILE *pFile, Image *pData)
{
    if(!pData || !pFile)
        return STS_ERR_NULL_PTR;

    RGBquad      palette[256];
    BMPHeader    header;

    memset(&palette[0], 0, sizeof(palette));
    memset(&header, 0, sizeof(BMPHeader));

    // read header
    vm_file_fread(&header.bfType, 1, sizeof(header.bfType), pFile);
    vm_file_fread(&header.bfSize, 1, sizeof(header.bfSize), pFile);

    if(header.bfType != 'MB')
        return STS_ERR_FORMAT;

    vm_file_fread(&header.bfReserved1, 1, sizeof(header.bfReserved1), pFile);
    vm_file_fread(&header.bfReserved2, 1, sizeof(header.bfReserved2), pFile);
    vm_file_fread(&header.bfOffBits, 1, sizeof(header.bfOffBits), pFile);

    vm_file_fread(&header.biSize, 1, sizeof(header.biSize), pFile);
    vm_file_fread(&header.biWidth, 1, sizeof(header.biWidth), pFile);
    vm_file_fread(&header.biHeight, 1, sizeof(header.biHeight), pFile);
    vm_file_fread(&header.biPlanes, 1, sizeof(header.biPlanes), pFile);
    vm_file_fread(&header.biBitCount, 1, sizeof(header.biBitCount), pFile);

    if(header.biBitCount != 8 && header.biBitCount != 24 && header.biBitCount != 32)
        return STS_ERR_FAILED;

    vm_file_fread(&header.biCompression, 1, sizeof(header.biCompression), pFile);

    switch(header.biCompression)
    {
    case 0L: //0L == BI_RGB
        break;

    case 3L: //3L == BI_BITFIELDS (we support only 8uC4 images)
        {
        if(header.biBitCount != 32)
            return STS_ERR_FAILED;
        }
        break;

    default:
        return STS_ERR_FAILED;
    }

    vm_file_fread(&header.biSizeImage, 1, sizeof(header.biSizeImage), pFile);
    vm_file_fread(&header.biXPelsPerMeter, 1, sizeof(header.biXPelsPerMeter), pFile);
    vm_file_fread(&header.biYPelsPerMeter, 1, sizeof(header.biYPelsPerMeter), pFile);
    vm_file_fread(&header.biClrUsed, 1, sizeof(header.biClrUsed), pFile);
    vm_file_fread(&header.biClrImportant, 1, sizeof(header.biClrImportant), pFile);

    if(header.biBitCount == 8)
        vm_file_fread(&palette, 1, sizeof(RGBquad)*256, pFile);
    else if(header.biBitCount == 32 && header.biCompression == 3L)
        vm_file_fread(&palette, 1, sizeof(RGBquad)*3, pFile);

    pData->m_iWidth   = header.biWidth;
    pData->m_iHeight  = ABS(header.biHeight);
    pData->m_iSamples = header.biBitCount >> 3;

    if(pData->m_iSamples == 1)
        pData->m_color = IC_GRAY;
    else if(pData->m_iSamples == 3)
        pData->m_color = IC_BGR;
    else
        return STS_ERR_UNSUPPORTED;

    // read data
    if(!header.bfOffBits)
        return STS_ERR_FAILED;

    if(vm_file_fseek(pFile, header.bfOffBits, SEEK_SET))
        return STS_ERR_FAILED;

    pData->Alloc();

    unsigned int iFileStep = alignValue<unsigned int>(header.biWidth*(header.biBitCount >> 3), 4);

    if(0 < header.biHeight) // read bottom-up BMP
    {
        unsigned char *pPtr = pData->m_pPointer + pData->m_iStep * (pData->m_iHeight - 1);

        for(unsigned int i = 0; i < pData->m_iHeight; i++)
        {
            if(iFileStep != vm_file_fread((unsigned char*)(pPtr - i * pData->m_iStep), 1, iFileStep, pFile))
                return STS_ERR_NOT_ENOUGH_DATA;
        }
    }
    else // read up-bottom BMP
    {
        for(unsigned int i = 0; i < pData->m_iHeight; i++)
        {
            if(iFileStep != vm_file_fread(pData->m_pPointer + i * pData->m_iStep, 1, iFileStep, pFile))
                return STS_ERR_NOT_ENOUGH_DATA;
        }
    }

    if(3L == header.biCompression && header.biBitCount == 32)
    {
//        IppiSize size = {header.biWidth, iHeight};
//        int order[4]  = {3,2,1,0}; // convert from ABGR to RGBA

//        ippiSwapChannels_8u_C4IR(pData->m_pBuffer, iStep, size, order);
    }

    return STS_OK;
}

Status BmpWriteData(Image *pData, FILE *pFile)
{
    unsigned int iFileSize;
    unsigned int iImageSize;
    unsigned int iIHSize = 40;
    unsigned int iFHSize = 14;
    unsigned int iFileStep;
    unsigned int i;

    RGBquad    palette[256] = {0};
    BMPHeader  header;

    if(!pData || !pFile)
        return STS_ERR_NULL_PTR;

    iFileStep   = alignValue<unsigned int>(pData->m_iWidth * pData->m_iSamples, 4);
    iImageSize  = iFileStep * pData->m_iHeight;
    iFileSize   = iImageSize + iIHSize + iFHSize;

    header.bfType      = 'MB';
    header.bfSize      = iFileSize;
    header.bfReserved1 = 0;
    header.bfReserved2 = 0;
    header.bfOffBits   = iIHSize + iFHSize;

    if(pData->m_iSamples == 1)
        header.bfOffBits += sizeof(palette);

    // write header
    vm_file_fwrite(&header.bfType, 1, sizeof(header.bfType), pFile);
    vm_file_fwrite(&header.bfSize, 1, sizeof(header.bfSize), pFile);
    vm_file_fwrite(&header.bfReserved1, 1, sizeof(header.bfReserved1), pFile);
    vm_file_fwrite(&header.bfReserved2, 1, sizeof(header.bfReserved2), pFile);
    vm_file_fwrite(&header.bfOffBits, 1, sizeof(header.bfOffBits), pFile);

    header.biSize          = iIHSize;
    header.biWidth         = pData->m_iWidth;
    header.biHeight        = pData->m_iHeight;
    header.biPlanes        = 1;
    header.biBitCount      = (short)(pData->m_iSamples << 3);
    header.biCompression   = 0L;
    header.biSizeImage     = iImageSize;
    header.biXPelsPerMeter = 0;
    header.biYPelsPerMeter = 0;
    header.biClrUsed       = ((pData->m_iSamples == 1) ? 256 : 0);
    header.biClrImportant  = ((pData->m_iSamples == 1) ? 256 : 0);

    vm_file_fwrite(&header.biSize, 1, sizeof(header.biSize), pFile);
    vm_file_fwrite(&header.biWidth, 1, sizeof(header.biWidth), pFile);
    vm_file_fwrite(&header.biHeight, 1, sizeof(header.biHeight), pFile);
    vm_file_fwrite(&header.biPlanes, 1, sizeof(header.biPlanes), pFile);
    vm_file_fwrite(&header.biBitCount, 1, sizeof(header.biBitCount), pFile);
    vm_file_fwrite(&header.biCompression, 1, sizeof(header.biCompression), pFile);
    vm_file_fwrite(&header.biSizeImage, 1, sizeof(header.biSizeImage), pFile);
    vm_file_fwrite(&header.biXPelsPerMeter, 1, sizeof(header.biXPelsPerMeter), pFile);
    vm_file_fwrite(&header.biYPelsPerMeter, 1, sizeof(header.biYPelsPerMeter), pFile);
    vm_file_fwrite(&header.biClrUsed, 1, sizeof(header.biClrUsed), pFile);
    vm_file_fwrite(&header.biClrImportant, 1, sizeof(header.biClrImportant), pFile);

    if(pData->m_iSamples == 1)
    {
        for(i = 0; i < 256; i++)
        {
            palette[i].rgbBlue     = (unsigned char)i;
            palette[i].rgbGreen    = (unsigned char)i;
            palette[i].rgbRed      = (unsigned char)i;
            palette[i].rgbReserved = (unsigned char)0;
        }

        vm_file_fwrite(&palette[0], 1, sizeof(palette), pFile);
    }

    // write data
    unsigned char *pPtr = (unsigned char*)pData->m_pPointer + pData->m_iStep * (pData->m_iHeight - 1);

    for(i = 0; i < pData->m_iHeight; i++)
    {
        if(iFileStep != vm_file_fwrite(pPtr - i*pData->m_iStep, 1, iFileStep, pFile))
            return STS_ERR_FAILED;
    }

    return STS_OK;
}
