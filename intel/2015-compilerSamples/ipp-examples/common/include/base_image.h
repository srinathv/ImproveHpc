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

#ifndef __BASE_IMAGE_H__
#define __BASE_IMAGE_H__

#include "base.h"

enum ImageColor
{
    IC_UNKNOWN = 0,
    IC_GRAY,
    IC_RGB,
    IC_BGR,
    IC_RGBA,
    IC_BGRA
};

enum ImageType
{
    IT_UNKNOWN = 0,
    IT_RAW,
    IT_BMP,
};

enum ImageGen
{
    IG_HSIN // horizontal "sin" pattern
};

static const char* colorName[] = {"UNKNOWN", "Grayscale", "RGB", "BGR", "RGBA", "BGRA"};
static const char* typeName[]  = {"UNKNOWN", "RAW", "BMP"};

static const int colorChannels[] = {0, 1, 3, 3, 4, 4};

static const unsigned char defPalette[][4] =
{
    {0,255,0,0},
    {0,0,255,0},
    {0,128,255,0},
    {0,255,255,0},
    {255,128,0,0},
    {255,255,0,0},
    {255,0,0,0},
    {255,0,255,0}
};

struct Border
{
    unsigned int iLeft;
    unsigned int iTop;
    unsigned int iRight;
    unsigned int iBottom;
};

enum BorderType
{
    BT_REPLICATE = 0,
};

class Image
{
public:
    Image();
    Image(unsigned int iWidth, unsigned int iHeight, unsigned int iSamples, unsigned int iSampleSize = 1);
    Image(unsigned int iWidth, unsigned int iHeight, ImageColor color, unsigned int iSampleSize = 1);
    virtual ~Image();

    Status Read(const char *pFileName, ImageColor dstColor = IC_UNKNOWN);
    Status Read(FILE *pFile, ImageColor dstColor = IC_UNKNOWN);

    Status Write(const char *pFileName, ImageType dstType = IT_BMP, bool bAppend = false);
    Status Write(FILE *pFile, ImageType dstType = IT_BMP);

    // allocate image
    // manual specification of buffer size and step are requered to meet shared buffer requirements
    // default values of buffer size and step will be used if image cannot fit in these values
    // step alignment is ignored if manual step is in use
    Status Alloc(unsigned int iWidth, unsigned int iHeight, unsigned int iSamples, unsigned int iSampleSize = 1, unsigned int iBufferSize = 0, unsigned int iStep = 0);
    Status Alloc(unsigned int iWidth, unsigned int iHeight, ImageColor color, unsigned int iSampleSize = 1, unsigned int iBufferSize = 0, unsigned int iStep = 0);
    Status Alloc(unsigned int iBufferSize = 0, unsigned int iStep = 0);
    Status Release();

    // convert image to dst format, if dst is NULL it will be inplace conversion
    Status Convert(ImageColor dstColor, Image *pDstImage = 0);

     // fill buffer with specific pattern
    Status Generate(ImageGen imageGen);

    Status DrawPixel(unsigned int iX, unsigned int iY, unsigned char color[4]);
    Status DrawRect(Rect rect, unsigned char color[4], unsigned int iThickness = 1);
    Status DrawLine(Vect vect, unsigned char color[4]);

    Status CopyTo(Image *pDstImage);

    Status FillBorder(BorderType borderType, int iConstValue = 0);

    // image structure comparison
    bool Compare(Image *pImage);
    bool operator==(const Image&);
    bool operator!=(const Image&);

    // assignment overload
    Image(const Image&);
    Image& operator=(const Image&);

public:
    unsigned char *m_pPointer;         // pointer to ROI start point
    unsigned int   m_iImageSize;       // actual size of image buffer realative to m_pPointer
    unsigned int   m_iBufferSize;      // full size of bufer
    unsigned int   m_iBufferAlignment; // buffer allocation alignment
    unsigned int   m_iStep;            // size of buffer row in bytes
    unsigned int   m_iStepAlignment;   // alignment of step value
    unsigned int   m_iSampleSize;      // size of one sample in bytes
    unsigned int   m_iSamples;         // amount of samples in pixel
    unsigned int   m_iWidth;           // ROI width
    unsigned int   m_iHeight;          // ROI height
    Border         m_border;           // image border in pixels
    ImageColor     m_color;            // samples pattern

    ImageType      m_origType;         // type of source file

private:
    void Reset();

    unsigned char *m_pBuffer;
    bool           m_bAllocated;
};

Status BmpReadData(FILE *pFile, Image *pData);
Status BmpWriteData(Image *pData, FILE *pFile);

void NormalizeMinMax_32f8u(Image *pSrcData, Image *pDstData);

#endif
