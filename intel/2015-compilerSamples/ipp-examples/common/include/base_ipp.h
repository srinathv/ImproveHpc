/*******************************************************************************
** Copyright(C) 2011-2014 Intel Corporation. All Rights Reserved.
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

#ifndef __BASE_IPP_H__
#define __BASE_IPP_H__

#include "base.h"

#if defined(__MIC__)
    #pragma offload_attribute(push, target(mic))
    #include "ippcore.h"
    #include "ippi.h"
    #pragma offload_attribute(pop)
#else
    #include "ippcore.h"
    #include "ippi.h"
#endif

#define PRINT_LIB_VERSION(LIB, VER) \
    VER = ipp##LIB##GetLibVersion(); \
    printf("  %s %s %s \n", VER->Name, VER->Version, VER->BuildDate);

#define PX_FM ( ippCPUID_MMX | ippCPUID_SSE )
#define W7_FM ( PX_FM | ippCPUID_SSE2 )
#define V8_FM ( W7_FM | ippCPUID_SSE3 | ippCPUID_SSSE3 )
#define S8_FM ( V8_FM | ippCPUID_MOVBE )
#define P8_FM ( V8_FM | ippCPUID_SSE41 | ippCPUID_SSE42 | ippCPUID_AES | ippCPUID_CLMUL | ippCPUID_SHA )
#define G9_FM ( P8_FM | ippCPUID_AVX | ippAVX_ENABLEDBYOS | ippCPUID_RDRAND | ippCPUID_F16C )
#define H9_FM ( G9_FM | ippCPUID_AVX2 | ippCPUID_MOVBE | ippCPUID_ADCOX | ippCPUID_RDSEED | ippCPUID_PREFETCHW )

static IppStatus InitPreferredCpu(const char *sCpu)
{
    if(sCpu && strlen(sCpu))
    {
        if(!vm_string_stricmp(sCpu, "SSE"))
            return ippSetCpuFeatures(PX_FM);
        else if (!vm_string_stricmp(sCpu, "SSE2"))
            return ippSetCpuFeatures(W7_FM);
        else if (!vm_string_stricmp(sCpu, "SSE3"))
            return ippSetCpuFeatures(W7_FM | ippCPUID_SSE3);
        else if(!vm_string_stricmp(sCpu, "SSSE3"))
            return ippSetCpuFeatures(V8_FM);
        else if(!vm_string_stricmp(sCpu, "SSE41"))
            return ippSetCpuFeatures(V8_FM | ippCPUID_SSE41);
        else if(!vm_string_stricmp(sCpu, "SSE42"))
            return ippSetCpuFeatures(V8_FM | ippCPUID_SSE41 | ippCPUID_SSE42);
        else if(!vm_string_stricmp(sCpu, "AVX"))
            return ippSetCpuFeatures(G9_FM);
        else if(!vm_string_stricmp(sCpu, "AES"))
            return ippSetCpuFeatures(P8_FM);
        else if(!vm_string_stricmp(sCpu, "AVX2"))
            return ippSetCpuFeatures(H9_FM);
    }
    return ippInit();
}

#endif
