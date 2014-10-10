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

#include "base.h"

/*
// Dynamic strings
*/
DString::DString()
{
    m_pData = NULL;
    m_iSize = 0;
    m_iLen  = 0;
    Replace((const vm_char*)&(VM_STRING("")), 0); // initialize with zero-length string to avoid typecast memory problems
}

DString::DString(const vm_char *pSrc)
{
    m_pData = NULL;
    m_iSize = 0;
    m_iLen  = 0;
    if(!pSrc)
        Replace((const vm_char*)&(VM_STRING("")), 0);
    else
        Replace(pSrc, (unsigned int)vm_string_strlen(pSrc));
}

DString::DString(const DString &src)
{
    m_pData = NULL;
    m_iSize = 0;
    m_iLen  = 0;
    Replace(src.m_pData, src.m_iLen);
}

DString::~DString()
{
    if(m_pData)
    {
        delete[] m_pData;
        m_pData = NULL;
    }
    m_iSize = 0;
    m_iLen  = 0;
}

void DString::Clear()
{
    if(m_pData)
    {
        delete[] m_pData;
        m_pData = NULL;
    }
    m_iSize = 0;
    m_iLen  = 0;
    Replace((const vm_char*)&(VM_STRING("")), 0);
}

size_t DString::Replace(const vm_char* pSrc, size_t iSrcSize)
{
    if(pSrc && m_pData != pSrc)
    {
        if(m_iSize < iSrcSize + 1)
        {
            if(m_pData)
                delete[] m_pData;
            m_iSize = iSrcSize + 1;
            m_pData = (vm_char*)new vm_char[m_iSize];
        }
        m_iLen = iSrcSize;
        memcpy(m_pData, pSrc, iSrcSize*sizeof(vm_char));
        m_pData[m_iLen] = VM_STRING('\0');

        return iSrcSize;
    }
    return 0;
}

size_t DString::Append(const vm_char* pSrc, size_t iSrcSize)
{
    if(pSrc)
    {
        if(m_iSize < iSrcSize + m_iLen + 1)
        {
            vm_char *pDataOld = m_pData;

            m_iSize = iSrcSize + m_iLen + 1;
            m_pData = new vm_char[m_iSize];
            if(pDataOld)
            {
                memcpy(m_pData, pDataOld, m_iLen*sizeof(vm_char));
                delete[] pDataOld;
            }
        }
        memcpy(&m_pData[m_iLen], pSrc, iSrcSize);
        m_iLen += iSrcSize;
        m_pData[m_iLen] = VM_STRING('\0');
        return iSrcSize;
    }
    return 0;
}

int DString::Compare(const vm_char *pSrc, bool bCaseSensitive)
{
    if(!pSrc || !m_pData)
        return -1;

    if(bCaseSensitive)
        return vm_string_strcmp(m_pData, pSrc);
    else
        return vm_string_stricmp(m_pData, pSrc);
}

int DString::Find(const vm_char *pSrc, bool bCaseSensitive)
{
    size_t iSrcSize;
    int    iChar1;
    int    iChar2;
    unsigned int iCount;
    size_t i, k;

    if(!pSrc || !m_pData)
        return -1;

    iSrcSize = vm_string_strlen(pSrc);

    for(i = 0; (i < m_iLen) && ((i + iSrcSize) <= m_iLen); i++)
    {
        iCount = 0;

        for(k = 0; k < iSrcSize; k++)
        {
            if(bCaseSensitive)
            {
                iChar1 = m_pData[i + k];
                iChar2 = pSrc[k];
            }
            else
            {
                iChar1 = vm_string_tolower(m_pData[i + k]);
                iChar2 = vm_string_tolower(pSrc[k]);
            }

            if(iChar1 == iChar2)
                iCount++;
            else
                break;
        }

        if(iCount == iSrcSize)
            return (int)i;
    }

    return -1;
}

size_t DString::Split(vm_char iCode, DString *pDstStrings, size_t iSize)
{
    if(!m_pData || !pDstStrings)
        return 0;

    size_t iCount = 0;
    size_t iSubLen = 0;
    vm_char *pStart = m_pData;

    for(size_t i = 0; i <= m_iLen; i++)
    {
        if(m_pData[i] == iCode || m_pData[i] == 0)
        {
            pDstStrings[iCount].Replace(pStart, iSubLen);
            pStart = &m_pData[i + 1];
            iSubLen = 0;
            iCount++;
        }
        else
            iSubLen++;

        if(iCount >= iSize)
            break;
    }

    return iCount;
}

size_t DString::Count(vm_char iCode)
{
    if(!m_pData)
        return -1;

    size_t iCount = 0;

    for(size_t i = 0; i < m_iLen; i++)
    {
        if(m_pData[i] == iCode)
            iCount++;
    }

    return iCount;
}

size_t DString::Trim()
{
    return TrimRight() + TrimLeft();
}

size_t DString::TrimLeft()
{
    size_t iSpaces = 0;
    size_t i;

    if(!m_iLen)
        return 0;

    for(i = 0; i < m_iLen; i++)
    {
        if(m_pData[i] != VM_STRING(' '))
            break;

        iSpaces++;
    }

    if(iSpaces)
    {
        for(i = iSpaces; i < m_iLen; i++)
            m_pData[i - iSpaces] = m_pData[i];

        m_iLen -= iSpaces;
        m_pData[m_iLen] = VM_STRING('\0');
    }

    return iSpaces;
}

size_t DString::TrimRight()
{
    size_t iSpaces = 0;

    if(!m_iLen)
        return 0;

    for(size_t i = m_iLen - 1; ; i--)
    {
        if(m_pData[i] != VM_STRING(' '))
            break;

        iSpaces++;
        if(!i)
            break;
    }
    m_iLen = m_iLen - iSpaces;
    m_pData[m_iLen] = VM_STRING('\0');

    return iSpaces;
}

size_t DString::RemoveDuplicates(int iCode)
{
    size_t iShift = 0;

    if(!m_iLen)
        return 0;

    for(size_t i = 0; i < m_iLen - 1; i++)
    {
        if((!iCode || iCode == m_pData[i]) && m_pData[i] == m_pData[i + 1])
            iShift++;
        else if(iShift)
            m_pData[i - iShift + 1] = m_pData[i + 1];
    }
    m_iLen = m_iLen - iShift;
    m_pData[m_iLen] = VM_STRING('\0');

    return iShift;
}

size_t DString::Resize(size_t iSize)
{
    if(m_iSize < iSize + 1)
    {
        vm_char *pDataOld = m_pData;

        m_iSize = iSize + 1;
        m_pData = new vm_char[m_iSize];
        if(pDataOld)
        {
            memcpy(m_pData, pDataOld, m_iLen*sizeof(vm_char));
            delete[] pDataOld;
        }

        m_pData[m_iLen] = VM_STRING('\0');
    }

    return m_iSize;
}

size_t DString::AdjustLength()
{
    m_iLen = vm_string_strlen(m_pData);
    if(m_iLen >= m_iSize)
    {
        m_iLen = m_iSize - 1;
        m_pData[m_iLen] = VM_STRING('\0');
    }
    return m_iLen;
}

/*
// CMD parser
*/
/*
 * Function to parse command line according to options specified in pOptions argument.
 * Returns number of successfully parsed options
 */
Status OptParse(int argc, char **argv, OptDef pOptions[])
{
    OptDef      *pCurOption;
    unsigned int iOptions = 0;
    unsigned int iParsed  = 0;
    unsigned int iShift   = 0;
    bool         bDash    = false;
    bool         bMatch   = false;
    unsigned int i, k;
    int          j;

    /* Determine number of options in opt table */
    for(i = 0; ; i++)
    {
        pCurOption = &pOptions[i];
        if(0 == pCurOption->iShortKey)
            break;

        if(pCurOption->keyType == KT_BOOL)
        {
            if(pCurOption->iFlags & KF_OPTIONAL) // booleans cannot be optional keys
                return STS_ERR_INVALID_PARAMS;
        }
        else if(!pCurOption->iArraySize)
            return STS_ERR_INVALID_PARAMS;

        iOptions++;
    }

    for(j = 1; j < argc; j++)
    {
        bDash  = false;
        bMatch = false;

        if(argv[j][0] == '-' && argv[j][1] != 0)
            bDash = true;

        for(i = 0; i < iOptions; i++)
        {
            pCurOption = &pOptions[i];
            if(pCurOption->iFlags & KF_WAS_PARSED)
                continue;

            if(bDash)
            {
                if(argv[j][1] == pCurOption->iShortKey)
                {
                    if(argv[j][2] != 0) // no space between key and value
                        iShift = 2;

                    if(pCurOption->keyType == KT_BOOL)
                    {
                        *((bool*)pCurOption->pValue) = true;
                        pCurOption->iFlags |= KF_WAS_PARSED;

                        while(iShift && argv[j][iShift]) // boolean sequence
                        {
                            bMatch = false;

                            for(k = 0; k < iOptions; k++)
                            {
                                if(pOptions[k].iFlags & KF_WAS_PARSED)
                                    continue;

                                if(argv[j][iShift] == pOptions[k].iShortKey)
                                {
                                    if(pOptions[k].keyType == KT_BOOL)
                                    {
                                        *((bool*)pOptions[k].pValue) = true;
                                        pOptions[k].iFlags |= KF_WAS_PARSED;
                                    }
                                    else
                                        return STS_ERR_INVALID_PARAMS; // wrong sequence

                                    bMatch = true;
                                    break;
                                }
                            }
                            if(!bMatch)
                                return STS_ERR_INVALID_PARAMS; // wrong sequence

                            iShift++;
                        }
                    }
                    else
                    {
                        if(!iShift && (j + 1 >= argc))
                            return STS_ERR_INVALID_PARAMS;

                        for(k = 0; k < pCurOption->iArraySize; k++)
                        {
                            if(!iShift)
                            {
                                if(argv[j+1][0] == '-' && !vm_string_isdigit(argv[j+1][1])) // next argument, check if it is minus
                                    break;
                                j++;
                            }
                            
                            switch(pCurOption->keyType)
                            {
                            case KT_INTEGER:
                                ((int*)pCurOption->pValue)[k] = vm_string_atoi(&argv[j][iShift]);
                                break;
                            case KT_POSITIVE:
                                if( ( ( (int*)pCurOption->pValue)[k] = vm_string_atoi(&argv[j][iShift]) ) <= 0)
                                    return STS_ERR_INVALID_PARAMS;
                                break;
                            case KT_DOUBLE:
                                ((double*)pCurOption->pValue)[k] = vm_string_atof(&argv[j][iShift]);
                                break;
                            case KT_STRING:
                                ((char**)pCurOption->pValue)[k] = &argv[j][iShift];
                                break;
                            case KT_DSTRING:
                                ((DString*)pCurOption->pValue)[k] = &argv[j][iShift];
                                break;
                            }
                            iParsed++;
                            iShift = 0;

                            if(j + 1 >= argc)
                                break;
                        }
                    }
                    pCurOption->iFlags |= KF_WAS_PARSED;
                    bMatch = true;
                    break;
                }
            }
            else if(pCurOption->iFlags & KF_OPTIONAL)
            {
                for(k = 0; k < pCurOption->iArraySize; k++)
                {
                    if(k != 0)
                    {
                        if(argv[j+1][0] == '-') // next argument
                            break;
                        j++;
                    }

                    switch(pCurOption->keyType)
                    {
                    case KT_INTEGER:
                        ((int*)pCurOption->pValue)[k] = vm_string_atoi(argv[j]);
                        break;
                    case KT_DOUBLE:
                        ((double*)pCurOption->pValue)[k] = vm_string_atof(argv[j]);
                        break;
                    case KT_STRING:
                        ((char**)pCurOption->pValue)[k] = argv[j];
                        break;
                    case KT_DSTRING:
                        ((DString*)pCurOption->pValue)[k] = argv[j];
                        break;
                    }
                    iParsed++;

                    if(j + 1 >= argc)
                        break;
                }

                pCurOption->iFlags |= KF_WAS_PARSED;
                bMatch = true;
                break;
            }
        }
        if(!bMatch) // invalid key
            return STS_ERR_INVALID_PARAMS;
    }

    return STS_OK;
}

/*
 * Function to print option table as usage message
 */
void OptUsage(const OptDef pOptions[])
{
    char          sKeyMsg[64];
    char          sSpaces[64];
    const OptDef *pCurOption;
    unsigned int  iStringSize = 0;
    unsigned int  iMaxString  = 0;
    unsigned int  iPass;
    unsigned int  i, k;

    char *pPtr;
    char *pDescBuffer = 0;
    unsigned int iPos = 0;
    unsigned int iMaxDesc = 0;


    for(iPass = 0; iPass < 2; iPass++)
    {
        for(i = 0; ; i++)
        {
            pCurOption = &pOptions[i];
            if(0 == pCurOption->iShortKey)
                break;

            if(pCurOption->keyType == KT_BOOL)
                iStringSize = vm_string_snprintf(sKeyMsg, 64, "  -%c", pCurOption->iShortKey);
            else
            {
                if(pCurOption->iArraySize == 1)
                    iStringSize = vm_string_snprintf(sKeyMsg, 64, "  -%c <1 arg>", pCurOption->iShortKey);
                else
                    iStringSize = vm_string_snprintf(sKeyMsg, 64, "  -%c <%d args>", pCurOption->iShortKey, pCurOption->iArraySize);
            }

            if(iPass == 0)
            {
                if(iStringSize > iMaxString)
                    iMaxString = iStringSize;

                iStringSize = (unsigned int)vm_string_strlen(pCurOption->pDescription) + 1;
                if(iStringSize > iMaxDesc)
                    iMaxDesc = iStringSize;
            }
            else
            {
                if(iStringSize < iMaxString)
                {
                    for(k = 0; k < iMaxString - iStringSize; k++)
                        sKeyMsg[iStringSize + k] = ' ';
                    sKeyMsg[iMaxString] = 0;
                }
                printf("%s ", sKeyMsg);

                pPtr = (char*)vm_string_strchr(pCurOption->pDescription, '|');
                if(pPtr)
                {
                    if(!pDescBuffer)
                    {
                        pDescBuffer = new char[iMaxDesc];
                        for(k = 0; k < iMaxString; k++)
                            sSpaces[k] = ' ';
                        sSpaces[iMaxString] = 0;
                    }
                    vm_string_strcpy(pDescBuffer, pCurOption->pDescription);

                    do
                    {
                        pPtr = (char*)vm_string_strchr(pDescBuffer + iPos, '|');
                        if(pPtr)
                        {
                            pPtr[0] = 0;

                            if(iPos == 0)
                                printf("%s\n", pDescBuffer);
                            else
                                printf(" %s%s\n", sSpaces, pDescBuffer + iPos);

                            iPos = (unsigned int)(pPtr - pDescBuffer) + 1;
                        }
                        else
                            printf(" %s%s\n\n", sSpaces, pDescBuffer + iPos);
                    } while(pPtr);
                }
                else
                    printf("%s\n", pCurOption->pDescription);
            }
        }
    }

    if(pDescBuffer)
        delete[] pDescBuffer;
}

const char* GetBaseStatusString(Status status)
{
    int iTableSize = sizeof(StringOfBaseStatus)/sizeof(StringOfBaseStatus[0]);

    for (int i = 0; i < iTableSize; i++)
    {
        if (StringOfBaseStatus[i].iCode == status)
            return StringOfBaseStatus[i].pString;
    }
    return sStatusEmpty;
}

static const char *sTestDirs[] = {"", "../", "../kernels/", "../bin/", "../bin/kernels/", "../../../../bin/", "../../../../bin/kernels/",
        "../../../../sources/bin/", "../../../../sources/bin/kernels", 0}; // local, prebuild, custom build, debug, cmake relative paths
DString CheckTestDirs(DString sTestFile)
{
    DString sDir;
    DString sOut;
    int i = 0;

    while(sTestDirs[i])
    {
        sDir = sTestDirs[i];
        sOut = sDir + sTestFile;
        if(vm_file_access(sOut, 0) != -1)
            return sOut;
        i++;
    }

    return sTestFile;
}

char* GetProgName(char* argv[])
{
    char *pName = argv[0];
    for(size_t i = vm_string_strlen(argv[0]) - 1; i > 0; i--)
    {
        if(argv[0][i] == '\\' || argv[0][i] == '/')
        {
            pName = &argv[0][i+1];
            break;
        }
    }
    return pName;
}
