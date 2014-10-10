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

#ifndef __BASE_H__
#define __BASE_H__

#include "vm_base.h"

/*
//////////////////////////////////////////////////
// Define added via compiler flag, to simplify use
#if !defined(__APPLE__) && !defined(USE_MIC)
    #define ENABLE_RENDERING
#endif
//////////////////////////////////////////////////
*/

#define MIN(x, y) ((x < y)?x:y)
#define MAX(x, y) ((x > y)?x:y)
#define ABS(x) ((x < 0)?(-x):x)
#define ROUND0(DIGIT) ((DIGIT < 0)?ceil(DIGIT - 0.5):floor(DIGIT + 0.5))

#if !defined(USE_MIC)
    #define PRINT_FUNC_MESSAGE(STATUS, NAME, MESSAGE) printf("\nError %d in %s: %s\n", (int)STATUS, NAME, MESSAGE);
    #define PRINT_MESSAGE(MESSAGE) printf("\nError: %s\n", MESSAGE);
#else
    #define PRINT_FUNC_MESSAGE(STATUS, NAME, MESSAGE) printf("\nError %d in %s: %s\n", (int)STATUS, NAME, MESSAGE); fflush(0);
    #define PRINT_MESSAGE(MESSAGE) printf("\nError: %s\n", MESSAGE); fflush(0);
#endif

#define CHECK_STATUS_PRINT_AC(STATUS, NAME, MESSAGE, ACTION) \
    if(STATUS != 0) { PRINT_FUNC_MESSAGE(STATUS, NAME, MESSAGE); if(STATUS < 0) {ACTION;} }

#define CHECK_STATUS_PRINT(STATUS, NAME, MESSAGE) \
    CHECK_STATUS_PRINT_AC(STATUS, NAME, MESSAGE,)

#define CHECK_STATUS_PRINT_BR(STATUS, NAME, MESSAGE) \
    CHECK_STATUS_PRINT_AC(STATUS, NAME, MESSAGE, break)

#define CHECK_STATUS_PRINT_RS(STATUS, NAME, MESSAGE) \
    CHECK_STATUS_PRINT_AC(STATUS, NAME, MESSAGE, return STATUS)

struct Rect
{
    int x;
    int y;
    unsigned int width;
    unsigned int height;
};

struct Vect
{
    int x1;
    int y1;
    int x2;
    int y2;
};

enum ReturnStatus
{
    STS_ERR_FAILED                = -999,
    STS_ERR_NOT_INITIALIZED       = -998,
    STS_ERR_NOT_ENOUGH_DATA       = -996,
    STS_ERR_NULL_PTR              = -995,
    STS_ERR_INIT                  = -899,
    STS_ERR_END_OF_STREAM         = -895,
    STS_ERR_ALLOC                 = -883,
    STS_ERR_UNSUPPORTED           = -879,
    STS_ERR_INVALID_PARAMS        = -876,
    STS_ERR_FILE_OPEN             = -875,
    STS_ERR_FORMAT                = -874,
    STS_OK                        =  0,
};

typedef int Status;

struct CodeStringTable
{
    int         iCode;
    const char *pString;
};

static const CodeStringTable StringOfBaseStatus[] = {
    { STS_OK,                   "Success" },
    { STS_ERR_FAILED,           "General failure" },
    { STS_ERR_NOT_INITIALIZED,  "Object is not initialized" },
    { STS_ERR_NOT_ENOUGH_DATA,  "Not enough input data" },
    { STS_ERR_NULL_PTR,         "Unexpected NULL pointer" },
    { STS_ERR_INIT,             "Failed to initialize object" },
    { STS_ERR_END_OF_STREAM,    "End of stream" },
    { STS_ERR_ALLOC,            "Failed to allocate memory" },
    { STS_ERR_UNSUPPORTED,      "Unsupported parameters/mode" },
    { STS_ERR_INVALID_PARAMS,   "Invalid parameters" },
    { STS_ERR_FILE_OPEN,        "Failed to open file" },
    { STS_ERR_FORMAT,           "Invalid format" },
};
static const char *sStatusEmpty = "<no status string>";

/*
// Dynamic strings
*/
class DString
{
public:
    DString();
    DString(const vm_char *pSrc);
    DString(const DString &src);

    ~DString();

    void Clear(); // reset string buffer

    // replace current string with a new string
    size_t Replace(const vm_char* pSrc, size_t iSrcSize);

    // add new string to the end of the current string
    size_t Append(const vm_char* pSrc, size_t iSrcSize);

    int Compare(const vm_char *pSrc, bool bCaseSensitive = true);

    // find substring inside current string and return its position. -1 if nothing was found
    int Find(const vm_char *pSrc, bool bCaseSensitive = true);

    // split string using iCode and write substrings to dst array. Output is an actual number of output strings
    size_t Split(vm_char iCode, DString *pDstStrings, size_t iSize);

    // count the number of specified symbols
    size_t Count(vm_char iCode);

    size_t Trim();
    size_t TrimLeft();
    size_t TrimRight();

    // removes successive repeating symbol codes
    size_t RemoveDuplicates(int iCode = 0);

    // set actual size of string buffer to iSize+1 if it smaller
    size_t Resize(size_t iSize);

    // adjust value of m_iLen if buffer was written indirectly
    size_t AdjustLength();

    size_t                  Size() const { return m_iLen; }
    operator       vm_char*     ()       { return m_pData; }
    operator const vm_char*     () const { return m_pData; }

    vm_char &operator[](size_t i) { return (i < m_iLen)?m_pData[i]:m_pData[m_iLen]; }

    DString& operator=(const vm_char* pSrc)
    {
        Replace(pSrc, (unsigned int)vm_string_strlen(pSrc));
        return *this;
    }

    DString& operator=(const DString &str)
    {
        Replace(str.m_pData, str.m_iLen);
        return *this;
    }

    DString operator+ (const DString &right)
    {
        DString temp;
        temp.Resize(this->Size() + right.Size());
        temp = *this;
        temp += right;
        return temp;
    }
    DString operator+ (const vm_char *right)
    {
        DString temp;
        temp.Resize(this->Size() + vm_string_strlen(right));
        temp = *this;
        temp += right;
        return temp;
    }
    DString operator+ (const vm_char right)
    {
        DString temp;
        temp.Resize(this->Size() + 1);
        temp = *this;
        temp += right;
        return temp;
    }

    void operator+= (const DString &right)
    {
        Append(right.m_pData, right.m_iLen);
    }
    void operator+= (const vm_char *right)
    {
        Append(right, (unsigned int)vm_string_strlen(right));
    }
    void operator+= (const vm_char right)
    {
        Append(&right, 1);
    }

    bool operator== (const vm_char *right)
    {
        if(!Compare(right))
            return true;
        return false;
    }

    bool operator== (const DString &right)
    {
        if(!Compare(right.m_pData))
            return true;
        return false;
    }

    bool operator!= (const vm_char *right)
    {
        if(Compare(right))
            return true;
        return false;
    }

    bool operator!= (const DString &right)
    {
        if(Compare(right.m_pData))
            return true;
        return false;
    }

protected:
    vm_char *m_pData; // string buffer
    size_t   m_iSize; // buffer size in symbols
    size_t   m_iLen;  // string length
};

/*
// CMD parser
*/
/* Option flags */
#define KF_OPTIONAL   0x0001 // value can be specified without key, in parsing order
#define KF_WAS_PARSED 0x1000 // key was processed by parser

enum KeyType
{
    KT_STRING = 0,
    KT_INTEGER,
    KT_POSITIVE,
    KT_DOUBLE,
    KT_BOOL,
    KT_DSTRING
};

struct OptDef
{
    const char   iShortKey;
    unsigned int iArraySize;
    KeyType      keyType;
    int          iFlags;
    void        *pValue;
    const char  *pDescription;
};

// Function to parse command line according to options specified in pOptions argument.
// Returns number of successfully parsed options
Status OptParse(int argc, char **argv, OptDef pOptions[]);

// Function to print option table as usage message
void OptUsage(const OptDef pOptions[]);

/*
// Utility functions
*/
const char* GetBaseStatusString(Status status);

template<class T> inline T alignValue(size_t iValue, size_t iAlignValue)
{
    return (T)((iValue + (iAlignValue - 1)) & ~(iAlignValue - 1));
}

template<class T> void SortObjects(T *pArray, unsigned int iSize)
{
    bool bOutOfOrder = false;
    T    tempEntry;
    unsigned int i;

    do
    {
        bOutOfOrder = false;
        tempEntry   = pArray[0];
        for(i = 1; i < iSize; i++)
        {
            if(tempEntry.PriorityCompare(pArray[i]))
            {
                pArray[i - 1] = pArray[i];
                pArray[i]     = tempEntry;
                bOutOfOrder    = true;
            }
            else
                tempEntry = pArray[i];
        }
        iSize--;
    } while(bOutOfOrder);
}

DString CheckTestDirs(DString sTestFile);

char* GetProgName(char* argv[]);

#endif
