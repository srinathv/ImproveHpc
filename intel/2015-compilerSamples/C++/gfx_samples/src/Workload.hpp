/*
    Copyright 2005-2014 Intel Corporation.  All Rights Reserved.

* The source code contained or described herein and all
* documents related to the source code ("Material") are owned by 
* Intel Corporation or its suppliers or licensors. Title to the
* Material remains with Intel Corporation or its suppliers and
* licensors. The Material is protected by worldwide copyright
* laws and treaty provisions.  No part of the Material may be
* used, copied, reproduced, modified, published, uploaded,
* posted, transmitted, distributed,  or disclosed in any way
* except as expressly provided in the license provided with the
* Materials.  No license under any patent, copyright, trade
* secret or other intellectual property right is granted to or
* conferred upon you by disclosure or delivery of the Materials,
* either expressly, by implication, inducement, estoppel or
* otherwise, except as expressly provided in the license
* provided with the Materials. 
*/


/*
 * Workload.hpp - the base class for any sample workload
 */

#pragma once
#include "Properties.hpp"
#include <string>

#ifndef ALIGN
    // no auto-alignment done yet, need to align to page boundary for pin
    #define ALIGN 4096
#endif

class Workload
{
protected:
public:

	enum ExecMode
	{
		OFFLOADoff,
		OFFLOADon,
		CPU
	};

	Properties* props;
	ExecMode exmode;

    double cpu_share;

	static int registry (const char* (*name)(), Workload* (*create)());

	bool prop_get (const char* key, char*& value)			{return props->get(key, value);};
	bool prop_get (const char* key, std::string& value) 	{return props->get(key, value);};
	bool prop_get (const char* key, int& value) 			{return props->get(key, value);};
	bool prop_get (const char* key, unsigned int& value) 	{return props->get(key, value);};
	bool prop_get (const char* key, float&  value) 			{return props->get(key, value);};
	bool prop_get (const char* key, double& value) 			{return props->get(key, value);}
	bool prop_get (const char* key, bool& value) 			{return props->get(key, value);}

    unsigned get_cpu_share(unsigned n, unsigned alignment = 64)         
    {
        if (exmode != OFFLOADon)
            return n;
        unsigned rn = align((unsigned)((double)n * cpu_share), alignment);
        return rn > n ? n : rn; 
    }
    unsigned get_gpu_share(unsigned n, unsigned alignment = 64)         
    { return n - get_cpu_share(n, alignment); }

public:

	bool verify;

	virtual ~Workload ()		{};

	bool setup (Properties*);

	virtual bool open ()		= 0;
	virtual bool execute_offload (int do_offload)	= 0;
	virtual bool execute_cpu ()	= 0;
	virtual bool validate ()	= 0;
	virtual bool close ()		= 0;

	bool execute_offload ()		{return execute_offload(exmode == OFFLOADon);}

    inline static unsigned align(unsigned v, unsigned alignment)
    {
        return (v + alignment - 1) & ~(alignment - 1);
    }
};




#ifdef ALIGN
/*
 * w_malloc is special malloc for memory which can be used in pin clauses 
 * of pragma offload: it is aligned and padded on ALIGH (at least 64 byte) 
 */
    inline void* w_malloc(size_t bytes)						{return _mm_malloc(Workload::align(bytes, ALIGN), ALIGN);}
	inline void  w_free(void* memory)						{_mm_free(memory);}
#else
	inline void* w_malloc(size_t bytes)						{return malloc(bytes);}
	inline void  w_free(void* memory)						{free(memory);}
#endif //#ifdef ALIGN

	inline void* w_malloc_check (size_t bytes)
	{
		void* ptr = w_malloc(bytes);
		if (ptr == 0)
			throw "Memory allocation failure";
		return ptr;
	}

