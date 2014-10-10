/*
      Copyright 2005-2014 Intel Corporation.  All Rights Reserved.

    The source code contained or described herein and all
    documents related to the source code ("Material") are owned by 
    Intel Corporation or its suppliers or licensors. Title to the
    Material remains with Intel Corporation or its suppliers and
    licensors. The Material is protected by worldwide copyright
    laws and treaty provisions.  No part of the Material may be
    used, copied, reproduced, modified, published, uploaded,
    posted, transmitted, distributed,  or disclosed in any way
    except as expressly provided in the license provided with the
    Materials.  No license under any patent, copyright, trade
    secret or other intellectual property right is granted to or
    conferred upon you by disclosure or delivery of the Materials,
    either expressly, by implication, inducement, estoppel or
    otherwise, except as expressly provided in the license
    provided with the Materials. 
*/

#ifndef __PERFTIMER_H__
#define __PERFTIMER_H__

#ifdef __linux__
#include <time.h>
#else
#include <windows.h>
#endif

class PerfTimer
{
private:
#ifdef __linux__
    timespec stamp1, stamp2;
#else
    LARGE_INTEGER freq, stamp1, stamp2;
#endif

public:
	PerfTimer();

	void start();
	void stop();

	size_t nanosec() const;
	double sec() const;

private:
#ifdef __linux__
    static int subtract(timespec &result, const timespec &x, const timespec &y);
#endif
};

class simple_timer
{
private:
	int sampling_count;
	size_t time;

public:
	simple_timer();

	void clear();

	void add(const PerfTimer& pc);
	void add(const size_t& ns);

	size_t nano_sec() const;
	int samples() const;
};

#endif //__PERFTIMER_H__
