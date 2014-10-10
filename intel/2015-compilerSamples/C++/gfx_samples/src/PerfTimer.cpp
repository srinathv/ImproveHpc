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

#include <PerfTimer.h>

const int GNSEC = 1000000000;

PerfTimer::PerfTimer() {
#ifndef __linux__
    QueryPerformanceFrequency(&freq);
#endif
}

void PerfTimer::start()
{
#ifdef __linux__
    clock_gettime(CLOCK_REALTIME, &stamp1);
#else
    QueryPerformanceCounter(&stamp1);
#endif
}

void PerfTimer::stop()
{
#ifdef __linux__
    clock_gettime(CLOCK_REALTIME, &stamp2);
#else
    QueryPerformanceCounter(&stamp2);
#endif
}

#ifdef __linux__
int PerfTimer::subtract(timespec &result, const timespec &x, const timespec &y)
{
	if (y.tv_nsec < x.tv_nsec) {
        result.tv_nsec = GNSEC + y.tv_nsec - x.tv_nsec;        
        result.tv_sec = y.tv_sec - 1 - x.tv_sec;
    } else {
        result.tv_nsec = y.tv_nsec - x.tv_nsec;        
        result.tv_sec = y.tv_sec - x.tv_sec;
    }
}
#endif

size_t PerfTimer::nanosec() const {
#ifdef __linux__
	timespec sub;
	subtract(sub, stamp1, stamp2);

	return sub.tv_sec * GNSEC + sub.tv_nsec;
#else
	return (stamp2.QuadPart - stamp1.QuadPart)*1000000000 / freq.QuadPart;
#endif
}

double PerfTimer::sec() const {
#ifdef __linux__
	timespec sub;
	subtract(sub, stamp1, stamp2);

	return (double)sub.tv_sec + (double)sub.tv_nsec/GNSEC;
#else
	return double(stamp2.QuadPart - stamp1.QuadPart) / double(freq.QuadPart);
#endif
}

simple_timer::simple_timer()				
	:sampling_count(0), time(0)	{
}

void simple_timer::clear() {
	time = 0;
	sampling_count = 0;
}

void simple_timer::add(const PerfTimer& pc) {
	time += pc.nanosec();
	sampling_count++;
}

void simple_timer::add(const size_t& ns) {
	time += ns;
	sampling_count++;
}

size_t simple_timer::nano_sec () const {
	return time;
}

int simple_timer::samples () const {
	return sampling_count;
}
