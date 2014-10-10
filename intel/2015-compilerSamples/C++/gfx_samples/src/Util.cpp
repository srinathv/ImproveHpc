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

#include "Util.hpp"
#include <stdlib.h>
#include <float.h>
#include <math.h>
#include <iostream>
#include <iomanip>

using namespace std;


bool errorf (float a, float b, int scale)
{
	const float maxAbsoluteError = FLT_MIN ;		//1.175494351e–38
	const float maxRelativeError = FLT_EPSILON ;	//1.192092896e–07

	float fab = fabs(a - b);
    if (fab <= maxAbsoluteError)
        return false;

	float fa  = fabs(a),
		  fb  = fabs(b);
	if (fab / ((fa > fb) ? fa : fb) <= scale*maxRelativeError)
		return false;

    return true;
}


FloatError::FloatError (int scale)
: maxAbsoluteError(FLT_MIN),		//1.175494351e–38
  maxRelativeError(FLT_EPSILON),	//1.192092896e–07
  relativeLimit(scale*maxRelativeError)
{
	max_error = 0;
}


FloatError::FloatError (float err)
: maxAbsoluteError(FLT_MIN),		//1.175494351e–38
  maxRelativeError(FLT_EPSILON),	//1.192092896e–07
  relativeLimit(err)
{
	max_error = 0;
}


bool FloatError::errorf(float a, float b)
{
	float fab = fabs(a - b);
    if (fab <= maxAbsoluteError)
        return false;

	float fa = fabs(a),
		  fb = fabs(b),
		  fx = fab / ((fa > fb) ? fa : fb);
	if (fx <= relativeLimit)
		return false;

	if (max_error < fx)
	{
		max_error = fx;
		//cout << "*max_error: " << fx << " for (" << a << ", " << b << ")" << endl;
	}

    return true;
}


float randf (float fmin, float fmax)
{
	return fmin + (fmax - fmin)*(float)rand()/RAND_MAX;
}


int ilog2 (int x)
{
	int ilog = 0;

	for (; x > 1; x >>= 1)
		++ilog;

	return ilog;
}
