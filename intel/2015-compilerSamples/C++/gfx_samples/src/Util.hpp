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

#pragma once



       bool errorf (float a,    float b,    int scale = 1);
//inline bool errorf (int a,      int b,      int scale = 1)		{return a != b;}
inline bool errorf (unsigned a, unsigned b, int scale = 1)		{return a != b;}


struct FloatError
{
	const float maxAbsoluteError,	//1.175494351e–38
				maxRelativeError,	//1.192092896e–07
				relativeLimit;		// scale*maxRelativeError

	float max_error;

	FloatError (int scale = 1);
	FloatError (float err /*= 1.0e-3f*/);

	bool errorf (float a, float b);
};


float randf (float fmin, float fmax);

int ilog2 (int x);

