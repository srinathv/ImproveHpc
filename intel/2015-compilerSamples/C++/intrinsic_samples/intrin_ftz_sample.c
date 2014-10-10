/*
* Copyright (C) 2006-2014 Intel Corporation. All Rights Reserved.
*
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
*
* [DESCRIPTION]
* This code sample demonstrates how to use the  
* _MM_GET_FLUSH_ZERO_MODE() and _MM_GET_DENORMALS_ZERO_MODE() 
* macros to read the FTZ and DAZ flags in the control register.
*
* [Compile]
* Windows*: icl
*
* Linux* and OS X*: icc -o <output file name>
*
* Turning off optimization changes the state of the registers.
* Windows*: icl /Od 
*
* Linux* and OS X*: icc -O0
*
* [Output]
* Shows the state of the FTZ and DAZ registers.
*
*/

#include <stdio.h>
#include <xmmintrin.h>
#include <pmmintrin.h>
#pragma warning (disable: 4003)

int main(void){

	/* Test the control register for flush to zero mode */
	if ( _MM_GET_FLUSH_ZERO_MODE() )
		printf("FTZ is set.\n");
	else
		printf("FTZ is not set.\n");

	/* Test the control register for denormals mode */
	if ( _MM_GET_DENORMALS_ZERO_MODE() )
		printf("DAZ is set.\n");
	else
		printf("DAZ is not set.\n");
	return (0);
}
