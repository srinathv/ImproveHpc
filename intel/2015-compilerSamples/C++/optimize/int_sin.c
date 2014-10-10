/*
 * Copyright (C) 2005-2012 Intel Corporation. All Rights Reserved.
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
 * This sample integrates a function over a given interval using the well-known
 * midpoint rule:
 * Given height = f(x_i), compute the area of the rectangle of width "step"
 * Simply sum the areas of internal rectangles: f(xi + step) * step 
 *
 * Compile the sample several times using different optimization options.
 *
 * Read the Intel(R) C++ Compiler Documentation for more information about these options.
 * 
 * Some of these automatic optimizations use features and options
 * that can restrict program execution to specific architectures. 
 * 
 * [COMPILE] 
 * Specify one of the following compiler options: 
 *
 * Windows*: /O1, /O2, /O3
 *
 * Linux* and OS X*: -O1, -O2, -O3
 *
 */

#include <stdio.h>
#include <stdlib.h> 
#include <time.h> 
#include <mathimf.h>

// Function to be integrated
// Define and prototype it here
// | sin(x) |
#define INTEG_FUNC(x)  fabs(sin(x))

// Prototype timing function
double dclock(void);

int main(void)
{
   // Loop counters and number of interior points
   unsigned int i, j, N;
   // Stepsize, independent variable x, and accumulated sum
   double step, x_i, sum;
   // Timing variables for evaluation   
   double start, finish, duration, clock_t;
   // Start integral from 
   double interval_begin = 0.0;
   // Complete integral at 
   double interval_end = 2.0 * 3.141592653589793238;

   // Start timing for the entire application
   start = clock();

   printf("     \n");
   printf("    Number of    | Computed Integral | \n");
   printf(" Interior Points |                   | \n");
   for (j=2;j<27;j++)
   {
    printf("------------------------------------- \n");

     // Compute the number of (internal rectangles + 1)
     N =  1 << j;

     // Compute stepsize for N-1 internal rectangles 
     step = (interval_end - interval_begin) / N;

     // Approx. 1/2 area in first rectangle: f(x0) * [step/2] 
     sum = INTEG_FUNC(interval_begin) * step / 2.0;

     // Apply midpoint rule:
     // Given length = f(x), compute the area of the
     // rectangle of width step
     // Sum areas of internal rectangle: f(xi + step) * step 

     for (i=1;i<N;i++)
     {
        x_i = i * step;
        sum += INTEG_FUNC(x_i) * step;
     }

     // Approx. 1/2 area in last rectangle: f(xN) * [step/2] 
     sum += INTEG_FUNC(interval_end) * step / 2.0;

     printf(" %10d      |  %14e   | \n", N, sum);
   }
   finish = clock();
   duration = (finish - start);
   printf("     \n");
   printf("   Application Clocks   = %10e  \n", duration);
   printf("     \n");
}


