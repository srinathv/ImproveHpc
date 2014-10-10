/*
 * Copyright (C) 2006-2012 Intel Corporation. All Rights Reserved.
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
 * One of three source files, and one library file, used
 * to demonstrate how to use Interprocedural Optimization (IPO)
 * during compilation and linking.
 *
 * The files needed to compile are:
 *  ipo_sample_defs.h
 *  ipo_sample_init.c
 *  ipo_sample_main.c
 *  ipo_sample_sum.c
 * 
 * [COMPILE] 
 * Use the following compiler options to compile: 
 * 
 * Windows*: /Qipo
 * 
 * Linux* and OS X*: -ipo
 *
 */

#define N 1000

#include <stdio.h>
#include <math.h>
#include "ipo_sample_defs.h"

int main(void)
{
  float sumres;
  double powres;
  float a[N];
  // Call a function from another file to be inlined.
  // Not inlined due to compiler heuristics.
  init(a, N);
  // Call a function from another file to be inlined.
  // This should be inlined.
  sumres = sum(a, N);
  // This won't be inlined unless IPO is active and the math
  // library is also compiled with IPO.
  powres = pow(2.0, 4.0);

  fprintf(stdout, "%g %g\n", sumres, powres);
  return 0;
}
