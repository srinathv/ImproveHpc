! Copyright (C) 2012 Intel Corporation. All Rights Reserved.
!
! The source code contained or described herein and all
! documents related to the source code ("Material") are owned by
! Intel Corporation or its suppliers or licensors. Title to the
! Material remains with Intel Corporation or its suppliers and
! licensors. The Material is protected by worldwide copyright
! laws and treaty provisions.  No part of the Material may be
! used, copied, reproduced, modified, published, uploaded,
! posted, transmitted, distributed,  or disclosed in any way
! except as expressly provided in the license provided with the
! Materials.  No license under any patent, copyright, trade
! secret or other intellectual property right is granted to or
! conferred upon you by disclosure or delivery of the Materials,
! either expressly, by implication, inducement, estoppel or
! otherwise, except as expressly provided in the license
! provided with the Materials.
!
! [DESCRIPTION]
! One of three source files, and one library file, used
! to demonstrate how to use Interprocedural Optimization (IPO)
! during compilation and linking.
!
! The files needed to compile are:
!  ipo_sample_init.f90
!  ipo_sample_main.f90
!  ipo_sample_sum.f90
! 
! [COMPILE] 
! Use the following compiler options to compile: 
!
! Windows*: /Qipo
!
! Linux* and OS X*: -ipo
!

! This function will be inlined and deleted by intraprocedural
! optimization (IP).
function add3(x)
  implicit none
  real :: add3
  real, intent(in) :: x
  add3 = x + 3.
end function add3

! This function will be inlined into external callers by
! interprocedural optimization (IPO).
function mysum(a, n)
  implicit none
  real :: mysum, add3
  integer, intent(in) :: n
  real, intent(in), dimension(n) :: a
  integer :: i
  mysum = 0
  do i = 1, n 
    mysum = mysum + a(i) + add3(a(i))
  end do 
end function mysum
