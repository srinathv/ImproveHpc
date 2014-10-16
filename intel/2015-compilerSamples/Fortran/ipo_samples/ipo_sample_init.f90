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

! Initialize array 'a' to REALs.
!   This function is not inlined due to compiler heuristics
!   for initialization routines.
subroutine init(a, n)
  implicit none
  integer, intent(in) :: n
  real, intent(inout) :: a(n) 
  integer :: i
  do i = 1, n 
    a(i) = real(i) 
  end do 
end subroutine init


