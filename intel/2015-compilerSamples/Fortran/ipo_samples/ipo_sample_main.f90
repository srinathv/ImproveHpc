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

program main
  implicit none
  interface
    subroutine init(a, n)
!!$omp declare simd (init) uniform(n) 
!$omp declare simd (init) uniform(n) simdlen(4)
      integer, intent(in) :: n
      real, intent(inout) :: a(n)
    end subroutine init 

    real function mysum(a, n)
!!$omp declare simd (mysum) uniform(n)
!$omp declare simd (init) uniform(n) simdlen(4)
      integer, intent(in) :: n
      real, intent(inout) :: a(n)
    end function mysum
  end interface

  real :: res   !!, mysum
  integer :: i
  integer, parameter :: n=10000
  real, dimension(n) :: a
  integer ( kind = 4 ) clock_count
  integer ( kind = 4 ) clock_count1
  integer ( kind = 4 ) clock_count2
  integer ( kind = 4 ) clock_max
  integer ( kind = 4 ) clock_rate
  
  print *, "USING SIMD"
  call system_clock(clock_count,clock_rate,clock_max)
!$omp simd private(a)
  do i = 1, n
  call init(a, n)
  res = mysum(a, n)
  enddo
  call system_clock(clock_count1,clock_rate,clock_max)
  print *, "result is ", res
  print *, clock_count1, clock_count, clock_rate
  print *, "time in seconds = ", (clock_count1-clock_count)/real(clock_rate)
end program main
