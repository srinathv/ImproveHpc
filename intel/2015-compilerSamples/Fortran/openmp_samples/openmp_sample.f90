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
! This code finds all primes in the first 10,000,000 integers, the number of
! 4n+1 primes, and the number of 4n-1 primes in the same range.
!
! This source illustrates two OpenMP directives to help speed up 
! the code.  First, a dynamic "schedule" clause is used with the OpenMP "for"
! directive. Because the "for" loop's workload increases as its index
! gets bigger, the default "static" scheduling does not work well.
! Instead dynamic scheduling is used to account for the increasing
! workload.  But dynamic scheduling itself has more overhead than
! static scheduling, so a "chunk size" of 10 is used to reduce the
! overhead for dynamic scheduling.  Second, a "reduction" clause is
! used intead of an OpenMP "critical" directive to eliminate lock overhead.
! A "critical" directive would cause excessive lock overhead due to 
! the one-thread-at-time update of the shared variables each 
! time through the "for" loop.  Instead the reduction clause causes only
! one update of the shared variables once at the end of the loop.
!
! [COMPILE] 
! Use the following compiler options to compile both multi- and 
! single-threaded versions. 
!
! Parallel compilation:
!
!   Windows*: /Qopenmp /Qfpp 
!
!   Linux* and OS X*: -qopenmp -fpp 
!
! Serial compilation:
!
!   Use the same command, but omit the -openmp (Linux and OS X)
!   or /Qopenmp (Windows) option.
!

program ompPrime

#ifdef _OPENMP 
   include 'omp_lib.h'  !needed for OMP_GET_NUM_THREADS()
#endif

integer :: start = 1
integer :: end = 10000000
integer :: number_of_primes = 0
integer :: number_of_41primes = 0
integer :: number_of_43primes = 0
integer index, factor, limit, nthr
real rindex, rlimit
logical prime, print_primes

print_primes = .false.
nthr = 1 ! assume just one thread
print *, ' Range to check for Primes:',start,end

#ifdef _OPENMP
!$omp parallel 

!$omp single
   nthr = OMP_GET_NUM_THREADS()
   print *, ' We are using',nthr,' thread(s)'
!$omp end single
!

!
!$omp do private(factor, limit, prime) &
   schedule(dynamic,10) &
   reduction(+:number_of_primes,number_of_41primes,number_of_43primes)
#else
   print *, ' We are using',nthr,' thread(s)'
#endif

do index = start, end, 2   !workshared loop

   limit = int(sqrt(real(index)))
   prime = .true.  ! assume number is prime
   factor = 3

   do
      if(prime .and. factor .le. limit) then
         if(mod(index,factor) .eq. 0) then
            prime = .false.
         endif
         factor = factor + 2
      else
         exit  ! we can jump out of non-workshared loop
      endif
   enddo

   if(prime) then
      if(print_primes) then
         print *, index, ' is prime'
      endif

      number_of_primes = number_of_primes + 1

      if(mod(index,4) .eq. 1) then
         number_of_41primes = number_of_41primes + 1
      endif

      if(mod(index,4) .eq. 3) then
         number_of_43primes = number_of_43primes + 1
      endif

   endif   ! if(prime)
enddo
!$omp end do
!$omp end parallel

print *, ' Number of primes found:',number_of_primes 
print *, ' Number of 4n+1 primes found:',number_of_41primes
print *, ' Number of 4n-1 primes found:',number_of_43primes
end program ompPrime
