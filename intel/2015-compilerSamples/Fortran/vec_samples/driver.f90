! Copyright (C) 2013 Intel Corporation. All Rights Reserved.
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
! Part of the vec_samples tutorial. For information, please read
! Tutorial: Auto-vectorization in the Intel(R) Fortran Composer XE 2013
! Getting Started Tutorials document
!


program driver
  implicit none
  
  integer, parameter           :: ROW=101
  integer, parameter           :: COL=101

! Using ROWBUF=3 makes each column of 'a' be aligned at 16-byte intervals by
! adding three elements of padding to each column.

!DIR$ IF DEFINED(ALIGNED)
  integer, parameter           :: ROWBUF=3
!DIR$ ELSE
  integer, parameter           :: ROWBUF=0
!DIR$ END IF

  integer, parameter           :: TOTROW = ROW + ROWBUF
  integer, parameter           :: REPEATNTIMES = 1000000

  integer                      :: i, j
  integer                      :: size1=TOTROW, size2=COL
  real, dimension(TOTROW,COL)  :: a
  real, dimension(COL)         :: b
  real, dimension(TOTROW)      :: c
  real                         :: sum
  real(8)                      :: cptim1, cptim2

!DIR$ IF DEFINED(ALIGNED)
!  aligning the start of each array is unimportant in this simple example.
!  preserving the same alignment for all rows of the matrix is much more important.
!dir$attributes align : 16 :: a,b,c
!DIR$ ENDIF

!   initialize the matrix and vector

    a = reshape( (/((mod(i*j+1,10), i=0,size1-1), j=0,size2-1)/), &
&                (/size1, size2/) )
    b          = (/(mod(j+3,10), j=0,size2-1)/)

    if(ROWBUF.gt.0) a(ROW+1:TOTROW,:) = 0.
			     
!  initialize timing
      call cpu_time(cptim1)
       
!  just do it
      do i=1,REPEATNTIMES
        call matvec(size1, size2, a, b, c)
!  this line so that each iteration is different, so that 
!  the compiler can't optimize away every iteration except one.
        b(1) = b(1) + 0.000001
      enddo

!  print cpu time taken	and a simple checksum
!  (use a different timer for threaded programs)
   call cpu_time(cptim2)
   print '(''time taken '',f8.3,''  sum='',6pe20.12/)', &
&        (cptim2 - cptim1), sum(c)
 
   end program driver