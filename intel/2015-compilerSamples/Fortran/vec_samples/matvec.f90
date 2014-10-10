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
! Part of the vec_samples tutorial. For information, please read
! Tutorial: Auto-vectorization in the Intel(R) Fortran Composer XE 2013
! Getting Started Tutorials document
!

subroutine matvec(size1,size2,a,b,c)
  implicit none
  integer,                      intent(in)  :: size1,size2
  real, dimension(size1,size2), intent(in)  :: a
  real, dimension(size2),       intent(in)  :: b
  real, dimension(size1),       intent(out) :: c
  integer                                   :: i,j,k

  c=0.
  do j=1,size2

!DIR$ IF DEFINED(ALIGNED)
!dir$ vector aligned
!DIR$ END IF
     do i=1,size1
        c(i) = c(i) + a(i,j) * b(j)
     enddo
  enddo

end subroutine matvec