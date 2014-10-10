 ! Copyright (C) 2010 Intel Corporation. All Rights Reserved.
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
 ! Part of the Guided Auto Parallelization tutorial.  For details,
 ! please see Intel(R) Fortran Composer Getting Started Tutorials
 !

module scalar_dep
    integer, parameter :: size=100000
    real(8), dimension(size) :: a
    real(8)                  :: b

! loop will be vectorized if variable t can be unconditionally initialized 
! at the top of every iteration.

    contains
    subroutine test_scalar_dep(n)
        integer n
        integer i
        integer t
    !TODO: GAP suggested the following changes to get the loop to parallelize

    ! GAP REPORT LOG OPENED ON Thu May 20 15:21:12 2010
    ! C:\scalar_dep.f90(66): remark #30523: (PAR) Loop at line 66 cannot be parallelized 
    ! due to conditional assignment(s) into the following variable(s): T. This loop will 
    ! be parallelized if the variable(s) become unconditionally initialized at the top of 
    ! every iteration. [VERIFY] Make sure that the value(s) of the variable(s) read in any 
    ! iteration of the loop must have been written earlier in the same iteration. 
    ! [ALTERNATIVE] Another way is to use "!dir$ parallel private(T)" to parallelize the loop. 
    ! [VERIFY] The same conditions described previously must hold.
    ! C:\scalar_dep.f90(66): remark #30525: (PAR) If the trip count of the loop at line 66 is 
    ! greater than 36, then use "!dir$ loop count min(36)" to parallelize this loop. [VERIFY] 
    ! Make sure that the loop has a minimum of 36 iterations.
    ! Number of advice-messages emitted for this compilation session: 2.
    ! END OF GAP REPORT LOG

    !TODO: GAP suggested the following changes to get the loop to vectorized

    ! GAP REPORT LOG OPENED ON Thu May 20 15:22:14 2010
    ! C:\scalar_dep.f90(66): remark #30515: (VECT) Loop at line 66 cannot be vectorized due to 
    ! conditional assignment(s) into the following variable(s): T. This loop will be vectorized 
    ! if the variable(s) become unconditionally initialized at the top of every iteration. 
    ! [VERIFY] Make sure that the value(s) of the variable(s) read in any iteration of the loop 
    ! must have been written earlier in the same iteration.
    ! Number of advice-messages emitted for this compilation session: 1.
    ! END OF GAP REPORT LOG

!dir$ if defined(test_gap)
!dir$ loop count min(44)
!dir$ endif
        do i = 1, n
!dir$ if defined(test_gap)
            t = i
!dir$else
            if (a(i) >= 0) then
                t = i
            end if
!dir$ endif
            if (a(i) > 0) then
                a(i) = t * (1 / (a(i) * a(i)))
            end if
        end do
    end subroutine test_scalar_dep
end module scalar_dep
