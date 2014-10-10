!# Copyright (C) 2012-2014 Intel Corporation. All Rights Reserved.
!#
!# The source code contained or described herein and all
!# documents related to the source code ("Material") are owned by
!# Intel Corporation or its suppliers or licensors. Title to the
!# Material remains with Intel Corporation or its suppliers and
!# licensors. The Material is protected by worldwide copyright
!# laws and treaty provisions.  No part of the Material may be
!# used, copied, reproduced, modified, published, uploaded,
!# posted, transmitted, distributed,  or disclosed in any way
!# except as expressly provided in the license provided with the
!# Materials.  No license under any patent, copyright, trade
!# secret or other intellectual property right is granted to or
!# conferred upon you by disclosure or delivery of the Materials,
!# either expressly, by implication, inducement, estoppel or
!# otherwise, except as expressly provided in the license
!# provided with the Materials.
!#
!#
!#******************************************************************************
!# Content:
!#
!#  Example Program Text from Sample LEO_Fortran_intro
!#******************************************************************************

!* leoF01_scalar .............................................................
!
! This sample demonstrates using the OMP OFFLOAD and OFFLOAD BEGIN/END 
! directives to offload code sections that use scalar variables.
!
! Clarifications:
!
!  1. All scalar variables (cnt, i, pi_omp, pi_off, t) exchanged between the
!     Host and Target CPUs are lexically visible (to the compiler) within
!     the offloaded code; therefore, none require naming in an IN/OUT/INOUT
!     clause and all are treated as INOUT by default by the compiler.
!
!  2. All variables (cnt, i, pi_omp, pi_off, t) are scalars and small in
!     size which lessens concern for tailoring their exchange between the
!     Host and Target CPUs with explicit IN/OUT clauses befitting of use
!     within the offload code.  (i.e., IN(cnt,i,t) )
!
!.............................................................................


    subroutine leoF01_scalar()

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    implicit none

    character (LEN=*),parameter    :: sample_name = "leoF01_scalar"

    real                :: pi_omp, pi_off, t
    integer             :: i, cnt = 10000
    character (LEN=15)  :: err_str

    ! Setup and initialization 
    num_offload_sections = 2       ! number of offload sections in sample

    call leoF_setup(sample_name)

    pi_omp = 0.0
    pi_off = 0.0

    offload_section_id = 1         ! offload section #1

    !  Offload OMP construct with OMP OFFLOAD directive

    !  Use of offload_section_id and modification of offload_sections
    !  in offload_check is outside the compiler's lexical scope; therefore,
    !  use explicit IN/INOUT clauses

    !  For demonstration purposes limit # of OMP threads to 4 


    !DIR$ OMP OFFLOAD target(mic) optional   &
                      in(offload_section_id) inout(offload_sections)
    !$omp parallel private (t) reduction(+:pi_omp) num_threads(4)

        !$omp do 
        do i = 0, (cnt - 1)
           t = ( REAL(i) + 0.5) / REAL(cnt)
           pi_omp = pi_omp + (4.0 / (1.0 + (t * t)))
        enddo

        ! Verify offload execution with one thread only.
        !$omp single
           call offload_check()
        !$omp end single
    !$omp end parallel

    pi_omp = pi_omp / cnt                                  

    ! Increment offload section id
    offload_section_id = offload_section_id + 1 

    !  Offload code block using OFFLOAD BEGIN/END directives.

    !  Use of offload_section_id and modification of offload_sections
    !  in offload_check is outside the compiler's lexical scope; therefore,
    !  use explicit IN/INOUT clauses

    !DIR$ OFFLOAD BEGIN target(mic)  optional   &
                  in(offload_section_id) inout(offload_sections)
    do i = 0, (cnt - 1)
       t = ( REAL(i) + 0.5) / REAL(cnt)
       pi_off = pi_off + (4.0 / (1.0 + (t * t)))
    enddo 

    ! Verify offload execution
    call offload_check()

    !DIR$ END OFFLOAD 

    pi_off = pi_off / cnt

    if (verbosity >= 2) then

       print "(4X,A,F)", "Results: pi_omp = ", pi_omp
       print "(13X,A,F)", "pi_off = ", pi_off

       ! Display offload section details
       call offload_summary()
    endif

    ! Validate results
    if ( (ABS(pi_omp - 3.1415927) <= 0.00001) .AND. &
         (ABS(pi_off - 3.1415927) <= 0.00001) .AND. &
         offload_verify(3) ) then
         print "(4X,2(A))", "PASS ",sample_name
    else
         if (.NOT. offload_verify(1)) then
            err_str="offload failure"
         else
            err_str="data mismatch"
         endif
         print "(4X,4(A))", "*** FAIL ", sample_name, ": ", err_str
    endif

    ! Cleanup
    call leoF_cleanup(sample_name)

    return
    end subroutine leoF01_scalar
!*.............................................................. leoF01_scalar
