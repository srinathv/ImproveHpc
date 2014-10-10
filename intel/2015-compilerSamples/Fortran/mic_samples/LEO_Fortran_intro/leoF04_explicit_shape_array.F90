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
!#******************************************************************************
!# Content:
!#
!#  Example Program Text from Sample LEO_Fortran_intro
!#******************************************************************************

!* leoF04_explicit_shape_array ...............................................
!
! This sample demonstrates using the OMP OFFLOAD and OFFLOAD BEGIN/END
! directives with tailored data transfers to offload code sections that
! use local explicit-shape arrays.
!
! Clarifications:
!
!  1. All variables (i, sz, in1, in2, res_omp, res_off) exchanged between the
!     Host and Target CPUs are lexically visible (to the compiler) within
!     the offloaded code; therefore, none require naming in an IN/OUT/INOUT
!     clause and all are treated as INOUT by default by the compiler.
!
!  2. The scalar variables (i, sz) are small in size and of lesser concern 
!     for tailoring their exchange between the Host and Target CPUs.
!
!  3. The arrays are larger and candidates for tailoring their exchange
!     between the Host and Target CPUs with explicit IN/OUT clauses befitting 
!     of use within the offload code.  (i.e., IN(in1, in2) )
!
!.............................................................................


    subroutine leoF04_explicit_shape_array()

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    implicit none

    character (LEN=*), parameter  :: sample_name = "leoF04_explicit_shape_array"
    integer          , parameter  :: SZ = 1000

    integer               :: i
    real, dimension(SZ)   :: in1, in2, res_omp, res_off
    character (LEN=15)    :: err_str

    ! Setup and initialization
    num_offload_sections = 2       ! number of offload sections in sample

    call leoF_setup(sample_name)

    in1 = (/(REAL(i), i=1,SZ)/)
    in2 = in1
    res_omp = 0

    offload_section_id = 1         ! offload section #1

    !  Offload OMP construct with OMP OFFLOAD directive

    !  Tailor data transfers with an IN clause to only transfer values of 
    !  arrays IN1 and IN2 from the Host CPU to the Target CPU

    !  Use of offload_section_id and modification of offload_sections
    !  in offload_check is outside the compiler's lexical scope; therefore,
    !  use explicit IN/INOUT clauses

    !  For demonstration purposes limit # of OMP threads to 4

    !DIR$ OMP OFFLOAD target(mic) optional   in(in1, in2)    &
                      in(offload_section_id) inout(offload_sections)
    !$omp parallel num_threads(4)
        !$omp do 
        do i = 1,SZ
           res_omp(i) = in1(i) + in2(i)
        enddo

        ! Verify offload execution with one thread only.
        !$omp single
           call offload_check()
        !$omp end single
    !$omp end parallel 

    ! Increment offload section id
    offload_section_id = offload_section_id + 1

    !  Offload code block using OFFLOAD BEGIN/END directives

    !  Tailor data transfers with an IN clause to only transfer values of 
    !  arrays IN1 and IN2 from the CPU to the target

    !  Use of offload_section_id and modification of offload_sections
    !  in offload_check is outside the compiler's lexical scope; therefore,
    !  use explicit IN/INOUT clauses

    !DIR$ OFFLOAD BEGIN target(mic) optional   in(in1, in2)    &
                  in(offload_section_id) inout(offload_sections)
    do i = 1,SZ
       res_off(i) = in1(i) + in2(i)
    enddo

    ! Verify offload execution
    call offload_check
    !DIR$ END OFFLOAD 

    if (verbosity >= 2) then
       print "(4X,A,F,A,I0,A,F)", "Results: res_omp(1) =",res_omp(1), &
                          " res_omp(",sz,") =",res_omp(sz)
       print "(13X,A,F,A,I0,A,F)", "res_off(1) =",res_off(1), &
                          " res_off(",sz,") =",res_off(sz)

       ! Display offload section details
       call offload_summary
    endif

    ! Validate results
    if ( (res_omp(1) == 2.0 .AND. res_omp(sz) == (2.0*sz)) .AND. &
         (res_off(1) == 2.0 .AND. res_off(sz) == (2.0*sz)) .AND. &
         offload_verify(3) ) then
         print "(4X,2(A))", "PASS ",sample_name
    else
        if (.NOT. offload_verify(1)) then
           err_str="offload failure"
        else
           err_str="data mismatch"
        endif
        print "(4X,4(A),I0,2(A))", "*** FAIL ", sample_name, "): ", err_str
    endif

    ! Cleanup
    call leoF_cleanup(sample_name)

    return
    end subroutine leoF04_explicit_shape_array
!*................................................ leoF04_explicit_shape_array
