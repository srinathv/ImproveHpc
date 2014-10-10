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

!* leoF03_various_clauses ....................................................
!
! This sample demonstrates using the OMP OFFLOAD and OFFLOAD BEGIN/END
! directives with specific IN/OUT/INOUT clauses to tailor data transfers 
! associated with adjustable arrays used within an offload loop construct.
!
! Clarifications:
!
!  1. All variables (i, tmp_e, min_val, array_A, array_B, var_b, var_c) 
!     exchanged between the Host and Target CPUs are lexically visible
!     (to the compiler) within the offloaded code; therefore, none require 
!     naming in an IN/OUT/INOUT clause and all are treated as INOUT by 
!     default by the compiler. For demonstration purposes, the variables
!     appear in explicit clauses as described below.
!
!  2. Scalar variables (i,tmp_e, min_val) are small in size and of lesser 
!     concern for tailoring their exchange between the Host and Target 
!     CPUs; however, for demonstration purposes, the variables appear in
!     the IN clause to limit the transfer of their initial value only 
!     from the Host CPU to the Target CPU.
!
!  3. Scalar variable (tmp_e) is used exclusively within the offload loop 
!     construct; therefore, it is unnecessary to exchange its initial and 
!     final values between the Host and Target CPUs. Use the NOCOPY clause
!     to exclude the variable from any data exchange.
!
!  4. Scalar variable (DIMX) is associated with a read-only PARAMETER (SZ).
!     Currently, parameter "constants" (e.g., DIMX) used in an offload 
!     code section must be named in an IN clause to protect them against 
!     attempted update associated with the INOUT default. Failure to adhere 
!     to this current requirement may result in a run-time failure during 
!     execution of the offload code section.  
!
!  5. The arrays (array_A, array_B) are larger and candidates for tailoring 
!     their exchange between the Host and Target CPUs with explicit IN/OUT 
!     clauses befitting of use within the offload code. For demonstration
!     purposes, each appears in a specific clause.
!     (i.e., IN(array_A) OUT(array_B) )
!
!.............................................................................


    subroutine leoF03_clauses_offld(                                       &
               array_A, array_B, var_c, var_d, DIMX, min_val, offload_case)

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    implicit none

    integer,                  intent(in)     :: var_c, min_val, DIMX
    integer,                  intent(in)     :: offload_case
    integer,                  intent(inout)  :: var_d
    integer, dimension(DIMX), intent(in)     :: array_A
    integer, dimension(DIMX), intent(out)    :: array_B  

    integer                                  :: tmp_e, i


    OFFLOAD_TYPE: select case(offload_case)

    CASE(1)

       offload_section_id = 1         ! offload section #1

       !  Offload OMP construct using OMP OFFLOAD directive

       !  Use of offload_section_id and modification of offload_sections 
       !  in offload_check is outside the compiler's lexical scope; therefore, 
       !  use explicit IN/INOUT clauses

       !  For demonstration purposes limit # of OMP threads to 4

       !DIR$ OMP OFFLOAD target(mic) optional                           &
                                     in(array_A, var_c, DIMX, min_val)  &
                                     out(array_B)                       &
                                     inout(var_d)                       &
                                     nocopy(tmp_e)                      &
                         in(offload_section_id) inout(offload_sections)
       !$omp parallel reduction(+:var_d) private(tmp_e) num_threads(4)
           !$omp do
           do i = 1,DIMX
              tmp_e    = array_A(i)**2
              array_B(i) = tmp_e + var_c    
              if ( array_A(i) > min_val ) var_d = var_d + tmp_e
           enddo
  
           ! Verify offload execution with one thread only.
           !$omp single
              call offload_check()
           !$omp end single
       !$omp end parallel

    CASE(2)

       offload_section_id = 1         ! offload section #1

       !  Offload code block using OFFLOAD BEGIN/END directives

       !  Use of offload_section_id and modification of offload_sections 
       !  in offload_check is outside the compiler's lexical scope; therefore, 
       !  use explicit IN/INOUT clauses

       !  For demonstration purposes limit # of OMP threads to 4

       !DIR$ OFFLOAD BEGIN target(mic) optional                           &
                                       in(array_A, var_c, DIMX, min_val)  &
                                       out(array_B)                       &
                                       inout(var_d)                       &
                                       nocopy(tmp_e)                      &
                           in(offload_section_id) inout(offload_sections)
       do i = 1,DIMX
          tmp_e    = array_A(i)**2
          array_B(i) = tmp_e + var_c    
          if ( array_A(i) > min_val ) var_d = var_d + tmp_e
       enddo
  
       ! Verify offload execution
       call offload_check()
       !DIR$ END OFFLOAD 

    CASE DEFAULT
        call abort("*** ABORT - internal failure : leoF03_clauses_offld")

    end select OFFLOAD_TYPE

    return
    end subroutine leoF03_clauses_offld


    subroutine leoF03_various_clauses

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    implicit none

    character (LEN=*), parameter    :: sample_name = "leoF03_various_clauses"
    integer          , parameter    :: NUM_CASES = 2
    integer          , parameter    :: SZ = 1000

    integer, dimension(SZ)   :: array_A, array_B
    integer                  :: var_c, var_d, min_val

    integer                  :: i, offload_case


    ! Setup and initialization
    num_offload_sections = 1       ! number of offload sections in sample

    call leoF_setup(sample_name)

    ! Loop over OFFLOAD cases
    ! (case 1 = OMP OFFLOAD, case 2 = OFFLOAD BEGIN/END)

    do offload_case = 1, NUM_CASES

       if (verbosity >= 1) print "(4X,A,I0)", "--> Start Case #",offload_case

       array_A = (/(i, i=1,SZ)/)
       var_c = 4
       var_d = 0
       min_val = 10
    
       !  Manipulate arrays within subprogram containing offload code
       call leoF03_clauses_offld(  &
            array_A, array_B, var_c, var_d, SZ, min_val, offload_case)

       ! Check results
       call leoF03_clauses_chk_results( &
            array_B, var_c, var_d, SZ, min_val, offload_case, sample_name)

       if (verbosity >= 1) print "(4X,A,I0,/)", "--> End Case #",offload_case

    enddo

    ! Cleanup
    call leoF_cleanup(sample_name)

    return
    end subroutine leoF03_various_clauses

    subroutine leoF03_clauses_chk_results(                                &
               array_B, var_c, var_d, DIMX, min_val, offload_case, sample_name)

    ! leoF03_clauses_chk_results() validates results

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    implicit none

    integer                 , intent(in)     :: var_c, min_val, DIMX
    integer, dimension(DIMX), intent(in)     :: array_B
    integer                 , intent(in)     :: offload_case
    character (LEN=*)       , intent(in)     :: sample_name
    integer                 , intent(inout)  :: var_d

    integer                    :: sum_A, sum_B, sum_min, i
    character (LEN=15)         :: err_str


    ! Check results
    sum_A = SUM((/( ((i**2) + var_c), i=1,DIMX) /))
    sum_B = SUM(array_B, 1)
    sum_min = SUM((/ ((i**2), i=(min_val + 1),DIMX) /))

    if (verbosity >= 2) then
       print "(4X,A,I,A,I)", "Results: Expected sum =",sum_A, &
                             " Actual sum =",sum_B
       print "(13X,A,I,A,I)", "Expected reduction =",sum_min, &
                             " Actual reduction =",var_d

       if (sum_A /= sum_B) then
          print "(13X,A,6X,A)", "Expected values","Actual (array_B) values"
          do i = 1,DIMX
             if ( (array_B(i) - var_c) /= (i**2) ) then
                print "(13X,I,12X,A,I0,A,I)", ((i**2) + var_c), &
                                              "(",i,") =",array_B(i)
             endif
          enddo
       endif
    
       ! Display offload section details
       call offload_summary()
    endif

    ! Validate results
    if ( (sum_A == sum_B) .AND. (sum_min == var_d) .AND. &
          offload_verify(3) ) then
        print "(4X,3(A),I0,A)", "PASS ",sample_name," (Case #",offload_case,")"

    else
        if (.NOT. offload_verify(1)) then
           err_str="offload failure"
        else
           err_str="data mismatch"
        endif
        print "(4X,3(A),I0,2(A))", "*** FAIL ",sample_name," (Case #", &
              offload_case, "): ",err_str
    endif

    return
    end subroutine leoF03_clauses_chk_results
!*..................................................... leoF03_various_clauses
