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

!* leoF02_global .............................................................
!
! This sample demonstrates using global (COMMON) variables within an
! offload OMP construct, an OFFLOAD BEGIN/END code section, and other 
! non-offload code sections.
!
! Clarifications:
!
!  1. COMMON variables used in offload code requires an ATTRIBUTES OFFLOAD
!     directive within each scope used.
!
!  2. Only COMMON variables used within offload code must appear in an
!     ATTRIBUTES OFFLOAD directive statement within the scope used.
!
!  3. In this sample, COMMON variables are only used within offload code
!     appearing in subprogram leoF02_global_offld; therefore, the
!     ATTRIBUTES OFFLOAD directive is only required in that subprogram's
!     scope.
!
!  4. Placement of the ATTRIBUTES OFFLOAD directive within the subprogram
!     itself also permits customization of the corresponding variable list
!     based only on those variables used within the specific associated
!     scope.
!
!  5. When COMMON variables are used in offload code within multiple scopes,
!     the ATTRIBUTES OFFLOAD directive can alternatively appear in an
!     included file for convenience of inserting the directive within each
!     scope as demonstrated below.
!
!  6. The arrays are large and candidates  for tailoring their exchange
!     between the Host and Target CPUs with explicit IN/OUT clauses 
!     befitting of use within the offload code; however, no tailoring is 
!     used in this sample. The sample uses the INOUT default treatment by 
!     the compiler for all variables.
!
!.............................................................................


    subroutine leoF02_global_offld(offload_case)

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    implicit none

    integer, intent(in)      :: offload_case

    integer                  :: i

    !  Only COMMON variables used within offload code must appear in an
    !  ATTRIBUTES OFFLOAD directive statement within the scope used.
    !
    !  In this sample, COMMON variables are only used within offload code
    !  code within this subprogram; therefore, the ATTRIBUTES OFFLOAD 
    !  directive is required only in this subprogram scope.
    !
    !  Placement of the ATTRIBUTES OFFLOAD statement within the subprogram
    !  itself also permits customization based only on the variables used
    !  within the specific associated use.
    !
    !  When COMMON variables are used in multiple scopes, the ATTRIBUTES 
    !  OFFLOAD directive could alternatively appear in the included file 
    !  for convenience e.g.,
    !
    !       !DIR$ ATTRIBUTES OFFLOAD : mic :: in1, in2, res
    !       common /leoF02_global_cmnBLK/ in1, h_res, in2, res
    !
    !  Refer to file leoF02_global_common.inc for common block and parameter
    !  declarations details.

    !DIR$ ATTRIBUTES OFFLOAD : mic :: in1, in2, res
    INCLUDE "leoF02_global_common.inc"


    OFFLOAD_TYPE: select case(offload_case)

    CASE(1)
       offload_section_id = 1         ! offload section #1

       !  Offload OMP construct using OMP OFFLOAD directive

       !  Use of offload_section_id and modification of offload_sections
       !  in offload_check is outside the compiler's lexical scope; therefore,
       !  use explicit IN/INOUT clauses

       !  For demonstration purposes limit # of OMP threads to 4

       !DIR$ OMP OFFLOAD target(mic)  optional  &
                      in(offload_section_id) inout(offload_sections)
       !$omp parallel num_threads(4)
           !$omp do 
           do i = 1,SZ
              res(i) = in1(i) + in2(i)
           enddo

           ! Verify offload execution with one thread only.
           !$omp single
              call offload_check()
           !$omp end single
       !$omp end parallel 

       !  Update other COMMON variables on the Host CPU only using results
       !  generated within the offloaded OMP construct above

       do i = 1,(SZ/2)
          h_res(i) = res(i) + res(i + (SZ/2))
       enddo

    CASE(2)
       offload_section_id = 1         ! offload section #1

       !  Offload code block using OFFLOAD BEGIN/END directives

       !  Use of offload_section_id and modification of offload_sections
       !  in offload_check is outside the compiler's lexical scope; therefore,
       !  use explicit IN/INOUT clauses

       !DIR$ OFFLOAD BEGIN target(mic)  optional  &
                      in(offload_section_id) inout(offload_sections)

       do i = 1,SZ
          res(i) = in1(i) + in2(i)
       enddo

       ! Verify offload execution
       call offload_check
       !DIR$ END OFFLOAD

       !  Update other COMMON variables on the Host CPU only using results
       !  generated within the OFFLOAD BEGIN/END code section above

       do i = 1,(SZ/2)
          h_res(i) = res(i) + res(i + (SZ/2))
       enddo

    CASE DEFAULT
        call abort("*** ABORT - internal failure : leoF02_global_offld")

    end select OFFLOAD_TYPE

    return
    end subroutine leoF02_global_offld


    subroutine leoF02_global()

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    implicit none

    character (LEN=*),parameter    :: sample_name = "leoF02_global"
    integer          ,parameter    :: NUM_CASES = 2

    integer          :: i, j, offload_case
    real             :: sum_A, sum_B, sum_res, sum_hres


    !  Only COMMON variables used within offload code must appear in an
    !  ATTRIBUTES OFFLOAD directive statement e.g.,
    !
    !       !DIR$ ATTRIBUTES OFFLOAD : mic :: in1, in2, res
    !       common /leoF02_global_cmnBLK/ in1, h_res, in2, res
    !
    !  Refer to file leoF02_global_common.inc for common block and parameter
    !  declarations details.
    INCLUDE "leoF02_global_common.inc"


    ! Setup and initialization
    num_offload_sections = 1       ! number of offload sections in sample

    call leoF_setup(sample_name)

    if (MODULO(SZ,2) == 1) then
        print "(4X,3(A))", "*** FAIL ",sample_name,"- value for SZ not usable"
        return
    endif


    ! Loop over OFFLOAD cases 
    ! (case 1 = OMP OFFLOAD, case 2 = OFFLOAD BEGIN/END)

    do offload_case = 1,NUM_CASES

       if (verbosity >= 1) print "(4X,A,I0)", "--> Start Case #",offload_case

       in1 = (/(REAL(i), i=1,SZ)/)
       in2 = in1
       h_res = 0.0

       !  Manipulate COMMON variables within a subprogram containing 
       !  offload code
       call leoF02_global_offld(offload_case)

       ! Check results
       call leoF02_global_chk_results(offload_case, sample_name)

       if (verbosity >= 1) print "(4X,A,I0,/)", "--> End Case #",offload_case

    enddo 

    ! Cleanup
    call leoF_cleanup(sample_name)

    return
    end subroutine leoF02_global

    subroutine leoF02_global_chk_results(offload_case, sample_name)

    ! leoF02_global_chk_results() validates results

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    implicit none

    integer          , intent(in)       :: offload_case
    character (LEN=*), intent(in)       :: sample_name

    integer                    :: i,j
    real                       :: sum_A, sum_B, sum_res, sum_hres
    character (LEN=15)         :: err_str


    !  Only COMMON variables used within offload code must appear in an
    !  ATTRIBUTES OFFLOAD directive statement e.g.,
    !
    !       !DIR$ ATTRIBUTES OFFLOAD : mic :: in1, in2, res
    !       common /leoF02_global_cmnBLK/ in1, h_res, in2, res
    !
    !  Refer to file leoF02_global_common.inc for common block and parameter
    !  declarations details.
    INCLUDE "leoF02_global_common.inc"


    ! Check results
    sum_A = SUM(2.0 * (/(REAL(i), i=1,SZ)/))
    sum_res = SUM(res,1)
    sum_hres = SUM(h_res,1)

    if (verbosity >= 2) then
       print "(4X,2(A,F0.2))", "Results: Expected sum = ", sum_A, &
                             " Actual sum (res) = ", sum_res
       print "(13X,2(A,F0.2))", "Expected sum = ", sum_A, &
                             " Actual sum (h_res) = ", sum_hres

       if (sum_A /= sum_res) then
          print "(13X,A,6X,A)", "Expected values","Actual (res) values"
          do i = 1,SZ
             if ( res(i) /= (i * 2.0) ) then
                print "(13X,F0.2,12X,A,I0,A,F0.2)", (i * 2.0), &
                                              "(", i, ") = ", res(i)
             endif
          enddo
       endif

       if (sum_A /= sum_hres) then
          print "(/,13X,A,6X,A)", "Expected values","Actual (h_res) values"
          do i = 1,(SZ/2)
             if ( h_res(i) /= ((i * 4.0) + SZ )) then
                print "(13X,F0.2,12X,A,I0,A,F0.2)", ((i * 4.0) + SZ), &
                               "(", i, ") = ", h_res(i)
             endif
          enddo
       endif

       ! Display offload section details
       call offload_summary
    endif

    ! Validate results
    if ( (sum_A == sum_res) .AND. (sum_A == sum_hres) .AND. &
          offload_verify(3) ) then
        print "(4X,3(A),I0,A)", "PASS ",sample_name," (Case #",offload_case,")"
    else
        if (.NOT. offload_verify(1)) then
           err_str="offload failure"
        else
           err_str="data mismatch"
        endif
        print "(4X,3(A),I0,2(A))", "*** FAIL ",sample_name," (Case #", &
              offload_case, "): ", err_str
    endif

    return
    end subroutine leoF02_global_chk_results
!*.............................................................. leoF02_global
