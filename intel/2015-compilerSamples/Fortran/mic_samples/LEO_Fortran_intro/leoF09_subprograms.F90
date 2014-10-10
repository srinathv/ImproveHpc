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

!* leoF09_subprograms ........................................................
!
! This sample demonstrates calling a subroutine and function from within 
! both an OMP OFFLOAD and OFFLOAD BEGIN/END construct and the non-OMP OFFLOAD
! directive to offload a single subprogram call. The sample combines calling 
! the offload subprograms with using global variables within each.
!
! Clarifications:
!
! 1. Global variables used within offload code require declaration with
!    an ATTRIBUTES OFFLOAD within the defining routine/module and within
!    each scope used.
!
! 2. Subprograms called within offload code requires declaration with
!    an ATTRIBUTES OFFLOAD in the caller and for each callee definition.
!
! 3. There are two methods available to meet the requirements in items # 1
!    and 2 above. Both are demonstrated in this sample. The methods are:
!
!    a.  The ATTRIBUTES OFFLOAD directive is active only for the subsequent
!        subprogram scope that it precedes. (Used for callee definitions)
!
!    b.  The OPTIONS /offload_attribute_target=mic directive applies to  
!        declaration statements appearing between OPTIONS/END OPTIONS
!        directives. (Used for declarations in caller)
!
!        The OPTIONS /offload_attribute_target=mic directive applies to
!        an INTERFACE separately and appears between the INTERFACE/END
!        INTERFACE statements. (Used for declarations in caller)
!
! 4. Global variables referenced in an offloaded subprogram that are 
!    outside the compiler's lexical scope of offload code must be named
!    in an IN/OUT/INOUT clause to ensure their values are exchanged 
!    accordingly between the Host and Target CPUs.
!
!.............................................................................


    subroutine leoF09_subprograms

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    implicit none

    character (LEN=*), parameter    :: sample_name = "leoF09_subprograms"
    integer          , parameter    :: NUM_CASES = 3

    integer                  :: offload_case
    integer                  :: leoF09_subprograms_fun
    integer                  :: ival, jval
    integer                  :: expected_fun_res, expected_sub_res
    integer                  :: actual_sub_res
    character (LEN=15)       :: err_str

    ! Declare subprograms and global variables used in offload code within 
    ! subroutine leoF09_subprogram accordingly for use in offload code

    !DIR$ OPTIONS /offload_attribute_target=mic

    integer                  :: res

    !DIR$ END OPTIONS 

    interface
    !DIR$ OPTIONS /offload_attribute_target=mic
       subroutine leoF09_subprograms_sub(ival)
       integer, intent(in)   :: ival
       end subroutine leoF09_subprograms_sub
    !DIR$ END OPTIONS 
    end interface

    interface
    !DIR$ OPTIONS /offload_attribute_target=mic
       integer function leoF09_subprograms_func(jval)
       integer, intent(in)   :: jval
       end function leoF09_subprograms_func
    !DIR$ END OPTIONS 
    end interface


    common /leoF09_subprograms_cmnBLK/res

    ! Setup and initialization
    num_offload_sections = 2       ! number of offload sections in sample

    call leoF_setup(sample_name)

    !  For demonstration purposes, inlining of the subprograms is disabled 
    !  via the NOINLINE directive

    !  Loop over all cases
    !    case 1 = OMP OFFLOAD
    !    case 2 = OFFLOAD of sub-program 
    !    case 3 = OFFLOAD Begin/End

    do offload_case = 1, NUM_CASES

       if (verbosity >= 1) print "(4X,A,I0)", "--> Start Case #",offload_case

       ! Initial values
       ival = 4
       jval = 5
       res = 55

       ! Generate expected results. Use offload_mode global setup in 
       ! sample driver.

       ! The expected results depend on the calling order for the 
       ! subprograms (i.e., subroutine first, function second) and 
       ! whether the sample executes on the Target CPU (i.e., offload mode) 
       ! or on the Host CPU (i.e., fallback mode)

       if (offload_mode) then
          expected_sub_res = res + ival
          expected_fun_res = res + ival + jval
       else
          expected_sub_res = res - ival
          expected_fun_res = res - ival - jval
       endif


       OFFLOAD_TYPE: select case (offload_case)

       CASE (1)

          offload_section_id = 1         ! offload section #1

          !  Offload OMP construct using OMP OFFLOAD directive

          !  Explicit INOUT(res) required per Clarification #4 above.
          !  i.e., use is outside the compiler's lexical scope; therefore,
          !  use explicit IN/INOUT clause

          !  ival/jval are only referenced in offloaded subprograms; 
          !  therefore, limit transfer between Host and Target CPUs with 
          !  explicit IN clause

          !  Use of offload_section_id and modification of offload_sections
          !  in offload_check is outside the compiler's lexical scope;
          !  therefore, use explicit IN/INOUT clauses

          !  For demonstration purposes limit # of OMP threads to 4

          !DIR$ OMP OFFLOAD target(mic)  optional  in(ival) inout(res)  &
                            in(offload_section_id) inout(offload_sections)
          !$omp parallel num_threads(4)
              !$omp single
                 call leoF09_subprograms_sub(ival)
              !$omp end single
          !$omp end parallel

          ! Save actual sub current res value for use in summary below
          actual_sub_res = res

          ! Increment offload section #
          offload_section_id = offload_section_id + 1

          ! No explicit inout(res) is required because the use below 
          ! appears within scope of offload code; therefore, the compiler
          ! applies the INOUT default.

          !DIR$ OMP OFFLOAD target(mic)  optional  in(jval)   &
                            in(offload_section_id) inout(offload_sections)
          !$omp parallel num_threads(4)
              !$omp single
                 res = leoF09_subprograms_fun(jval)
              !$omp end single
          !$omp end parallel

          if (verbosity >= 2) then
             call leoF09_subprograms_summary(expected_sub_res, actual_sub_res)
             call leoF09_subprograms_summary(expected_fun_res, res)

             ! Display offload section details
             call offload_summary
          endif

       CASE (2)

          offload_section_id = 1         ! offload section #1

          !  Offload subprogram calls using OFFLOAD directive

          !  Explicit INOUT(res) required per Clarification #4 above.
          !  i.e., use is outside the compiler's lexical scope; therefore,
          !  use explicit IN/INOUT clause

          !  ival/jval are only referenced in offloaded subprograms; 
          !  therefore, limit transfer between CPU and target with 
          !  explicit IN clause

          !  Use of offload_section_id and modification of offload_sections
          !  in offload_check is outside the compiler's lexical scope;
          !  therefore, use explicit IN/INOUT clauses

          !DIR$ NOINLINE
          !DIR$ OFFLOAD target(mic)  optional  in(ival) inout(res) &
                        in(offload_section_id) inout(offload_sections)
          call leoF09_subprograms_sub(ival)

          ! Save actual sub current res value for use in summary below
          actual_sub_res = res

          ! Increment offload section #
          offload_section_id = offload_section_id + 1

          ! No explicit inout(res) is required because the use below 
          ! appears within scope of offload code; therefore, the compiler
          ! applies the INOUT default.

          !DIR$ NOINLINE
          !DIR$ OFFLOAD target(mic)  optional  in(jval)  &
                        in(offload_section_id) inout(offload_sections)
          res = leoF09_subprograms_fun(jval)

          if (verbosity >= 2) then
             call leoF09_subprograms_summary(expected_sub_res, actual_sub_res)
             call leoF09_subprograms_summary(expected_fun_res, res)

             ! Display offload section details
             call offload_summary
          endif

       CASE (3)

          offload_section_id = 1         ! offload section #1

          !  Offload subprogram calls using OFFLOAD BEGIN/END directive

          !  Explicit INOUT(res) required per Clarification #4 above.
          !  i.e., use is outside the compiler's lexical scope; therefore,
          !  use explicit IN/INOUT clause

          !  ival/jval are only referenced in offloaded subprograms; 
          !  therefore, limit transfer between CPU and target with 
          !  explicit IN clause

          !  Use of offload_section_id and modification of offload_sections
          !  in offload_check is outside the compiler's lexical scope;
          !  therefore, use explicit IN/INOUT clauses

          !DIR$ NOINLINE
          !DIR$ OFFLOAD BEGIN target(mic)  optional  in(ival) inout(res) &
                              in(offload_section_id) inout(offload_sections)
          call leoF09_subprograms_sub(ival)
          !DIR$ END OFFLOAD 

          ! Save actual sub current res value for use in summary below
          actual_sub_res = res

          ! Increment offload section #
          offload_section_id = offload_section_id + 1

          ! No explicit inout(res) is required because the use below 
          ! appears within scope of offload code; therefore, the compiler
          ! applies the INOUT default.

          !DIR$ NOINLINE
          !DIR$ OFFLOAD BEGIN target(mic)  optional  in(jval)  &
                              in(offload_section_id) inout(offload_sections)
          res = leoF09_subprograms_fun(jval)
          !DIR$ END OFFLOAD 

          if (verbosity >= 2) then
             call leoF09_subprograms_summary(expected_sub_res, actual_sub_res)
             call leoF09_subprograms_summary(expected_fun_res, res)

             ! Display offload section details
             call offload_summary
          endif

       CASE DEFAULT
          call abort("*** ABORT - internal failure in leoF09_subprograms")

       end select OFFLOAD_TYPE

       if ( (res == expected_fun_res) .AND. &
             offload_verify(3) ) then
          print "(4X,3(A),I0,A)", "PASS ", sample_name, " (Case #", &
                                  offload_case, ")"
       else
          if (.NOT. offload_verify(1)) then
             err_str="offload failure"
          else
             err_str="data mismatch"
          endif
          print "(4X,3(A),I0,2(A))", "*** FAIL ",sample_name," (Case #", &
                                     offload_case,") : ",err_str
       endif

       if (verbosity >= 1) print "(4X,A,I0,/)", "--> End Case #",offload_case

    enddo

    ! Cleanup
    call leoF_cleanup(sample_name)

    return
    end subroutine leoF09_subprograms


    subroutine leoF09_subprograms_summary(expected_res, actual_res)

    ! leoF09_subprograms_summary() prints summary of results 

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    implicit none

    integer, intent(in)      :: expected_res, actual_res

    print "(4X,A,I0,A,I0)", "Results: Expected res = ", &
                         expected_res," Actual res = ", actual_res
 
    return
    end subroutine leoF09_subprograms_summary
!*......................................................... leoF09_subprograms
