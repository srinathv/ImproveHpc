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

!* leoF07_deferred_shape_array ...............................................
!
! This sample demonstrates using deferred-shape arrays (Fortran 95/90 pointers)
! within an offload OMP construct, a non-OMP offload code section, and a 
! non-offload (Host-side) code section.
!
! Clarifications:
!
!  1. All variables (i, ptr_odds, ptr_evens, odds_n_evens, sum_odds, 
!     sum_evens) exchanged between the Host and Target CPUs are lexically 
!     visible (to the compiler) within the offloaded code; therefore, none 
!     require naming in an IN/OUT/INOUT clause and all are treated as INOUT 
!     by default by the compiler.
!
!  2. The arrays are large and some are candidates for tailoring the exchange
!     between the Host and Target CPUs with explicit IN/OUT clauses befitting
!     of use within the offload code. (i.e.,  IN(ptr_odds, ptr_evens) )
!
!  3. For global variable (odds_n_evens), in the absence of an explicit 
!     Target CPU card-expression (i.e., target(mic:<card expression>) ) to
!     ensure multiple offload code sections execute on the same Target CPU,
!     the INOUT default is crucial (or an explicit INOUT(odds_n_evens) clause 
!     is required) for one specific offload code section. The INOUT default 
!     (or explicit INOUT clause) ensures current values for this variable
!     (odds_n_evens) are exchanged between the Host and Target CPUs. Such 
!     exchange is required for correct execution because this variable is 
!     used within separate offload code sections and in a multi-Target 
!     configuration different offload code sections can execute on different 
!     Target CPUs.
!
!     In a multi-Target configuration, with round robin scheduling, NOT
!     transferring current values for this global variable (odds_n_evens)
!     to the Target for second offload code section can result in using 
!     previous (i.e., incorrect) values.
!
!  4. Global variables (sum_odds, sum_evens, sum_OnE, odds_n_evens) used
!     within offload code require declaration with an ATTRIBUTES OFFLOAD.
!
!  5. The deferred-shape array length is determined at run-time; therefore,
!     the LENGTH modifier is not required with the IN/OUT/INOUT clauses. 
!
!.............................................................................

    module leoF07_deferred_shp_glbls

       implicit none

       integer, parameter                           :: MAX_EXTENT = 100
    
       ! Global variables used within offloaded code require declaration
       ! with an ATTRIBUTES OFFLOAD 

       !DIR$ ATTRIBUTES OFFLOAD : mic :: odds_n_evens
       integer, allocatable, dimension(:)           :: odds_n_evens

       integer, allocatable, dimension(:,:), target :: odds_and_evens

       !DIR$ ATTRIBUTES OFFLOAD : mic :: sum_odds, sum_evens, sum_OnE
       integer                                      :: sum_odds, sum_evens
       integer                                      :: sum_OnE

    end module leoF07_deferred_shp_glbls

    subroutine leoF07_deferred_shp_offld(offload_case)

    ! leoF07_deferred_shp_offld() - Operate on deferred-shape arrays within
    ! offload OMP constructs, non-OMP offload code sections, and non-OMP 
    ! non-offload code sections

    ! Case 1 = no offload - execute on Host-CPU only
    ! Case 2 = OMP OFFLOAD
    ! Case 3 = OFFLOAD Begin/End

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    use leoF07_deferred_shp_glbls    !  leoF07 specific globals

    implicit none

    integer, intent(in)            :: offload_case
    integer, pointer               :: ptr_odds(:), ptr_evens(:)
    integer                        :: i, j


    ! Pointers reference even or odd column of values
    ptr_odds => odds_and_evens(1:MAX_EXTENT/2,1)
    ptr_evens => odds_and_evens(1:MAX_EXTENT/2,2)

    OFFLOAD_TYPE: select case (offload_case)

    CASE (1)

       ! No OFFLOAD - Host-CPU execution
       ! Merge even and odds into odds_n_evens

       do i = 0,( (MAX_EXTENT / 2) -1)
          odds_n_evens(1 + (i*2)) = ptr_odds(i+1)
          sum_odds = sum_odds + ptr_odds(i+1)
       enddo

       do i = 0,( (MAX_EXTENT / 2) -1)
          odds_n_evens(2 + (i*2)) = ptr_evens(i+1)
          sum_evens = sum_evens + ptr_evens(i+1)
       enddo

       do i = 1,MAX_EXTENT
          sum_OnE = sum_OnE + odds_n_evens(i)
       enddo

    CASE(2)

       offload_section_id = 1         ! offload section #1

       !  Offload OMP constructs using OMP OFFLOAD directive

       !  The deferred-shape array length is determined at run-time; therefore,
       !  a LENGTH modifier is not required with the IN/OUT/INOUT clauses. 

       !  Use of offload_section_id and modification of offload_sections
       !  in offload_check is outside the compiler's lexical scope; therefore,
       !  use explicit IN/INOUT clauses

       !  For demonstration purposes limit # of OMP threads to 4

       !DIR$ OMP OFFLOAD target(mic) optional                         &
                                     in(ptr_odds, ptr_evens)          &
                                     inout(sum_odds, sum_evens)       &
                         in(offload_section_id) inout(offload_sections)
       !$omp parallel reduction(+:sum_odds, sum_evens)  num_threads(4)
           !$omp do
           do i = 0,( (MAX_EXTENT / 2) -1)
              odds_n_evens(1 + (i*2)) = ptr_odds(i+1)
              sum_odds = sum_odds + ptr_odds(i+1)
           enddo

           !$omp do
           do i = 0,( (MAX_EXTENT / 2) -1)
              odds_n_evens(2 + (i*2)) = ptr_evens(i+1)
              sum_evens = sum_evens + ptr_evens(i+1)
           enddo

           ! Verify offload execution with one thread only.
           !$omp single
              call offload_check()
           !$omp end single
       !$omp end parallel 


       ! Increment offload section #
       offload_section_id = offload_section_id + 1  

       ! For demonstration purposes, use two independent OMP parallel
       ! constructs to permit use of an additional offload 

       ! The INOUT default for the offload section below is crucial to 
       ! ensuring the current values for the global variable odds_n_evens 
       ! are transferred to the Target CPU which may be a different Target
       ! CPU than executed the previous offload code section.

       ! In a multi-Target configuration, with round robin scheduling, 
       ! failing to transfer current values for this global variable
       ! (odds_n_evens) to the Target CPU can result in using previous 
       ! (i.e., incorrect) values.

       ! Use of offload_section_id and modification of offload_sections
       ! in offload_check is outside the compiler's lexical scope; therefore,
       ! use explicit IN/INOUT clauses

       !DIR$ OMP OFFLOAD target(mic) optional                         &
                         inout(sum_OnE)                               &
                         in(offload_section_id) inout(offload_sections)
       !$omp parallel reduction(+:sum_OnE) num_threads(4)
           !$omp do
           do i = 1,MAX_EXTENT
              sum_OnE = sum_OnE + odds_n_evens(i)
           enddo

           ! Verify offload execution with one thread only.
           !$omp single
              call offload_check()
           !$omp end single
       !$omp end parallel 

    CASE(3)

       offload_section_id = 1         ! offload section #1

       !  Offload code sections using OFFLOAD BEGIN/END directives

       !  The deferred-shape array length is determined at run-time; therefore,
       !  a LENGTH modifier is not required with the IN/OUT/INOUT clauses.

       !  Use of offload_section_id and modification of offload_sections
       !  in offload_check is outside the compiler's lexical scope; therefore,
       !  use explicit IN/INOUT clauses

       !DIR$ OFFLOAD BEGIN target(mic)  optional                        &
                           in(ptr_odds, ptr_evens)                      &
                           in(offload_section_id) inout(offload_sections)
       do i = 0,( (MAX_EXTENT / 2) -1)
          odds_n_evens(1 + (i*2)) = ptr_odds(i+1)
          sum_odds = sum_odds + ptr_odds(i+1)
       enddo

       do i = 0,( (MAX_EXTENT / 2) -1)
          odds_n_evens(2 + (i*2)) = ptr_evens(i+1)
          sum_evens = sum_evens + ptr_evens(i+1)
       enddo

       ! Verify offload execution
       call offload_check()
       !DIR$ END OFFLOAD

       ! Increment offload section #
       offload_section_id = offload_section_id + 1

       ! For demonstration purposes, use two independent offload code
       ! sections to permit use of an additional offload

       ! The INOUT default for the offload section below is crucial to
       ! ensuring the current values for the global variable odds_n_evens
       ! are transferred to the Target CPU which may be a different Target
       ! CPU than executed the previous offload code section.

       ! In a multi-Target configuration, with round robin scheduling,
       ! failing to transfer current values for this global variable
       ! (odds_n_evens) to the Target CPU can result in using previous
       ! (i.e., incorrect) values.

       ! Use of offload_section_id and modification of offload_sections
       ! in offload_check is outside the compiler's lexical scope; therefore,
       ! use explicit IN/INOUT clauses

       !DIR$ OFFLOAD BEGIN target(mic)  optional                        &
                           inout(sum_OnE)                               &
                           in(offload_section_id) inout(offload_sections)
       do i = 1,MAX_EXTENT
          sum_OnE = sum_OnE + odds_n_evens(i)
       enddo

       ! Verify offload execution
       call offload_check()
       !DIR$ END OFFLOAD

    CASE DEFAULT
        call abort("*** ABORT - internal failure in leoF07_deferred_shp_offld")

    end select OFFLOAD_TYPE

    return
    end subroutine leoF07_deferred_shp_offld


    subroutine leoF07_deferred_shape_array

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    use leoF07_deferred_shp_glbls    !  leoF07 specific globals

    implicit none

    character (LEN=*),parameter  :: sample_name = "leoF07_deferred_shape_array"
    integer          ,parameter  :: NUM_CASES = 3

    integer                      :: offload_case, i

    ! Setup and initialization
    num_offload_sections = 2       ! number of offload sections in sample

    call leoF_setup(sample_name)

    ! Max extent must be evenly divisible
    if (MODULO(MAX_EXTENT,2) /= 0) then
        print "(4X,3(A))", "*** FAIL ",sample_name," - value for MAX_EXTENT not usable"
        return
    endif

    ! Loop over cases
    !   Case 1 = no offload - execute on Host-CPU only)
    !   Case 2 = OMP OFFLOAD
    !   Case 3 = OFFLOAD Begin/End)

    do offload_case = 1, NUM_CASES

       if (verbosity >= 1) print "(4X,A,I0)", "--> Start Case #",offload_case

       ! Allocate global arrays and initialize 

       ! odds_n_evens values assigned in leoF07_deferred_shp_offld
       allocate(odds_n_evens(MAX_EXTENT))
       odds_n_evens = 0
       sum_OnE = 0

       ! Fill column 1 odd #s, column 2 with even #s
       allocate(odds_and_evens(MAX_EXTENT/2,2))
       odds_and_evens(:,1) = (/ (i,i=1,MAX_EXTENT,2) /)
       odds_and_evens(:,2) = (/ (i,i=2,MAX_EXTENT,2) /)

       sum_odds = 0
       sum_evens = 0

       ! Offload deferred-shape arrays (Fortran 95/90 pointers and allocatable)
       call leoF07_deferred_shp_offld(offload_case)

       ! Check results
       call leoF07_deferred_shp_chk_results(offload_case, sample_name)

       if (verbosity >= 1) print "(4X,A,I0,/)", "--> End Case #",offload_case

       ! Cleanup
       deallocate(odds_n_evens)
       deallocate(odds_and_evens)
    enddo

    ! Cleanup
    call leoF_cleanup(sample_name)

    return
    end subroutine leoF07_deferred_shape_array


    subroutine leoF07_deferred_shp_chk_results(offload_case, sample_name)

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    use leoF07_deferred_shp_glbls    !  leoF07 specific globals

    implicit none

    integer          , intent(in)  :: offload_case
    character (LEN=*), intent(in)  :: sample_name

    integer                :: i, sum_OnE_X, sum_odds_X, sum_evens_X
    character (LEN=15)     :: err_str


    sum_OnE_X = SUM((/ (i, i= 1, MAX_EXTENT) /))
    sum_odds_X = SUM((/ (i, i= 1, MAX_EXTENT,2) /))
    sum_evens_X = SUM((/ (i, i= 2, MAX_EXTENT,2) /))

    if (verbosity >= 2) then
       print "(4X,2(A,I))", "Results: Expected sum = ",sum_OnE_X, &
                             " Actual sum_OnE = ",sum_OnE
       print "(13X,2(A,I))", "Expected sum = ",sum_odds_X, &
                             " Actual sum_odds = ",sum_odds
       print "(13X,2(A,I))", "Expected sum = ",sum_evens_X, &
                             " Actual sum_evens = ",sum_evens

       if (sum_OnE_X /= sum_OnE) then
          print "(13X,A,6X,A)", "Expected values","Actual (odds_n_evens) values"
          do i = 1,MAX_EXTENT
             if ( odds_n_evens(i) /= i ) then
                print "(13X,I,12X,A,I0,A,I)", i, "(",i,") =",odds_n_evens(i)
             endif
          enddo
       endif

       ! Display offload section details
       call offload_summary
    endif

    ! Validate results
    if ( offload_case > 1) then
       if ( (sum_OnE_X == sum_OnE) .AND. &
            (sum_odds_X == sum_odds) .AND. &
            (sum_evens_X == sum_evens) .AND. &
             offload_verify(3) ) then
          print 2000, sample_name, offload_case
       else
          if (.NOT. offload_verify(1)) then
             err_str="offload failure"
          else
             err_str="data mismatch"
          endif
          print 2001, sample_name, offload_case, err_str
       endif
    else
       if ( (sum_OnE_X == sum_OnE) .AND. &
            (sum_odds_X == sum_odds) .AND. &
            (sum_evens_X == sum_evens) ) then
          print 2000, sample_name, offload_case
        else
          err_str="data mismatch"
          print 2001, sample_name, offload_case, err_str
        endif
    endif

2000  format(4X,"PASS ",A," (Case #",I0,")")
2001  format(4X,"*** FAIL ",A," (Case #",I0,") : ",A)

    return
    end subroutine leoF07_deferred_shp_chk_results
!*................................................ leoF07_deferred_shape_array
