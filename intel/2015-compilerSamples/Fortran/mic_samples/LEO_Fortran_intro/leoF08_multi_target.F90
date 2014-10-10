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

!* leoF08_multi_target .......................................................
!
! This sample demonstrates use of the OFFLOAD directive Target CPU 
! <card expression> feature to offload a designated code section to multiple 
! Target CPUs and the IF-specifier clause to control the actual offload
! of the designated code section.
!
!  Clarifications:
!
!  1. The OFFLOAD directive Target CPU <card expression> 
!     (i.e., target(mic : <card expression> ) ) includes a built-in
!     modular arithmetic feature of MODULO N, where N is the number of
!     installed and usable Target devices. This guarantees the
!     expression resolves to a valid Target CPU device number. 
!
!  2. Subprograms called from within an offload code section require
!     declaration with ATTRIBUTES OFFLOAD directive.
!
!  3. The Intel(R) compiler pre-define, __INTEL_OFFLOAD, may be used
!     to guard against use of Intel(R) compiler offload specific APIs.
!
!  4. The OFFLOAD directive provides an IF-specifier to control the
!     offload of the associated compound statement. 
!
!  5. The Offload API OFFLOAD_NUMBER_OF_DEVICES() returns the number
!     of installed and usable Target CPUs.
!
!  6. The Offload API OFFLOAD_GET_DEVICE_NUMBER() returns a zero-based
!     device number (i.e., the first card is numbered with 0) when
!     called within offloaded code. It returns a -1 when it executes
!     on the Host CPU.
!
!.............................................................................


    !DIR$ ATTRIBUTES OFFLOAD : mic :: leoF08_multi_tgt_GET_DEV_ID
    integer function leoF08_multi_tgt_GET_DEV_ID()

    ! leoF08_multi_tgt_GET_DEV_ID is a offload-able function that
    ! calls the OFFLOAD_GET_DEVICE_NUMBER() API


    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    implicit none

       ! Verify offload execution
       call offload_check

       ! Return Target CPUs device #
#      ifdef __INTEL_OFFLOAD
         leoF08_multi_tgt_GET_DEV_ID = OFFLOAD_GET_DEVICE_NUMBER()
#      else
         leoF08_multi_tgt_GET_DEV_ID = -1
#      endif

    end function leoF08_multi_tgt_GET_DEV_ID


    subroutine leoF08_multi_target()

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    implicit none

    character (LEN=*), parameter   :: sample_name = "leoF08_multi_target"
    integer          , parameter   :: NUM_CASES = 3

    ! Declare function accordingly for use within offload code
    !DIR$ ATTRIBUTES OFFLOAD : mic :: leoF08_multi_tgt_GET_DEV_ID
    integer                              :: leoF08_multi_tgt_GET_DEV_ID

    integer, allocatable, dimension(:)   :: actual_device_ID
    integer                              :: max_devices, num_devices
    integer                              :: target_id, offload_case, i
    integer                              :: fail, total_fails
    character (LEN=15)                   :: err_str

    ! Pre-setup

    ! The OFFLOAD_NUMBER_OF_DEVICES() API is specific to Intel(R) 
    ! Language Extensions for Offload and only callable when using
    ! the Intel(R) compiler. Guard call with Intel(R) compiler 
    ! specific pre-define.

#ifdef __INTEL_OFFLOAD
    num_devices = OFFLOAD_NUMBER_OF_DEVICES()
#else
    num_devices = 0
#endif

    ! For demonstration purposes of the built-in OFFLOAD directive 
    ! <card expression> MODULO feature, add an arbitrary three additional 
    ! devices to the actual number installed.
    max_devices = num_devices + 3

    ! Number of offload sections equals one per number of Target devices
    num_offload_sections = max_devices

    ! Setup and initialization
    call leoF_setup(sample_name)

    ! Loop over cases
    !   Case 1 = OFFLOAD OMP
    !   Case 2 = OFFLOAD sub-program call
    !   Case 3 = OFFLOAD BEGIN/END

    do offload_case = 1, NUM_CASES

       total_fails = 0

       ! Allocate array with indices reflective of zero-based target CPU 
       ! numbering per Offload API OFFLOAD_GET_DEVICE_NUMBER()
       allocate(actual_device_ID(0:max_devices-1))

       actual_device_ID = -2

       if (verbosity >= 1) print "(4X,A,I0)", "--> Start Case #",offload_case

       ! Loop over each Target CPU # (up to max_devices-1) and execute 
       ! the Offload API OFFLOAD_GET_DEVICE_NUMBER() within offload code
       ! on each Target CPU.

       ! This API is only meaningful when called within offload code; 
       ! therefore, guard execution of the offload code to only execute 
       ! Target devices are installed and usable (i.e., num_devices > 0)

       do target_id = 0, (max_devices - 1)

          offload_section_id = target_id + 1         ! offload section #

          OFFLOAD_TYPE: select case (offload_case)

          CASE (1)

             !  Offload OMP construct using OMP OFFLOAD directive

             !  Use of offload_section_id and modification of offload_sections
             !  in offload_check is outside the compiler's lexical scope; 
             !  therefore, use explicit IN/INOUT clauses

             !  For demonstration purposes limit # of OMP threads to 4

             !DIR$ OMP OFFLOAD target(mic: target_id) optional              &
                               if(num_devices > 0)                          &
                               inout(actual_device_ID)                      &
                               in(offload_section_id) inout(offload_sections)
             !$omp parallel num_threads(4)
                 ! Restrict execution to master only
                 !$omp master
#                ifdef __INTEL_OFFLOAD
                    actual_device_ID(target_id) = OFFLOAD_GET_DEVICE_NUMBER()
#                else
                    actual_device_ID(target_id) = -1
#                endif
                 !$omp end master

                 ! Verify offload execution with one thread only.
                 !$omp single
                    call offload_check()
                 !$omp end single
             !$omp end parallel
           
          CASE (2)

             !  Offload subprogram call where using OFFLOAD directive

             !  Use of offload_section_id and modification of offload_sections
             !  in offload_check is outside the compiler's lexical scope; 
             !  therefore, use explicit IN/INOUT clauses

             !DIR$ OFFLOAD target(mic: target_id) optional                  &
                               if(num_devices > 0)                          &
                               inout(actual_device_ID)                      &
                               in(offload_section_id) inout(offload_sections)
             actual_device_ID(target_id) = leoF08_multi_tgt_GET_DEV_ID()
           
          CASE (3)

             !  Offload call using OFFLOAD Begin/End directive 

             !  Use of offload_section_id and modification of offload_sections
             !  in offload_check is outside the compiler's lexical scope; 
             !  therefore, use explicit IN/INOUT clauses

             !DIR$ OFFLOAD BEGIN target(mic: target_id) optional            &
                               if(num_devices > 0)                          &
                               inout(actual_device_ID)                      &
                               in(offload_section_id) inout(offload_sections)

#            ifdef __INTEL_OFFLOAD
                actual_device_ID(target_id) = OFFLOAD_GET_DEVICE_NUMBER()
#            else
                actual_device_ID(target_id) = -1
#            endif

             ! Verify offload execution
             call offload_check()
             !DIR$ END OFFLOAD

          CASE DEFAULT
            call abort("*** ABORT - internal failure in leoF08_multi_target")

          end select OFFLOAD_TYPE

          if (num_devices > 0) then 

             ! MODULO intrinsic mimics the OFFLOAD directive build-in 
             ! MODULO of the Target CPU card expression. 

             ! When the actual_device_ID returned equals expected
             ! MODULO of target_id against num_devices then the API
             ! call passed, otherwise it failed

             if (actual_device_ID(target_id) .ne.  &
                 MODULO(target_id,num_devices)) then 
                total_fails = total_fails + 1
             endif
          else

             ! When no Target devices are available, execution of any
             ! case above returns an actual_device_ID of -1

             ! When the actual_device_ID equals -1 then the case passed,
             ! otherwise it failed
             if (actual_device_ID(target_id) .ne. (-1)) then
               total_fails = total_fails + 1
             endif
          endif
       enddo

       if (verbosity >= 2) then
          print "(4X,A)", "Results: Target IDs ( Expected   /  Actual )"
          do i = 0, (SIZE(actual_device_ID) - 1)
             if (num_devices > 0) then
                print "(19X,I,1X,I)",MOD(i,num_devices),actual_device_ID(i)
             else
                print "(19X,I,1X,I)",(-1),actual_device_ID(i)
             endif
          enddo

          print "(4X,A,I0)", "Total # of fails = ", total_fails

          ! Display offload section details
          call offload_summary
       endif

       ! Validate results
       if ( (total_fails  == 0) .AND. &
             offload_verify(3) ) then
          print "(4X,3(A),I0,A)", "PASS ", sample_name, " (Case #", &
                                  offload_case, ")"
       else
          if (.NOT. offload_verify(1)) then
             err_str="offload failure"
          else
             err_str="data mismatch"
          endif
          print "(4X,3(A),I0,2(A))", "*** FAIL ", sample_name, " (Case #", &
                             offload_case, ") : ", err_str
       endif

       if (verbosity >= 1) print "(4X,A,I0,/)", "--> End Case #",offload_case

       deallocate(actual_device_ID)
    enddo

    ! Cleanup
    call leoF_cleanup(sample_name)

    return
    end subroutine leoF08_multi_target
!*........................................................ leoF08_multi_target
