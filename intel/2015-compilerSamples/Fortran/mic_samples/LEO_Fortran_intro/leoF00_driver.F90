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

! LEO_Fortran_intro sample utility MODULE and Driver code

!* leoF_utils ................................................................
module leoF_utils

   use omp_lib         ! Provides OpenMP* specific APIs
   use mic_lib         ! Provides Intel(R) Xeon Phi(TM) specific APIs

   implicit none

   ! Declare globals accordingly (with ATTRIBUTES OFFLOAD) for use in 
   ! offload code

   ! offload_mode reflects state of offload-mode or fallback-mode 

   !DIR$ ATTRIBUTES OFFLOAD : mic :: offload_mode
   logical  :: offload_mode

   !  Offload status variable to capture STATUS clause results
   type (Offload_status)                 :: offload_stat

   ! offload_section_id, offload_sections, num_offload_sections are 
   ! for internal machinery to verify offload execution. The module 
   ! (global) variable affords re-use convenience within all samples 
   ! and added concealment within the sample code itself.

   ! Each sample contains a set number of offload code sections as
   ! designated by num_offload_sections.
   !
   ! Each offload code section within each sample is identified with 
   ! a unique offload_section_id set just prior to execution of that 
   ! offload code section.

   !DIR$ ATTRIBUTES OFFLOAD : mic :: offload_section_id, num_offload_sections
   integer  :: offload_section_id, num_offload_sections

   ! offload_sections is allocated with num_offload_sections number 
   ! of elements and captures offload execution status of offload code 
   ! sections executed within the sample. It is allocated/deallocated for 
   ! each individual sample.

   !DIR$ ATTRIBUTES OFFLOAD : mic :: offload_sections
   logical, allocatable, dimension(:)   :: offload_sections

   ! verbosity controls the level of verbose diagnostic console output.
   ! This is a global variable loaded to each target once at the start
   ! of the main. It remains static on the each target for the duration
   ! of the program.

   !DIR$ ATTRIBUTES OFFLOAD : mic :: verbosity
   integer   :: verbosity

   ! Interface to the fflush to ensure I/O from within offload
   ! is completely flushed back to the host 

   interface
     subroutine F_flush(strm) bind(C, name='fflush')
        use, intrinsic :: iso_c_binding
        integer (C_INT), value :: strm
     end subroutine F_flush
   end interface

   CONTAINS

   !  Declare all sample utility subprograms accordingly (with ATTRIBUTES 
   !  OFFLOAD) for use in offload code.

!* offload_check .............................................................
   !DIR$ ATTRIBUTES OFFLOAD : mic :: offload_check
   subroutine offload_check()

   !  offload_check() assigns TRUE/FALSE to array offload_sections for
   !  the designated offload code section (i.e., offload_section_id) 
   !  according to execution of the conditionlized assignment statement 
   !  occurring on the target (i.e., offload-mode) or the host 
   !  (i.e., fallback-mode).

   !  Print diagnostics corresponding to the code executing on target or
   !  host CPU according to setting of the global variable verbosity.

   !  How does this subprogram work?

   !  The conditional code below based on the __MIC__ pre-define compiles 
   !  a unique assignment statement for the array offload_sections into
   !  the Target (Intel(R) Xeon Phi(TM)) and HOST executable code for 
   !  this subprogram.

   !  In the Target executable code, the assignment statement assigns a 
   !  value of TRUE to the offload_sections(offload_section_id) element, 
   !  and in the HOST executable code, the statement assigns a value of 
   !  FALSE. This enables determining which instance of the assignment 
   !  statement actually executed.

   !  It is *required* that offload_section_id be set accordingly 
   !  in the caller. There is no means in this routine to verify that 
   !  offload_section_id is correct with respect to the numbering of
   !  offload sections in the caller.

   implicit none

#  ifdef __MIC__

      ! Execution occurred on the Target CPU
      offload_sections(offload_section_id) = .TRUE.

      if (verbosity >= 1) then 
         print "(4X,A,I4,A,I6)", "offload_check: section #", &
                offload_section_id, " ran on: Target CPU #", &
                OFFLOAD_GET_DEVICE_NUMBER()
         call F_flush(0)
      endif

#  else

      ! Execution occurred on the Host CPU
      offload_sections(offload_section_id) = .FALSE.

      if (verbosity >= 1) then 
         print "(4X,A,I4,A)", "offload_check: section #",offload_section_id, &
                              " ran on: Host CPU"
      endif

#  endif

   end subroutine offload_check
!*.............................................................. offload_check

!* offload_summary ...........................................................
   subroutine offload_summary()

   !  offload_summary() prints collective status of all offload sections 

   implicit none
   integer    :: i

   if (SIZE(offload_sections) > 0) then
      print "(4X,A)", "Offloads: ( section # / offloaded? )"
      do i = 1, SIZE(offload_sections)
         print "(8X,I,11X,L)",i,offload_sections(i)
      enddo
   endif

   end subroutine offload_summary
!*............................................................ offload_summary 

!* offload_verify ............................................................
   logical function offload_verify(case_id)

   ! offload_verify() returns a logical value representing offload_mode 
   ! and/or ALL(offload_sections) 

   implicit none
   integer, intent(in)      :: case_id

   OFFLOAD_VRFY: select case (case_id) 
      CASE(1)
          if (allocated(offload_sections)) then
             offload_verify=(ALL(offload_sections))
          else
             offload_verify=.TRUE.
          endif
      CASE(2)
          offload_verify=(offload_mode)
      CASE(3)
          if (allocated(offload_sections)) then
             offload_verify=(ALL(offload_sections) .OR. (.NOT. offload_mode))
          else
             offload_verify=(.TRUE. .OR. (.NOT. offload_mode))
          endif
      CASE DEFAULT
         call abort("*** ABORT - internal failure : offload_verify")
   end select OFFLOAD_VRFY

   end function offload_verify
!*............................................................. offload_verify

!* leoF_init_offload_status ..................................................
       subroutine leoF_init_offload_status

       !  Initialize offload status
       offload_stat%result = OFFLOAD_DISABLED
       offload_stat%device_number = -1
       offload_stat%data_sent = 0
       offload_stat%data_received = 0

       end subroutine leoF_init_offload_status
!*................................................... leoF_init_offload_status 

!* leoF_setup ................................................................
   subroutine leoF_setup(tst_str)

   ! leoF_setup() performs required common setup for the sample

   implicit none
   character(LEN=*), intent(in)   :: tst_str

   if (verbosity >= 1) print "(2(A))", "==> Start ",tst_str

   ! Allocate offload_sections used for internal verification of offload 
   ! execution
   if (num_offload_sections > 0) then
      allocate(offload_sections(num_offload_sections))
      offload_sections = .FALSE.
   endif

   end subroutine leoF_setup
!*................................................................. leof_setup

!* leoF_cleanup ..............................................................
   subroutine leoF_cleanup(tst_str)

   ! leoF_cleanup() performs required common cleanup for the sample

   implicit none
   character(LEN=*), intent(in)   :: tst_str

   if (verbosity >= 1) print "(2(A),/)", "==> End ",tst_str

   ! Deallocate offload_sections
   if (allocated(offload_sections)) deallocate(offload_sections)
   num_offload_sections = 0

   end subroutine leoF_cleanup
!*............................................................... leoF_cleanup

end module leoF_utils
!*................................................................. leoF_utils 

!* leoF_driver ...............................................................
program leoF_driver

  ! For demonstration purposes, disable optimization within this subprogram
  ! scope to avoid compiler auto-inlining of any sample subprograms
!DIR$ NOOPTIMIZE 

     use leoF_utils    ! Provides samples utility routines, globals, 
                       ! access to OMP (omp_lib) and Intel(R) Xeon 
                       ! Phi(TM) (mic_lib) APIs

     implicit none

     ! Local offload status variables
     integer            :: dev, num_devices
     character(LEN=25)  :: str

     ! Local command-line parsing variables

     integer, parameter :: MAX_SAMPLE_NUM = 11
     integer, parameter :: MIN_SAMPLE_NUM = 1
     integer, parameter :: MIN_VERBOSITY = 0
     integer, parameter :: MAX_VERBOSITY = 3

     integer                      :: arg_num, arg_cnt
     integer                      :: ierr = 0
     character(LEN=:),allocatable :: cmd_name

     integer           , allocatable, dimension(:)    :: arg_len, arg_status
     character(LEN=240), allocatable, dimension(:)    :: arg_str

     ! Local sample loop control variables
     integer           :: sample_num
     integer           :: sample_start_num
     integer           :: sample_end_num


     ! Initialization
     num_devices = 0 
     offload_section_id = 0
     num_offload_sections = 0 
     offload_mode = .FALSE. 
     verbosity = MIN_VERBOSITY 
     sample_start_num = MIN_SAMPLE_NUM
     sample_end_num = MAX_SAMPLE_NUM

     ! Parse command-line arguments when present

     ! Retrieve input argument count
     arg_cnt = COMMAND_ARGUMENT_COUNT ()

     ! Allocate command argument arrays
     allocate( arg_str(0:arg_cnt), arg_len(0:arg_cnt), arg_status(0:arg_cnt), &
               stat=ierr )

     if (ierr .ne. 0) &
        call abort("*** ABORT - internal failure : argument allocate failed")

     ! Gather all command-line arguments and command-name
     do arg_num = 0, arg_cnt 
        call GET_COMMAND_ARGUMENT(arg_num, arg_str(arg_num), &
                                  arg_len(arg_num), arg_status(arg_num))

        if (arg_status(arg_num) .ne. 0) &
           call abort("*** ABORT - internal failure :  get_command_argument failed ")
     enddo

     ! Retrieve the command-name 
     allocate(character(LEN=arg_len(0)) :: cmd_name)
     cmd_name = arg_str(0)(1:arg_len(0))

     ! Parse the command-line arguments
     ierr = 0
     arg_num = 1
     do while (arg_num <= arg_cnt) 

        ARGS: select case (arg_str(arg_num))

        CASE("-s")
            ! Read the next argument as numeric input to option
            read(arg_str(arg_num + 1),"(I)",err=2012,iostat=ierr) sample_start_num
            arg_num = arg_num + 1

        CASE("-e")
            ! Read the next argument as numeric input to option
            read(arg_str(arg_num + 1),"(I)",err=2012,iostat=ierr) sample_end_num
            arg_num = arg_num + 1

        CASE("-v")
            ! Read the next argument as numeric input to option
            read(arg_str(arg_num + 1),"(I)",err=2012,iostat=ierr) verbosity
            arg_num = arg_num + 1

        CASE("--help")
            print "(3(A),/,A,/)","Usage: ",cmd_name, &
                  " [Options]", &
                  "Execute the Language Extension for Offload (LEO) Fortran introductory samples"

            print "(A,/,2(A,I0,A,/),2(A,I0,A,I0,A,I0,A,/),A,/)" &
            ,"Options:" &
            ,"-s <NUMBER>      Sample start number (default " &
            ,sample_start_num,")" &
            ,"-e <NUMBER>      Sample end number (default "   &
            ,MAX_SAMPLE_NUM,")" &
            ,"-v <NUMBER>      Verbosity level [",MIN_VERBOSITY," (none) - " &
            ,MAX_VERBOSITY," (max)] (default " ,verbosity,")" &
            ,"--help           Display help"
            stop
        CASE DEFAULT
            print "(4(A),/,2(A))",cmd_name,": invalid option '" &
                  ,arg_str(arg_num)(1:arg_len(arg_num)),"'" &
                  ,cmd_name ,": use --help option for usage information"
            stop

        end select ARGS

        ! Next argument
        arg_num = arg_num + 1
     enddo

2012 if (arg_cnt > 0 .AND. ierr /= 0) then
        print "(4(A),/,2(A))",cmd_name,": invalid option '" &
                  ,arg_str(arg_num)(1:arg_len(arg_num)),"'" &
                  ,cmd_name ,": use --help option for usage information"
        stop
     endif

     if (arg_cnt > 0) then
        ! Adjust any numeric but invalid input values

        ! Convert negative values

        sample_start_num = ABS(sample_start_num)
        sample_end_num = ABS(sample_end_num)
        verbosity = ABS(verbosity)

        ! Force default values for out of range values

        if (sample_start_num == 0 .OR. sample_start_num > MAX_SAMPLE_NUM) &
           sample_start_num = MIN_SAMPLE_NUM

        if (sample_end_num == 0 .OR. sample_end_num > MAX_SAMPLE_NUM) &
           sample_end_num = MAX_SAMPLE_NUM

        if (sample_start_num > sample_end_num) &
           sample_end_num = sample_start_num

        if (verbosity > MAX_VERBOSITY) verbosity = MAX_VERBOSITY

     endif

     !  Configuration pre-checks. Determine # of devices and working state
     print "(/,A)", "System configuration pre-check"

     print "(4X,A,/)","Checking for Intel(R) Xeon Phi(TM) (Target CPU) devices..."
#ifdef __INTEL_OFFLOAD
     num_devices = OFFLOAD_NUMBER_OF_DEVICES()
#endif
     print "(4X,A,I6)","Number of Target devices installed: ",num_devices

     !  The loop below currently forces a program exit when the offload
     !  to a specific Target fails. Future enhancements may permit adding
     !  a call to set an environment variable to force execution with an
     !  explicit target expressions to fallback to host CPU when the target
     !  is unavailable. That will then permit the subsequent if block to 
     !  detect and issue an appropriate diagnostics when offload to a 
     !  specific card fails.

     if (num_devices > 0) then

       !  Verify offload to each target works

        allocate(offload_sections(num_devices))

        !  Modification of offload_sections in offload_check is outside
        !  the compiler's lexical scope of the offload code; therefore, 
        !  must use explicit INOUT. offload_section_id is not modified in 
        !  the offload code thus requires IN at most.

        !  Upload the global verbosity to all targets for use throughout all
        !  offload sections in each sample

        do dev = 1,num_devices
           offload_section_id = dev
           !DIR$ OFFLOAD target(mic : dev) optional                &
                                           in(offload_section_id)  &
                                           inout(offload_sections) &
                                           in(verbosity)
           call offload_check()
        enddo

        if (COUNT(offload_sections) == num_devices) then
           if (ALL(offload_sections)) then
              ! Run in offload-mode
              str="Target CPU (offload mode)"
              offload_mode = .TRUE.
           else
             ! Run in fallback-mode
              str="Host CPU (fallback mode)"
              offload_mode = .FALSE.
           endif
           print "(4X,A,5X,A,/)","Offload sections will execute on:",str
        else
           print "(A,/,9X,A)",                                      &
                 "  ***Warning: Offload to some targets failed***", &
                 "Offload to: ( target # / offload passed? )"

           do dev = 1,num_devices
              print "(15X,I,13X,L)",dev,offload_sections(dev)
           enddo
           print "(/)"

           ! Permit offload-mode
           print "(4X,A,5X,A,/)","Offload sections will execute on:", &
                                 "Target CPU (offload mode)"
           offload_mode = .TRUE.
        endif

        deallocate(offload_sections)
     else
           ! Run in fallback-mode
           print "(4X,A,5X,A,/)","Offload sections will execute on:", &
                                 "Host CPU (fallback mode)"
           offload_mode = .FALSE.
     endif

     ! Start calls to LEO Fortran introductory samples
     print "(A)", "LEO_Fortran_intro samples started"

     do sample_num = sample_start_num, sample_end_num

        SAMPLES: select case (sample_num)

        CASE(1);  call leoF01_scalar()    ! Scalar vars
        CASE(2);  call leoF02_global()    ! Global vars w/tailored data transfer
        CASE(3);  call leoF03_various_clauses()        ! Various clauses
        CASE(4);  call leoF04_explicit_shape_array()   ! Explicit shape array
        CASE(5);  call leoF05_assumed_shape_array()    ! Assumed shape array
        CASE(6);  call leoF06_assumed_size_array()     ! Assumed size array
        CASE(7);  call leoF07_deferred_shape_array()   ! Deferred shape array
        CASE(8);  call leoF08_multi_target()           ! Multi-target
        CASE(9);  call leoF09_subprograms()            ! Offload subprograms
        CASE(10); call leoF10_alloc_into()             ! ALLOC/INTO modifiers
        CASE(11); call leoF11_async()                  ! Async data transfers

        CASE DEFAULT
        call abort("*** ABORT - internal failure : leoF00_driver")

        end select SAMPLES
     enddo

     ! Cleanup
     if (allocated(cmd_name)) deallocate(cmd_name)
     if (allocated(arg_str)) deallocate(arg_str)
     if (allocated(arg_len)) deallocate(arg_len)
     if (allocated(arg_status)) deallocate(arg_status)

     ! End of sample calls 
     print "(A,/)", "LEO_Fortran_intro samples complete"

end program leoF_driver
!*................................................................ leoF_driver 
