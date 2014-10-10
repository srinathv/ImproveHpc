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

!* leoF05_assumed_shape_array ................................................
!
! This sample demonstrates using an assumed-shape array in an offload OMP
! construct, a non-OMP offload code section, and a non-offload (Host-side)
! code section.
!
! Clarifications:
!
!  1. All variables (x, y, z, res) exchanged between the Host and
!     Target CPUs are lexically visible (to the compiler) within the
!     offloaded code; therefore, none require naming in an IN/OUT/INOUT
!     clause and all are treated as INOUT by default by the compiler.
!
!  2. Dummy arguments (dimX, dimY, dimZ) declared intent(in) require 
!     specification in an IN clause.
!
!  3. The array is large and a candidate for tailoring the exchange
!     between the Host and Target CPUs with explicit IN/OUT clauses befitting
!     of use within the offload code. (i.e., IN(array3D) )
!
!.............................................................................


    subroutine leoF05_assumed_shp_offld(  &
                      array3D, dimX, dimY, dimZ, res, offload_case)

    ! leoF05_assumed_shp_offld() - Offload an OMP construct and non-OMP code 
    ! sections that operate on assumed-shape array argument 

    ! case 1 = no offload - execute on Host-CPU only
    ! case 2 = OMP OFFLOAD
    ! case 3 = OFFLOAD Begin/End

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    implicit none

    integer,                   intent(in)  :: dimX, dimY, dimZ, offload_case
    integer,                   intent(out) :: res
    integer, dimension(:,:,:), intent(in)  :: array3D

    integer                                :: x, y, z


    ! Initialize 
    res = 0

    OFFLOAD_TYPE: select case (offload_case)

    CASE (1)

       ! No OFFLOAD - host-CPU execution
       do x = 1,dimX
          do y = 1,dimY
             do z = 1,dimZ
                res = res + array3D(x,y,z)
             enddo
          enddo
       enddo

    CASE(2)

       offload_section_id = 1         ! offload section #1

       !  Offload OMP construct using OMP OFFLOAD directive

       !  Tailor data transfers with an IN clause to only transfer values 
       !  of the array from the Host CPU to the Target CPU
       !  *AND* dummy arguments declared intent(in) which require
       !  specification in an IN clause.

       !  Use of offload_section_id and modification of offload_sections
       !  in offload_check is outside the compiler's lexical scope; therefore,
       !  use explicit IN/INOUT clauses

       !  For demonstration purposes limit # of OMP threads to 4

       !DIR$ OMP OFFLOAD target(mic) optional                &
                                     in(array3D) inout(res)  &
                                     in(dimX,dimY,dimZ)      &
                         in(offload_section_id) inout(offload_sections)
       !$omp parallel reduction(+:res) num_threads(4)
           !$omp do
           do x = 1,dimX
              do y = 1,dimY
                 do z = 1,dimZ
                    res = res + array3D(x,y,z)
                 enddo
              enddo
           enddo

           ! Verify offload execution with one thread only.
           !$omp single
              call offload_check()
           !$omp end single
       !$omp end parallel 

    CASE(3)

       offload_section_id = 1         ! offload section #1

       !  Offload code block using OFFLOAD BEGIN/END directives

       !  Tailor data transfers with an IN clause to only transfer values
       !  of the array from the Host CPU to the Target CPU 
       !  *AND* dummy arguments declared intent(in) which require
       !  specification in an IN clause.

       !  Use of offload_section_id and modification of offload_sections
       !  in offload_check is outside the compiler's lexical scope; therefore,
       !  use explicit IN/INOUT clauses

       !DIR$ OFFLOAD BEGIN target(mic) optional               &
                                       in(array3D) inout(res) &
                                       in(dimX,dimY,dimZ)     &
                           in(offload_section_id) inout(offload_sections)
       do x = 1,dimX
          do y = 1,dimY
             do z = 1,dimZ
                res = res + array3D(x,y,z)
             enddo
          enddo
       enddo

       ! Verify offload execution
       call offload_check()
       !DIR$ END OFFLOAD
   
    CASE DEFAULT
        call abort("*** ABORT - internal failure : leoF05_assumed_shp_offld")

    end select OFFLOAD_TYPE

    return
    end subroutine leoF05_assumed_shp_offld


    subroutine leoF05_assumed_shape_array()

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    implicit none

    interface
       subroutine leoF05_assumed_shp_offld(  &
                         array3D, dimX, dimY, dimZ, res, offload_case)
       integer,                   intent(in)  :: dimX, dimY, dimZ, offload_case
       integer,                   intent(out) :: res
       integer, dimension(:,:,:), intent(in)  :: array3D
       end
    end interface

    character (LEN=*),parameter    :: sample_name = "leoF05_assumed_shape_array"
    integer, parameter             :: NUM_CASES = 3
    integer, parameter             :: NUM_EXTENTS = 3
    integer, parameter             :: MAX_EXTENT = 9

    integer, dimension(NUM_EXTENTS)  :: extents

    integer                                  :: i, j, k, sz, ext, off_res
    integer                                  :: offload_case, sum_res
    integer, allocatable, dimension(:,:,:)   :: arrayA


    ! Setup and initialization
    num_offload_sections = 1       ! number of offload sections in sample

    call leoF_setup(sample_name)

    ! Generate array extents to be tested
    extents = (/(i,i=(MAX_EXTENT / NUM_EXTENTS),MAX_EXTENT, &
                     (MAX_EXTENT / NUM_EXTENTS)) /)

    ! Loop to test all extents
    do ext = 1, NUM_EXTENTS

       sz =  extents(ext)

       ! Alloc arrayA to select extent and initialize
       allocate(arrayA(sz,sz,sz))

       arrayA = RESHAPE( (/((((i + 10*j + 100*k),i=1,sz),j=1,sz),k=1,sz)/), &
                 (/sz,sz,sz/) )

       sum_res = SUM(arrayA)

       i = sz
       j = sz
       k = sz

      ! Loop over OFFLOAD cases
      !    case 1 = no offload - execute on host-CPU only
      !    case 2 = OMP OFFLOAD
      !    case 3 = OFFLOAD Begin/End

      do offload_case = 1, NUM_CASES

         if (verbosity >= 1) print "(4X,2(A,I0),A)", "--> Start Case #", &
                             offload_case, " [dim=", sz, "]"

         ! Offload assumed-shape array
         call leoF05_assumed_shp_offld(arrayA, i, j, k, off_res, offload_case)

         ! Check results
         call leoF05_assumed_shp_chk_results( &
                     off_res, sum_res, sz, offload_case, sample_name)

         if (verbosity >= 1) print "(4X,A,I0,/)", "--> End Case #",offload_case

       enddo

       deallocate(arrayA)
    enddo 

    ! Cleanup
    call leoF_cleanup(sample_name)

    return
    end subroutine leoF05_assumed_shape_array


    subroutine leoF05_assumed_shp_chk_results( &
                      res, sum_res, sz, offload_case, sample_name)

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    implicit none

    integer          , intent(in)  :: res, sum_res, sz, offload_case
    character (LEN=*), intent(in)  :: sample_name

    character (LEN=15)         :: err_str

    if (verbosity >= 2) then
       print "(4X,2(A,I))", "Results: Expected sum = ",sum_res, &
                            " Actual sum = ",res

       ! Display offload section details
       call offload_summary
    endif

    ! Validate results
    if ( offload_case > 1) then
       if ( (res == sum_res) .AND. &
             offload_verify(3) ) then
          print 2000, sample_name, offload_case, sz
       else
          if (.NOT. offload_verify(1)) then
             err_str="offload failure"
          else
             err_str="data mismatch"
          endif
          print 2001, sample_name, offload_case, sz, err_str
       endif
    else
        if (res == sum_res) then
           print 2000, sample_name, offload_case, sz
        else
           err_str="data mismatch"
           print 2001, sample_name, offload_case, sz, err_str
        endif
    endif

2000  format(4X,"PASS ",A," (Case #",I0," - [dim=",I0,"])")
2001  format(4X,"*** FAIL ",A," (Case #",I0," - [dim=",I0,"]) : ",A)

    return
    end subroutine leoF05_assumed_shp_chk_results
!*................................................. leoF05_assumed_shape_array
