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

!* leoF09_subprograms_callees ................................................
!
! This file contains the callee subroutine and function for the 
! leoF09_subprograms sample.
!
! The leoF09_subprograms sample demonstrates calling a subroutine and function 
! from within both an OMP OFFLOAD and OFFLOAD BEGIN/END construct and the 
! non-OMP OFFLOAD directive to offload a single subprogram call. The sample 
! combines calling the offload subprograms with using global variables within 
! each.
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
! 4. Global variables referenced in an offloaded subprogram that are
!    outside the compiler's lexical scope of offload code must be named
!    in an IN/OUT/INOUT clause to ensure their values are exchanged
!    accordingly between the Host and Target CPUs.
!
!.............................................................................


    ! The ATTRIBUTES OFFLOAD directive is active only for the subsequent
    ! subprogram scope that follows it.
    !
    ! Declare subprograms and global variables used in offload code within
    ! subroutine leoF09_subprogram accordingly for use in offload code

    !DIR$ ATTRIBUTES OFFLOAD : mic :: leoF09_subprograms_sub, res
    subroutine leoF09_subprograms_sub(ival)

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    implicit none

    integer, intent(in)          :: ival
    integer                      :: res

    common /leoF09_subprograms_cmnBLK/res

    ! Execution on Target CPU adds ival, on Host CPU subtracts ival
#ifdef __MIC__
        res = res + ival
#else
        res = res - ival
#endif

    ! Verify offload execution 
    call offload_check()

    return
    end subroutine leoF09_subprograms_sub


    ! Declare subprograms and global variables used in offload code within
    ! subroutine leoF09_subprogram accordingly for use in offload code

    !DIR$ ATTRIBUTES OFFLOAD : mic :: leoF09_subprograms_fun, res
    integer function leoF09_subprograms_fun(jval)

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    implicit none

    integer, intent(in)          :: jval
    integer                      :: res

    common /leoF09_subprograms_cmnBLK/res

    ! Execution on Target CPU adds ival, on Host CPU subtracts ival
#ifdef __MIC__
        leoF09_subprograms_fun = res + jval
#else
        leoF09_subprograms_fun = res - jval
#endif

    ! Verify offload execution 
    call offload_check()

    end function leoF09_subprograms_fun
!*................................................. leoF09_subprograms_callees
