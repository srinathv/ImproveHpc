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

!* leoF02_global_common ......................................................
!
! This sample demonstrates using global (COMMON) variables within an
! offloaded OMP construct, a non-OMP offloaded code section, and other 
! non-offload code sections
!
! Clarifications:
!
!  1. Each COMMON variable used in offloaded code must appear in an 
!     ATTRIBUTES OFFLOAD directive within the scope used.
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

    ! leoF02_global common block and parameter declarations include file

    integer, parameter        :: SZ = 1000

    real, dimension(SZ)       :: in1, in2, res
    real, dimension(SZ/2)     :: h_res


    !  In this sample, COMMON variables are only used within offload code
    !  code within subprogram leoF02_global_offld; therefore, the
    !  ATTRIBUTES OFFLOAD directive is required only in one subprogram scope.
    !  For simplicity, the directive appears in the subprogram
    !  leoF02_global_offld itself. Refer to leoF02_global.F90 for details.

    !  When COMMON variables are used in offload code within multiple 
    !  scopes, for added convenience of inserting the required 
    !  ATTRIBUTES OFFLOAD directive, the directive can alternatively 
    !  appear in an include file such as this file as demonstrated 
    !  below.
    !
    !   !DIR$ ATTRIBUTES OFFLOAD : mic :: in1, in2, res

    common /leoF02_global_cmnBLK/ in1, h_res, in2, res

!*....................................................... leoF02_global_common
