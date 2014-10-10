 ! Copyright (C) 2012 Intel Corporation. All Rights Reserved. 
 ! 
 ! The source code contained or described herein and all 
 ! documents related to the source code ("Material") are owned by 
 ! Intel Corporation or its suppliers or licensors. Title to the 
 ! Material remains with Intel Corporation or its suppliers and 
 ! licensors. The Material is protected by worldwide copyright 
 ! laws and treaty provisions.  No part of the Material may be 
 ! used, copied, reproduced, modified, published, uploaded, 
 ! posted, transmitted, distributed,  or disclosed in any way 
 ! except as expressly provided in the license provided with the 
 ! Materials.  No license under any patent, copyright, trade 
 ! secret or other intellectual property right is granted to or 
 ! conferred upon you by disclosure or delivery of the Materials, 
 ! either expressly, by implication, inducement, estoppel or 
 ! otherwise, except as expressly provided in the license 
 ! provided with the Materials. 
 ! 
 ! [DESCRIPTION] 
 ! Demonstrates how to use Profile-guided Optimization (PGO) 
 ! to optimize performance. Illustrates how to use the PGO
 ! tools: code coverage and test prioritization.
 !
 ! For code coverage, the comment indicate the covered and uncovered
 ! functions; cover indicates covered functions, and 
 ! no_cover indicates functions that are not covered by
 ! the tool. begin_cover marks the beginning of a covered code block, and
 ! end_cover marks the end of the coverage for the block.
 ! 
 ! [COMPILE]
 ! See the Intel(R) Fortran Compiler Documentation for information 
 ! on using the PGO tools and using PGO to compile applications.
 ! Use the following general options and tool, in the order shown,
 ! to profile an application:
 ! 
 ! Windows*:            /Qprof-gen
 !                       profmerge
 !                      /Qprof-use   
 ! 
 ! Linux* and OS X*:    -prof-gen
 !                       profmerge
 !                      -prof-use   
 !
 
 module addermod 
   implicit none 
   contains 
   subroutine mod_add(i1, i2, i4, i8, res) 
     integer(1), intent(in) :: i1 
     integer(2), intent(in) :: i2 
     integer(4), intent(in) :: i4 
     integer(8), intent(in) :: i8 
     integer, intent(out) :: res 
     res = i1 + i2 + i4 + i8 
   end subroutine mod_add 
 end module addermod 
  
 subroutine delegate(non_existent, res)      ! no_cover 
   implicit none 
   external non_existent 
   integer, intent(in) :: res 
   call non_existent(1_1, 2_2, 4_4, 8_8, res) 
 end subroutine delegate 
  
 program main 
   use addermod 
   implicit none 
   integer :: res 
   call delegate(mod_add, res)               ! begin_cover 
   if (res /= 15) print *, 'mod_add failed' 
   call delegate(main_add, res) 
   if (res /= 15) print *, 'main_add failed' ! end_cover 
   contains 
   subroutine main_add(i1, i2, i4, i8, res) 
     integer(1), intent(in) :: i1 
     integer(2), intent(in) :: i2 
     integer(4), intent(in) :: i4 
     integer(8), intent(in) :: i8 
     integer, intent(out) :: res 
     res = i1 + i2 + i4 + i8 
   end subroutine main_add 
 end program main
