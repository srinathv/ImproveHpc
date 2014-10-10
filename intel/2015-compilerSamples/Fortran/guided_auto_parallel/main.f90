 ! Copyright (C) 2010 Intel Corporation. All Rights Reserved.
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
 ! Part of the Guided Auto Parallelization tutorial.  For details,
 ! please see Intel(R) Fortran Composer Getting Started Tutorials
 !

program scalar_dep_main
    use scalar_dep

    integer :: time_begin, time_end, count_rate
    
    call system_clock (time_begin)

    do i = 1, 100000
        call test_scalar_dep(size)
    end do

    call system_clock (time_end, count_rate)
    write(*,*) 'Time of operation was ', real(time_end - time_begin)/real(count_rate), ' seconds'

end program scalar_dep_main
