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
!#      Offload Demonstration
!#      Getting Started Tutorial
!#******************************************************************************

module tbo_glbls

   use omp_lib         ! Provides OpenMP* specific APIs
   use mic_lib         ! Provides Intel(R) Xeon Phi(TM) specific APIs

   implicit none

   character (LEN=*),parameter  :: &
                     Tname = "Fortran Tutorial: Offload Demonstration"

   ! Global variables used within offloaded code require declaration
   ! with an ATTRIBUTES OFFLOAD to ensure a copy appears in the 
   ! offloaded image

   ! Variables may be declared individually using the ATTRIBUTES OFFLOAD

       !DIR$ ATTRIBUTES OFFLOAD : mic :: all_Vals
       integer, allocatable, dimension(:)   :: all_Vals

       !DIR$ ATTRIBUTES OFFLOAD : mic :: E_vals, O_vals, P_vals
       integer, allocatable, dimension(:)   :: E_vals, O_vals, P_vals

   ! Or as a group using the OPTIONS /offload_attribute_target=mic 

       !DIR$ OPTIONS /offload_attribute_target=mic
           integer             :: numEs, numOs, numPs
           integer, parameter  :: MAXSZ = 26
       !DIR$ END OPTIONS


   ! target_id designates the target device. -1 allows offload
   ! execution on a target selected by the offload run-time library

   integer, parameter  :: target_id = -1

end module tbo_glbls


! Subprograms referenced within offloaded code must be declared with the 
! ATTRIBUTES OFFLOAD to ensure a copy appears in the offloaded image

!DIR$ ATTRIBUTES OFFLOAD : mic :: gather_odds
subroutine gather_odds

  use tbo_glbls

  integer :: k

  ! Gather odd numbered values into O_vals and count in numOs

  numOs = 0
  do k = 1, MAXSZ
     if ( MODULO(all_Vals(k),2) /= 0 ) then
        numOs = numOs + 1
        O_vals(numOs) = all_Vals(k)
     endif
  enddo

  ! References to other subprograms can appear within offloaded code

  ! Subprograms referenced from within offloaded code must be declared with
  ! the ATTRIBUTES OFFLOAD within the calling scope to inform the compiler 
  ! the reference involves an offloaded subprogram.

  !DIR$ ATTRIBUTES OFFLOAD : mic :: gather_primes

  ! Gather prime numbers 
  call gather_primes

end subroutine gather_odds


! Subprograms referenced from offloaded code must be declared with the 
! ATTRIBUTES OFFLOAD to ensure a copy appears in the offloaded image

!DIR$ ATTRIBUTES OFFLOAD : mic :: gather_primes
subroutine gather_primes()

  use tbo_glbls

  integer      ::   k, l, sqrt_tmp
  logical      ::   is_prime

  ! Gather prime numbered values into P_vals and count in numPs

  numPs = 0
  do k = 1, MAXSZ
     is_prime = .TRUE.

     if  ( all_Vals(k) > 1) then
        sqrt_tmp = INT ( AINT( SQRT ( REAL(all_Vals(k)) ) ) )

        do l = 2, sqrt_tmp
           if ( MODULO (all_Vals(k),l) == 0 ) is_prime = .FALSE.
        enddo

        if ( is_prime ) then
           numPs = numPs + 1
           P_vals(numPs) = all_Vals(k);
        endif
     endif
  enddo

end subroutine gather_primes


program TBO

  ! Fortran Tutorial 
  !   A basic demonstration of the Language Extension for Offload (LEO)

  use tbo_glbls

  integer        :: k
  integer        :: num_devices = 0


  print "(/2X,A,/)", Tname

  ! MAXSZ must be evenly divisible
  if (MODULO(MAXSZ,2) /= 0) then
     print "(4X,3(A))", "*** FAIL ",Tname," - value for MAXSZ not usable"
     stop
  endif

  ! Check for and report number of target devices installed when offload 
  ! compilation enabled 

  ! When no target device is available, target_id = -1 forces a run-time 
  ! error

  ! __INTEL_OFFLOAD is predefined only when offload compilation is enabled

#ifdef __INTEL_OFFLOAD
     print "(2X,A,/)", &
           "Checking for Intel(R) Xeon Phi(TM) (Target CPU) devices..."
     num_devices = OFFLOAD_NUMBER_OF_DEVICES()
     print "(4X,A,I6,/)","Number of Target devices installed: ",num_devices
#endif


 ! allocate all_Vals

  allocate(all_Vals(MAXSZ))
  all_Vals = 0


  ! Initialize all_Vals 

  ! Demonstrate offloading an OpenMP parallel construct using the 
  ! OMP OFFLOAD directive
  !
  ! Limit OpenMP construct to two threads for demonstration purposes
  !
  ! Offload IN/OUT/INOUT clauses:
  !    Transfer all_Vals new values "OUT" only

  !DIR$ OMP OFFLOAD target(mic : target_id) mandatory out(all_Vals)
  !$omp parallel num_threads(2)
      !$omp do
      do k = 1, MAXSZ
         all_Vals(k) = k
      enddo
  !$omp end parallel


  ! allocate E_vals

  allocate(E_vals(MAXSZ/2))
  E_vals = 0


  ! Gather even numbered values into E_vals and count in numEs

  ! Demonstrate offloading a code block using the OFFLOAD BEGIN/END directives
  !
  ! Offload IN/OUT/INOUT clauses:
  !    Transfer all_Vals values "IN" only
  !    Transfer numEs initial value (on host) in and the final value
  !    (from target) out with "INOUT"
  !    Transfer E_vals new values "OUT" only

  numEs = 0
  !DIR$ OFFLOAD BEGIN target(mic : target_id) mandatory &
                                   inout(numEs) in(all_Vals) out(E_vals)
      do k = 1, MAXSZ
         if ( MODULO(all_Vals(k),2) == 0 ) then
             numEs = numEs + 1
             E_vals(numEs) = all_Vals(k)
         endif
      enddo
  !DIR$ END OFFLOAD


  ! allocate O_vals and P_vals

  allocate(O_vals(MAXSZ/2))
  O_vals = 0

  allocate(P_vals(MAXSZ/2))
  P_vals = 0


  ! Gather odd numbered values into O_vals and count in numOs and
  ! prime numbered values into P_vals and count in numPs
   
  ! Demonstrate offloading a subprogram reference using the OFFLOAD directive
  !
  ! Subprograms referenced from within offloaded code must be declared with
  ! the OFFLOAD ATTRIBUTES within the calling scope to inform the compiler 
  ! the reference is to an offloaded subprogram.

  !DIR$ ATTRIBUTES OFFLOAD : mic :: gather_odds

  !  The use of global variables all_Vals, O_vals, numOs, P_vals, and
  !  numPs in gather_odds() and gather_primes() is outside the compiler's 
  !  lexical scope; therefore, to ensure the integrity of their values 
  !  between the host and target CPUs before and after execution of the 
  !  subprograms, explicit IN/OUT/INOUT clauses are required to ensure 
  !  their values are transferred between the host and target CPUs 
  !  accordingly.

  ! Offload IN/OUT/INOUT clauses:
  !    Transfer all_Vals values "IN" only
  !    Transfer O_vals, numOs, P_vals, and numPs new values "OUT" only

  !DIR$ OFFLOAD target(mic : target_id) mandatory &
                                        in(all_Vals) &
                                        out(O_vals, numOs, P_vals, numPs)
     call gather_odds()


  ! Display results
   
  print "(2X,A)", "Unsorted original values...first twenty (20) values:"
  print "(4X,A)", "Evens and Odds:"
  print "(11X,10(1x,I4))", (all_Vals(k),k=1,MIN(10,(MAXSZ/2)))
  print "(11X,10(1x,I4),/)",   &
         (all_Vals(k), k=(1 + MIN(10,(MAXSZ/2))),MIN(20,MAXSZ))

  print "(2X,A)", "Sorted results...first ten (10) values each:"
  print "(5X,A,10(1X,I4))", "Evens: ",(E_vals(k),k=1,MIN(10,numEs))
  print "(5X,A,10(1X,I4))", "Odds : ",(O_vals(k),k=1,MIN(10,numOs))
  print "(4X,A,10(1X,I4))", "Primes: ",(P_vals(k),k=1,MIN(10,numPs))
  print "(A)"," "


  ! Deallocate

  deallocate(P_vals)
  deallocate(O_vals)
  deallocate(E_vals)
  deallocate(all_Vals)

end program TBO
