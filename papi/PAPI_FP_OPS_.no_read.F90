
program simple

include 'f90papi.h'
use ifport


integer,parameter :: index = 1000
real,dimension(index,index):: mat1
real,dimension(index,index):: mat2
real,dimension(index,index):: resultMat
integer :: i,j,k

real :: papi1, papi2 ,rhs

INTEGER*8 :: papilonglong

!Declare the events you want to count and other error-related variables,
!for example:
       integer, parameter :: numevents = 1
       integer events (numevents),  ierr
       character*(PAPI_MAX_STR_LEN) errorstring
!Declare variables to hold the event counts:
       integer*8 values (numevents)

       !inialize matrix
        do i = 1, index, 1
          do j=1,index
          mat1(i,j)= rand()
          mat2(i,j)= rand()*(1.1)
        end do
       enddo

         resultMat=0.

!Set each event to the desired type, listed in f77papi.h (or below):
       events(1) = PAPI_FP_OPS
!       events(3) = PAPI_BR_MSP
!Start and clear the counters:
       call PAPIF_start_counters(events, numevents, ierr)
!Do some computation, then read and reset them but leave them running:
!       call PAPIF_read_counters(values, numevents, ierr)
!A similar routine, PAPIF_accum_counters, accepts the same arguments but
!adds the current values to the running totals already contained in the
!values array.
!Compute some more and then stop the counters and retrieve the values:


!multiply
 do i = 1, index, 1
   do j=1,index
     rhs=0.
     do k=1,index
       rhs=mat1(i,k)*mat2(j,k)
     enddo
     resultMat(i,j)=rhs
 end do
enddo


       call PAPIF_stop_counters(values, numevents, ierr)
!Each of those calls returns an error code that you can handle this way:
       if ( ierr .ne. PAPI_OK ) then
         call PAPIF_perror(ierr, errorstring, PAPI_MAX_STR_LEN)
         print *, errorstring
       endif


      print *, " PAPI_FP_OPS is ", values(1)
end program
