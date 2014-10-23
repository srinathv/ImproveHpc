!          DO n=1,np
!            DO m=1,np
!              div(m,n) = 0
!!DIR$ UNROLL(4)
!              DO j=1,np
!                div(m,n) = div(m,n) - (elem%spheremp(j,n)*vtemp(j,n,1)*deriv%dvv(m,j)+elem%spheremp(m,j)*vtemp(m,j,2)*deriv%dvv(n,j))
!	            ENDDO
!            ENDDO
!          ENDDO




 

  program reductLoop
  implicit none

  INTEGER , PARAMETER  :: np = 4
  real :: div(np,np)
  INTEGER :: n,m,j,i
  real :: vtemp(np,np,2)
  INTEGER, PARAMETER :: iter = 100000000

  integer ( kind = 4 ) clock_count
  integer ( kind = 4 ) clock_count1
  integer ( kind = 4 ) clock_count2
  integer ( kind = 4 ) clock_max
  integer ( kind = 4 ) clock_rate


   vtemp=1.
   div=0.
 call system_clock(clock_count,clock_rate,clock_max)
do i=1,iter
    do n=1,np
      do m=1,np
        !div(m,n)=0
#ifdef DUNROLL
!DIR$ UNROLL(4)
#endif
        do j=1,np
          div(m,n) = div(m,n) -( vtemp(j,n,1) + vtemp(j,n,2))
        enddo
      enddo
    enddo
    div=div+1
enddo
  call system_clock(clock_count1,clock_rate,clock_max)
  print *, clock_count1, clock_count, clock_rate
  print *, "time in seconds = ", (clock_count1-clock_count)/real(clock_rate)

 write(*,*) div
  
  endprogram reductLoop
