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
  INTEGER :: n,m,j
  real :: vtemp(np,np,2)


   vtemp=1.

    do n=1,np
      do m=1,np
        div(m,n)=0
#ifdef UNROLL
!DIR$ UNROLL(4)
#endif
        do j=1,np
          div(m,n) = div(m,n) -( vtemp(j,n,1) + vtemp(j,n,2))
        enddo
      enddo
    enddo


 write(*,*) div
  
  endprogram reductLoop
