c
	integer i,j,c,n,reps
	parameter (reps=10000,n=64,m=1000)
	real x(n,m)
	real*8 time1,second
        common /junk/ x 
c
c
c Initialise vector data
c
        do i=1,m 
           do j=1,n
              x(j,i) =  3.142
           end do
        end do
c
c sum operation
c
        print *, "Offset    Mflop/s"
c
        do c = 1,n
	time1=second()
           do i=1,reps
              call sumsubfs (x,n,c,m)
           end do 
	time1=second()-time1
C
        print *, c-20, "     ",real(m*reps)/(1000000.*time1)
        end do
c
	end
c
c
c
        subroutine sumsubfs (a,n,offset,m)
c
c .. Scalar arguments 
        integer n,offset,m
c
c .. Array arguments
        real a(n,m)
c
c .. Local scalars
        integer i,id

c .. Functions
        integer omp_get_thread_num
c
!$omp parallel  private(i,id) 
c
        id  = omp_get_thread_num()
c
        if (id .eq. 0) then
c
           do i=1,m
              a(20,i)=a(20,i)+3.142 
           enddo 
c
        elseif (id .eq. 1) then 
c
           do i=1,m
              a(offset,i)=a(offset,i)+3.142
           end do 
c
        end if 
c
!$omp end parallel 
c
        return
        end

