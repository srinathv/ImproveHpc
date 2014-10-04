

Program testPow

implicit none

integer, parameter :: k = 10000
integer,dimension(k) :: a=0,b=0,c=0,d=0
integer :: i
real:: t1=0,t2=0,t3=0,t4=0,t5=0,t6=0
real :: clock_rate, clock_max

call system_clock ( t1, clock_rate, clock_max )
do i = 1, k
  c(i)=a(i)**b(i)
end do
call system_clock ( t2, clock_rate, clock_max )

#ifdef MKL
call cpu_time(t3)
call vdpow(k,a,b,d)
call cpu_time(t4)
#endif

write(*,*) " ** c(1) = ",c(1), " time = ",real ( t2 - t1 ) / real ( clock_rate )
write(*,*) " vdpow d(1) = ",d(1), "time = ", t4-t3





End Program testPow
