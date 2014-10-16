
program main
  implicit none
  interface
    subroutine init(a, n)
include ifdef.include
      integer, intent(in) :: n
      real, intent(inout) :: a(n)
    end subroutine init 

    real function mysum(a, n)
include ifdef.include
      integer, intent(in) :: n
      real, intent(inout) :: a(n)
    end function mysum
  end interface

  real :: res   !!, mysum
  integer :: i
  integer, parameter :: n=10000
  real, dimension(n) :: a
  integer ( kind = 4 ) clock_count
  integer ( kind = 4 ) clock_count1
  integer ( kind = 4 ) clock_count2
  integer ( kind = 4 ) clock_max
  integer ( kind = 4 ) clock_rate
  
  print *, "USING SIMD"
  call system_clock(clock_count,clock_rate,clock_max)
!$omp simd private(a)
  do i = 1, n
  call init(a, n)
  res = mysum(a, n)
  enddo
  call system_clock(clock_count1,clock_rate,clock_max)
  print *, "result is ", res
  print *, clock_count1, clock_count, clock_rate
  print *, "time in seconds = ", (clock_count1-clock_count)/real(clock_rate)
end program main
