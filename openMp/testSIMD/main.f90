
program main
  implicit none
  interface
    subroutine init(a, n)
#ifdef SIMD
!$omp declare simd(init)
#ifdef SIMDU
!$omp declare simd(init) uniform(n) 
#ifdef SIMDUL
!$omp declare simd(init) uniform(n) simdlen(4)
#ifdef SIMDL
!$omp declare simd(init) simdlen(4)
#ifdef SIMDA
!$omp declare simd(init) alinged(a)
#ifdef SIMDAU
!$omp declare simd(init) alinged(a) uniform(n) simdlen(4)
#ifdef SIMDAL
!$omp declare simd(init) alinged(a) simdlen(4)
#ifdef SIMDAUL
!$omp declare simd(init) alinged(a) uniform(n) simdlen(4)
#endif

      integer, intent(in) :: n
      real, intent(inout) :: a(n)
    end subroutine init 

    real function mysum(a, n)
#ifdef SIMD
!$omp declare simd(init)
#ifdef SIMDU
!$omp declare simd(init) uniform(n) 
#ifdef SIMDUL
!$omp declare simd(init) uniform(n) simdlen(4)
#ifdef SIMDL
!$omp declare simd(init) simdlen(4)
#ifdef SIMDA
!$omp declare simd(init) alinged(a)
#ifdef SIMDAU
!$omp declare simd(init) alinged(a) uniform(n) simdlen(4)
#ifdef SIMDAL
!$omp declare simd(init) alinged(a) simdlen(4)
#ifdef SIMDAUL
!$omp declare simd(init) alinged(a) uniform(n) simdlen(4)
#endif
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
