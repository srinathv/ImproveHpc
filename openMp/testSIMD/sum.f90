function add3(x)
  implicit none
  real :: add3
  real, intent(in) :: x
  add3 = x + 3.
end function add3

! This function will be inlined into external callers by
! interprocedural optimization (IPO).
function mysum(a, n)
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
  implicit none
  real :: mysum, add3
  integer, intent(in) :: n
  real, intent(in), dimension(n) :: a
  integer :: i
  mysum = 0
  do i = 1, n 
    mysum = mysum + a(i) + add3(a(i))
  end do 
end function mysum
