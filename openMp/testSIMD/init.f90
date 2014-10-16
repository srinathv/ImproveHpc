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


  implicit none
  integer, intent(in) :: n
  real, intent(inout) :: a(n) 
  integer :: i
  do i = 1, n 
    a(i) = real(i) 
  end do 
end subroutine init


