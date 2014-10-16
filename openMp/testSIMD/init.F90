subroutine init(a, n)
#include "ifdef.include"

  implicit none
  integer, intent(in) :: n
  real, intent(inout) :: a(n) 
  integer :: i
  do i = 1, n 
    a(i) = real(i) 
  end do 
end subroutine init


