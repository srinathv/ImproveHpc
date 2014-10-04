        implicit none

        integer, parameter :: M = 16, N = 2

        integer :: i, j

        real :: src(N*M) = 1
        integer :: dst(M)

        do i = 1, M
           j = (i-1)*N + 1
           dst(i) = product(src(j:j+N-1))
        end do

        write (*,*) dst(1)
        end
