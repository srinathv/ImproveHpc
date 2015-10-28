program area_mandelbrot
  use omp_lib
  implicit none
!
  integer, parameter :: sp = kind(1.0)   
  integer, parameter :: dp = kind(1.0d0)
  integer :: i, j, iter, numoutside
  integer, parameter :: npoints = 2000, maxiter = 2000
  real (kind=dp) :: area, error, tstart, tend
  complex (kind=dp) :: c , z

!
! Calculate area of mandelbrot set
!
!    Outer loops runs over npoints, initialize z=c
!
!    Inner loop has the iteration z=z*z+c, and threshold test
!

  numoutside = 0 

  tstart = omp_get_wtime()
 
  do i = 0,npoints-1 
     do j= 0,npoints-1 
        c = cmplx(-2.0+(2.5*i)/npoints + 1.0d-07,(1.125*j)/npoints + 1.0d-07)
        z = c
        iter = 0 
        do while (iter < maxiter) 
           z = z*z + c 
           iter = iter + 1
           if (real(z)*real(z)+aimag(z)*aimag(z) > 4) then
              numoutside = numoutside + 1 
              exit
           endif
        end do 
     end do
  end do


  tend = omp_get_wtime()
!
! Output results
!
  area = 2.0*2.5*1.125 * real(npoints*npoints-numoutside)/real(npoints*npoints)
  error = area/real(npoints)
  print *, "Area of Mandelbrot set = ",area," +/- ",error
  print *, "Time = ",tend-tstart, " on ", omp_get_max_threads(), "threads"
!
  stop
end program area_mandelbrot
