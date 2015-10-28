program mpca

  implicit none

  integer, parameter :: dp = kind(1.0d0)

  integer :: i, iter, nmove, ncars, nlocal, icell, cellval
  integer :: procid, procup, procdn, numproc

  integer, parameter :: N = 480000
  integer, parameter :: maxiter = 200
  integer, parameter :: sumfreq = maxiter/10

  real :: density, rng

  integer, allocatable, dimension(:) :: cn
  integer, allocatable, dimension(:) :: cnminus1

  real (kind = dp) :: begin, end 
  real (kind = dp) :: gettime 

! Set target density of cars

  density = 0.503

! Initialise message passing system, get process id and number of processes

  call mpstart(numproc, procid)

  if (procid == 0) then
    write(*,*) 'Length of road is ', N
    write(*,*) 'Running on ', numproc, ' process(es)'
    write(*,*) 'Number of iterations is ', maxiter
    write(*,*) 'Target density of cars is ', density
    write(*,*)
  end if

! Compute local size of road and check it is OK

  nlocal = N/numproc

  if (nlocal*numproc /= N) then
    if (procid == 0) then
      write(*,*) 'ERROR: road size of ', N, &
                ' is not a multiple of number of processes ', numproc
    end if

    call mpstop()
    stop
  end if

! Allocate arrays

  allocate(cn(nlocal))
  allocate(cnminus1(0:nlocal+1))

! Find neighbours up and down

  procup = mod(procid+1, numproc)
  procdn = mod(procid+numproc-1, numproc)

! Initialise road accordingly using random number generator

  if (procid == 0) then
    write(*,*) 'Initialising ...'
  end if

  ncars = 0

  icell = procid*nlocal

  do i = 1, nlocal

    icell = icell + 1

! Set the value based on the global cell number for reproducibility

    call initcell(cellval, icell, density)

    cnminus1(i) = cellval
    ncars = ncars + cnminus1(i)

  end do

  if (procid == 0) then
    write(*,*) '... done'
    write(*,*)
  end if

! Sum the number of cars across processes

  call mpgsum(ncars)

  if (procid == 0) then
    write(*,*) 'Actual Density of cars is ', float(ncars)/float(N)
    write(*,*)
    begin = gettime();
  end if


  do iter = 1, maxiter

    nmove = 0

!  Serial code would simply be:
!
!    cnminus1(0)   = cnminus1(N)
!    cnminus1(N+1) = cnminus1(1)

! First send element nlocal upwards and receive element 0 downwards

    call mpsendrecv(cnminus1(nlocal), procup, cnminus1(0), procdn)

! Now send element 1 downwards and receive element nlocal+1 upwards

    call mpsendrecv(cnminus1(1), procdn, cnminus1(nlocal+1), procup)

! Apply CA rules to all local cells

    do i = 1, nlocal

      if (cnminus1(i) == 1) then

        if (cnminus1(i+1) == 1) then
          cn(i) = 1
        else
          cn(i) = 0
          nmove = nmove + 1
        end if

      else

        if (cnminus1(i-1) == 1) then
          cn(i) = 1
        else
          cn(i) = 0
        end if

      end if

    end do

! Copy new to old array

    do i = 1, nlocal
      cnminus1(i) = cn(i)
    end do

    if (mod(iter, sumfreq) == 0) then

! Globally sum the number of moves

      call mpgsum(nmove)

    end if

  end do


  if (procid == 0) then
    end = gettime()
     write(*,*) 'At iteration ', iter, ' average velocity is ', &
     float(nmove)/float(ncars)
    write(*,*)
    write(*,*) 'Finished'
    write(*,*) 'Time  = ', end-begin, ' seconds'
  end if

! Finalise

  call mpstop()

end program mpca


subroutine initcell(cell, index, density)

  implicit none

  integer :: cell, index, i 
  real    :: density, rng

  real, external :: uni
 
  integer (kind=8), parameter :: aa = 16807 
  integer (kind=8), parameter :: mm = 2147483647   
  integer (kind=8) :: tmp, res

  res = index*aa
  do i=1,20
     res = mod(aa*res,mm)
  end do  
  rng = dble(res)/dble(mm) 

  if (rng < density) then
    cell = 1
  else
    cell = 0
  end if


end subroutine initcell
  
