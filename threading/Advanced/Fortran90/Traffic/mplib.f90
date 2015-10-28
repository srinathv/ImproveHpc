subroutine mpstart(numproc, procid)

  implicit none

  include 'mpif.h'

  integer :: numproc, procid, comm, ierr

  comm = MPI_COMM_WORLD

  call mpi_init(ierr)
  call mpi_comm_size(comm, numproc, ierr)
  call mpi_comm_rank(comm, procid,  ierr)

end subroutine mpstart


subroutine mpstop()

  implicit none

  include 'mpif.h'

  integer :: ierr

  call mpi_finalize(ierr)

end subroutine mpstop


subroutine mpgsum(n)

  implicit none

  include 'mpif.h'

  integer :: n, itmp, comm, ierr

  comm = MPI_COMM_WORLD

  call mpi_allreduce(n, itmp, 1, MPI_INTEGER, MPI_SUM, comm, ierr)

  n = itmp

end subroutine mpgsum


subroutine mpsendrecv(destdat, destproc, srcbuff, srcproc)

  implicit none

  include 'mpif.h'

  integer :: destproc, destdat, srcproc, srcbuff, tag, comm, ierr
  integer, dimension(MPI_STATUS_SIZE) :: status

  comm = MPI_COMM_WORLD

  tag = 1

  call mpi_sendrecv(destdat, 1, MPI_INTEGER, destproc, tag, &
                    srcbuff, 1, MPI_INTEGER, srcproc,  tag, &
                    comm, status, ierr)

end subroutine mpsendrecv



function gettime()

  implicit none

  integer, parameter :: dp = kind(1.0d0)

  real (kind = dp) gettime

  include 'mpif.h'

  gettime = MPI_Wtime() 

end function gettime 



