subroutine data_not_collected_routine()
   implicit none
   integer i, total
   total = 0
   do i = 1, 10000000
     total = total + 1
   end do
end subroutine data_not_collected_routine

subroutine data_collected_routine()
   implicit none
   integer i, total
   total = 0
   do i = 1, 10000000
     total = total + 1
   end do
end subroutine data_collected_routine


program test
   USE, INTRINSIC :: ISO_C_BINDING
   USE  ITT_FORTRAN

   CALL FORTRAN_ITT_RESUME()
   CALL data_collected_routine()
   CALL FORTRAN_ITT_PAUSE()
   CALL data_not_collected_routine()
   CALL FORTRAN_ITT_RESUME()
   CALL data_collected_routine()
end program test
