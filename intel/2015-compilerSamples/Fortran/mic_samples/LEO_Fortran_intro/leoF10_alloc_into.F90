!# Copyright (C) 2012-2014 Intel Corporation. All Rights Reserved.
!#
!# The source code contained or described herein and all
!# documents related to the source code ("Material") are owned by
!# Intel Corporation or its suppliers or licensors. Title to the
!# Material remains with Intel Corporation or its suppliers and
!# licensors. The Material is protected by worldwide copyright
!# laws and treaty provisions.  No part of the Material may be
!# used, copied, reproduced, modified, published, uploaded,
!# posted, transmitted, distributed,  or disclosed in any way
!# except as expressly provided in the license provided with the
!# Materials.  No license under any patent, copyright, trade
!# secret or other intellectual property right is granted to or
!# conferred upon you by disclosure or delivery of the Materials,
!# either expressly, by implication, inducement, estoppel or
!# otherwise, except as expressly provided in the license
!# provided with the Materials.
!#
!#
!#******************************************************************************
!# Content:
!#
!#  Example Program Text from Sample LEO_Fortran_intro
!#******************************************************************************

!* leoF10_alloc_into .........................................................
!
! This sample demonstrates using sections of static arrays within an offload
! offload OMP construct and non-OMP offload code section through the use of 
! the ALLOC() and INTO() modifiers.
!
!.............................................................................

    module leoF10_alloc_into_glbls

       implicit none

       ! Max extents for matrices
       ! Modify MAX_EXTENT_rowA ONLY to test with larger matrices

       integer, parameter           :: MAX_EXTENT_rowA = 8
       integer, parameter           :: MAX_EXTENT_colA = (MAX_EXTENT_rowA - 1)

       integer, parameter           :: MAX_EXTENT_rowB = MAX_EXTENT_colA
       integer, parameter           :: MAX_EXTENT_colB = (MAX_EXTENT_rowB - 1)

       integer, parameter           :: MAX_EXTENT_rowC = MAX_EXTENT_rowA
       integer, parameter           :: MAX_EXTENT_colC = MAX_EXTENT_colB

       integer, parameter           :: MAX_EXTENT_rowA_tp = MAX_EXTENT_colA
       integer, parameter           :: MAX_EXTENT_colA_tp = MAX_EXTENT_rowA

    
       ! Global variables used within offloaded code require declaration
       ! with an ATTRIBUTES OFFLOAD 

       !DIR$ ATTRIBUTES OFFLOAD : mic :: matrix_B, matrix_C
       integer, dimension(MAX_EXTENT_rowA,MAX_EXTENT_colA), target  :: matrix_A
       integer, dimension(MAX_EXTENT_rowB,MAX_EXTENT_colB), target  :: matrix_B
       integer, dimension(MAX_EXTENT_rowC,MAX_EXTENT_colC), target  :: matrix_C

       !DIR$ ATTRIBUTES OFFLOAD : mic :: sum_vector, sum_matrix_C, sum_C
       integer, allocatable, dimension(:,:)  :: sum_vector
       integer, allocatable, dimension(:,:)  :: sum_matrix_C
       integer                               :: sum_C

    end module leoF10_alloc_into_glbls

    subroutine leoF10_alloc_into_offld(offload_case)

    ! leoF10_alloc_into_offld() - Operate on static arrays within 
    ! offload OMP constructs and non-OMP offload code sections. 

    ! Case 1 = no offload - execute on Host-CPU only
    ! Case 2 = OFFLOAD Begin/End using INTO()
    ! Case 3 = OFFLOAD Begin/End using ALLOC()

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    use leoF10_alloc_into_glbls    !  leoF10 specific globals

    implicit none

    integer, intent(in)                   :: offload_case

    ! Temporary matrix A transpose
    integer, dimension(MAX_EXTENT_rowA_tp, MAX_EXTENT_colA_tp)  :: matrix_A_tp

    ! Matrix row, column, and cell pointers 
    integer, dimension(:,:), pointer        :: ptr_rowA, ptr_colB
    integer,                 pointer        :: ptr_C

    ! Matrix row and column vectors
    integer, dimension(MAX_EXTENT_rowA_tp,1)  :: colA_tp
    integer, dimension(MAX_EXTENT_rowB,1)     :: colB

    integer                               :: target_id
    integer                               :: i, j, k, res


    target_id = 0

    OFFLOAD_TYPE: select case (offload_case)

    CASE(1)

       ! No OFFLOAD - Host-CPU execution

       ! Loop over source matrices to perform matrix multiplication
       ! using pointers to rows and columns in source matrices

       ! Store result into the product matrix via pointer 

       ! Initialize
       ptr_rowA => NULL()
       ptr_colB => NULL()
       ptr_C => NULL()

       do i = 1, MAX_EXTENT_rowA

          ! Set pointer to row in source matrix A
          ptr_rowA => matrix_A(i:i,1:MAX_EXTENT_colA)

          do j = 1, MAX_EXTENT_colB

             ! Set pointer to column in source matrix B
             ptr_colB => matrix_B(1:MAX_EXTENT_rowB,j:j)

             ! Set pointer to result cell in product matrix C
             ptr_C => matrix_C(i,j)

             ! Perform matrix multiply for current row/column

             ! # columns matrix_A = # rows matrix_B = SIZE(ptr_rowA) =
             !   SIZE(ptr_colB)

             do k = 1, MAX_EXTENT_colA
                ptr_C = ptr_rowA(1,k) * ptr_colB(k,1) +  ptr_C
             enddo

          enddo    ! do j = 1, MAX_EXTENT_colB
       enddo    ! do i = 1, MAX_EXTENT_rowA


       ! Generate matrix_C sum matrix and sum

       allocate(sum_matrix_C ( 1, (MAX_EXTENT_colC)))

       allocate(sum_vector ( 1, (MAX_EXTENT_rowC)))
       sum_vector(1,:) = 1

       sum_matrix_C = MATMUL(sum_vector, matrix_C)
       sum_C = SUM(sum_matrix_C)

       if (ALLOCATED(sum_vector)) deallocate(sum_vector)
       

    CASE(2)

       offload_section_id = 0         ! initialize offload section 

       ! IN/INOUT/OUT clauses require the use of contiguous memory
       ! Fortran arrays are stored in column-major order. Transpose 
       ! matrix_A to enable offloading contiguous columns of data.

       matrix_A_tp = TRANSPOSE(matrix_A)

       !  Offload code sections using OFFLOAD BEGIN/END directives

       !  The loops perform matrix multiplication using two-dimensional 
       !  (single-column) arrays filled with data from columns from the 
       !  source matrices.  The column of data is allocated on the Target 
       !  CPU using the INTO() modifier.

       !  Two-dimensional (single-column) arrays are required because
       !  the array rank for IN() and INTO() variables must be equal.

       !  The result is stored into the product matrix cells on the CPU 
       !  from data allocated on the Target CPU using the INTO() modifier.

       !  Separate offload sections are used to exploit data persistence.
       !  The outer loop transfers a column of transpose_A to the Target CPU.
       !  The inner loop transfers a column of matrix B  to the Target CPU,
       !  performs the multiplication, and returns the result into the 
       !  corresponding cell in the product matrix C.

       !  The offload section for the outer loop (do i...) determines the 
       !  Target CPU (target_id) used for the inner loop. In a multi-Target
       !  CPU configuration, each outer loop iteration selects a different 
       !  Target CPU.

       !  Loop on column count for transpose_A matrix
       do i = 1, MAX_EXTENT_colA_tp

          offload_section_id = offload_section_id + 1   ! offload section #n+1

          !  Initialize offload status
          call leoF_init_offload_status()

          !  Use of offload_section_id and modification of offload_sections
          !  in offload_check is outside the compiler's lexical scope; 
          !  therefore, use explicit IN/INOUT clauses

          !DIR$ OFFLOAD begin target(mic) optional                           &
                              status(offload_stat)                           &
                              inout(target_id)                               &
                              in( matrix_A_tp(:,i:i) :                       &
                                  alloc_if(.true.) free_if(.false.)          &
                                  into(colA_tp)        )                     &
                              in(offload_section_id) inout(offload_sections)

#         ifdef __INTEL_OFFLOAD
             target_id = OFFLOAD_GET_DEVICE_NUMBER()
#         else
             target_id = -1
#         endif

          ! Verify offload execution 
          call offload_check()
          !DIR$ end OFFLOAD 

          !  Check offload status
          call leoF10_chkoff( (1000 + offload_section_id) )

          !  Loop on column count for source matrix B
          do j = 1, MAX_EXTENT_colB

             offload_section_id = offload_section_id + 1 ! offload section #n+1

             !  Initialize offload status
             call leoF_init_offload_status()

             !DIR$ OFFLOAD begin target(mic:target_id) optional              &
                              status(offload_stat)                           &
                              if(target_id > -1)                             &
                              nocopy(colA_tp)                                &
                              in( matrix_B(:,j:j) :                          &
                                  alloc_if(.true.) free_if(.false.)          &
                                  into(colB)           )                     &
                              out(res : into(matrix_C(i,j)) )                &
                              in(offload_section_id) inout(offload_sections)

             res = 0
             do k = 1, MAX_EXTENT_rowA_tp
                res = colA_tp(k,1) * colB(k,1) + res
             enddo

             ! Verify offload execution 
             call offload_check()
             !DIR$ end OFFLOAD 

             !  Check offload status
             call leoF10_chkoff( (2000 + offload_section_id) )

          enddo    !  do j = 1, MAX_EXTENT_colB
       enddo    !  do i = 1, MAX_EXTENT_colA_tp


       ! Generate matrix_C sum matrix and sum

       allocate(sum_matrix_C ( 1, (MAX_EXTENT_colC)))

       offload_section_id = offload_section_id + 1    ! offload section #n+1

       !DIR$ OFFLOAD begin target(mic)  optional                         &
                                        in(matrix_C)                     &
                                        out(sum_matrix_C, sum_C)         &
                                        nocopy (sum_vector)              &    
                           in(offload_section_id) inout(offload_sections)


       allocate(sum_vector ( 1, (MAX_EXTENT_rowC)))

       sum_vector(1,:) = 1
       sum_matrix_C = MATMUL(sum_vector, matrix_C)
       sum_C = SUM(sum_matrix_C)

       if (ALLOCATED(sum_vector)) deallocate(sum_vector)

       ! Verify offload execution 
       call offload_check()
       !DIR$ end OFFLOAD 


    CASE(3)

       offload_section_id = 0         ! initialize offload section 

       ! IN/INOUT/OUT clauses require the use of contiguous memory
       ! Fortran arrays are stored in column-major order. Transpose 
       ! matrix_A to enable offloading contiguous columns of data.

       matrix_A_tp = TRANSPOSE(matrix_A)

       !  Offload code sections using OFFLOAD BEGIN/END directives

       !  The loops perform matrix multiplication of a single-column of
       !  data from the source matrices.  The column of data is allocated 
       !  on the Target CPU using the ALLOC() modifier.

       !  The result is stored into the product matrix cells on the CPU 
       !  from data allocated on the Target CPU using the ALLOC() modifier.

       !  Separate offload sections are used to exploit data persistence.
       !  The outer loop transfers a column of transpose_A to the Target CPU.
       !  The inner loop transfers a column of matrix B  to the Target CPU,
       !  performs the multiplication, and returns the result into the 
       !  corresponding cell in the product matrix C.

       !  The offload section for the outer loop (do i...) determines the 
       !  Target CPU (target_id) used for the inner loop. In a multi-Target
       !  CPU configuration, each outer loop iteration selects a different 
       !  Target CPU.

       !  Loop on column count for transpose_A matrix
       do i = 1, MAX_EXTENT_colA_tp

          offload_section_id = offload_section_id + 1   ! offload section #n+1

          !  Use of offload_section_id and modification of offload_sections
          !  in offload_check is outside the compiler's lexical scope; 
          !  therefore, use explicit IN/INOUT clauses

          !DIR$ OFFLOAD begin target(mic) optional                           &
                              inout(target_id)                               &
                              in(i)                                          &
                              in( matrix_A_tp(:,i:i)  :                      &
                                  alloc(matrix_A_tp(:,i:i))                  &
                                  alloc_if(.true.) free_if(.false.))         &
                              in(offload_section_id) inout(offload_sections)

#         ifdef __INTEL_OFFLOAD
             target_id = OFFLOAD_GET_DEVICE_NUMBER()
#         else
             target_id = -1
#         endif

          ! Verify offload execution 
          call offload_check()
          !DIR$ end OFFLOAD 


          !  Loop on column count for source matrix B
          do j = 1, MAX_EXTENT_colB

             offload_section_id = offload_section_id + 1 ! offload section #n+1

             !DIR$ OFFLOAD begin target(mic:target_id) optional              &
                                 if(target_id > -1)                          &
                                 in(i,j)                                     &
                                 nocopy(matrix_A_tp)                         &
                                 in( matrix_B(:,j:j) :                       &
                                     alloc( matrix_B(:,j:j) )                &
                                     alloc_if(.true.) free_if(.false.))      &
                                 out(matrix_C(i,j): alloc(matrix_C(i,j)) )   &
                                 in(offload_section_id) inout(offload_sections)

             matrix_C(i,j) = 0
             do k = 1, MAX_EXTENT_rowA_tp
                matrix_C(i,j) = matrix_A_tp(k,i) * matrix_B(k,j) + matrix_C(i,j)
             enddo

          ! Verify offload execution 
          call offload_check()
          !DIR$ end OFFLOAD 

          enddo   ! do j = 1, MAX_EXTENT_colB
       enddo    ! do i = 1, MAX_EXTENT_colA_tp

       ! Generate matrix_C sum matrix and sum

       allocate(sum_matrix_C ( 1, (MAX_EXTENT_colC)))

       offload_section_id = offload_section_id + 1 ! offload section #n+1

       !DIR$ OFFLOAD begin target(mic) optional                           &
                                       in(matrix_C)                       &
                                       out(sum_matrix_C, sum_C)           &
                                       nocopy (sum_vector)                &
                           in(offload_section_id) inout(offload_sections)

        allocate(sum_vector ( 1, (MAX_EXTENT_rowC)))

        sum_vector(1,:) = 1
        sum_matrix_C = MATMUL(sum_vector, matrix_C)
        sum_C = SUM(sum_matrix_C)

        if (ALLOCATED(sum_vector)) deallocate(sum_vector)

       ! Verify offload execution 
       call offload_check()
       !DIR$ end OFFLOAD 


    CASE DEFAULT
        call abort("*** ABORT - internal failure in leoF10_alloc_into_offld")

    end select OFFLOAD_TYPE

    return
    end subroutine leoF10_alloc_into_offld


    subroutine leoF10_alloc_into

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    use leoF10_alloc_into_glbls    !  leoF10 specific globals

    implicit none

    character (LEN=*),parameter  :: sample_name = "leoF10_alloc_into"
    integer          ,parameter  :: NUM_CASES = 3

    integer                      :: offload_case, i

    ! Setup and initialization

    ! number of offload sections in sample
    num_offload_sections = (MAX_EXTENT_colA_tp +   &
                           (MAX_EXTENT_colA_tp * MAX_EXTENT_colB)) + 1


    call leoF_setup(sample_name)

    ! Max extent for matrix A must be >= 3 
    if (MAX_EXTENT_rowA < 3) then
        print "(4X,3(A))", "*** FAIL ",sample_name," - value for MAX_EXTENT_rowA not usable - must be >= 3"
        return
    ! # columns in matrix A must equal # rows in matrix B
    else if (MAX_EXTENT_colA /= MAX_EXTENT_rowB) then
        print "(4X,3(A))", "*** FAIL ",sample_name," - value for MAX_EXTENT_rowA must equal MAX_EXTENT_rowB"
        return
    endif

    ! Loop over cases
    !   Case 1 = no offload - execute on Host-CPU only
    !   Case 2 = OFFLOAD Begin/End using INTO()
    !   Case 3 = OFFLOAD Begin/End using ALLOC()

    do offload_case = 1, NUM_CASES

       if (verbosity >= 1) print "(4X,A,I0)", "--> Start Case #",offload_case

      ! Initialize matrices
       matrix_C = 0

       matrix_A = RESHAPE( (/ (i,i=1,MAX_EXTENT_rowA*MAX_EXTENT_colA)/),(/MAX_EXTENT_rowA,MAX_EXTENT_colA/),ORDER=(/2,1/))

       matrix_B = RESHAPE( (/ (i,i=1,MAX_EXTENT_rowB*MAX_EXTENT_colB)/),(/MAX_EXTENT_rowB,MAX_EXTENT_colB/),ORDER=(/2,1/))


       ! Offload using ALLOC() and INTO() 
       call leoF10_alloc_into_offld(offload_case)

       ! Check results
       call leoF10_alloc_into_chk_results(offload_case, sample_name)

       if (verbosity >= 1) print "(4X,A,I0,/)", "--> End Case #",offload_case

       ! Cleanup

    enddo

    ! Cleanup
    call leoF_cleanup(sample_name)

    return
    end subroutine leoF10_alloc_into


    subroutine leoF10_chkoff(offid)

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    ! Check STATUS clause

    integer, intent(in)             :: offid

    character (LEN=*), parameter, dimension(0:5)  :: offld_status_vals = &
                              (/"SUCCESS","DISABLED","UNAVAILABLE", &
                              "OUT_OF_MEMORY","PROCESS_DIED","ERROR"/)

    !  Create an empty subroutine when compiling with -offload=none

    !  __INTEL_OFFLOAD predefined macro is only defined when the
    !  offload compilation is enabled

#   ifdef __INTEL_OFFLOAD

    if (verbosity >= 2) then
       print "(4X,3(A),I0)", "STATUS clause : OFFLOAD_", &
                       TRIM(offld_status_vals(offload_stat%result)), &
                       " at offload #",offid
    endif

#   endif

    end subroutine leoF10_chkoff

    subroutine leoF10_alloc_into_chk_results(offload_case, sample_name)

    use leoF_utils    ! Provides samples utility routines, globals,
                      ! access to OMP (omp_lib) and Intel(R) Xeon
                      ! Phi(TM) (mic_lib) APIs

    use leoF10_alloc_into_glbls    !  leoF10 specific globals

    implicit none

    integer          , intent(in)  :: offload_case
    character (LEN=*), intent(in)  :: sample_name

    integer, allocatable, dimension(:,:)  :: matrix_C_vrfy
    integer, allocatable, dimension(:,:)  :: sum_matrix_C_vrfy
    integer                               :: sum_C_vrfy

    integer                :: m, n
    character (LEN=15)     :: err_str


    ! Allocate verification matrices and vector 

    allocate(matrix_C_vrfy( (MAX_EXTENT_rowC) , (MAX_EXTENT_colC) ))
    allocate(sum_vector ( 1, (MAX_EXTENT_rowC)))
    allocate(sum_matrix_C_vrfy ( 1, (MAX_EXTENT_colC)))

    matrix_C_vrfy = MATMUL(matrix_A, matrix_B)

    sum_vector(1,:) = 1
    sum_matrix_C_vrfy = MATMUL(sum_vector, matrix_C_vrfy)
    sum_C_vrfy = SUM(sum_matrix_C_vrfy)


    if (verbosity >= 2) then
       print "(4X,2(A,I))", "Results: Expected sum = ",sum_C_vrfy, &
                             " Actual sum_C = ",sum_C

       if (sum_C /= sum_C_vrfy) then
          print "(13X,A)", "Matrices Values"
          print "(13X,A,6X,A,7X,A)", "Element","Expected","Actual (matrix_C)"
          do m = 1,MAX_EXTENT_rowC
             do n = 1,MAX_EXTENT_colC
             if ( matrix_C_vrfy(m,n) /= matrix_C(m,n) ) then
                print "(14X,A,I0,A,I0,A,1X,I,6X,I)","(",m,",",n,")",matrix_C_vrfy(m,n),matrix_C(m,n)
             endif
          enddo
          enddo
       endif

       ! Display offload section details
       call offload_summary
    endif

    ! Validate results
    if ( offload_case > 1) then
       if ( (sum_C == sum_C_vrfy) .AND. &
             offload_verify(3) ) then
          print 2000, sample_name, offload_case
       else
          if (.NOT. offload_verify(1)) then
             err_str="offload failure"
          else
             err_str="data mismatch"
          endif
          print 2001, sample_name, offload_case, err_str
       endif
    else
       if (sum_C == sum_C_vrfy) then
          print 2000, sample_name, offload_case
        else
          err_str="data mismatch"
          print 2001, sample_name, offload_case, err_str
        endif
    endif

 2000  format(4X,"PASS ",A," (Case #",I0,")")
 2001  format(4X,"*** FAIL ",A," (Case #",I0,") : ",A)


    ! Cleanup
    if (ALLOCATED(matrix_C_vrfy)) deallocate(matrix_C_vrfy)
    if (ALLOCATED(sum_vector)) deallocate(sum_vector)
    if (ALLOCATED(sum_matrix_C_vrfy)) deallocate(sum_matrix_C_vrfy)
    if (ALLOCATED(sum_matrix_C)) deallocate(sum_matrix_C)

    return

    end subroutine leoF10_alloc_into_chk_results
!*.......................................................... leoF10_alloc_into
