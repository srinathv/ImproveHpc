C*******************************************************************************
C   Copyright(C) 2012 Intel Corporation. All Rights Reserved.
C   
C   The source code, information  and  material ("Material") contained herein is
C   owned  by Intel Corporation or its suppliers or licensors, and title to such
C   Material remains  with Intel Corporation  or its suppliers or licensors. The
C   Material  contains proprietary information  of  Intel or  its  suppliers and
C   licensors. The  Material is protected by worldwide copyright laws and treaty
C   provisions. No  part  of  the  Material  may  be  used,  copied, reproduced,
C   modified, published, uploaded, posted, transmitted, distributed or disclosed
C   in any way  without Intel's  prior  express written  permission. No  license
C   under  any patent, copyright  or  other intellectual property rights  in the
C   Material  is  granted  to  or  conferred  upon  you,  either  expressly,  by
C   implication, inducement,  estoppel or  otherwise.  Any  license  under  such
C   intellectual  property  rights must  be express  and  approved  by  Intel in
C   writing.
C   
C   *Third Party trademarks are the property of their respective owners.
C   
C   Unless otherwise  agreed  by Intel  in writing, you may not remove  or alter
C   this  notice or  any other notice embedded  in Materials by Intel or Intel's
C   suppliers or licensors in any way.
C
C*******************************************************************************

C*******************************************************************************
C   This example demonstrates threading impact on computing real matrix product 
C   C=alpha*A*B+beta*C using Intel(R) MKL subroutine DGEMM, where A, B, and C 
C   are matrices and alpha and beta are double precision scalars. 
C
C   In this simple example, practices such as memory management, data alignment, 
C   and I/O that are necessary for good programming style and high MKL 
C   performance are omitted to improve readability.
C*******************************************************************************

      PROGRAM   MAIN

      IMPLICIT NONE

      DOUBLE PRECISION ALPHA, BETA
      INTEGER          I, J, K, M, P, N, R, LOOP_COUNT, MAX_THREADS
C     Consider adjusting LOOP_COUNT based on the performance of your 
C     computer to make sure that total run time is at least 1 second
      PARAMETER        (M=2000, P=200, N=1000, LOOP_COUNT=10)
      DOUBLE PRECISION A(M,P), B(P,N), C(M,N)
      DOUBLE PRECISION S_INITIAL, S_ELAPSED
      DOUBLE PRECISION DSECND
      INTEGER MKL_GET_MAX_THREADS

      PRINT *, "This example demonstrates threading impact on computing"
      PRINT *, " real matrix product C=alpha*A*B+beta*C using "
      PRINT *, "Intel(R) MKL function dgemm, where A, B, and C are"
      PRINT *, "matrices. alpha and beta are double precision scalars "
      PRINT *, ""

      PRINT *, "Initializing data for matrix multiplication C=A*B for "
      PRINT 10, " matrix A(",M," x",P, ") and matrix B(", P," x", N, ")"
10    FORMAT(a,I5,a,I5,a,I5,a,I5,a)
      PRINT *, ""
      ALPHA = 1.0 
      BETA = 0.0

      PRINT *, "Intializing matrix data"
      PRINT *, ""
      DO I = 1, M
        DO J = 1, P
          A(I,J) = (I-1) * P + J
        END DO
      END DO

      DO I = 1, P
        DO J = 1, N
          B(I,J) = -((I-1) * N + J)
        END DO
      END DO

      DO I = 1, M
        DO J = 1, N
          C(I,J) = 0.0
        END DO
      END DO

      PRINT *, "Finding max number of threads Intel(R) MKL can use for"
      PRINT *, "parallel runs"
      PRINT *, ""
      MAX_THREADS = MKL_GET_MAX_THREADS()

      PRINT 20," Running Intel(R) MKL from 1 to ",MAX_THREADS," threads"
 20   FORMAT(A,I2,A)
      PRINT *, ""
      DO K = 1, MAX_THREADS
        DO I = 1, M
          DO J = 1, N
            C(I,J) = 0.0
          ENDDO
        ENDDO

        PRINT 30, " Requesting Intel(R) MKL to use ",K," thread(s)"
 30     FORMAT(A,I2,A)
        CALL MKL_SET_NUM_THREADS(K)

        PRINT *, "Making the first run of matrix product using "
        PRINT *, "Intel(R) MKL DGEMM subroutine to get stable "
        PRINT *, "run time measurements"
        PRINT *, ""
        CALL DGEMM('N','N',M,N,P,ALPHA,A,M,B,P,BETA,C,M)

        PRINT *, "Measuring performance of matrix product using "
        PRINT 40, " Intel(R) MKL DGEMM subroutine on ",K," thread(s)"
 40     FORMAT(A,I2,A)
        PRINT *, ""
        S_INITIAL = DSECND()
        DO R = 1, LOOP_COUNT
          CALL DGEMM('N','N',M,N,P,ALPHA,A,M,B,P,BETA,C,M)
        END DO
        S_ELAPSED = (DSECND() - S_INITIAL) / LOOP_COUNT

        PRINT *, "== Matrix multiplication using Intel(R) MKL DGEMM =="
        PRINT 50, " == completed at ",S_ELAPSED*1000," milliseconds =="
        PRINT 60, " == using ",K," thread(s) =="
 50     FORMAT(A,F12.5,A)
 60     FORMAT(A,I2,A)
        PRINT *, ""
      END DO

      IF (S_ELAPSED < 0.9/LOOP_COUNT) THEN
      S_ELAPSED=1.D0/LOOP_COUNT/S_ELAPSED
      K=(S_ELAPSED*LOOP_COUNT)+1;
      PRINT *, "It is highly recommended to parameter LOOP_COUNT for"
        PRINT *, "this example on your computer as ",K," to have total "
        PRINT *, "execution time about 1 second for reliability"
      PRINT *, "of measurements"
        PRINT *, ""
      ENDIF

      PRINT *, "Example completed."
      PRINT *, ""
      STOP 

      END
