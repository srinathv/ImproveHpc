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
C   This example computes real matrix C=alpha*A*B+beta*C using Intel(R) MKL 
C   subroutine DGEMM, where A,B, and C are matrices and alpha and beta are 
C   double precision scalars. 
C
C   In this simple example, practices such as memory management, data alignment, 
C   and I/O that are necessary for good programming style and high MKL 
C   performance are omitted to improve readability.
C*******************************************************************************

      PROGRAM   MAIN

      IMPLICIT NONE

      DOUBLE PRECISION ALPHA, BETA
      INTEGER          M, P, N, I, J
      PARAMETER        (M=2000, P=200, N=1000)
      DOUBLE PRECISION A(M,P), B(P,N), C(M,N)

      PRINT *, "This example computes real matrix C=alpha*A*B+beta*C"
      PRINT *, "using Intel(R) MKL function dgemm, where A, B, and C"
      PRINT *, "are matrices and alpha and beta are double precision "
      PRINT *, "scalars"
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

      PRINT *, "Computing matrix product using Intel(R) MKL DGEMM "
      PRINT *, "subroutine"
      CALL DGEMM('N','N',M,N,P,ALPHA,A,M,B,P,BETA,C,M)
      PRINT *, "Computations completed."
      PRINT *, ""

      PRINT *, "Top left corner of matrix A:"
      PRINT 20, ((A(I,J), J = 1,MIN(P,6)), I = 1,MIN(M,6))
      PRINT *, ""

      PRINT *, "Top left corner of matrix B:"
      PRINT 20, ((B(I,J),J = 1,MIN(N,6)), I = 1,MIN(P,6))
      PRINT *, ""

 20   FORMAT(6(F12.0,1x))

      PRINT *, "Top left corner of matrix C:"
      PRINT 30, ((C(I,J), J = 1,MIN(N,6)), I = 1,MIN(M,6))
      PRINT *, ""

 30   FORMAT(6(ES12.4,1x))

      PRINT *, "Example completed."
      STOP 

      END
