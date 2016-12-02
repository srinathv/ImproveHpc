#include <stdio.h>
#include "matmult_initialize.h"
#include "tbb/tbb.h"

void initialize(double **matrix, int rows, int cols) {
  int i,j;
#pragma omp parallel private(i,j) shared(matrix)
  {
    //set_num_threads();
    /*** Initialize matrices ***/
  printf ("initializing matricies \n");
#pragma omp for nowait
    for (i=0; i<rows; i++) {
      for (j=0; j<cols; j++) {
//      tbb::parallel_for (0, cols, [=](int i) {
        matrix[i][j]= i+j;
//      });
      }
    }
  }
}

