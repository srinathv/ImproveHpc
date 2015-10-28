#include <stdio.h>
#include <math.h>
#include <omp.h>


#define N 64
#define M 1000
#define REPS 10000

void sumsubfs(int); 
double second(void); 

float x[M][N];


int main(int argv, char **argc){

  int i,j,c;
  double time1, time2; 


  for (i=0;i<M;i++) {
    for (j=0;j<N;j++) {
      x[i][j] = 3.142; 
    }
  }


  printf("Offset    Mflop/s\n");

  for (c=0;c<N;c++){
    time1=second();
    for (i=0;i<REPS;i++){
      sumsubfs(c); 
    }
    if (x[0][0] < 0.0) printf("Help!\n"); 
    time2=second();

    printf("%d        %f\n",c-20,((float)(M*REPS))/(1000000.*(time2-time1)));
  }
	   
}


void sumsubfs(int c){

      int i,id;

#pragma omp parallel private(i,id)
      {
        id = omp_get_thread_num(); 
	if (id==0) {
	  
	  for (i=0;i<M;i++){
	    x[i][20]+=3.142;
	  }
	}
	else {
	  if (id==1) {
	    
	    for (i=0;i<M;i++){
	      x[i][c]+=3.142;
	    }

	  }
	}
      }
  

}

