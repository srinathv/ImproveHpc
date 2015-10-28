#include <stdio.h>

# define N 480000

void mpstart (int*, int*, int*, char***); 
void mpstop(void); 
void mpgsum(int *);
void mpsendrecv(int*, int, int*, int); 
double gettime(void); 

int initcell(int, float);
 
int cn[N+2],cnminus1[N+2]; 

int main(int argc, char **argv){

  int i, iter, nmove, ncars, nlocal, icell, cellval; 
  int procid, procup, procdn, numproc, maxiter, sumfreq; 

  float  density, rng; 
  double begin, end;

  maxiter = 200; 
  sumfreq = maxiter/10; 

  // Set target density of cars

  density = 0.503;

  //Initialise message passing system, get process id and number of processes

  mpstart(&numproc, &procid, &argc, &argv); 

  if (procid == 0) { 
    printf("Length of road is %d\n", N);
    printf("Running  on %d processes\n", numproc);
    printf("Number of iterations is %d \n", maxiter);
    printf("Target density of cars is %f \n", density);
  }

  // Compute local size of road and check it is OK

  nlocal = N/numproc; 

  if (nlocal*numproc != N) { 
    if (procid == 0) {
      printf("ERROR: road size of %d is not a multiple of number of processes%d\n", N, numproc);
    }
    mpstop(); 
  }

  // Find neighbours up and down

  procup = (procid+1)%numproc; 
  procdn = (procid+numproc-1)%numproc;

  // Initialise road accordingly using random number generator

    if (procid == 0) {
      printf("Initialising ...\n");
    }
  
    ncars = 0;
    icell = procid*nlocal;

    // Set the value based on the global cell number for reproducibility   
    for (i=1; i<=nlocal; i++) {
      icell++; 
      cnminus1[i] = initcell(icell,density); 
      ncars += cnminus1[i]; 
    } 

    if (procid == 0) {
      printf("...done\n\n");
    }
  
    //Sum the number of cars across processes

    mpgsum(&ncars);

    if (procid == 0) {    
      printf("Actual density of cars is %f\n", (float) ncars / (float) N);
      begin = gettime(); 
    } 

    for (iter=1; iter<=maxiter; iter++) { 



      nmove = 0; 

      // Serial code would simply be:
      //
      // cnminus1[0]   = cnminus1[N];
      // cnminus1[N+1] = cnminus1[1];

      // First send element nlocal upwards and receive element 0 downwards

      mpsendrecv(&cnminus1[nlocal], procup, &cnminus1[0], procdn);

      //  Now send element 1 downwards and receive element nlocal+1 upwards

      mpsendrecv(&cnminus1[1], procdn, &cnminus1[nlocal+1], procup);

      // Apply CA rules to all local cells

      for (i=1; i<=nlocal; i++) {
	
	if (cnminus1[i] == 1) {
 
	  if (cnminus1[i+1] == 1) {
	    cn[i] = 1;  
	  }
	  else {
	    cn[i] = 0; 
	    nmove ++; 
	  }
	    
	}
	else {

	  if (cnminus1[i-1] == 1) { 
	    cn[i] = 1;  
	  }
	  else {
	    cn[i] = 0; 
	  }

	}

      }

      // Copy new to old array

      for (i=1; i<=nlocal; i++) {
	cnminus1[i] = cn[i]; 
      }


      if (iter%sumfreq == 0) {

      //  Globally sum the number of moves

	mpgsum(&nmove); 

      }

    }

    if (procid == 0) {   
      end = gettime();
      printf("\nFinished\nTime = %f\n",(end-begin)); 
      printf("At iteration %d average velocity is %f \n", iter, (float) nmove / (float) ncars);
    }

    //Finalise

    mpstop();
      
} 


int initcell(int index, float density) { 

  int i;
  long long res,aa,mm; 
  float rng;

  aa = 16807; 
  mm = 2147483647;
 
  res = index*aa; 
  for (i=0; i<20; i++) {
    res = (res*aa)%mm;
  }
  rng = (float) res/ (float) mm; 

  if (rng < density) {
    return(1);
  } 
  else {
    return(0);
  }
 

  
} 
