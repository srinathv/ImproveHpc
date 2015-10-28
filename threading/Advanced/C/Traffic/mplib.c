#include <stdio.h>
#include <mpi.h> 


void mpstart (int* numprocp , int* procidp, int* argcp, char*** argvp){ 


  MPI_Init(argcp,argvp);
  MPI_Comm_rank(MPI_COMM_WORLD,procidp);
  MPI_Comm_size(MPI_COMM_WORLD,numprocp);

}
  
void mpstop(void){ 
  
  MPI_Finalize();

}
 
void mpgsum(int* np){

  int tmp;
 
  MPI_Allreduce(np,&tmp,1,MPI_INT,MPI_SUM,MPI_COMM_WORLD);

  *np = tmp; 

} 

void mpsendrecv(int* destdatp, int destproc, int* srcbuffp, int srcproc){ 
  MPI_Status status;
    
  MPI_Sendrecv(destdatp,1,MPI_INT,destproc,1,
	       srcbuffp,1,MPI_INT,srcproc,1,MPI_COMM_WORLD,&status);

}
  
double gettime(void) { 

  return MPI_Wtime(); 

} 
