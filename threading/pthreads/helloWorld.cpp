// from https://www.archer.ac.uk/training/course-material/2014/05/AdvancedOpenMP_Oxford/Slides/L10-AlternativesToOpenMP.pdf
//
//
//

#include <pthread.h>
#define NTHREADS 5


  int i, threadnum[NTHREADS];
  pthread_t tid[NTHREADS];


void main() {
  for (i=0; i<NTHREADS; i++) {
   threadnum[i]=i;
   pthread_create(&tid[i], NULL, hello, &threadnum[i]);
  }
  for (i=0; i<NTHREADS; i++) {
   pthread_join(tid[i], NULL); }
}

   void* hello (void *arg) {
   int myid;

   myid = *(int *)arg;
   printf(“Hello world from thread %d\n”, myid);
   return (0);
  }
