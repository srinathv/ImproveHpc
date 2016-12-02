#include "nomad_lib/nomad_deque.hpp"
#include <iostream>
#include <utility>

#include "tbb/tbb.h"
#include "tbb/scalable_allocator.h"
#include "tbb/tick_count.h"
#include "tbb/spin_mutex.h"
#include "tbb/concurrent_queue.h"
#include "tbb/pipeline.h"
#include "tbb/compat/thread"
#include <boost/format.hpp>

using namespace std;
using namespace tbb;


int main(int argc, char **argv) {

  // initialize TBB 
  tbb::task_scheduler_init init();

  // initialize MPI
  int numtasks, rank, hostname_len;
  char hostname[MPI_MAX_PROCESSOR_NAME];
  
  int mpi_thread_provided;
  MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &mpi_thread_provided);
  
  if (mpi_thread_provided != MPI_THREAD_MULTIPLE) {
    cerr << "MPI multiple thread not provided!!! " << mpi_thread_provided << "  " << MPI_THREAD_MULTIPLE << endl;
    return 1;
  }

  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &numtasks);
  MPI_Get_processor_name(hostname, &hostname_len);
    
  cout << boost::format("processor name: %s, number of tasks: %d, rank: %d\n") 
    % hostname % numtasks % rank;


  // run program for 10 seconds
  double RUN_SEC = 10;
  // size of message
  int MBUFSIZ = 100;

  tick_count start_time = tick_count::now();

  // receive thread: keep receiving messages from any sources
  thread receive_thread([&]() {
      int monitor_num = 0;
      double elapsed_seconds;

      int data_done;
      MPI_Status data_status;
      MPI_Request data_request;

      char recvbuf[MBUFSIZ];

      MPI_Irecv(recvbuf, MBUFSIZ, MPI_CHAR,
		MPI_ANY_SOURCE, 1, MPI_COMM_WORLD, &data_request);

      while(true) {
	elapsed_seconds = (tbb::tick_count::now() - start_time).seconds();
	
	if (monitor_num < elapsed_seconds + 0.5) {
	  cout << "rank: " << rank << ", receive thread alive" << endl;
	  monitor_num++;
	}

	if (elapsed_seconds > RUN_SEC + 5.0) {
	  break;
	}

	MPI_Test(&data_request, &data_done, &data_status);
	if (true == data_done) {
	  cout << "rank: " << rank << ", message received!" << endl;
	  MPI_Irecv(recvbuf, MBUFSIZ, MPI_CHAR,
		    MPI_ANY_SOURCE, 1, MPI_COMM_WORLD, &data_request);

	}

      }

      MPI_Cancel(&data_request);

      cout << "rank: " << rank << ", recv thread dying!" << endl;

      return;
    });
	
  // send thread: send one (meaningless) message to (rank + 1) every second
  thread send_thread([&]() {
      int monitor_num = 0;
      double elapsed_seconds;

      char sendbuf[MBUFSIZ];
      fill_n(sendbuf, MBUFSIZ, 0);

      while (true) {
	elapsed_seconds = (tbb::tick_count::now() - start_time).seconds();
	
	if (monitor_num < elapsed_seconds) {
	  cout << "rank: " << rank << ", start sending message" << endl;
	  monitor_num++;

	  MPI_Ssend(sendbuf, MBUFSIZ, MPI_CHAR, 
		    (rank + 1) % numtasks, 1, MPI_COMM_WORLD);

	  cout << "rank: " << rank << ", send successfully done!" << endl;

	}
      
	if (elapsed_seconds > RUN_SEC) {
	  break;
	}
      }

      cout << "rank: " << rank << ", send thread dying!" << endl;

      return;
    });
  
  receive_thread.join();
  send_thread.join();
  
  return 0;

}
