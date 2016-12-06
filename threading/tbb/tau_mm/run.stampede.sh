source tau_source.stampede.this
#tau_exec -T knl,icpc,papi,mpi,tbb ./matmult_tbb_tau.exe
#mpirun -n 2 tau_exec -T knl,icpc,papi,mpi,tbb ./matmult_tbb_mpi_tau.exe
ibrun -n 2 -o 0 tau_exec -T knl,icpc,papi,mpi,tbb ./matmult_tbb_mpi_tau.exe
