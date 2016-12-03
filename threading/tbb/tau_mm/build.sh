#! /bin/bash

rm *.exe *.o
TBB=-D__USE_TBB
##TBB=
icpc -g matmult_initialize.cpp -o matmult_initialize.o -c 
icpc -g matmult.cpp matmult_initialize.o -o matmult.exe 

icpc -g matmult_initialize.cpp -o matmult_initialize_tbb.o -c -ltbb -std=c++11
icpc -g ${TBB} matmult.cpp matmult_initialize_tbb.o -o matmult_tbb.exe -ltbb -std=c++11

export TAU_MAKEFILE=/work/02463/srinathv/tau2/x86_64/lib/Makefile.tau-knl-icpc-papi-mpi-tbb
export TAU_OPTIONS="-optLinkOnly -optShared"

export PATH=/work/02463/srinathv/tau2/x86_64/bin:$PATH

tau_cxx.sh -g -D__USE_TAU ${TBB} matmult.cpp matmult_initialize_tbb.o -o matmult_tbb_tau.exe -ltbb -std=c++11

