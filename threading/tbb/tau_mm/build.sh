#! /bin/bash

rm *.exe *.o
TBB=-D__USE_TBB
##TBB=
icpc -g matmult_initialize.cpp -o matmult_initialize.o -c 
icpc -g matmult.cpp matmult_initialize.o -o matmult.exe 

icpc -g matmult_initialize.cpp -o matmult_initialize_tbb.o -c -ltbb -std=c++11
icpc -g ${TBB} matmult.cpp matmult_initialize_tbb.o -o matmult_tbb.exe -ltbb -std=c++11

