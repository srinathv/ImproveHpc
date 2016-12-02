#! /bin/bash

icpc -g matmult_initialize.cpp -o matmult_initialize.o -c -ltbb -std=c++11
icpc -g matmult.cpp matmult_initialize.o -o matmult.exe -ltbb
