#! /bin/bash

icpc -g matmult_initialize.c -o matmult_initialize.o -c -ltbb -std=c++11
icpc -g matmult.c matmult_initialize.o -o matmult.exe -ltbb
