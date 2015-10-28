# Makefile for mandelbrot area code

#
# Fortran compiler and options
#
FC=	ftn -h omp 
CC=     cc

#
# Object files
#
OBJ=	 coherency.o second_wall.o 

#
# Compile
#
coherency:	$(OBJ)
	$(FC) -o $@ $(OBJ) 

.f.o:
	$(FC) -c $< 

#
# Clean out object files and the executable.
#
clean:
	rm *.o coherency
