#
# Fortran compiler and options
#
FC=	ftn -h omp  
CC =    cc  -h omp

#
# Object files
#
OBJ=	stream_d.o second_wall.o

#
# Compile
#
stream_d:	$(OBJ)
	$(FC) -o $@ $(OBJ) 

.f.o:
	$(FC) -c $< 

#
# Clean out object files and the executable.
#
clean:
	rm *.o stream_d
