# Makefile for mandelbrot area code

#
# C compiler and options
#
CC=   cc  -h omp 

#
# Object files
#
OBJ=	 coherency.o second_wall.o 

#
# Compile
#
coherency:	$(OBJ)
	$(CC) -o $@ $(OBJ) 

.c.o:
	$(CC) -c $< 

#
# Clean out object files and the executable.
#
clean:
	rm *.o coherency
