# Makefile for mandelbrot area code

#
# C compiler and options 
#
CC=     cc -O1 -h omp
LIB=    -lm

#
# Object files
#
OBJ=    area.o

#
# Compile
#
area:   $(OBJ)
	$(CC) -o $@ $(OBJ) $(LIB)

.c.o:
	$(CC) -c $<

#
# Clean out object files and the executable.
#
clean:
	rm *.o area
