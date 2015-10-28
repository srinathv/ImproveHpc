#
# C compiler and options
#
CC = cc -h omp 

#
# Object files
#
OBJ=	stream_d.o second_wall.o

#
# Compile
#
stream_d:	$(OBJ)
	$(CC) -o $@ $(OBJ) 

.c.o:
	$(CC) -c $< 

#
# Clean out object files and the executable.
#
clean:
	rm *.o stream_d
