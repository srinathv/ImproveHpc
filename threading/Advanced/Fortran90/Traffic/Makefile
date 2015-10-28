.SUFFIXES: .f90 .o  
# Set up the flags you want in FFLAGS and compile with compiler specified in FC
# All objects will be compiled with the options specified in FFLAGS
#
FFLAGS =  -h omp 
FC = ftn
#
#
# Declare all the separate files in one as OBJ
# These are the files created by fsplit and are what the executable depends on
#
OBJ = mpca.o mplib.o 
#
##########################################################
#
# Rules for making executable and cleaning directory
#
# This rule says that the executable md depends on the object files in OBJ
# and that they need to be complied then linked to produce the executable.
#
mpca:	$(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ)  
.f90.o:
	$(FC) $(FFLAGS) -c $<
#
# This is a rule for cleaning out object files and the executable.
# If the comand "make clean" is typed it will work
#
clean:
	rm *.o mpca
#


