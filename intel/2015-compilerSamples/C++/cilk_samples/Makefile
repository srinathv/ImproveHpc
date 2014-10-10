CXX=icpc
#BITS=-m64
INCLUDES=-I.
CXXFLAGS=-O2 $(INCLUDES) $(BITS)

# If you are using a version of binutils prior to 2.17, 
# uncomment following line to work around a known issue. 
# please reference the Release Notes for more information. 
#LIBS=-lcilkrts

PROGS=linear-recurrence matrix matrix-multiply qsort qsort-mutex

all:: $(PROGS)

linear-recurrence: linear-recurrence.o
	$(CXX) $(BITS) -o $@ linear-recurrence.o $(LIBS)

matrix: matrix.o
	$(CXX) $(BITS) -o $@ matrix.o $(LIBS)

matrix-multiply: matrix-multiply.o
	$(CXX) $(BITS) -o $@ matrix-multiply.o $(LIBS)

qsort: qsort.o
	$(CXX) $(BITS) -o $@ qsort.o $(LIBS)

qsort-mutex: qsort-mutex.o
	$(CXX) $(BITS) -tbb -pthread -o $@ qsort-mutex.o $(LIBS)


clean::
	-rm -f $(PROGS) *.o
