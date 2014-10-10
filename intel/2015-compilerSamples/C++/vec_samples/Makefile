CXX=icc
INCLUDES=-I.
CXXFLAGS=-c -O2 -xSSE3 -vec-report2 $(INCLUDES) $(BITS)

PROGS=Driver Multiply

default: Driver.o Multiply.o
	$(CXX) Driver.o Multiply.o -o default $(LIBS)

Driver.o: Driver.c
	$(CXX) $(BITS) $(CXXFLAGS) -o $@ Driver.c

Multiply.o: Multiply.c
	$(CXX) $(BITS) $(CXXFLAGS) -o $@ Multiply.c

clean::
	-rm -f $(PROGS) *.o default
