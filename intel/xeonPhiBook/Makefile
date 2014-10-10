all: dhelloflops helloflops


dhelloflops: dhelloflops3.F90 
	ifort -mmic -align array64byte -openmp -vec-report=6 -O3 dhelloflops3.F90 -o dhelloflops

helloflops: helloflops3.f90 
	ifort -mmic -align array64byte -openmp -vec-report=6 -O3 helloflops3.f90 -o helloflops

novecDhelloflops: dhelloflops3.F90
	ifort -mmic -align array64byte -openmp -vec-report=6 -O3 -no-vec dhelloflops3.F90 -o novecDhelloflops
clean: 
	rm *flops
