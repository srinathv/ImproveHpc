# Build tutorial that demonstrates use of the -guide compiler option for Guided
# Autoparallelism compiler feature which will provide guidance on how to make 
# changes to the program to allow the compiler to autoparallel or autovectorize
# loops in the program

all:  par vec w_changes

par: 
	ifort -c -parallel -guide scalar_dep.f90
	ifort -parallel main.f90 scalar_dep.f90 -o par
	@echo

gap_par_report: 
	ifort -c -parallel -guide scalar_dep.f90
	ifort -parallel main.f90 scalar_dep.f90 -o par
	@echo

vec:
	ifort -c -guide scalar_dep.f90
	ifort main.f90 scalar_dep.f90 -o vec
	@echo

w_changes:
	ifort -c -parallel -Dtest_gap -opt-report=2 -opt-report-phase=par -opt-report-file=stdout scalar_dep.f90
	ifort -parallel main.f90 scalar_dep.o -o w_changes
	@echo

run:  par vec w_changes
	./par
	./vec
	./w_changes

clean:
	@rm -f *.o *.mod par vec w_changes

