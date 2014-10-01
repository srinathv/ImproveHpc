$ icc -g -c -I /opt/intel/vtune_amplifier_xe_2011/include api_fortran_itt.c

$ ifort -g -c fortran_vtune.f90

$ ifort -g -o test_itt.ifort api_fortran_itt.o fortran_vtune.o /opt/intel/vtune_amplifier_xe_2011/lib64/libittnotify.a

$ amplxe-cl -collect hotspots -start-paused -- ./test_itt.ifort
