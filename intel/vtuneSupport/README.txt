$ icc -g -c -I /opt/intel/vtune_amplifier_xe_2011/include api_fortran_itt.c

$ ifort -g -c itt_fortran.f90
$ ifort -g -c fortran_vtune.f90

$ ifort -g -o test_itt.ifort api_fortran_itt.o itt_fortran.mod fortran_vtune.o /opt/intel/vtune_amplifier_xe_2011/lib64/libittnotify.a

$ amplxe-cl -collect hotspots -start-paused -- ./test_itt.ifort

-----

If building for KNC then use the -mmic flag but also the general library to link is:

${VTUNE_DIR}/bin64/k1om/libittnotify.a or -L${VTUNE_DIR}/bin64/k1om/runtime -littnotify

---

Not that I can reduce the number of pauses and resumes by launching vtune collection with the "-start-paused" flag.

___
icc -g -mmic -c api_fortran_itt.c -I/global/babbage/nsg/opt/intel/vtune/vtune_amplifier_xe_2015.15.1-367959/include

ifort -g -c -mmic itt_fortran.f90
