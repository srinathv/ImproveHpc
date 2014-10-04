
testProduct.f90:
  From Sean Sanot (9/30/14):
Hi Srinath,

Just look at the code below. It should print "1", since you have the product of a bunch of 1's, but for Intel 13 and 14, it prints "0" if you compile like so:

> ifort test.F90
> ifort -O2 test.F90

These two do not trigger the bug (program prints "1" as expected):

> ifort -O1 test.F90
> ifort -no-vec test.F90

- Sean
On Babbage 10/1/14:
-bash-4.1$ ifort testProduct.f90 -o testProduct
-bash-4.1$ ifort -O2 testProduct.f90 -o testProductO2
-bash-4.1$ ifort -O1 testProduct.f90 -o testProductO1
-bash-4.1$ ifort -no-vec testProduct.f90 -o testProductNoVec
-bash-4.1$ ifort -xHost testProduct.f90 -o testProductXhost
-bash-4.1$ ./testProductXhost
           1
-bash-4.1$ ./testProduct
           1
-bash-4.1$ ./testProductO1
           1
-bash-4.1$ ./testProductNoVec
           1
-bash-4.1$ ./testProductO2
           1
-bash-4.1$ ifort --version
ifort (IFORT) 15.0.0 20140723
Copyright (C) 1985-2014 Intel Corporation.  All rights reserved.

