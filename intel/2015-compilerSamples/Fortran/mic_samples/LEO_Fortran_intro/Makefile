## Copyright (C) 2012-2014 Intel Corporation. All Rights Reserved.
##
## The source code contained or described herein and all
## documents related to the source code ("Material") are owned by
## Intel Corporation or its suppliers or licensors. Title to the
## Material remains with Intel Corporation or its suppliers and
## licensors. The Material is protected by worldwide copyright
## laws and treaty provisions.  No part of the Material may be
## used, copied, reproduced, modified, published, uploaded,
## posted, transmitted, distributed,  or disclosed in any way
## except as expressly provided in the license provided with the
## Materials.  No license under any patent, copyright, trade
## secret or other intellectual property right is granted to or
## conferred upon you by disclosure or delivery of the Materials,
## either expressly, by implication, inducement, estoppel or
## otherwise, except as expressly provided in the license
## provided with the Materials.
##
##
##******************************************************************************
## Content:
##
##  Build for Sample LEO_Fortran_intro
##******************************************************************************

NAME		=	leoF_intro
FC		=	ifort
LD		=	ifort
DEBUG		+=		
FFLAGS		+=	-qopenmp $(DEBUG)
LDFLAGS		+=	

EXE		=	$(NAME).out
fFILES		=	$(wildcard *.f)
f90FILES	=	$(wildcard *.f90)
F90FILES	=	$(wildcard *.F90)
MODFILES	=	$(wildcard *.mod)
INCFILES	=	$(wildcard *.inc)
OBJECTS		=	$(fFILES:.f=.o) $(f90FILES:.f90=.o) $(F90FILES:.F90=.o)

help:
	@echo "Usage:"
	@echo "    Build : make mic"
	@echo "    Run   : leoF_intro.out [--help]"
	@echo 

mic : $(EXE)
	@echo "...."
	@echo "Build complete"
	@echo "Run : leoF_intro.out [--help]"

mic_debug : DEBUG	+= -g -traceback -O0
mic_debug : mic

no_mic : FFLAGS		+= -qoffload=none
no_mic : mic

$(EXE) : $(OBJECTS)
	$(LD) -V $^ $(FFLAGS) $(LDFLAGS) -o $@

%.o : %.f 
	$(FC) $(FFLAGS) -c $< -o $@

%.o : %.f90
	$(FC) $(FFLAGS) -c $< -o $@

%.o : %.F90
	$(FC) $(FFLAGS) -c $< -o $@
clean :	
	rm -fr $(EXE) \
	$(OBJECTS) $(OBJECTS:.o=MIC.o) \
	$(MODFILES) $(MODFILES:.mod=.modmic)
